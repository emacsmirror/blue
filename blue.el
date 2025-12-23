;;; blue.el --- BLUE build system interface -*- lexical-binding: t -*-
;;
;; Copyright © 2025 Sergio Pastor Pérez
;;
;; Author: Sergio Pastor Pérez <sergio.pastorperez@gmail.com>
;; Version: 0.0.21
;; Package-Requires: ((emacs "30.1") (magit-section "4.3.8"))
;; Keywords: blue, tools
;; URL: https://codeberg.org/lapislazuli/blue.el

;;; Commentary:

;; Interface for the BLUE build system, this package provides a completion
;; interface to ease the dispatch of BLUE project commands.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'compile)
(require 'crm)
(require 'outline)
(require 'pcomplete)
(require 'seq)


;;; Configuration.

(defgroup blue nil
  "Operations on the current project."
  :group 'tools)

(defcustom blue-binary "blue"
  "Name to use to call the BLUE."
  :group 'blue
  :type 'string)

(defcustom blue-interactive-commands '("repl")
  "List of strings of interactive BLUE commands.
Interactive commands will run in comint mode compilation buffers."
  :group 'blue
  :type '(repeat string))

(defcustom blue-cache-file
  (locate-user-emacs-file "blue.eld")
  "File in which to save the list of known BLUE caches."
  :type 'file
  :group 'blue)

(defcustom blue-require-build-directory nil
  "Whether to ensure that BLUE commands run under a known build directory.

This makes `blue-run-command' prompt for a build directory if no build
directory has been stored in the cache."
  :group 'blue
  :type 'boolean)

(defcustom blue-default-options nil
  "String or list of strings passed as arguments to BLUE."
  :group 'blue
  :type '(choice
          (const :tag "None" nil)
          (string :tag "Single string")
          (repeat :tag "List of strings" string)))

(defcustom blue-annotation-padding 8
  "Padding used for alignment of synopsis in completing read."
  :type 'integer
  :group 'blue)


;;; Faces.

(defface blue-documentation
  '((t :inherit completions-annotations))
  "Face used to highlight documentation strings.")

(defface blue-hint-separator
  '((t :inherit completions-group-separator :weight regular))
  "Face used to separate hint overlay.")

(defface blue-hint-index
  '((t :inherit font-lock-keyword-face/regular))
  "Face used to draw attention in hint overlay.")

(defface blue-hint-highlight
  '((t :inherit completions-common-part))
  "Face used to draw attention in hint overlay.")

(defface blue-hint-faded
  '((t :inherit shadow :weight regular))
  "Face used to display inactive items in hint overlay.")


;;; Internal Variables.

(defvar blue--blueprint nil
  "Current blueprint being processed.")

(defvar blue--data nil
  "Current blueprint data.")

(defvar blue--cache-list nil
  "List structure containing directories of known BLUE caches for project.")

(defvar blue--build-dir nil
  "Path to last known build directory.")

(defvar-local blue--search-path nil
  "Directories where to search for files.

These directories are used for extending `compilation-search-path' and
locating BLUE replay file references.")

(defvar blue--overiden-build-dir nil
  "Internal variable used to override `blue--build-dir'.

This is used when passing universal prefix argument `C-u' to
`blue-run-command'.")

(defvar blue--buffer-name-function #'blue--default-buffer-name
  "Function used by BLUE to name the compilation buffer.")

;; NOTE: this buffer cannot be hidden since `font-lock-mode' disables itself for
;; hidden buffers. The fontifications is used to propertize ANSI color
;; sequences.
(defvar blue--log-buffer "*blue log*"
  "Buffer used to capture output from BLUE commands.")

(defvar blue--hint-overlay nil
  "Overlay for minibuffer hints.")


;;; Utilities.

(defvar-keymap blue-log-mode-map
  :doc "Keymap for `blue-replay-mode'."
  :parent outline-mode-map
  "q" #'quit-window
  "n" #'outline-next-visible-heading
  "p" #'outline-previous-visible-heading)

(define-derived-mode blue-log-mode outline-mode "Blue-log"
  "Mode for looking at BLUE logs."
  :interactive nil
  :group 'blue
  (setq-local outline-regexp "▶")
  (read-only-mode 1))

(defun blue--get-log-buffer ()
  "Return `blue--log-buffer'."
  (let ((buf (get-buffer blue--log-buffer)) ; Already existing.
        (buf* (get-buffer-create blue--log-buffer)))
    (unless buf
      (with-current-buffer buf*
        (blue-log-mode)))
    buf*))

(defun blue--get-build-dir ()
  "Return `blue--build-dir' if it exists, nil otherwise."
  (if (and blue--build-dir
           (file-exists-p blue--build-dir))
      blue--build-dir
    nil))

(defun blue--check-blue-binary ()
  "Check if `blue-binary' is in PATH."
  (let ((bin (executable-find blue-binary)))
    (unless bin
      (let* ((prefix (propertize "[ERROR] " 'face 'error))
             (msg (concat (propertize "`blue'" 'face 'font-lock-constant-face)
                          " command not found in "
                          (propertize "`exec-path'" 'face 'font-lock-type-face)))
             (msg* (concat prefix msg)))
        (blue--log-output (concat msg* "\n") "executable-find" 1)
        (error msg*)))
    bin))

(defun blue--set-default-directory (dir)
  "Helper to safely set the `default-directory' to DIR."
  (when (and (stringp dir) (file-exists-p dir))
    (setq default-directory dir)))

;; TODO: should we use 'blue' to locate the 'blueprint.scm'?
(defun blue--find-blueprint (&optional path)
  "Return path to top-level `blueprint.scm'.
If PATH is non-nil, locate `blueprint.scm' from PATH."
  (if-let* ((blueprint (locate-dominating-file (or path default-directory)
                                               "blueprint.scm")))
      ;; This expands resolving symlinks. It's needed since in the build
      ;; directory ther can be a symlinked 'blueprint.scm' for projects that
      ;; require configuration.
      (file-truename
       (directory-file-name
        (concat blueprint "/blueprint.scm")))
    (error (concat "[BLUE] Failed to locate `blueprint.scm'"
                   (when path
                     (concat " in " path))))))

(defun blue--normalize-options (options)
  "Normalize OPTIONS to a list of strings."
  (cond
   ((null options) nil)
   ((stringp options) (string-split options))
   ((listp options) options)
   (t (list (format "%s" options)))))

(defun blue--file-safe-p (file)
  "Return t if FILE exists and is readable."
  (and file (file-exists-p file) (file-readable-p file)))


;;; Memoization.

(defun blue--memoize (function)
  "Return a memoized version of FUNCTION."
  (let ((cache (make-hash-table :test 'equal)))
    (lambda (&rest args)
      (let ((result (gethash args cache 'cache-miss)))
        (if (eq result 'cache-miss)
            (let ((result (apply function args)))
              (puthash args result cache)
              result)
          result)))))

(defmacro blue--define-memoized (name arglist docstring &rest body)
  "Define a memoized function NAME.
See `defun' for the meaning of NAME ARGLIST DOCSTRING and BODY."
  (declare (doc-string 3) (indent 2))
  `(defalias ',name
     (blue--memoize (lambda ,arglist ,@body))
     ,(format "(%S %s)\n\n%s"
              name
              (mapconcat #'symbol-name arglist " ")
              docstring)))


;;; Logging.

(defun blue--format-header (command)
  "Format COMMAND header for output buffer.

TIME is the timestamp of the header."
  (propertize (format "▶ %s  [%s]\n" command (current-time-string))
              'font-lock-face 'bold))

(defun blue--format-footer (exit-code)
  "Format status footer for output buffer.

EXIT-CODE displays the status of the command."
  (propertize (format "⏹ Exit status: %s\n\n" exit-code)
              'font-lock-face (if (= exit-code 0)
                                  'success
                                'error)))

(defun blue--log-output (output &optional command exit-code)
  "Log OUTPUT string to `blue--log-buffer'.

COMMAND is the command string that generated OUTPUT.
EXIT-CODE is the return value of CMD."
  (with-current-buffer (blue--get-log-buffer)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (when command (insert (blue--format-header command)))
        (insert output)
        (unless (equal (point) (line-beginning-position))
          (insert "\n"))
        (when exit-code (insert (blue--format-footer exit-code)))))))


;;; Cache.

(defun blue--read-cache ()
  "Initialize `blue--cache-list' from `blue-cache-file'."
  (when (blue--file-safe-p blue-cache-file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents blue-cache-file)
          (setq blue--cache-list (read (current-buffer))))
      (error
       (message "[BLUE] Failed to read cache file: %s" (error-message-string err))
       (setq blue--cache-list nil)))))

(defun blue--write-cache ()
  "Save `blue--cache-list' to `blue-cache-file'."
  (condition-case err
      (with-temp-buffer
        (insert ";;; -*- lisp-data -*-\n")
        (let ((print-length nil)
              (print-level nil))
          (pp blue--cache-list (current-buffer)))
        (write-region nil nil blue-cache-file nil 'silent))
    (error
     (message "[BLUE] Failed to write cache file: %s" (error-message-string err)))))

(defun blue--sanitize-cache ()
  "Remove non-existent directories from cache list."
  (setq blue--cache-list
        (delq nil
              (mapcar (lambda (entry)
                        (let ((project (car entry))
                              (build-dirs (cadr entry)))
                          (when (file-exists-p project)
                            (list project (seq-filter #'file-exists-p build-dirs)))))
                      blue--cache-list))))

(defun blue--ensure-cache ()
  "Initialize cache if needed and sanitize it."
  (blue--read-cache)
  (when blue--cache-list
    (blue--sanitize-cache)))

(defun blue--cache-add (dir &optional no-save)
  "Add DIR to cache under its project root.
If NO-SAVE is non-nil, don't save to disk immediately."
  (blue--ensure-cache)
  (let* ((blueprint (blue--find-blueprint dir))
         (dir (directory-file-name (expand-file-name dir)))
         (existing-configs (blue--cache-get-build-dirs blueprint))
         (updated-configs (cons dir (delete dir existing-configs)))
         (updated-cache (cons (list blueprint updated-configs)
                              (seq-remove (lambda (entry)
                                            (string-equal blueprint (car entry)))
                                          (or blue--cache-list nil)))))
    (setq blue--cache-list updated-cache))
  (unless no-save
    (blue--write-cache)))

(defun blue--cache-get-build-dirs (dir)
  "Get cached build directories for project containing DIR."
  (let ((blueprint (blue--find-blueprint dir)))
    (cadr (assoc-string blueprint blue--cache-list))))


;;; Deserialization.

(defun blue--execute-deserialize (options commands)
  "Execute BLUE serialization COMMANDS with OPTIONS and return parsed output.

If RAW is non nil, the serialized string will not be evaluated."
  (let* ((default-directory (or (blue--get-build-dir) default-directory))
         (process-environment (cons "GUILE_AUTO_COMPILE=0" process-environment))
         (args (append (or options '()) (cons "--color=always" commands)))
         (command-string (string-join (cons blue-binary args) " "))
         exit-code
         (raw-output (with-output-to-string
                       (setq exit-code
                             (apply #'call-process blue-binary nil standard-output nil args))))
         (propertized-output (ansi-color-apply raw-output))
         (filtered-output (replace-regexp-in-string ";;;.*\n?" "" raw-output)))
    (blue--log-output propertized-output command-string exit-code)
    (cons filtered-output exit-code)))

(defun blue--get-data (blueprint)
  "Return the commands and execution environment provided by BLUEPRINT."
  (when-let* ((options (when blueprint
                         (list (concat "--file=" blueprint))))
              (cmd '(".serialize-commands" "--" ".serialize-execution-environment"))
              (output (blue--execute-deserialize options cmd))
              (data (car output))
              (exit-code (cdr output)))
    (if (zerop exit-code)
        (with-temp-buffer
          (insert data)
          (goto-char (point-min))
          (condition-case err
              (re-search-forward "\\({.*}\\)\\({.*}\\)")
            (search-failed
             (blue--log-output (error-message-string err) "[BLUE] `re-search-forward'" 1)
             (with-current-buffer blue--log-buffer
               (goto-char (point-min)))
             (display-buffer blue--log-buffer)
             (error "[BLUE] Deserialization failed")))
          (let ((json1 (match-string-no-properties 1))
                (json2 (match-string-no-properties 2)))
            (cons (json-parse-string json1
                                     :object-type 'alist
                                     :array-type 'list
                                     :null-object nil
                                     :false-object nil)
                  (json-parse-string json2
                                     :object-type 'alist
                                     :array-type 'list
                                     :null-object nil
                                     :false-object nil))))
      (with-current-buffer blue--log-buffer
        (goto-char (point-min)))
      (display-buffer blue--log-buffer)
      (error "[BLUE] Deserialization failed"))))

(defun blue--config-get (var config)
  "Retrieve variable VAR value from CONFIG."
  (cdr (assoc-string var config)))

(defun blue--get-command (command commands)
  "Retrieve COMMAND from COMMANDS."
  (assoc command commands))

(defun blue--command-get-slot (slot command)
  "Retrieve SLOT from COMMAND."
  (let ((fields (alist-get 'fields command)))
    (alist-get slot fields)))

(defun blue--get-command-invocations (commands)
  "Retrieve command names from COMMANDS."
  (mapcar (lambda (command)
            (symbol-name (car command)))
          commands))

(defun blue--get-command-categories (commands)
  "Retrieve command categories from COMMANDS."
  (seq-uniq (mapcar (lambda (command)
                      (blue--command-get-slot 'category command))
                    commands)))

(defun blue--get-option-long-labels (option)
  "Retrieve long lable from OPTION."
  (when-let* ((labels (alist-get 'labels option))
              (long-labels (alist-get 'long labels)))
    long-labels))

(defun blue--get-option-from-label (label command)
  "Retrieve option from COMMAND that matches LABEL."
  (let ((options (blue--command-get-slot 'options command)))
    (seq-find (lambda (option)
                (let ((long-labels (blue--get-option-long-labels option)))
                  (member label long-labels)))
              options)))


;;; Compilation.

(defun blue--default-buffer-name (command name-of-mode)
  "Default buffer naming function for compilation buffers.

COMMAND is the invocation passed to BLUE.
NAME-OF-MODE is the major mode name that the compilation buffer will use."
  (format "*%s | %s*" name-of-mode command))

(defun blue--setup-buffer (buffer)
  "Setup compilation BUFFER with DIR and error patterns."
  (with-current-buffer buffer
    (make-local-variable 'compilation-error-regexp-alist)
    (add-to-list 'compilation-error-regexp-alist
                 '("^.* at \\(.*?\\):\\([0-9]+\\)" 1 2))
    ;; Bound dynamicaly for the context of this function, let's write it buffer
    ;; locally so it persists after the dynamic context ends.
    (setq default-directory default-directory)))

(defun blue--set-search-path ()
  "Set current buffer search path.

This is meant to be used in compilation buffers."
  (when-let* ((conf (cdr blue--data))
              (srcdir (blue--config-get "srcdir" conf)))
    ;; Make 'srcdir' errors searchable in compilation buffer.
    (setq-local blue--search-path (seq-uniq (cons srcdir compilation-search-path))
                compilation-search-path blue--search-path)))

(defun blue--compile (command &optional comint-p)
  "Compile COMMAND with BLUE-specific setup.
COMINT-P selects `comint-mode' for compilation buffer."
  (interactive
   (list
    (let ((command (eval compile-command)))
      (if (or compilation-read-command current-prefix-arg)
          (compilation-read-command command)
        command))
    (consp current-prefix-arg)))

  (unless (equal command (eval compile-command))
    (setq compile-command command))

  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)

  (let* ((mode (or comint-p 'compilation-mode))
         (name-of-mode (if (eq mode t)
                           "compilation"
                         (replace-regexp-in-string "-mode\\'" "" (symbol-name mode))))
         (compilation-buffer-name-function (lambda (name-of-mode)
                                             (funcall #'blue--default-buffer-name
                                                      command name-of-mode)))
         (buf (get-buffer-create
               (compilation-buffer-name name-of-mode comint-p compilation-buffer-name-function)))
         (default-directory (or (blue--get-build-dir) default-directory)))
    (setq-default compilation-directory default-directory)
    (blue--setup-buffer buf)
    (compilation-start command comint-p)
    ;; Make 'srcdir' errors searchable in compilation buffer.
    (with-current-buffer buf
      (blue--set-search-path))))


;;; Command Analysis.

(defun blue--parse-invocations (input-tokens)
  "Parse invocation names from INPUT-TOKENS."
  (let ((first-invoke (car (last (car input-tokens))))
        (rest-invokes (mapcar #'car (cdr input-tokens))))
    (cons first-invoke rest-invokes)))

(defun blue--find-command-entries (invocations commands)
  "Find command entries for INVOCATIONS in COMMANDS list."
  (mapcar (lambda (invoke)
            (seq-find (lambda (cmd)
                        (string= invoke (alist-get 'invoke cmd)))
                      commands))
          invocations))

(defun blue--interactive-p (command)
  "Return t if COMMAND is a member of `blue-interactive-commands'."
  (when (member command blue-interactive-commands)
    t))

(defun blue--any-interactive-p (input-tokens)
  "Return t if an interactive command is part of INPUT-TOKENS.

A comand is considered interactive if it is a member of
`blue-interactive-commands'."
  (let ((invocations (blue--parse-invocations input-tokens)))
    (seq-some #'blue--interactive-p invocations)))


;;; Command Prompt.

(defun blue--create-group-fn (commands)
  "Create group function for COMMANDS."
  (lambda (candidate transform)
    (if transform
        candidate
      (when-let* ((invocation (intern candidate))
                  (command (blue--get-command invocation commands)))
        (blue--command-get-slot 'category command)))))

(defun blue--command-completion-properties (commands)
  "Create completion properties for COMMANDS and INVOCATIONS."
  (when-let* ((invocations (blue--get-command-invocations commands))
              (max-width (apply #'max (mapcar #'string-width invocations)))
              (annotation-function
               (lambda (candidate)
                 (when-let* ((invocation (intern candidate))
                             (command (blue--get-command invocation commands))
                             (synopsis (blue--command-get-slot 'synopsis command))
                             (padding (max (+ blue-annotation-padding
                                              (- max-width (string-width candidate)))
                                           2)))
                   (concat (make-string padding ?\s)
                           (propertize synopsis 'face 'blue-documentation)))))
              (options-doc-buffer-function
               (lambda (candidate)
                 (when-let* ((invocation (intern candidate))
                             (command (blue--get-command invocation commands))
                             (help-msg (blue--command-get-slot 'help command)))
                   (with-current-buffer (get-buffer-create "*blue-capf-doc*")
                     (erase-buffer)
                     (insert help-msg)
                     (font-lock-add-keywords
                      nil
                      `(("\\[.+\\]\s*\\.\\{3\\}*" . 'blue-documentation)))
                     (font-lock-mode 1)
                     (font-lock-ensure)
                     (current-buffer))))))
    (list
     :annotation-function annotation-function
     :company-doc-buffer options-doc-buffer-function
     :company-kind (lambda (candidate)
                     (cond
                      ((string-prefix-p "--" candidate) 'property)
                      ((member candidate invocations) 'command)
                      (t 'event)))
     :exclusive 'no
     :group-function (blue--create-group-fn commands))))

(defun blue--prompt-dir (&optional create-p mustmatch)
  "Prompt for directory.

If CREATE-P is non nil, create the directory and it's parents if they do
not exist.

MUSTMATCH is passed directly to `read-directory-name'."
  (let ((dir (directory-file-name
              (expand-file-name
               (read-directory-name "Build directory: " nil nil mustmatch)))))
    (when (and create-p
               (not (file-exists-p dir)))
      (mkdir dir t))
    dir))



;;; UI.

;;;###autoload
(defun blue-forget-blueprint (blueprint)
  "Forget BLUEPRINT from cache."
  (interactive
   (progn
     (blue--ensure-cache)
     (let ((blueprints (mapcar (lambda (entry)
                                 (car entry))
                               blue--cache-list)))
       (list (completing-read "Forget blueprint: " blueprints nil t)))))
  (let ((updated-cache (seq-remove (lambda (entry)
                                     (string-equal blueprint (car entry)))
                                   (or blue--cache-list nil))))
    (setq blue--cache-list updated-cache)
    (blue--write-cache)))

;;;###autoload
(defun blue-forget-build-dir (build-dir)
  "Forget BUILD-DIR from cache."
  (interactive
   (progn
     (blue--ensure-cache)
     (let ((blue--blueprint (or blue--blueprint default-directory))
           (build-dirs (blue--cache-get-build-dirs blue--blueprint)))
       (list (completing-read "Known build-dir:" build-dirs)))))
  (let* ((blueprint (blue--find-blueprint build-dir))
         (existing-configs (blue--cache-get-build-dirs blueprint))
         (updated-configs (delete build-dir existing-configs))
         (entry (when updated-configs
                  (list blueprint updated-configs)))
         (rest (seq-remove (lambda (entry)
                             (string-equal blueprint (car entry)))
                           (or blue--cache-list nil)))
         (updated-cache (if entry
                            (cons entry rest)
                          rest)))
    (setq blue--cache-list updated-cache)
    (blue--write-cache)))

(provide 'blue)
;;; blue.el ends here.
