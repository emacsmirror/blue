;;; blue.el --- BLUE build system interface -*- lexical-binding: t -*-
;;
;; Copyright © 2025 Sergio Pastor Pérez
;;
;; Author: Sergio Pastor Pérez <sergio.pastorperez@gmail.com>
;; Version: 0.0.19
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


;;; Completion.

(blue--define-memoized blue--autocomplete (blueprint input)
  "Use blue '.autocomplete' command to provide completion from INPUT."
  (let* ((default-directory (or (blue--get-build-dir) default-directory))
         (command (concat blue-binary
                          " --file=" blueprint
                          " .autocomplete bash \"blue " input "\""))
         (output (shell-command-to-string command)))
    (string-split output)))

(defun blue--completion-table (&rest _)
  "Completion table function for minibuffer prompt."
  (let ((result
         (while-no-input
           (when-let* ((blueprint (or blue--blueprint
                                      (blue--find-blueprint)))
                       (prompt-start (minibuffer-prompt-end))
                       (input (buffer-substring prompt-start (point)))
                       (completions (blue--autocomplete blueprint input)))
             completions))))
    (and (consp result) result)))

(defun pcomplete/blue ()
  "Completion for `blue'."
  (while (pcomplete-here* (blue--completion-table))))

(defcustom blue-complete--file-prefix "="
  "File completion trigger prefixes.
The value can be a string or a list of strings.  The default
`file:' is the prefix of Org file links which work in arbitrary
buffers via `org-open-at-point-global'."
  :type '(choice string (repeat string)))

(defvar blue-complete--file-properties
  (list :annotation-function (lambda (s) (if (string-suffix-p "/" s) " Dir" " File"))
        :company-kind (lambda (s) (if (string-suffix-p "/" s) 'folder 'file))
        :exclusive 'no
        :category 'file)
  "Completion extra properties for `blue--complete-file'.")

(defun blue-complete--bounds (thing)
  "Return bounds of THING."
  (or (bounds-of-thing-at-point thing) (cons (point) (point))))

(defun blue--complete-file ()
  "Complete file name at point."
  (pcase-let* ((prefix (and blue-complete--file-prefix
                            (looking-back
                             (concat
                              (regexp-opt (ensure-list blue-complete--file-prefix) t)
                              "[^ \n\t]*")
                             (pos-bol))
                            (match-end 1)))
               (`(,beg . ,end) (if prefix
                                   (cons prefix (point))
                                 (blue-complete--bounds 'filename)))
               (non-essential t)
               (file (buffer-substring-no-properties beg end)))
    (when (or prefix
              (and (string-search "/" file)
                   (file-exists-p (file-name-directory
                                   (substitute-in-file-name file)))))
      (unless (boundp 'comint-unquote-function)
        (require 'comint))
      `( ,beg ,end
         ,(completion-table-with-quoting
           #'read-file-name-internal
           comint-unquote-function
           comint-requote-function)
         ,@(when (or prefix (string-match-p "./" file))
             '(:company-prefix-length t))
         ,@blue-complete--file-properties))))

(defun blue--complete-directory ()
  "Complete directory name at point."
  (pcase-let* ((prefix (and blue-complete--file-prefix
                            (looking-back
                             (concat
                              (regexp-opt (ensure-list blue-complete--file-prefix) t)
                              "[^ \n\t]*")
                             (pos-bol))
                            (match-end 1)))
               (`(,beg . ,end) (if prefix
                                   (cons prefix (point))
                                 (blue-complete--bounds 'filename)))
               (non-essential t)
               (file (buffer-substring-no-properties beg end)))
    (when (or prefix
              (and (string-search "/" file)
                   (file-exists-p (file-name-directory
                                   (substitute-in-file-name file)))))
      (unless (boundp 'comint-unquote-function)
        (require 'comint))
      `( ,beg ,end
         ,(completion-table-with-quoting
           (apply-partially #'completion-table-with-predicate
                            #'read-file-name-internal
                            #'file-directory-p
                            'strict)
           comint-unquote-function
           comint-requote-function)
         ,@(when (or prefix (string-match-p "./" file))
             '(:company-prefix-length t))
         ,@blue-complete--file-properties))))

(defun blue--get-completion-table (command)
  "Generate completion table for COMMAND."
  (let* ((autocompletion (blue--command-get-slot 'autocomplete command))
         (values (alist-get 'values autocompletion))
         (options (blue--command-get-slot 'options command)))
    options))

(defun blue--get-command-completion-table (command)
  "Generate an appropriate completion table for command."
  (let* ((autocompletion (blue--command-get-slot 'autocomplete command))
         (values (alist-get 'values autocompletion))
         (options (blue--command-get-slot 'options command))
         (long-labels (mapcar #'(lambda (option)
                                  (let* ((arguments (alist-get 'arguments option))
                                         (type (alist-get 'type arguments)))
                                    (concat "--" (car (blue--get-option-long-labels option))
                                            (if (string= type "required")
                                                "="
                                              " "))))
                              options)))
    (append values long-labels)))

(defun blue--completion-at-point ()
  "`completion-at-point' function for `blue-run-command'."
  (when blue--data
    (let* ((commands (car blue--data))
           (prompt-start (minibuffer-prompt-end))
           (input (buffer-substring-no-properties prompt-start (point)))
           (cmds+args (string-split input " -- "))
           (last-cmd+args (car (last cmds+args)))
           (last-cmd (and last-cmd+args
                          (car (string-split last-cmd+args))))
           (cmd (and last-cmd (blue--get-command (intern last-cmd) commands)))
           (options (blue--command-get-slot 'options cmd))
           (labels-width
            (+ (apply #'max
                      (mapcan
                       #'(lambda (option)
                           (let ((long-labels (blue--get-option-long-labels option)))
                             (mapcar #'string-width long-labels)))
                       options))
               3)) ; To accommodate for prefix and suffix '--...='.
           (table
            (while-no-input
              (and cmd
                   (blue--get-command-completion-table cmd)))))
      (pcase (bounds-of-thing-at-point 'symbol)
        ;; Long option argument completion.
        ((pred (lambda (bounds)
                 (and blue-complete--file-prefix
                      (looking-back
                       (concat
                        (regexp-opt (ensure-list blue-complete--file-prefix) t)
                        "[^ \n\t]*")
                       (pos-bol))
                      (match-end 1))))
         (let* ((thing (thing-at-point 'symbol))
                (long-label (string-trim thing "--" "="))
                (option (blue--get-option-from-label long-label cmd))
                (autocomplete (alist-get 'autocomplete option)))
           (if (string-equal autocomplete "directory")
               (blue--complete-directory)
             (blue--complete-file))))
        (`(,beg . ,end)
         `( ,beg ,end
            ,table
            :exclusive 'no
            :annotation-function
            (lambda (candidate)
              (let* ((long-label (string-trim candidate "--" "="))
                     (option (blue--get-option-from-label long-label ',cmd))
                     (doc (alist-get 'doc option))
                     (arguments (alist-get 'arguments option))
                     (arg-name (alist-get 'name arguments))
                     (padding (max (+ blue-annotation-padding
                                      (- ,labels-width (+ (string-width candidate)
                                                          (string-width arg-name))))
                                   2)))
                (concat
                 (propertize arg-name 'face 'blue-documentation)
                 (make-string padding ?\s)
                 (propertize doc 'face 'blue-documentation))))))
        ;; Command argument completion.
        (_
         (when-let* ((autocomplete (blue--command-get-slot 'autocomplete cmd))
                     (type (alist-get 'type autocomplete))
                     (blue-complete--file-prefix ""))
           (cond
            ((string-equal type "directory")
             (blue--complete-directory))
            ((string-equal type "file")
             (blue--complete-file))
            ((and (string-equal type "set")
                  table)
             `( ,(point) ,(point)
                ,table
                :exclusive 'no
                :annotation-function
                (lambda (candidate)
                  (let* ((long-label (string-trim candidate "--" "="))
                         (option (blue--get-option-from-label long-label ',cmd))
                         (doc (alist-get 'doc option))
                         (arguments (alist-get 'arguments option))
                         (arg-name (alist-get 'name arguments))
                         (padding (max (+ blue-annotation-padding
                                          (- ,labels-width (+ (string-width candidate)
                                                              (string-width arg-name))))
                                       2)))
                    (concat
                     (propertize arg-name 'face 'blue-documentation)
                     (make-string padding ?\s)
                     (propertize doc 'face 'blue-documentation)))))))))))))


;;; Minibuffer Hints.

(defun blue--format-build-dir-hint (index build-dir current)
  "Format a single build directory hint line.

INDEX is the number prefixing the displayed known build directory in the
hint message.
BUILD-DIR is the directory of the known build directory.
If CURRENT is non-nil the entry will be highlighted."
  (let ((face (if (string-equal build-dir current)
                  'blue-hint-highlight
                'blue-hint-faded))
        (build-dir* (buttonize build-dir
                               (lambda (_)
                                 (execute-kbd-macro
                                  (kbd (concat "M-" (number-to-string index))))))))
    (concat
     (propertize (number-to-string index) 'face 'blue-hint-index)
     " "
     (propertize build-dir*
                 'face face
                 'help-echo "Click to select build directory"))))

(defun blue--create-hint-overlay (build-dirs current override)
  "Create hint overlay content from BUILD-DIRS and CURRENT build-dir.

If OVERRIDE is non nil disable CONFIGS."
  (when-let* (build-dirs
              (indices (number-sequence 1 (length build-dirs)))
              (formatted (seq-mapn (lambda (idx dir)
                                     (blue--format-build-dir-hint idx dir current))
                                   indices build-dirs))
              (lines (string-join formatted "\n"))
              (lines* (if override
                          (concat
                           (blue--format-build-dir-hint 0 override override)
                           "\n"
                           (propertize lines 'face 'blue-hint-separator))
                        lines)))
    (concat
     "Build directory (M-<num> to select):\n"
     lines*
     "\n"
     (propertize " " 'face 'blue-hint-separator
                 'display '(space :align-to right))
     "\n")))

(defun blue--show-hints (&rest _)
  "Display build directory hints in minibuffer overlay."
  (when-let* ((build-dirs (blue--cache-get-build-dirs blue--blueprint))
              (content (blue--create-hint-overlay build-dirs (blue--get-build-dir) blue--overiden-build-dir)))
    (unless blue--hint-overlay
      (setq blue--hint-overlay (make-overlay (point) (point))))
    (overlay-put blue--hint-overlay 'after-string content)
    (move-overlay blue--hint-overlay (point-min) (point-min) (current-buffer))))


;;; Minibuffer Setup.

(defun blue--bind-build-dir-key (index)
  "Setup keybinding for build directory INDEX."
  (let ((key (kbd (format "M-%d" index))))
    (define-key (current-local-map) key
                (lambda ()
                  (interactive)
                  (setq blue--build-dir
                        (nth (1- index) (blue--cache-get-build-dirs blue--blueprint))
                        default-directory blue--build-dir) ; Make completion work from selected build dir.
                  (blue--show-hints)))))

(defun blue--setup-minibuffer ()
  "Setup keybindings and completion for minibuffer prompt."
  ;; Work on a copy of the current minibuffer keymap so it doesn’t leak
  (use-local-map (copy-keymap (current-local-map)))
  (define-key (current-local-map) (kbd "SPC") nil)
  (unless blue--overiden-build-dir
    (let ((build-dirs (blue--cache-get-build-dirs blue--blueprint)))
      (seq-do #'blue--bind-build-dir-key (number-sequence 1 (length build-dirs)))))
  ;; KLUDGE: force only blue completion.
  (setq completion-at-point-functions #'blue--completion-at-point)
  ;; (add-hook 'completion-at-point-functions #'blue--completion-at-point nil t)
  (blue--show-hints))


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

(defun blue--create-annotation-fn (commands width)
  "Create annotation function for COMMANDS with WIDTH alignment."
  (lambda (candidate)
    (when-let* ((invocation (intern candidate))
                (command (blue--get-command invocation commands))
                (synopsis (blue--command-get-slot 'synopsis command)))
      (concat (make-string (+ blue-annotation-padding
                              (- width (string-width candidate)))
                           ?\s)
              (propertize synopsis 'face 'blue-documentation)))))

(defun blue--create-group-fn (commands)
  "Create group function for COMMANDS."
  (lambda (candidate transform)
    (if transform
        candidate
      (when-let* ((invocation (intern candidate))
                  (command (blue--get-command invocation commands)))
        (blue--command-get-slot 'category command)))))

(defun blue--create-completion-properties (commands)
  "Create completion properties for COMMANDS and INVOCATIONS."
  (when-let* ((invocations (blue--get-command-invocations commands))
              (max-width (apply #'max (mapcar #'string-width invocations))))
    (list :annotation-function (blue--create-annotation-fn commands max-width)
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

(defun blue--prompt-for-commands ()
  "Interactive prompt for BLUE commands."
  (blue--check-blue-binary)
  (blue--ensure-cache)
  (setq blue--blueprint (blue--find-blueprint))
  (let* ((prefix (car current-prefix-arg))
         (build-dirs (blue--cache-get-build-dirs blue--blueprint))
         (last-build-dir (car build-dirs))
         (prompt-dir-p (or (eql prefix 4) ; Single universal argument 'C-u'.
                           (and blue-require-build-directory
                                (not last-build-dir))))
         (comint-flip (eql prefix 16))) ; Double universal argument 'C-u C-u'.
    (setq blue--overiden-build-dir (when prompt-dir-p
                                     (blue--prompt-dir t))
          blue--build-dir (or blue--overiden-build-dir last-build-dir)
          blue--data (blue--get-data blue--blueprint))
    ;; Make completion work from selected build dir.
    (blue--set-default-directory blue--build-dir)
    (if-let* ((commands (car blue--data))
              (invocations (blue--get-command-invocations commands))
              (completion-extra-properties
               (blue--create-completion-properties commands))
              (crm-separator (propertize "[ \t]*--[ \t]+"
                                         'separator "--"
                                         'description "double-dash-separated list"))
              (crm-prompt "[%d] [CMR%s] Command: "))
        (list (minibuffer-with-setup-hook #'blue--setup-minibuffer
                (completing-read-multiple "Command: " invocations))
              comint-flip)
      (list nil))))


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

;;;###autoload
(defun blue-run-command (input &optional comint-flip)
  "Run a BLUE command interactively.

The prompt will hint for a directory where to run the BLUE command in a
directory.  The hinted directories are directories where BLUE has been
previously executed by `blue.el'.  The execution directory can be
changed using `M-<num>'.

Invoked with universal prefix argument '\\[universal-argument]', prompt
for a directory to use when running `blue'.

Invoked with double universal prefix argument '\\[universal-argument]
\\[universal-argument]', invert the interactive heuristics configured by
`blue-interactive-commands'.

INPUT is a list of command strings.
COMINT-FLIP inverts the interactive compilation logic."
  (interactive (blue--prompt-for-commands))

  (when input
    (let* ((options (blue--normalize-options blue-default-options))
           (tokens (mapcar #'string-split input))
           (is-interactive (blue--any-interactive-p tokens))
           (comint (xor is-interactive comint-flip)))
      (let ((command-string (string-join
                             (cons blue-binary
                                   (append (when options options)
                                           (list (string-join input " -- "))))
                             " ")))
        ;; Bring `blue--build-dir' to the from of the list so it's ordered by
        ;; usage.
        (when-let* ((build-dir (blue--get-build-dir)))
          (blue--cache-add build-dir))
        (blue--compile command-string comint)))))

(provide 'blue)
;;; blue.el ends here.
