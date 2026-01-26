;;; blue.el --- BLUE build system interface -*- lexical-binding: t -*-
;;
;; Copyright © 2025 Sergio Pastor Pérez
;;
;; Author: Sergio Pastor Pérez <sergio.pastorperez@gmail.com>
;; Version: 0.0.26
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

(defvar blue--overiden-build-dir-p nil
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
    (setq default-directory (file-name-as-directory dir))))

;; TODO: should we use 'blue' to locate the 'blueprint.scm'?
(defun blue--find-blueprint (&optional path)
  "Return path to top-level `blueprint.scm'.
If PATH is non-nil, locate `blueprint.scm' from PATH."
  (if-let* ((blueprint (concat
                        (or
                         (getenv "BLUE_EMBEDDED")
                         (locate-dominating-file (or path default-directory)
                                                 "blueprint.scm"))
                        "/blueprint.scm")))
      ;; This expands resolving symlinks. It's needed since in the build
      ;; directory ther can be a symlinked 'blueprint.scm' for projects that
      ;; require configuration.
      (file-truename (directory-file-name blueprint))
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

(defun blue--get-log-buffer ()
  "Return `blue--log-buffer'."
  (let ((buf (get-buffer blue--log-buffer)) ; Already existing.
        (buf* (get-buffer-create blue--log-buffer)))
    (unless buf
      (with-current-buffer buf*
        (blue-log-mode)))
    buf*))

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
    (let ((inhibit-read-only t)
          (start (point-max)))
      (goto-char start)
      (when command (insert (blue--format-header command)))
      (insert output)
      (blue--apply-filter-in-region
       start (point)
       #'blue-prettify-compilation-filter)
      (unless (equal (point) (line-beginning-position))
        (insert "\n"))
      (save-excursion
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

(defun blue--cache-add-current-build-dir ()
  "Add the current build directory to cache if it exists.

This is a convenience wrapper around `blue--cache-add' that checks if
the build directory `blue--build-dir' exists before adding it."
  (when-let* ((build-dir (blue--get-build-dir)))
    (blue--cache-add build-dir)))

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
         (output (with-output-to-string
                       (setq exit-code
                             (apply #'call-process blue-binary nil standard-output nil args))))
         (filtered-output (replace-regexp-in-string ";;;.*\n?" "" output)))
    (blue--log-output output command-string exit-code)
    (cons filtered-output exit-code)))

(defun blue--get-data (blueprint)
  "Return the commands and execution environment provided by BLUEPRINT."
  (when-let* ((options (when blueprint
                         (list (concat "--file=" blueprint))))
              (cmd '(".serialize-commands" "--"
                     ".serialize-execution-environment" "--"
                     ".serialize-ui"))
              (output (blue--execute-deserialize options cmd))
              (data (car output))
              (exit-code (cdr output)))
    (if (zerop exit-code)
        (with-temp-buffer
          (insert data)
          (goto-char (point-min))
          (condition-case err
              (re-search-forward "\\(\\[.*\\]\\)\\({.*}\\)\\(\\[.*\\]\\)")
            (search-failed
             (blue--log-output (error-message-string err) "[BLUE] `re-search-forward'" 1)
             (display-buffer (blue--get-log-buffer))
             (error "[BLUE] Deserialization failed")))
          (let ((commands (match-string-no-properties 1))
                (exec-env (match-string-no-properties 2))
                (ui (match-string-no-properties 3)))
            (mapcar (lambda (json)
                      (json-parse-string json
                                         :object-type 'alist
                                         :array-type 'list
                                         :null-object nil
                                         :false-object nil))
                    (list commands exec-env ui))))
      (display-buffer (blue--get-log-buffer))
      (error "[BLUE] Deserialization failed"))))

(defun blue--config-get (var config)
  "Retrieve variable VAR value from CONFIG."
  (cdr (assoc-string var config)))

(defun blue--get-command (command commands)
  "Retrieve COMMAND from COMMANDS."
  (seq-find (lambda (cmd)
              (string-equal (alist-get 'invoke cmd)
                            command))
            commands))

(defun blue--get-command-invocations (commands)
  "Retrieve command names from COMMANDS."
  (mapcar (lambda (command)
            (alist-get 'invoke command))
          commands))

(defun blue--get-command-categories (commands)
  "Retrieve command categories from COMMANDS."
  (seq-uniq (mapcar (lambda (command)
                      (alist-get 'category command))
                    commands)))

(defun blue--get-option-labels (option)
  "Retrieve lables from OPTION."
  (when-let* ((labels (alist-get 'labels option)))
    (let ((short-labels (alist-get 'short labels))
          (long-labels (alist-get 'long labels)))
      (cons short-labels long-labels))))

(defun blue--get-option-from-label (label options)
  "Retrieve option from OPTIONS that matches LABEL."
  (seq-find (lambda (option)
              (let* ((labels (blue--get-option-labels option))
                     (short-labels (car labels))
                     (long-labels (cdr labels)))
                (member label (append short-labels long-labels))))
            options))

(defun blue--format-option-label (option &optional raw)
  "Format OPTION into a label string suitable for completion.

Returns a string with the option's label prefixed appropriately
(-- for long options, - for short options) and suffixed with = or space
depending on whether the option requires a value.

If RAW is t return the label without adding any prefix or suffix."
  (when-let* ((labels (blue--get-option-labels option)))
    (let* ((short-label (caar labels))
           (long-label (cadr labels))
           (arguments (alist-get 'arguments option))
           (type (alist-get 'type arguments))
           (suffix (unless (string= type "switch")
                     "=")))
      (cond
       (long-label
        (if raw
            long-label
            (concat "--" long-label suffix)))
       (short-label
        (if raw
            short-label
          (concat "-" short-label suffix)))
       (t nil)))))


;;; Compilation.

(defun blue--default-buffer-name (command name-of-mode)
  "Default buffer naming function for compilation buffers.

COMMAND is the invocation passed to BLUE.
NAME-OF-MODE is the major mode name that the compilation buffer will use."
  (format "*%s | %s*" name-of-mode command))

(defun blue--set-search-path ()
  "Set current buffer search path.

This is meant to be used in compilation buffers."
  (when-let* ((conf (cadr blue--data))
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
    (with-current-buffer buf
      ;; Setup compilation BUFFER with DIR and error patterns.
      (make-local-variable 'compilation-error-regexp-alist)
      (add-to-list 'compilation-error-regexp-alist
                   '("^.* at \\(.*?\\):\\([0-9]+\\)" 1 2))
      ;; Bound dynamicaly for the context of this function, let's write it buffer
      ;; locally so it persists after the dynamic context ends.
      (setq default-directory default-directory
            compilation-directory default-directory
            compilation-finish-functions
            (let ((orig compilation-finish-functions))
              #'(lambda (buf _)
                  (with-current-buffer buf
                    ;; Make completion work from selected build dir.
                    (blue--set-default-directory (blue--get-build-dir))
                    (setq compilation-finish-functions orig)
                    ;; Make 'srcdir' errors searchable in compilation buffer.
                    (blue--set-search-path))))))
    (compilation-start command comint-p)))

(defun blue--visit-location (file &optional line column)
  "Open FILE and move point to LINE and COLUMN if provided."
  (when-let* ((path (if (file-exists-p file)
                        file
                      (locate-file file blue--search-path))))
    (when (file-exists-p path)
      (find-file path)
      (when line
        (goto-char (point-min))
        (forward-line (1- line)))
      (when column
        (move-to-column column)))))

(defun blue-open-hyperlink (url)
  "Open a hyperlink URL, handling file:// URLs specially."
  (cond
   ((string-match
     (rx bos "file://"
         (* (not "/"))                           ; hostname (optional)
         (group "/" (*? anything))               ; path (capture group 1)
         (? "#L"                                 ; optional line number
            (group (+ digit))                    ; line (capture group 2)
            (? ":" (group (+ digit))))           ; optional column (capture group 3)
         eos)
     url)
    (let* ((path (match-string 1 url))
           (line-str (match-string 2 url))
           (line (when line-str (string-to-number line-str)))
           (col-str (match-string 3 url))
           (col (when col-str (string-to-number col-str))))
      (blue--visit-location path line col)))
   (t (browse-url url))))

(defun blue-ansi-buttonize-hyperlinks (beg end)
  "Convert ANSI OSC 8 hyperlink sequences into clickable buttons in the region."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward
            (rx "\033]8;;"
                (group (* (not (any "\033" "\007"))))  ; URL capture group
                (or "\033\\" "\007"))                   ; Terminator
            end t)
      (let ((url (match-string-no-properties 1))
            (open-start (match-beginning 0))
            (open-end (match-end 0)))
        ;; Delete the opening sequence
        (delete-region open-start open-end)
        ;; Adjust end boundary
        (setq end (- end (- open-end open-start)))
        ;; Now point is at link-start (where the text begins)
        (let ((link-start (point)))
          ;; Find the closing sequence
          (when (re-search-forward
                 (rx "\033]8;;" (or "\033\\" "\007"))
                 end t)
            (let ((close-start (match-beginning 0))
                  (close-end (match-end 0)))
              ;; Delete the closing sequence
              (delete-region close-start close-end)
              ;; Adjust end boundary again
              (setq end (- end (- close-end close-start)))
              ;; Make the text between them a button
              (when (and url (> close-start link-start))
                (make-text-button link-start close-start
                                  'action `(lambda (_)
                                             (blue-open-hyperlink ,url))
                                  'follow-link t
                                  'help-echo (format "Click to open %s" url))))))))))

(defun blue-hyperlinks-compilation-filter ()
  "Translate OSC hyperlink escape sequences button text properties."
  (let ((inhibit-read-only t))
    (blue-ansi-buttonize-hyperlinks compilation-filter-start (point))))

(defun blue-prettify-compilation-filter ()
  "Combination of filters to prettify output in compilation buffers."
  ;; HACK: apply first our own OSC filter since the built-in one,
  ;; `ansi-osc-compilation-filter', does not visit lines properly. This
  ;; ensures our text properties are applied before the OSC filter does
  ;; anything.
  (blue-hyperlinks-compilation-filter)
  (ansi-color-compilation-filter)
  (ansi-osc-compilation-filter))

(defun blue--apply-filter-in-region (beg end filter)
  "Helper to apply FILTER to string STR."
  (save-excursion
    (let ((compilation-filter-start beg))
      (goto-char end) ; Filters take point as ending possition.
      (funcall filter))))

(defun blue-prettify-comint-filter (_)
  "Combination of filters to prettify output in comint buffers."
  (let ((start-marker (if (and (markerp comint-last-output-start)
			                   (eq (marker-buffer comint-last-output-start)
				                   (current-buffer))
			                   (marker-position comint-last-output-start))
			              comint-last-output-start
			            (point-min-marker)))
	    (end-marker (process-mark (get-buffer-process (current-buffer)))))
    (blue-ansi-buttonize-hyperlinks start-marker end-marker)
    (ansi-color-apply-on-region start-marker end-marker)
    (ansi-osc-apply-on-region start-marker end-marker)))

;;;###autoload
(define-minor-mode blue-prettify-compilation-mode
  "Minor mode to enhance compilation mode's prettifying functions."
  :global t
  :group 'blue
  :lighter " blue-pc"
  (if blue-prettify-compilation-mode
      (progn
        (add-hook 'compilation-filter-hook #'blue-prettify-compilation-filter)
        (add-hook 'comint-output-filter-functions #'blue-prettify-comint-filter))
    (remove-hook 'compilation-filter-hook #'blue-prettify-compilation-filter)
    (remove-hook 'comint-output-filter-functions #'blue-prettify-comint-filter)))


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

(defun blue--determine-build-dir ()
  "Helper to determine the build directory to use by reading prefix argument.

Returns a pair of (BUILD-DIR . PROMPTED-P)."
  (let* ((prefix (car current-prefix-arg))
         (build-dirs (blue--cache-get-build-dirs blue--blueprint))
         (last-build-dir (car build-dirs))
         (prompt-dir-p (or (eql prefix 4) ; Single universal argument 'C-u'.
                           (and blue-require-build-directory
                                (not last-build-dir))))
         (build-dir (if prompt-dir-p
                        (blue--prompt-dir t)
                      last-build-dir)))
    (cons build-dir prompt-dir-p)))


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
