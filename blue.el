;;; blue.el --- BLUE build system interface -*- lexical-binding: t -*-
;;
;; Copyright © 2025 Sergio Pastor Pérez
;;
;; Author: Sergio Pastor Pérez <sergio.pastorperez@gmail.com>
;; Version: 0.0.4
;; Package-Requires: ((emacs "30.1"))
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
(require 'pcomplete)

;;; Configuration

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

(defcustom blue-default-flags nil
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

;;; Faces

(defface blue-documentation
  '((t :inherit completions-annotations))
  "Face used to highlight documentation strings.")

(defface blue-hint-separator
  '((t :inherit shadow :strike-through t))
  "Face used to separate hint overlay.")

(defface blue-hint-highlight
  '((t :inherit bold :foreground "#3f578f" :background "#e0f2fa"))
  "Face used to draw attention in hint overlay.")

(defface blue-hint-faded
  '((t :inherit shadow :weight regular))
  "Face used to display inactive items in hint overlay.")

;;; Internal Variables

(defvar blue--unset 'unset
  "Unset symbol.")

(defvar blue--blueprint nil
  "Current blueprint being processed.")

(defvar blue--cache-list blue--unset
  "List structure containing directories of known BLUE caches for project.")

(defvar blue--build-dir nil
  "Path to last known build directory.")

(defvar blue--overiden-build-dir nil
  "Internal variable used to override `blue--build-dir'.

This is used when passing universal prefix argument `C-u' to
`blue-run-command'.")

(defvar blue--buffer-name-function #'blue--default-buffer-name
  "Function used by BLUE to name the compilation buffer.")

(defvar blue--output-buffer " *blue output*"
  "Buffer used to capture output from BLUE commands.")

(defvar blue--hint-overlay nil
  "Overlay for minibuffer hints.")

;;; Utilities

(defun blue--check-blue-binary ()
  "Check if `blue-binary' is in PATH."
  (if (executable-find blue-binary)
      t
    (let ((output-buf (get-buffer-create blue--output-buffer)))
      (with-current-buffer output-buf
        (blue--handle-error 'missing)))
    nil))

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
    (error
     (warn (concat "Failed to locate "
                   (propertize "`blueprint.scm'" 'face 'font-lock-constant-face)
                   (when path
                     (concat " in "
                             (propertize path 'face 'font-lock-type-face))))))))

(defun blue--normalize-flags (flags)
  "Normalize FLAGS to a list of strings."
  (cond
   ((null flags) nil)
   ((stringp flags) (string-split flags))
   ((listp flags) flags)
   (t (list (format "%s" flags)))))

(defun blue--file-safe-p (file)
  "Return t if FILE exists and is readable."
  (and file (file-exists-p file) (file-readable-p file)))

;;; Memoization

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

;;; Cache Management

(defun blue--read-cache ()
  "Initialize `blue--cache-list' from `blue-cache-file'."
  (when (blue--file-safe-p blue-cache-file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents blue-cache-file)
          (setq blue--cache-list (read (current-buffer))))
      (error
       (warn "Failed to read BLUE cache file: %s" (error-message-string err))
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
     (warn "Failed to write BLUE cache file: %s" (error-message-string err)))))

(defun blue--sanitize-cache ()
  "Remove non-existent directories from cache list."
  (setq blue--cache-list
        (delq nil
              (mapcar (lambda (entry)
                        (let ((project (car entry))
                              (configs (cadr entry)))
                          (when (file-exists-p project)
                            (list project (seq-filter #'file-exists-p configs)))))
                      blue--cache-list))))

(defun blue--ensure-cache ()
  "Initialize cache if needed and sanitize it."
  (when (eq blue--cache-list blue--unset)
    (blue--read-cache))
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


;;; Command Execution.

(defun blue--format-header (command time)
  "Format COMMAND header for output buffer.

TIME is the timestamp of the header."
  (format "▶ %s  [%s]\n" command time))

(defun blue--format-footer (exit-code)
  "Format status footer for output buffer.

EXIT-CODE displays the status of the command."
  (propertize (format "⏹ Status: %s\n" exit-code)
              'face (pcase exit-code
                      (0 'success)
                      ('missing 'error)
                      (_ 'warning))))

(defun blue--handle-error (exit-code)
  "Handle and display command execution errors.

Give a relevant error message according to EXIT-CODE."
  (let* ((prefix (propertize "[ERROR] " 'face 'error))
         (msg (if (numberp exit-code)
                  (propertize (format "Error %s" exit-code) 'face 'error)
                (concat (propertize "`blue'" 'face 'font-lock-constant-face)
                        " command not found in "
                        (propertize "`exec-path'" 'face 'font-lock-type-face))))
         (msg* (concat prefix msg)))
    (insert (concat msg* "\n\n"))
    (message "%s" msg*)))

(defun blue--execute-serialize (flags command)
  "Execute BLUE serialization COMMAND with FLAGS and return parsed output."
  (let* ((buffer (get-buffer-create blue--output-buffer))
         (timestamp (current-time-string))
         (env (cons "GUILE_AUTO_COMPILE=0" process-environment))
         (path exec-path)
         (args (append (or flags '()) (list command)))
         (command-string (string-join (cons blue-binary args) " "))
         (header (blue--format-header command-string timestamp))
         start-pos end-pos exit-code result)

    (with-current-buffer buffer
      (let ((process-environment env)
            (exec-path path)
            (inhibit-read-only t))
        (goto-char (point-min))
        (unless (bobp) (insert "\n"))

        (insert (propertize header 'face 'bold))
        (setq start-pos (point))

        (condition-case _
            (setq exit-code (apply #'call-process blue-binary nil buffer nil args))
          (file-missing (setq exit-code 'missing)))

        (setq end-pos (point))
        (insert (blue--format-footer exit-code))

        (if (eq exit-code 0)
            (condition-case parse-error
                (setq result (read (buffer-substring-no-properties start-pos end-pos)))
              (error
               (let ((error-message (propertize (format "[Parse error] %s\n" parse-error)
                                                'face 'error)))
                 (insert error-message)
                 (message "%s" error-message))))
          (blue--handle-error exit-code))))
    result))

(defun blue--get-commands (blueprint)
  "Return the commands provided by BLUEPRINT."
  (let ((flags (when blueprint (list "--file" blueprint))))
    (blue--execute-serialize flags ".elisp-serialize-commands")))

(defun blue--get-config (blueprint dir)
  "Return the BLUEPRINT configuration stored in DIR."
  (let ((flags (when blueprint
                 (list "--file" blueprint "--build-directory" dir))))
    (blue--execute-serialize flags ".elisp-serialize-configuration")))

(defun blue--config-get (var config)
  "Retrieve variable VAR value from CONFIG."
  (cdr (assoc-string var config)))


;;; Completion.

(blue--define-memoized blue--autocomplete (input)
  "Use blue '.autocomplete' command to provide completion from INPUT."
  (let* ((command (concat blue-binary " .autocomplete \"blue " input "\""))
         (output (shell-command-to-string command)))
    (string-split output)))

(defun blue--completion-table (&rest _)
  "Completion table function for minibuffer prompt."
  (let ((result
         (while-no-input
           (when-let* ((prompt-start (minibuffer-prompt-end))
                       (input (buffer-substring prompt-start (point)))
                       (completions (blue--autocomplete input)))
             completions))))
    (and (consp result) result)))

(defun pcomplete/blue ()
  "Completion for `blue'."
  (while (pcomplete-here* (blue--completion-table))))

(defun blue--completion-at-point ()
  "`completion-at-point' function for `blue-run-command'."
  (pcase (bounds-of-thing-at-point 'symbol)
    (`(,beg . ,end)
     (list beg end
           (completion-table-with-cache #'blue--completion-table)
           :exclusive 'no))))


;;; Minibuffer Hints.

(defun blue--format-build-dir-hint (index build-dir current)
  "Format a single build directory hint line.

INDEX is the number prefixing the displayed known build directory in the
hint message.
BUILD-DIR is the directory of the known build directory.
If CURRENT is non-nil the entry will be highlighted."
  (let ((face (if (string-equal build-dir current)
                  'blue-hint-highlight
                'blue-hint-faded)))
    (concat
     (propertize (number-to-string index) 'face 'font-lock-keyword-face)
     " "
     (propertize build-dir 'face face))))

(defun blue--create-hint-overlay (build-dirs current override)
  "Create hint overlay content from BUILD-DIRS and CURRENT build-dir.

If OVERRIDE is non nil disable CONFIGS."
  (when-let* (build-dirs
              (indices (number-sequence 1 (length build-dirs)))
              (formatted (seq-mapn (lambda (idx config)
                                     (blue--format-build-dir-hint idx config current))
                                   indices build-dirs))
              (lines (string-join formatted "\n"))
              (lines* (if override
                          (concat
                           (blue--format-build-dir-hint 0 override override)
                           "\n"
                           (propertize lines 'face '(:inherit blue-hint-faded :strike-through t)))
                        lines)))
    (concat
     "Previous build directory (M-<num> to select):\n"
     lines*
     "\n"
     (propertize " " 'face 'blue-hint-separator
                 'display '(space :align-to right))
     "\n")))

(defun blue--show-hints (&rest _)
  "Display build directory hints in minibuffer overlay."
  (when-let* ((configs (blue--cache-get-build-dirs blue--blueprint))
              (content (blue--create-hint-overlay configs blue--build-dir blue--overiden-build-dir)))
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
                        (nth (1- index) (blue--cache-get-build-dirs blue--blueprint)))
                  (blue--show-hints)))))

(defun blue--setup-minibuffer ()
  "Setup keybindings and completion for minibuffer prompt."
  ;; Work on a copy of the current minibuffer keymap so it doesn’t leak
  (use-local-map (copy-keymap (current-local-map)))
  (define-key (current-local-map) (kbd "SPC") nil)
  (unless blue--overiden-build-dir
    (let ((configs (blue--cache-get-build-dirs blue--blueprint)))
      (seq-do #'blue--bind-build-dir-key (number-sequence 1 (length configs)))))
  (add-hook 'completion-at-point-functions #'blue--completion-at-point nil t)
  (blue--show-hints))


;;; Compilation.

(defun blue--default-buffer-name (command name-of-mode)
  "Default buffer naming function for compilation buffers.

COMMAND is the invocation passed to BLUE.
NAME-OF-MODE is the major mode name that the compilation buffer will use."
  (format "*%s | %s*" name-of-mode command))

(defun blue--setup-buffer (buffer dir)
  "Setup compilation BUFFER with DIR and error patterns."
  (with-current-buffer buffer
    (make-local-variable 'compilation-error-regexp-alist)
    (add-to-list 'compilation-error-regexp-alist
                 '("^.* at \\(.*?\\):\\([0-9]+\\)" 1 2))
    (setq default-directory dir)))

(defun blue--compile (command &optional configure-p comint-p)
  "Compile COMMAND with BLUE-specific setup.
CONFIGURE-P runs command in `default-directory'.
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
         (dir (if configure-p
                  default-directory
                (or blue--build-dir default-directory))))

    (setq-default compilation-directory dir)
    (blue--setup-buffer buf dir)

    (let ((default-directory dir))
      (compilation-start command comint-p))))


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

(defun blue--analyze-commands (input-tokens commands)
  "Analyze INPUT-TOKENS against COMMANDS to extract properties."
  (let* ((invocations (blue--parse-invocations input-tokens))
         (entries (blue--find-command-entries invocations commands)))
    (list :invocations invocations
          :entries entries
          :interactive-p (seq-some (lambda (cmd)
                                     (when (member cmd blue-interactive-commands)
                                       t))
                                   invocations)
          :needs-config-p (seq-some (lambda (entry)
                                      (alist-get 'requires-configuration? entry))
                                    entries)
          :configure-p (member "configure" invocations))))


;;; Command Prompt.

(defun blue--create-annotation-fn (commands width)
  "Create annotation function for COMMANDS with WIDTH alignment."
  (lambda (candidate)
    (when-let* ((entry (seq-find (lambda (cmd)
                                   (string= candidate (alist-get 'invoke cmd)))
                                 commands))
                (synopsis (alist-get 'synopsis entry)))
      (concat (make-string (+ blue-annotation-padding
                              (- width (string-width candidate))) ?\s)
              (propertize synopsis 'face 'blue-documentation)))))

(defun blue--create-group-fn (commands)
  "Create group function for COMMANDS."
  (lambda (candidate transform)
    (if transform
        candidate
      (when-let* ((entry (seq-find (lambda (cmd)
                                     (string= candidate (alist-get 'invoke cmd)))
                                   commands))
                  (category (alist-get 'category entry)))
        (symbol-name category)))))

(defun blue--create-completion-properties (commands invocations)
  "Create completion properties for COMMANDS and INVOCATIONS."
  (let ((width (if invocations (apply #'max (mapcar #'string-width invocations)) 0)))
    (list :annotation-function (blue--create-annotation-fn commands width)
          :group-function (blue--create-group-fn commands))))

(defun blue--prompt-dir (&optional cache)
  "Prompt for directory, create it if it does not exists.

If CACHE is non nil, add directory to cache."
  (let ((dir (expand-file-name (read-directory-name "Build directory: "))))
    (unless (file-exists-p dir)
      (mkdir dir t))
    ;; Needed to force the change of the execution directory since for the
    ;; configure command the value of `default-directory' is respected in
    ;; `blue--compile'.
    (setq default-directory dir
          blue--build-dir dir)
    (when cache
      (blue--cache-add dir))
    dir))

(defun blue--prompt-for-commands ()
  "Interactive prompt for BLUE commands."
  (if (not (blue--check-blue-binary))
      (list blue--unset)
    (blue--ensure-cache)
    (setq blue--build-dir (car (blue--cache-get-build-dirs default-directory)))

    (let* ((prefix (car current-prefix-arg))
           (prompt-dir-p (eql prefix 4)) ; Single universal argument 'C-u'.
           (comint-flip (eql prefix 16)) ; Double universal argument 'C-u C-u'.
           (blue--overiden-build-dir (when prompt-dir-p
                                       (blue--prompt-dir t))))
      (if-let* ((blue--blueprint (blue--find-blueprint))
                (commands (blue--get-commands blue--blueprint))
                (invocations (mapcar (lambda (cmd) (alist-get 'invoke cmd)) commands))
                (completion-extra-properties
                 (blue--create-completion-properties commands invocations))
                (crm-separator (propertize "[ \t]*--[ \t]+"
                                           'separator "--"
                                           'description "double-dash-separated list"))
                (crm-prompt "[%d] [CMR%s] Command: "))
          (list (minibuffer-with-setup-hook #'blue--setup-minibuffer
                  (prog1 (completing-read-multiple "Command: " invocations)
                    (when blue--build-dir
                      (if blue--overiden-build-dir
                          ;; FIXME: this should not be cached ONLY for projects
                          ;; that use configuration.

                          ;; Do not set cache for this case since this is only
                          ;; used for bringing the directory to the front of the
                          ;; cache list and it could be that there is no
                          ;; configuration command in the user input meaning that
                          ;; this directory should not be cached. The caching will
                          ;; be done later in `blue-run-command'.
                          (setq blue--build-dir blue--overiden-build-dir)
                        (blue--cache-add blue--build-dir)))))
                commands
                comint-flip)
        (list blue--unset)))))


;;; UI.

;;;###autoload
(defun blue-forget-blueprint (blueprint)
  "Forget BLUEPRINT from cache."
  (interactive
   (let ((blueprint (blue--find-blueprint default-directory)))
     (list blueprint)))
  (let ((updated-cache (seq-remove (lambda (entry)
                                     (string-equal blueprint (car entry)))
                                   (or blue--cache-list nil))))
    (setq blue--cache-list updated-cache)))

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
    (setq blue--cache-list updated-cache)))

;;;###autoload
(defun blue-run-command (input &optional commands comint-flip)
  "Run a BLUE command interactively.

Invoked with universal prefix argument '\\[universal-argument]', prompt
for a directory use when running 'blue'.

Invoked with double universal prefix argument '\\[universal-argument]
\\[universal-argument]', invert the interactive heuristics configured by
`blue-interactive-commands'.

INPUT is a list of command strings.
COMMANDS contains command metadata.
COMINT-FLIP inverts the interactive compilation logic."
  (interactive (blue--prompt-for-commands))

  (unless (eq input blue--unset)
    (let* ((flags (blue--normalize-flags blue-default-flags))
           (tokens (mapcar #'string-split input))
           (analysis (blue--analyze-commands tokens commands))
           (comint (xor (plist-get analysis :interactive-p) comint-flip))
           (configure-p (plist-get analysis :configure-p))
           (needs-config-p (plist-get analysis :needs-config-p)))
      (cond
       ((and needs-config-p
             (not blue--build-dir)
             (not configure-p))
        (let ((conf-dir (read-directory-name "Configuration directory: ")))
          (unless (file-exists-p conf-dir)
            (mkdir conf-dir t))
          ;; Needed to force the change of the execution directory since for the
          ;; configure command the value of `default-directory' is respected in
          ;; `blue--compile'.
          (setq default-directory conf-dir
                blue--build-dir conf-dir)))
       (configure-p
        (message (concat "Configuration requested, next command requiring configuration will "
                         "run under " (propertize "`blue--build-dir'" 'face 'bold) " directory."))
        (blue--cache-add default-directory)))

      (let ((command-string (string-join
                             (cons blue-binary
                                   (append (when flags flags)
                                           (cons (string-join input " -- ") nil)))
                             " ")))
        (blue--compile command-string configure-p comint)))))

(provide 'blue)
;;; blue.el ends here.
