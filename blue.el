;;; blue.el --- BLUE build system interface -*- lexical-binding: t -*-
;;
;; Copyright © 2025 Sergio Pastor Pérez
;;
;; Author: Sergio Pastor Pérez <sergio.pastorperez@gmail.com>
;; Version: 0.0.7
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
(require 'outline)
(require 'pcomplete)
(require 'seq)


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


;;; Faces

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

(defvar blue--log-buffer " *blue log*"
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

(defun blue--check-blue-binary ()
  "Check if `blue-binary' is in PATH."
  (let ((bin (executable-find blue-binary)))
    (unless bin
      (let* ((prefix (concat "▶ " (propertize "[ERROR] " 'face 'error)))
             (msg (concat (propertize "`blue'" 'face 'font-lock-constant-face)
                          " command not found in "
                          (propertize "`exec-path'" 'face 'font-lock-type-face)))
             (msg* (concat prefix msg)))
        (blue--log-output (concat msg* "\n"))
        (message msg*)))
    bin))

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
              'face 'bold))

(defun blue--format-footer (exit-code)
  "Format status footer for output buffer.

EXIT-CODE displays the status of the command."
  (propertize (format "⏹ Status: %s\n\n" exit-code)
              'face (if (= exit-code 0)
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
    (blue--log-output (concat msg* "\n"))
    (message msg*)))


;;; Cache.

(defun blue--read-cache ()
  "Initialize `blue--cache-list' from `blue-cache-file'."
  (when (blue--file-safe-p blue-cache-file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents blue-cache-file)
          (setq blue--cache-list (read (current-buffer))))
      (error (concat "[BLUE] Failed to read cache file: "
                     (error-message-string err))
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
    (error (concat "[BLUE] Failed to write cache file: "
                   (error-message-string err)))))

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


;;; Serialization.

(defun blue--execute-serialize (options command &optional raw)
  "Execute BLUE serialization COMMAND with OPTIONS and return parsed output.

If RAW is non nil, the serialized string will not be evaluated."
  (let* ((process-environment (cons "GUILE_AUTO_COMPILE=0" process-environment))
         (args (append (or options '()) (list command)))
         (command-string (string-join (cons blue-binary args) " "))
         exit-code
         (output (with-output-to-string
                   (setq exit-code
                         (apply #'call-process blue-binary nil standard-output nil args)))))
    (blue--log-output output command-string exit-code)
    (cons (if raw
              output
            (read output))
          exit-code)))

(defun blue--get-commands (blueprint)
  "Return the commands provided by BLUEPRINT."
  ;; Running in `temp-dir' is done to ensure that `.blue-store' does not get
  ;; created in the current directory when invoking `blue' commands.
  (let* ((temp-dir (make-temp-file "blue-" t))
         (options (when blueprint
                    (list "--file" blueprint
                          "--store-directory" temp-dir)))
         (output (unwind-protect
                     (blue--execute-serialize options ".elisp-serialize-commands")
                   (delete-directory temp-dir t)))
         (data (car output))
         (exit-code (cdr output)))
    (if (zerop exit-code)
        data
      (error "[BLUE] Command serialization failed"
             (with-current-buffer blue--log-buffer
               (goto-char (point-min)))
             (display-buffer blue--log-buffer)))))

(defun blue--get-config (blueprint &optional dir)
  "Return the BLUEPRINT configuration.

If DIR is non-nil return the configuration stored in DIR."
  (let* ((temp-dir (make-temp-file "blue-" t))
         (options (when blueprint (list "--file" blueprint
                                        "--store-directory" temp-dir)))
         (output (unwind-protect
                     (blue--execute-serialize options ".elisp-serialize-configuration")
                   (delete-directory temp-dir t)))
         (data (car output))
         (exit-code (cdr output)))
    (if (zerop exit-code)
        data
      (error "[BLUE] Configuration serialization failed"
             (with-current-buffer blue--log-buffer
               (goto-char (point-min)))
             (display-buffer blue--log-buffer)))))

(defun blue--config-get (var config)
  "Retrieve variable VAR value from CONFIG."
  (cdr (assoc-string var config)))


;;; Completion.

(blue--define-memoized blue--autocomplete (input)
  "Use blue '.autocomplete' command to provide completion from INPUT."
  (let* ((temp-dir (make-temp-file "blue-" t))
         (command (concat blue-binary
                          " --store-directory " temp-dir
                          " .autocomplete \"blue " input "\""))
         (output (unwind-protect
                     (shell-command-to-string command)
                   (delete-directory temp-dir t))))
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
     ;; `beg-no-prompt' is required to ensure that completion receives the
     ;; correct user input bounds even for prompts that do not leave any
     ;; whitespace between the prompt and the user input. For example, the
     ;; `transient-infix' default prompts, eg.: 'output=...'.
     (let ((beg-no-prompt (max beg (minibuffer-prompt-end))))
       (list beg-no-prompt end
             (completion-table-with-cache #'blue--completion-table)
             :exclusive 'no)))))


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
     (propertize (number-to-string index) 'face 'blue-hint-index)
     " "
     (propertize build-dir 'face face))))

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
     "Previous build directory (M-<num> to select):\n"
     lines*
     "\n"
     (propertize " " 'face 'blue-hint-separator
                 'display '(space :align-to right))
     "\n")))

(defun blue--show-hints (&rest _)
  "Display build directory hints in minibuffer overlay."
  (when-let* ((build-dirs (blue--cache-get-build-dirs blue--blueprint))
              (content (blue--create-hint-overlay build-dirs blue--build-dir blue--overiden-build-dir)))
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
    (let ((build-dirs (blue--cache-get-build-dirs blue--blueprint)))
      (seq-do #'blue--bind-build-dir-key (number-sequence 1 (length build-dirs)))))
  (add-hook 'completion-at-point-functions #'blue--completion-at-point nil t)
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

(defun blue--set-search-path (blueprint)
  "Set search path for BLUEPRINT."
  (let* ((conf (blue--get-config blueprint))
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
         (default-directory (if blue--build-dir
                                blue--build-dir
                              default-directory)))
    (setq-default compilation-directory default-directory)
    (blue--setup-buffer buf)
    (compilation-start command comint-p)
    ;; Make 'srcdir' errors searchable in compilation buffer.
    (with-current-buffer buf
      (blue--set-search-path (blue--find-blueprint)))))


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

A comand is considered interactive if it is a member of `blue-interactive-commands'."
  (let ((invocations (blue--parse-invocations input-tokens)))
    (seq-some #'blue--interactive-p invocations)))


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

(defun blue--prompt-dir (&optional create-p)
  "Prompt for directory.

If CREATE-P is non nil, create the directory and it's parents if they do
not exist."
  (let ((dir (directory-file-name
              (expand-file-name
               (read-directory-name "Build directory: ")))))
    (when (and create-p
               (not (file-exists-p dir)))
      (mkdir dir t))
    dir))

(defun blue--prompt-for-commands ()
  "Interactive prompt for BLUE commands."
  (if (not (blue--check-blue-binary))
      (list nil)
    (blue--ensure-cache)

    (let* ((prefix (car current-prefix-arg))
           (build-dirs (blue--cache-get-build-dirs default-directory))
           (last-build-dir (car build-dirs))
           (prompt-dir-p (or (eql prefix 4) ; Single universal argument 'C-u'.
                             (and blue-require-build-directory
                                  (not last-build-dir))))
           (comint-flip (eql prefix 16))) ; Double universal argument 'C-u C-u'.
      (setq blue--overiden-build-dir (when prompt-dir-p
                                       (blue--prompt-dir t))
            blue--build-dir (or blue--overiden-build-dir last-build-dir))
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
                  (completing-read-multiple "Command: " invocations))
                commands
                comint-flip)
        (list nil)))))


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
(defun blue-run-command (input &optional commands comint-flip)
  "Run a BLUE command interactively.

The prompt will hint for a directory where to run the BLUE command in a
directory.  The hinted directories are directories where BLUE has been
previously executed by `blue.el'.  The execution directory can be
changed using 'M-<num>'.

Invoked with universal prefix argument '\\[universal-argument]', prompt
for a directory to use when running 'blue'.

Invoked with double universal prefix argument '\\[universal-argument]
\\[universal-argument]', invert the interactive heuristics configured by
`blue-interactive-commands'.

INPUT is a list of command strings.
COMMANDS contains command metadata.
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
        (when blue--build-dir
          (blue--cache-add blue--build-dir))
        (blue--compile command-string comint)))))

(provide 'blue)
;;; blue.el ends here.
