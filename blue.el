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
(require 'project)

;;; Configuration variables.

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

(defcustom blue-cache-list-file
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

(defface blue-documentation
  '((t :inherit completions-annotations))
  "Face used to highlight documentation strings.")

(defvar blue--current-blueprint nil
  "Current blueprint being processed.")

(defvar blue--cache-list 'unset
  "List structure containing directories of known BLUE caches for project.")

(defvar blue--anotation-padding 8
  "Padding used for alignment of synopsis in completing read.")

(defvar blue--last-configuration nil
  "Path to last known configuration directory.")

(defvar blue--compilation-buffer-name-function #'blue--compilation-default-buffer-name
  "Function used by BLUE to name the compilation buffer used to run command.

The function must take 2 arguments, the COMMAND being run and
NAME-OF-MODE which is the major mode of the compilation buffer.")

(defun blue--compilation-default-buffer-name (command name-of-mode)
  "Compilation naming function for `blue-run-command'.

COMMAND will be passed the BLUE CLI arguments.

NAME-OF-MODE will be passed the major mode of the compilation buffer."
  (concat "*" name-of-mode " | " command "*"))

(defun blue--compile (command &optional requires-configuration comint)
  "Like `compile' but tailored for BLUE.

COMMAND is the command to run in the compilation buffer, it has the same
meaning as in `compile'

REQUIRES-CONFIGURATION ensures that command is run in
`blue--last-configuration' directory.

COMINT selects wheter the compilation buffer should be interactive."
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
  (setq-default compilation-directory default-directory)

  ;; Create the compilation buffer beforehand to setup some variables.
  (let* ((mode (or comint
                   'compilation-mode))
         (name-of-mode (if (eq mode t)
                           "compilation"
                         (replace-regexp-in-string "-mode\\'" "" (symbol-name mode))))
         (compilation-buffer-name-function (lambda (name-of-mode)
                                             (funcall #'blue--compilation-default-buffer-name
                                                      command name-of-mode)))
         (buf (get-buffer-create
               (compilation-buffer-name name-of-mode
                                        comint
                                        compilation-buffer-name-function))))
    ;; Setup compilation buffer variables.
    (with-current-buffer buf
      ;; Extend error list to make it understand Guile errors.
      (make-local-variable 'compilation-error-regexp-alist)
      (add-to-list 'compilation-error-regexp-alist '("^.* at \\(.*?\\):\\([0-9]+\\)" 1 2))

      ;; This needs to be set bufer-local so `recompile' runs in the correct
      ;; directory.
      (setq default-directory (if (and requires-configuration
                                       blue--last-configuration)
                                  blue--last-configuration
                                default-directory)))

    ;; If commands needs a configuration, setup `default-directory' so it runs
    ;; in the previously configured directory.
    (let ((default-directory (if (and requires-configuration
                                      blue--last-configuration)
                                 blue--last-configuration
                               default-directory)))
      ;; Start compilation from original directory to ensure '.envrc' is loaded
      ;; if needed.
      (compilation-start command comint))))


;;; Utilities.

(defun blue--expand-if-remote-name (path)
  "Expand PATH if it's a file."
  (if (file-remote-p path) path
    (expand-file-name path)))

(defun blue--locate-blueprint (&optional path)
  "Return path to top-level `blueprint.scm'.

If PATH is non nil locate `blueprint.scm' from PATH."
  (when-let* ((blueprint (locate-dominating-file (or path default-directory)
                                                 "blueprint.scm")))
    (expand-file-name
     (directory-file-name
      (concat blueprint "/blueprint.scm")))))


;;; Memoizing.

(defun blue--memoize (function)
  "Return a memoized version of FUNCTION."
  (let ((cache (make-hash-table :test 'equal)))
    (lambda (&rest args)
      (let ((result (gethash args cache 'not-found)))
        (if (eq result 'not-found)
            (let ((result (apply function args)))
              (puthash args result cache)
              result)
          result)))))

(defmacro blue--memoized-defun (name arglist docstring &rest body)
  "Define a memoized function NAME.

See `defun' for the meaning of NAME ARGLIST DOCSTRING and BODY."
  (declare (doc-string 3) (indent 2))
  `(defalias ',name
     (blue--memoize (lambda ,arglist ,@body))
     ;; Add '(name args ...)' string with real arglist to the docstring,
     ;; because *Help* will display '(name &rest ARGS)' for a defined
     ;; function (since `blue--memoize' returns a lambda with '(&rest
     ;; args)').
     ,(format "(%S %s)\n\n%s"
              name
              (mapconcat #'symbol-name arglist " ")
              docstring)))


;;; Cache.

(defun blue--read-cache-list ()
  "Initialize `blue--cache-list' using contents of `blue-cache-list-file'.

Each entry in `blue--cache-list` has the form:
  (root . (dir0 ... dirN))"
  (when-let* ((filename blue-cache-list-file)
              (contents (when (file-exists-p filename)
                          (with-temp-buffer
                            (insert-file-contents filename)
                            (condition-case nil
                                (read (current-buffer))
                              (end-of-file
                               (warn "Failed to read the BLUE cache list file due to unexpected EOF")))))))
    (setq blue--cache-list contents)))

(defun blue--ensure-read-cache-list ()
  "Initialize `blue--cache-list' if it isn't already initialized."
  (when (eq blue--cache-list 'unset)
    (blue--read-cache-list)))

(defun blue--write-cache-list ()
  "Save `blue--cache-list' in `blue-cache-list-file'.

Each entry has the form:
  (root . (dir0 ... dirN))"
  (let ((filename blue-cache-list-file))
    (with-temp-buffer
      (insert ";;; -*- lisp-data -*-\n")
      (let ((print-length nil)
            (print-level nil))
        (pp (mapcar (lambda (elem)
                      (let ((root (car elem))
                            (dirs (cdr elem)))
                        (list (blue--expand-if-remote-name root)
                              (mapcar #'blue--expand-if-remote-name dirs))))
                    blue--cache-list)
            (current-buffer)))
      (write-region nil nil filename nil 'silent))))

(defun blue--remember-cache (dir &optional no-write)
  "Add DIR under its project ROOT in `blue--cache-list`.

Each entry in `blue--cache-list` has the form:
  (root . (dir0 ...))

If ROOT is already present, DIR is added to its list (most recent first).
If ROOT is not present, a new entry is created.

If NO-WRITE is nil, save the updated list to `blue-cache-list-file'."
  (blue--ensure-read-cache-list)
  (let* ((root (expand-file-name (project-root (project-current nil dir))))
         (dir  (expand-file-name dir))
         (existing (assoc root blue--cache-list)))
    (if existing
        (let ((dirs (cdr existing)))
          ;; Move dir to front if already present, otherwise cons it
          (setcdr existing (cons dir (delete dir dirs))))
      (push (cons root (list dir)) blue--cache-list)))
  (unless no-write
    (blue--write-cache-list)))


(defun blue--project-cache (dir)
  "Get last cache configured for project containing DIR."
  (let ((root (expand-file-name (project-root (project-current nil dir)))))
    (caadr (assoc-string root blue--cache-list))))

(defvar blue--output-buffer " *blue output*"
  "Buffer used to capture output from BLUE commands.")


;;; Completion.

;; Example output:
;; (((invoke . "build")
;;   (category . build)
;;   (synopsis . "Build the project")
;;   (help . "[INPUTS] ...\nCompile all blue modules or only INPUTS."))
;;  ...)
(defun blue--run-serialize (blue-flags serialize-cmd)
  "Helper to run BLUE serialization commands and retrieve output.

The command logs in `blue--output-buffer'.

BLUE-FLAGS are the flags passed to 'blue'.
SERIALIZE-CMD is the serialization command to run."
  (let* ((buf (get-buffer-create blue--output-buffer))
         (time (current-time-string))
         ;; Disable autocompilation.
         (env (cons "GUILE_AUTO_COMPILE=0" process-environment))
         (path exec-path)
         (header (format "▶ %s  [%s]\n" (concat blue-binary " "
                                                (string-join blue-flags " ") " "
                                                serialize-cmd)
                         time))
         start-pos end-pos
         exit-code
         result)
    (with-current-buffer buf
      (let ((process-environment env)
            (exec-path path)
            (inhibit-read-only t))
        ;; Always prepend: go to beginning of buffer
        (goto-char (point-min))
        (unless (bobp) (insert "\n"))

        ;; Insert header and mark where command output starts
        (insert (propertize header 'face 'bold))
        (setq start-pos (point))

        ;; Run `blue`, capturing stdout in the buffer
        (condition-case _
            (setq exit-code (apply #'call-process blue-binary nil buf nil
                                   (append blue-flags (list serialize-cmd))))
          (file-missing
           (setq exit-code 'missing)))

        ;; Mark where command output ended
        (setq end-pos (point))

        ;; Footer
        (insert (propertize (format "\n⏹ Status: %s\n\n" exit-code)
                            'face (cond
                                   ((eq exit-code 0) 'success)
                                   ((eq exit-code 'missing) 'error)
                                   (t 'warning))))

        ;; Parse only the command’s stdout when exit-code was 0
        (if (eq exit-code 0)
            (let ((output (buffer-substring-no-properties start-pos end-pos)))
              (condition-case parse-err
                  (setq result (read output))
                ((end-of-file error)
                 (let ((error-msg (propertize (format "[Parse error] %s\n" parse-err)
                                              'face 'error)))
                   (insert error-msg)
                   (message error-msg)))))
          (let ((error-msg (if (numberp exit-code)
                               (propertize (format "[Blue] Error %s" exit-code)
                                           'face 'error)
                             (concat (propertize "[ERROR] " 'face 'error)
                                     (propertize "`blue'" 'face 'font-lock-constant-face)
                                     " command not found in "
                                     (propertize "`exec-path'" 'face 'font-lock-type-face)))))
            (insert error-msg)
            (message error-msg)))))
    result))

(defun blue--get-commands (blueprint)
  "Return the commands provided by BLUEPRINT."
  (interactive)
  (let ((blue-flags (if blueprint
                        (list "--file" blueprint)
                      "")))
    (blue--run-serialize blue-flags ".elisp-serialize-commands")))

(defun blue--get-configuration (blueprint dir)
  "Return the BLUEPRINT configuration stored in DIR."
  (interactive)
  (let ((blue-flags (if blueprint
                        (list "--file" blueprint
                              "--build-directory" dir)
                      "")))
    (blue--run-serialize blue-flags ".elisp-serialize-configuration")))

(defun blue--get-from-configuration (var configuration)
  "Retrieve variable VAR value from CONFIGURATION."
  (cdr (assoc-string var configuration)))

(blue--memoized-defun blue--autocomplete (input)
  "Use blue '.autocomplete' command to provide completion from INPUT."
  (let* ((completion-cmd (concat blue-binary " .autocomplete \"blue " input "\""))
         (completion (shell-command-to-string completion-cmd)))
    (string-split completion)))

(defun blue--autocomplete-from-prompt (&rest _)
  "Internal function to use in `blue--completion-at-point'."
  (let ((result
         (while-no-input
           (when-let* ((prompt-start (minibuffer-prompt-end))
                       (input (buffer-substring prompt-start (point)))
                       (completion-table (blue--autocomplete input)))
             completion-table))))
    (and (consp result) result)))

(defun pcomplete/blue ()
  "Completion for `blue'."
  (while (pcomplete-here* (blue--autocomplete-from-prompt))))

(defun blue--completion-at-point ()
  "`completion-at-point' for `blue-run-command'."
  (pcase (bounds-of-thing-at-point 'symbol)
    (`(,beg . ,end)
     (list beg end
           (completion-table-with-cache #'blue--autocomplete-from-prompt)
           :exclusive 'no))))

;; --- HINTS

(defface blue-minibuffer-hint-separator-face '((t :inherit shadow
                                                  :strike-through t))
  "Face used to separate hint overlay.")

(defvar blue--minibuffer-hint-overlay nil
  "Overlay for `dape--minibuffer-hint'.")

(defun blue--minibuffer-hint (&rest _)
  "Display current configuration in minibuffer in overlay."
  (let* ((configuration (blue--get-configuration blue--current-blueprint blue--last-configuration))
         (vars '("srcdir" "builddir"))
         (hint-rows (append (list "Previous configuration:")
                            (mapcar (lambda (var)
                                      (concat
                                       (propertize var 'face 'font-lock-keyword-face)
                                       " "
                                       (propertize (blue--get-from-configuration var configuration)
                                                   'face '(:inherit shadow :weight regular))))
                                    vars))))
    (unless blue--minibuffer-hint-overlay
      (setq blue--minibuffer-hint-overlay (make-overlay (point) (point))))
    (overlay-put blue--minibuffer-hint-overlay
                 'after-string
                 (concat
                  (when hint-rows
                    (concat
                     (mapconcat #'identity hint-rows "\n")
                     "\n"
                     (propertize
                      " " 'face 'blue-minibuffer-hint-separator-face
                      'display '(space :align-to right))
                     "\n"))))
    (move-overlay blue--minibuffer-hint-overlay
                  (point-min) (point-min) (current-buffer))))

;; --- HINTS

(defun blue--run-command-prompt ()
  "Interactive prompt used by `blue-run-command'."
  (if-let* ((blue--current-blueprint (blue--locate-blueprint))
            (commands (blue--get-commands blue--current-blueprint))
            (invocations (mapcar (lambda (cmd)
                                   (alist-get 'invoke cmd))
                                 commands))
            (width (if invocations
                       (apply #'max (mapcar #'string-width invocations))
                     0))
            (completion-extra-properties
             (list
              :annotation-function
              (lambda (cand)
                (let* ((entry (seq-find (lambda (cmd)
                                          (string= cand
                                                   (alist-get 'invoke cmd)))
                                        commands))
                       (synopsis (alist-get 'synopsis entry)))
                  (when synopsis
                    (concat (make-string (+ blue--anotation-padding
                                            (- width (string-width cand)))
                                         ?\s)
                            (propertize synopsis 'face 'blue-documentation)))))
              :group-function
              (lambda (cand transform)
                (if transform
                    cand
                  (let* ((entry (seq-find (lambda (cmd)
                                            (string= cand
                                                     (alist-get 'invoke cmd)))
                                          commands))
                         (category (alist-get 'category entry)))
                    (symbol-name category))))))
            (crm-separator
             (propertize "[ \t]*--[ \t]+"
                         'separator "--"
                         'description "double-dash-separated list"))
            ;; HACK: redundant but avoids displaying `crm-separator' as part of
            ;; the prompt. Otherwhise prompt will looke like:
            ;; '[double-dash-separated list] [CRM--[ 	]+] Command:'
            (prompt "Command: ")
            (crm-prompt
             (concat "[%d] [CMR%s] " prompt)))
      (list (minibuffer-with-setup-hook
                (lambda ()
                  ;; Emacs default completion maps <SPC> to `crm-complete-word'
                  ;; which forces the user to introduce spaces using
                  ;; `quoted-insert'. Remove the keybinding so literal spaces can
                  ;; be introduces.
                  (define-key crm-local-completion-map (kbd "SPC") nil)
                  (add-hook 'completion-at-point-functions
                            #'blue--completion-at-point nil t)
                  (add-hook 'after-change-functions
                            #'blue--minibuffer-hint nil t)
                  (blue--minibuffer-hint))
              (completing-read-multiple prompt invocations))
            commands
            (consp current-prefix-arg))
    '(unset)))

;;;###autoload
(defun blue-run-command (input &optional commands comint-flip)
  "Run a BLUE command.

INPUT is a list of strings of BLUE arguments, COMMANDS and their
arguments.

COMINT-FLIP whether to invert the comint euristics logic.  If input
contains a member of `blue-interactive-commands', starts an interactive
compilation buffer in comint mode, if COMINT-FLIP is t, invert that
behavior so the interactive buffer will be created if the command is not
a member of `blue-interactive-commands'."
  (interactive (blue--run-command-prompt))
  ;; List of chained commands in user input, with arguments. Each element is a
  ;; list of strings representing the arguments and invoke of commands.
  (unless (eq input 'unset) ; Special value since empty input is allowed.
    (let* ((blue-flags (if (or (null blue-default-flags)
                               (stringp blue-default-flags))
                           blue-default-flags
                         (string-join blue-default-flags " ")))
           (input-tokens (mapcar (lambda (command)
                                   (string-split command))
                                 input))
           ;; The first token list needs to be treated specially since it includes
           ;; the arguments meant for BLUE as well as the first chained command
           ;; and it's arguments.
           (first-invoke (car (last (car input-tokens))))
           ;; Aside from the first one, all other tokens start with the command
           ;; name followed by it's arguments.
           (rest-invokes (mapcar (lambda (token)
                                   (car token))
                                 (cdr input-tokens)))
           (invokes (cons first-invoke rest-invokes))
           (any-interactive (seq-some (lambda (command)
                                        (member command blue-interactive-commands))
                                      invokes))
           ;; `xor' allows to use `comint-flip' to invert the mode for the
           ;; compilation, if `input' is part of `blue-interactive-commands',
           ;; calling `blue-run-command' with universal prefix argument
           ;; (comint-flip = t) will disable comint mode for the compilation
           ;; buffers while enabling it for all other commands.
           (inter (if (xor any-interactive
                           comint-flip)
                      t nil))
           (configuration (seq-find (lambda (command)
                                      (string= command "configure"))
                                    invokes))
           ;; Each entry is a serialized command data from BLUE.
           (entries (mapcar (lambda (command)
                              (seq-find (lambda (cmd)
                                          (string= command
                                                   (alist-get 'invoke cmd)))
                                        commands))
                            invokes))
           (any-requires-configuration (seq-some (lambda (entry)
                                                   (alist-get 'requires-configuration? entry))
                                                 entries)))
      (blue--ensure-read-cache-list)
      (when configuration
        (message (concat "Configuration requested, next command that requires a configuration will
run under " (propertize "`blue--last-configuration'" 'face 'bold) " directory."))
        (blue--remember-cache default-directory))
      (setq blue--last-configuration (blue--project-cache default-directory))
      (blue--compile (concat blue-binary " " blue-flags (when blue-flags " ")
                             (string-join input " -- "))
                     any-requires-configuration inter))))

(provide 'blue)
;;; blue.el ends here.
