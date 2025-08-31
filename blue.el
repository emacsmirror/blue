;;; blue.el --- BLUE build system interface -*- lexical-binding: t -*-
;;
;; Copyright © 2025 Sergio Pastor Pérez
;;
;; Author: Sergio Pastor Pérez <sergio.pastorperez@gmail.com>
;; Version: 0.0.2
;; Package-Requires: ((emacs "31.0.50"))
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

(defgroup blue nil
  "Operations on the current project."
  :version "31.1"
  :group 'tools)

(defcustom blue-interactive-commands '("repl")
  "List of strings of interactive BLUE commands.
Interactive commands will run in comint mode compilation buffers."
  :group 'blue
  :type '(repeat string))

(defcustom blue-cache-list-file
  (locate-user-emacs-file "blue.eld")
  "File in which to save the list of known BLUE caches."
  :type 'file
  :version "31.1"
  :group 'blue)

(defface blue-documentation
  '((t :inherit completions-annotations))
  "Face used to highlight documentation strings.")

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
         (compilation-buffer-name-function #'(lambda (name-of-mode)
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

(defun blue--expand-if-remote-name (path)
  "Expand PATH if it's a file."
  (if (file-remote-p path) path
    (expand-file-name path)))

(defun blue--read-cache-list ()
  "Initialize `blue--cache-list' using contents of `blue-cache-list-file'."
  (let ((filename blue-cache-list-file))
    (setq blue--cache-list
          (when (file-exists-p filename)
            (with-temp-buffer
              (insert-file-contents filename)
              (mapcar (lambda (elem)
                        (let ((root (car elem))
                              (dir (cdr elem)))
                          (cons (blue--expand-if-remote-name root)
                                (blue--expand-if-remote-name dir))))
                      (condition-case nil
                          (read (current-buffer))
                        (end-of-file
                         (warn "Failed to read the BLUE cache list file due to unexpected EOF")))))))
    (unless (seq-every-p
             (lambda (elt) (stringp (car-safe elt)))
             blue--cache-list)
      (warn "Contents of %s are in wrong format, resetting"
            blue-cache-list-file)
      (setq blue--cache-list nil))))

(defun blue--ensure-read-cache-list ()
  "Initialize `blue--cache-list' if it isn't already initialized."
  (when (eq blue--cache-list 'unset)
    (blue--read-cache-list)))

(defun blue--write-cache-list ()
  "Save `blue--cache-list' in `blue-cache-list-file'."
  (let ((filename blue-cache-list-file))
    (with-temp-buffer
      (insert ";;; -*- lisp-data -*-\n")
      (let ((print-length nil)
            (print-level nil))
        (pp (mapcar (lambda (elem)
                      (let ((root (car elem))
                            (dir (cdr elem)))
                        (cons (blue--expand-if-remote-name root)
                              (blue--expand-if-remote-name dir))))
                    blue--cache-list)
            (current-buffer)))
      (write-region nil nil filename nil 'silent))))

(defun blue--remember-cache (dir &optional no-write)
  "Add DIR to the front of the cache list.
Save the result in `blue-cache-list-file' if the list of dir has
changed, and NO-WRITE is nil."
  (blue--ensure-read-cache-list)
  (let* ((root (expand-file-name (project-root (project-current nil dir))))
         (dir (expand-file-name dir))
         (pair (cons root dir)))
    (unless (equal (car blue--cache-list) pair)
      (dolist (ent blue--cache-list)
        (when (equal root (car ent))
          (setq blue--cache-list (delq ent blue--cache-list))))
      (push pair blue--cache-list)
      (unless no-write
        (blue--write-cache-list)))))

(defun blue--project-cache (dir)
  "Get last cache configured for project containing DIR."
  (let ((root (expand-file-name (project-root (project-current nil dir)))))
    (cdr (assoc-string root blue--cache-list))))

;; Example output:
;; (((invoke . "build")
;;   (category . build)
;;   (synopsis . "Build the project")
;;   (help . "[INPUTS] ...\nCompile all blue modules or only INPUTS."))
;;  ...)
(defvar blue--output-buffer " *blue output*"
  "Buffer used to capture output from BLUE commands.")

(defun blue--get-commands ()
  "Return the commands provided by `blue .elisp-serialize-commands`.

Each invocation prepends output to `blue--output-buffer' with a header
[command + timestamp] and a propertized status footer.

On success, returns the parsed Lisp value.
On failure, returns nil."
  (interactive)
  (let* ((buf (get-buffer-create blue--output-buffer))
         (cmd "blue .elisp-serialize-commands")
         (time (current-time-string))
         ;; Disable autocompilation.
         (env (cons "GUILE_AUTO_COMPILE=0" process-environment))
         (path exec-path)
         (header (format "▶ %s  [%s]\n" cmd time))
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
        (condition-case call-err
            (setq exit-code (call-process "blue" nil buf nil ".elisp-serialize-commands"))
          (file-missing
           (let ((error-msg (concat (propertize "[ERROR] " 'face 'error)
                                    (propertize "`blue'" 'face 'font-lock-constant-face)
                                    " command not found in "
                                    (propertize "`exec-path'" 'face 'font-lock-type-face))))
             (insert error-msg)
             (message error-msg))
           (setq exit-code 'missing
                 result nil)))

        ;; Mark where command output ended
        (setq end-pos (point))

        ;; Footer
        (insert (concat(propertize (format "\n⏹ Status: %s\n\n" exit-code)
                                   'face (cond
                                          ((eq exit-code 0) 'success)
                                          ((eq exit-code 'missing) 'error)
                                          (t 'warning)))))

        ;; Handle failures
        (unless result
          (when (and (numberp exit-code) (not (zerop exit-code)))
            (setq result (list 'blue-exit exit-code))))

        ;; Parse only the command’s stdout when exit-code was 0
        (when (and (eq exit-code 0) (not result))
          (let ((output (buffer-substring-no-properties start-pos end-pos)))
            (condition-case parse-err
                (setq result (read output))
              ((end-of-file error)
               (let ((error-msg (propertize (format "[Parse error] %S\n" parse-err)
                                            'face 'error)))
                 (insert error-msg)
                 (message error-msg))
               (setq result nil)))))))
    result))

(defun blue--autocomplete (input)
  "Invoke BLUE autocompletion command with INPUT string."
  (let* ((beg (minibuffer-prompt-end))
         (end (point-max))
         (input (buffer-substring beg end)))
    input))

(defun blue--completion-at-point ()
  "Function for `completion-at-point' fn for `blue-run-command'."
  (let* ((prompt-start (minibuffer-prompt-end))
         (pt (point))
         (input (buffer-substring prompt-start pt))
         (autocompletion-raw (shell-command-to-string
                              (concat "blue .autocomplete \"blue " input "\"")))
         (completion-table (string-split autocompletion-raw))
         (symbol-boundaries (bounds-of-thing-at-point 'symbol)))
    (if completion-table
        (if-let* (symbol-boundaries
                  (symbol-start (car symbol-boundaries))
                  (symbol-end (cdr symbol-boundaries)))
            (list symbol-start symbol-end completion-table)
          (list pt pt completion-table))
      nil)))

(defun blue--run-command-prompt ()
  "Interactive prompt used by `blue-run-command'."
  (if-let* ((commands (blue--get-commands))
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
                            #'blue--completion-at-point nil t))
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
    (let* ((input-tokens (mapcar (lambda (command)
                                   (string-split command))
                                 input))
           ;; The first token list needs to be treated specially since it includes
           ;; the arguments meant for BLUE as well as the first chained command
           ;; and it's arguments.
           (first-invoke (seq-find (lambda (token)
                                     (not (string-prefix-p "--" token)))
                                   (car input-tokens)))
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
           ;; (commint-flip = t) will disable comint mode for the compilation
           ;; buffers while enabling it for all other commands.
           (inter (if (xor any-interactive
                           comint-flip)
                      t nil))
           (configuration (seq-find (lambda (command)
                                      (string= command "configure"))
                                    input))
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
        (blue--remember-cache default-directory))
      (setq blue--last-configuration (blue--project-cache default-directory))
      (blue--compile (concat "blue " (string-join input " -- "))
                     any-requires-configuration inter))))

(provide 'blue)
;;; blue.el ends here.
