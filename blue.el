;;; blue.el --- BLUE build system interface -*- lexical-binding: t -*-
;;
;; Copyright © 2025 Sergio Pastor Pérez
;;
;; Author: Sergio Pastor Pérez <sergio.pastorperez@gmail.com>
;; Version: 0.0.1
;; URL: https://codeberg.org/pastor/blue.el
;; Keywords: extensions
;; Package-Requires: ((emacs "31.0.50"))

;;; Commentary:

;; Interface for the BLUE build system.

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
  "Function used by BLUE to name the compilation buffer used to run target
command.

The function must take 2 arguments, the COMMAND being run and
NAME-OF-MODE which is the major mode of the compilation buffer.")

(defun blue--compilation-default-buffer-name (command name-of-mode)
  (concat "*" name-of-mode " | " command "*"))

(defun blue--compile (command &optional requires-configuration comint)
  "Like `compile' but tailored for BLUE."
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
  (let* ((root (abbreviate-file-name (project-root (project-current nil dir))))
         (dir (abbreviate-file-name dir))
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
;; TODO: rewrite to handle errors and not rely on a shell, maybe use
;; `call-process'.
(defun blue--get-commands ()
  ;; Run `blue' in `default-directory'.
  (let ((commands-raw (shell-command-to-string "blue .elisp-serialize-commands")))
    (read commands-raw)))

(defun blue--autocomplete (input)
  "Invoke BLUE autocompletion command with INPUT string."
  (let* ((beg (minibuffer-prompt-end))
         (end (point-max))
         (input (buffer-substring beg end)))
    input))

(defun blue--completion-at-point ()
  "Function for `completion-at-point' fn for `blue-run-command'."
  (let* ((prompt-start (minibuffer-prompt-end))
         (prompt-end (point-max))
         (input (buffer-substring prompt-start prompt-end))
         (autocompletion-raw (shell-command-to-string
                              (concat "blue .autocomplete \"blue " input "\"")))
         (completion-table (string-split autocompletion-raw))
         (symbol-boundaries (bounds-of-thing-at-point 'symbol))
         (pt (point)))
    (if completion-table
        (if-let* (symbol-boundaries
                  (symbol-start (car symbol-boundaries))
                  (symbol-end (cdr symbol-boundaries)))
            (list symbol-start symbol-end completion-table)
          (list pt pt completion-table))
      nil)))

;;;###autoload
(defun blue-run-command (input &optional commands comint-flip)
  "Run a BLUE command."
  (interactive
   (let* ((commands (blue--get-commands))
          (invocations (mapcar (lambda (cmd)
                                 (alist-get 'invoke cmd))
                               commands))
          (width (apply #'max (mapcar #'string-width invocations)))
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
           (consp current-prefix-arg))))

  ;; List of chained commands in user input, with arguments. Each element is a
  ;; list of strings representing the arguments and invoke of commands.
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
         (invokes (print (cons first-invoke rest-invokes)))
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
      (blue--remember-cache blue--last-configuration))
    (setq blue--last-configuration (or (blue--project-cache default-directory)
                                       default-directory))
    (blue--compile (concat "blue " (string-join input " -- "))
                   any-requires-configuration inter)))

(provide 'blue)
;;; blue.el ends here.
