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

(defface blue-documentation
  '((t :inherit completions-annotations))
  "Face used to highlight documentation strings.")

(defvar blue-interactive-commands '("repl")
  "List of strings of interactive BLUE commands.
Interactive commands will run in comint mode compilation buffers.")

(defvar blue--anotation-padding 8
  "Padding used for alignment of synopsis in completing read.")

(defvar blue--last-configuration nil
  "Path to last known configuration directory.")

;; Example output:
;; (((invoke . "build")
;;   (category . build)
;;   (synopsis . "Build the project")
;;   (help . "[INPUTS] ...\nCompile all blue modules or only INPUTS."))
;;  ...)
(defun blue--get-commands ()
  ;; Run `blue' in `default-directory'.
  (let ((commands-raw (shell-command-to-string "blue .elisp-serialize-commands")))
    (read commands-raw)))

;;;###autoload
(defun blue-run-command (command &optional commands comint-flip)
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
                  (symbol-name category)))))))
     (list (completing-read "Command: " invocations)
           commands
           (consp current-prefix-arg))))
  ;; `xor' allows to use `comint-flip' to invert the mode for the compilation,
  ;; if `command' is part of `blue-interactive-commands', calling
  ;; `blue-run-command' with universal prefix argument (commint-flip = t) will
  ;; disable comint mode for the compilation buffers while enabling it for all
  ;; other commands.
  (let* ((inter (if (xor (member command blue-interactive-commands)
                         comint-flip)
                    t nil))
         (configuration (string= command "configure"))
         (entry (seq-find (lambda (cmd)
                            (string= command
                                     (alist-get 'invoke cmd)))
                          commands))
         (requires-configuration (alist-get 'requires-configuration? entry)))
    (when configuration
      (setq blue--last-configuration default-directory))
    ;; If the command requires a configuration cache, try to run it in the last
    ;; configured directory.
    (let ((default-directory (if requires-configuration
                                 blue--last-configuration
                               default-directory)))
      (compile (concat "blue " command) inter))))

(provide 'blue)
;;; blue.el ends here.
