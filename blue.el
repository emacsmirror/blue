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

(defvar blue--anotation-padding 8
  "Padding used for alignment of synopsis in completing read.")

;; Example output:
;; (((invoke . "build")
;;   (category . build)
;;   (synopsys . "Build the project")
;;   (help . "[INPUTS] ...\nCompile all blue modules or only INPUTS."))
;;  ...)
(defun blue--get-commands ()
  ;; Run `blue' in `default-directory'.
  (let ((commands-raw (shell-command-to-string "blue .elisp-serialize")))
    (read commands-raw)))

;;;###autoload
(defun blue-run-command (command)
  "Run a BLUE command."
  (interactive
   (let* ((commands (blue--get-commands))
          (invocations (mapcar (lambda (c) (cdr (assq 'invoke c))) commands))
          (width (apply #'max (mapcar #'string-width invocations)))
          (completion-extra-properties
           (list
            :annotation-function
            (lambda (cand)
              (let* ((entry (seq-find (lambda (c)
                                        (string= cand (cdr (assq 'invoke c))))
                                      commands))
                     (syn (cdr (assq 'synopsys entry))))
                (when syn
                  (concat (make-string (+ blue--anotation-padding
                                          (- width (string-width cand)))
                                       ?\s)
                          (propertize syn 'face 'blue-documentation)))))
            :group-function
            (lambda (cand transform)
              (if transform
                  cand
                (let* ((entry (seq-find (lambda (invoke)
                                          (string= cand
                                                   (alist-get 'invoke invoke)))
                                        commands))
                       (category (alist-get 'category entry)))
                  (symbol-name category)))))))
     (list (completing-read "Command: " invocations))))
  (message (concat "Running " command "...")))

(provide 'blue)
;;; blue.el ends here.
