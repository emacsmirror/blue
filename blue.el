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

;; Example output: ((:invoke "build" :category build :synopsys "Build the
;; project" :help "[INPUTS] ...  Compile all blue modules or only INPUTS.") ...)
(defun blue--get-commands ()
  ;; Run `blue' in `default-directory'.
  (let ((commands-raw (shell-command-to-string "blue .elisp-serialize")))
    (read commands-raw)))

;;;###autoload
(defun blue-run-command (command)
  "Run a BLUE command."
  (interactive
   (let* ((commands (blue--get-commands))
          (invocations (mapcar (lambda (c) (plist-get c :invoke)) commands))
          (synopses (mapcar (lambda (c) (plist-get c :synopsys)) commands))
          (collection (seq-mapn #'cons invocations synopses))
          (width (apply #'max (mapcar #'string-width invocations)))
          (padding 8)
          (completion-extra-properties
           (list :annotation-function
                 (lambda (cand)
                   (let ((desc (alist-get cand minibuffer-completion-table nil nil #'string=)))
                     (when desc
                       (concat (make-string (+ padding (- width (string-width cand))) ?\s)
                               (propertize desc 'face 'blue-documentation))))))))
     (list (completing-read "Command: " collection))))
  (message (concat "Running " command "...")))

(provide 'blue)
;;; blue.el ends here.
