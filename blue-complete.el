;;; blue-complete.el --- BLUE build system completion -*- lexical-binding: t -*-
;;
;; Copyright © 2025 Sergio Pastor Pérez

;;; Commentary:

;; This module implements completion related functionality for BLUE.

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

(require 'blue)

(defcustom blue-complete--prefix "="
  "File completion trigger prefixes.
The value can be a string or a list of strings.  The default
`file:' is the prefix of Org file links which work in arbitrary
buffers via `org-open-at-point-global'."
  :type 'string
  :group 'blue-complete)

(defcustom blue-complete-target-names
  '(
    "aarch64-linux-gnu"
    "arm-linux-gnueabihf"
    "avr"
    "i586-pc-gnu"
    "i686-linux-gnu"
    "i686-w64-mingw32"
    "loongarch64-linux-gnu"
    "mips64el-linux-gnu"
    "or1k-elf"
    "powerpc-linux-gnu"
    "powerpc64-linux-gnu"
    "powerpc64le-linux-gnu"
    "riscv64-linux-gnu"
    "x86_64-linux-gnu"
    "x86_64-linux-gnux32"
    "x86_64-pc-gnu"
    "x86_64-w64-mingw32"
    "xtensa-ath9k-elf")
  "Common system names."
  :group 'blue-complete
  :type '(repeat string))

(defvar blue-complete--file-properties
  (list :annotation-function (lambda (s) (if (string-suffix-p "/" s) " Dir" " File"))
        :company-kind (lambda (s) (if (string-suffix-p "/" s) 'folder 'file))
        :exclusive 'no
        :category 'file)
  "Completion extra properties for `blue--complete-file'.")


;;; Helpers.

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

(defun blue-complete--bounds (thing)
  "Return bounds of THING."
  (or (bounds-of-thing-at-point thing) (cons (point) (point))))

(defun blue--complete-file ()
  "Complete file name at point."
  (pcase-let* ((prefix (and blue-complete--prefix
                            (looking-back
                             (concat
                              (regexp-opt (ensure-list blue-complete--prefix) t)
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
  (pcase-let* ((prefix (and blue-complete--prefix
                            (looking-back
                             (concat
                              (regexp-opt (ensure-list blue-complete--prefix) t)
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

(defun blue--complete-system-name ()
  "Complete system name at point."
  (pcase-let* ((prefix (and blue-complete--prefix
                            (looking-back
                             (concat
                              (regexp-opt (ensure-list blue-complete--prefix) t)
                              "[^ \n\t]*")
                             (pos-bol))
                            (match-end 1)))
               (`(,beg . ,end) (if prefix
                                   (cons prefix (point))
                                 (blue-complete--bounds 'filename)))
               (non-essential t))
    (when prefix
      `( ,beg ,end
         ,blue-complete-target-names
         :company-kind (lambda (s) 'macro)
         :exclusive 'no))))

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


;;; Interfaces.

(defun blue-completion-at-point ()
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
           (affixation-function
            (lambda (candidates)
              (mapcar
               (lambda (candidate)
                 (if-let* ((long-label (string-trim candidate "--" "="))
                           (option (blue--get-option-from-label long-label cmd))
                           (arguments (alist-get 'arguments option))
                           (arg-name (alist-get 'name arguments)))
                     (list candidate "" (propertize arg-name 'face 'blue-documentation))
                   (list candidate "" "")))
               candidates)))
           (kind-function
            (lambda (candidate)
              (cond
               ((string-prefix-p "--" candidate) 'property)
               ((member candidate commands) 'command)
               (t 'event))))
           (options-doc-buffer-function
            (lambda (candidate)
              (when-let* ((long-label (string-trim candidate "--" "="))
                          (option (blue--get-option-from-label long-label cmd))
                          (doc (alist-get 'doc option))
                          (arguments (alist-get 'arguments option))
                          (arg-name (alist-get 'name arguments)))
                (with-current-buffer (get-buffer-create "*blue-capf-doc*")
                  (erase-buffer)
                  (insert doc)
                  (font-lock-add-keywords
                   nil
                   `((,arg-name . 'blue-documentation)))
                  (font-lock-mode 1)
                  (font-lock-ensure)
                  (current-buffer)))))
           (argument-completion-properties
            (list
             :affixation-function affixation-function
             :company-doc-buffer options-doc-buffer-function
             :company-kind kind-function
             :exclusive 'no))
           (table
            (while-no-input
              (and cmd
                   (blue--get-command-completion-table cmd)))))
      (pcase (bounds-of-thing-at-point 'symbol)
        ;; Long option argument completion.
        ((pred (lambda (_)
                 (and blue-complete--prefix
                      (looking-back
                       (concat
                        (regexp-opt (ensure-list blue-complete--prefix) t)
                        "[^ \n\t]*")
                       (pos-bol))
                      (match-end 1))))
         (let* ((thing (thing-at-point 'symbol))
                (long-label (string-trim thing "--" "="))
                (option (blue--get-option-from-label long-label cmd))
                (autocomplete (alist-get 'autocomplete option)))
           (cond
            ((string-equal autocomplete "directory")
             (blue--complete-directory))
            ((string-equal autocomplete "file")
             (blue--complete-file))
            ((string-equal autocomplete "system-name")
             (blue--complete-system-name))
            ((and (string-equal autocomplete "set")
                  table)
             `( ,(point) ,(point)
                ,table
                ,@argument-completion-properties)))))
        ;; Resume completion of partial argument input.
        (`(,beg . ,end)
         `( ,beg ,end
            ,table
            ,@argument-completion-properties))
        ;; Command argument completion.
        (_
         (if-let* ((autocomplete (blue--command-get-slot 'autocomplete cmd))
                   (type (alist-get 'type autocomplete))
                   (blue-complete--prefix ""))
             (cond
              ((string-equal type "directory")
               (blue--complete-directory))
              ((string-equal type "file")
               (blue--complete-file))
              ((string-equal type "system-name")
               (blue--complete-system-name))
              ((and (string-equal type "set")
                    table)
               `( ,(point) ,(point)
                  ,table
                  ,@argument-completion-properties)))
           `( ,(point) ,(point)
              ,commands
              ,@(blue--command-completion-properties commands))))))))

;;;###autoload
(defun pcomplete/blue ()
  "Completion for `blue'."
  (while (pcomplete-here* (blue--completion-table))))

(provide 'blue-complete)
;;; blue-complete.el ends here.
