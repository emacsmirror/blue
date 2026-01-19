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

;; TODO: refactor duplicated code.

(require 'blue)

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

(defvar blue-complete--option-value-prefix "="
  "Regex to identify inputs expecting a value for an option")

(defvar blue-complete--file-properties
  (list :annotation-function (lambda (s) (if (string-suffix-p "/" s) " Dir" " File"))
        :company-kind (lambda (s) (if (string-suffix-p "/" s) 'folder 'file))
        :exclusive 'no
        :category 'file)
  "Completion extra properties for `blue-complete--file'.")


;;; Helpers.

(blue--define-memoized blue-complete--autocomplete (blueprint input)
  "Use blue '.autocomplete' command to provide completion from INPUT."
  (let* ((default-directory (or (blue--get-build-dir) default-directory))
         (command (concat blue-binary
                          " --file=" blueprint
                          " .autocomplete bash \"blue " input "\""))
         (output (shell-command-to-string command)))
    (string-split output)))

(defun blue-complete--table (&rest _)
  "Completion table function for minibuffer prompt."
  (let ((result
         (while-no-input
           (when-let* ((blueprint (or blue--blueprint
                                      (blue--find-blueprint)))
                       (prompt-start (minibuffer-prompt-end))
                       (input (buffer-substring prompt-start (point)))
                       (completions (blue-complete--autocomplete blueprint input)))
             completions))))
    (and (consp result) result)))

(defun blue-complete--bounds (thing)
  "Return bounds of THING."
  (or (bounds-of-thing-at-point thing) (cons (point) (point))))

(defun blue-complete--file ()
  "Complete file name at point."
  (pcase-let* ((prefix
                (and
                 (looking-back
                  (regexp-opt (ensure-list blue-complete--option-value-prefix) t)
                  (pos-bol))
                 (match-end 1)))
               (`(,beg . ,end) (if prefix
                                   (cons prefix (point))
                                 (blue-complete--bounds 'filename)))
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

(defun blue-complete--directory ()
  "Complete directory name at point."
  (pcase-let* ((prefix
                (and
                 (looking-back
                  (regexp-opt (ensure-list blue-complete--option-value-prefix) t)
                  (pos-bol))
                 (match-end 1)))
               (`(,beg . ,end) (if prefix
                                   (cons prefix (point))
                                 (blue-complete--bounds 'filename)))
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

(defun blue-complete--system-name ()
  "Complete system name at point."
  (pcase-let* ((prefix
                (and
                 (looking-back
                  (regexp-opt (ensure-list blue-complete--option-value-prefix) t)
                  (pos-bol))
                 (match-end 1)))
               (`(,beg . ,end) (if prefix
                                   (cons prefix (point))
                                 (blue-complete--bounds 'symbol))))
    (when prefix
      `( ,beg ,end
         ,blue-complete-target-names
         :company-kind (lambda (_) 'macro)
         :exclusive 'no))))

(defun blue-complete--set (values)
  "Complete VALUES from a set at point."
  (pcase-let* ((prefix
                (and
                 (looking-back
                  (regexp-opt (ensure-list blue-complete--option-value-prefix) t)
                  (pos-bol))
                 (match-end 1)))
               (`(,beg . ,end) (if prefix
                                   (cons prefix (point))
                                 (blue-complete--bounds 'symbol))))
    (when prefix
      `( ,beg ,end
         ,values
         :company-kind (lambda (_) 'event)
         :exclusive 'no))))

(defun blue--get-command-options-completion-table (command bounds)
  "Generate an appropriate completion table for COMMAND respecting BOUNDS."
  (let* ((options (alist-get 'options command))
         (labels
          (mapcar
           #'(lambda (option)
               (let* ((arguments (alist-get 'arguments option))
                      (type (alist-get 'type arguments))
                      (labels (blue--get-option-labels option))
                      (short-labels (car labels))
                      (long-labels (cdr labels)))
                 (cond
                  (long-labels
                   (concat "--" (car long-labels)
                           (if (string= type "required")
                               "="
                             " ")))
                  (short-labels
                   (concat "-" (car short-labels) " "))
                  (t
                   nil))))
           options)))
    `( ,(car bounds) ,(cdr bounds)
       ,labels
       :company-doc-buffer
       (lambda (candidate)
         (when-let* ((label (string-trim candidate "--?" "="))
                     (option (blue--get-option-from-label label ',options))
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
             (current-buffer))))
       :affixation-function
       (lambda (candidates)
         (mapcar
          (lambda (candidate)
            (if-let* ((label (string-trim candidate "--?" "="))
                      (option (blue--get-option-from-label label ',options))
                      (arguments (alist-get 'arguments option))
                      (arg-name (alist-get 'name arguments)))
                (list candidate "" (propertize arg-name 'face 'blue-documentation))
              (list candidate "" "")))
          candidates))
       :company-kind (lambda (_) 'property)
       :exclusive no)))

(defun blue--get-ui-completion-table (options bounds)
  "Generate an appropriate completion table for UI OPTIONS."
  (let* ((labels
          (mapcar
           #'(lambda (option)
               (let* ((arguments (alist-get 'arguments option))
                      (type (alist-get 'type arguments))
                      (labels (blue--get-option-labels option))
                      (short-labels (car labels))
                      (long-labels (cdr labels)))
                 (cond
                  (long-labels
                   (concat "--" (car long-labels)
                           (if (string= type "required")
                               "="
                             " ")))
                  (short-labels
                   (concat "-" (car short-labels) " "))
                  (t
                   nil))))
           options)))
    `( ,(car bounds) ,(cdr bounds)
       ,labels
       :company-doc-buffer
       (lambda (candidate)
         (when-let* ((label (string-trim candidate "--?" "="))
                     (option (blue--get-option-from-label label ',options))
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
             (current-buffer))))
       :affixation-function
       (lambda (candidates)
         (mapcar
          (lambda (candidate)
            (if-let* ((label (string-trim candidate "--?" "="))
                      (option (blue--get-option-from-label label ',options))
                      (arguments (alist-get 'arguments option))
                      (arg-name (alist-get 'name arguments)))
                (list candidate "" (propertize arg-name 'face 'blue-documentation))
              (list candidate "" "")))
          candidates))
       :company-kind (lambda (_) 'property)
       :exclusive no)))

(defun blue-completion--complete-autocompletable (autocompletable)
  "Helper to produce completion table for AUTOCOMPLETABLE object respecting
bounds."
  (let* ((autocomplete (alist-get 'autocomplete autocompletable))
         (type (alist-get 'type autocomplete))
         (values (alist-get 'values autocomplete)))
    (cond
     ((string-equal type "directory")
      (blue-complete--directory))
     ((string-equal type "file")
      (blue-complete--file))
     ((string-equal type "system-name")
      (blue-complete--system-name))
     ((string-equal type "set")
      (blue-complete--set values)))))


;;; Interfaces.

(defun blue-completion-at-point ()
  "`completion-at-point' function for `blue-run-command'."
  (when blue--data
    (let* ((commands (car blue--data))
           (invocations (blue--get-command-invocations commands))
           (ui-options (caddr blue--data))
           (prompt-start (minibuffer-prompt-end))
           (input (buffer-substring-no-properties prompt-start (point)))
           (bounds-at-pt (or (bounds-of-thing-at-point 'symbol)
                             (cons (point) (point))))
           (cmds+args (string-split input " -- "))
           (last-cmd+args (car (last cmds+args)))
           (last-cmd (and last-cmd+args
                          (seq-find (lambda (token) ; Remove BLUE flags.
                                      (member token invocations))
                                    (string-split last-cmd+args))))
           (cmd (and last-cmd (blue--get-command last-cmd commands)))
           (options (alist-get 'options cmd)))
      (cond
       ;; Option value completion (from command or UI).
       ((and (looking-back
              (regexp-opt (ensure-list blue-complete--option-value-prefix) t)
              (pos-bol))
             (match-end 1))
        (let* ((thing (thing-at-point 'symbol))
               (label (string-trim thing "--?" "="))
               (options (if cmd options ui-options))
               (option (blue--get-option-from-label label options)))
          (blue-completion--complete-autocompletable option)))
        ;; UI option completion.
       ((and (not cmd)
             (looking-back "\\(^\\|\s\\|\t\\)-+[^\s]*" (pos-bol))
             (match-end 1))
        (blue--get-ui-completion-table ui-options bounds-at-pt))
       ;; Command option completion.
       ((and cmd
             (looking-back "\\(^\\|\s\\|\t\\)-+[^\s]*" (pos-bol))
             (match-end 1))
        (blue--get-command-options-completion-table cmd bounds-at-pt))
       ;; Command argument completion.
       (cmd
        (let ((blue-complete--option-value-prefix " ")) ; Arguments are space separated.
          (blue-completion--complete-autocompletable cmd)))
       ;; Command invocation completion.
       ((not cmd)
        `( ,(car bounds-at-pt) ,(cdr bounds-at-pt)
           ,invocations
           ,@(blue--command-completion-properties commands)))))))

;;;###autoload
(defun pcomplete/blue ()
  "Completion for `blue'."
  (while (pcomplete-here* (blue-complete--table))))

(provide 'blue-complete)
;;; blue-complete.el ends here.
