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
(require 'minibuffer)

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


;;; Async autocomplete state.

(defvar blue-complete--process nil
  "Currently running autocomplete process.")

(defvar blue-complete--last-input nil
  "Input string that produced `blue-complete--candidates'.")

(defvar blue-complete--candidates nil
  "Cached completion candidates from the last finished process.")

(defvar blue-complete--debounce-timer nil
  "Timer used to debounce process spawning.")

(defconst blue-complete--debounce-delay 0.08
  "Delay after last keystroke before spawning the autocomplete process.")

;;; Helpers.

(defun blue-complete--kill-process (process)
  "Kill running autocomplete PROCESS and clear `blue-complete--process'."
  (when (process-live-p process)
    (delete-process process))
  (setq blue-complete--process nil))

(defun blue-complete--spawn (blueprint input callback)
  "Spawn an async autocomplete process with INPUT for BLUEPRINT.

CALLBACK is called with the list of completion strings when done."
  (blue-complete--kill-process blue-complete--process)
  (let* ((default-directory (or (blue--get-build-dir) default-directory))
         (process-environment
          (append (list "GUILE_AUTO_COMPILE=0"
                        (concat "BLUE_BLUEPRINT=" blueprint))
                  process-environment))
         (proc (make-process
                :name "blue-autocomplete"
                :buffer (generate-new-buffer " *blue-autocomplete*")
                :command (list blue-binary ",autocomplete" "bash" input)
                :noquery t
                :sentinel
                (lambda (p _event)
                  (when (eq (process-status p) 'exit)
                    (let ((candidates
                           (when (zerop (process-exit-status p))
                             (with-current-buffer (process-buffer p)
                               (string-split (buffer-string))))))
                      (blue-complete--kill-process p)
                      (setq blue-complete--last-input input
                            blue-complete--candidates candidates)
                      (funcall callback candidates)))))))
    (setq blue-complete--process proc)))

(defun blue-complete--async-table (blueprint input update-fn)
  "Kick off async completion for BLUEPRINT with INPUT.

Call UPDATE-FN when completion candidates have been collected.  Returns
the cached candidate list immediately (may be nil or stale)."
  (when blue-complete--debounce-timer
    (cancel-timer blue-complete--debounce-timer))
  ;; Only spawn when the input has actually changed.
  (unless (equal input blue-complete--last-input)
    (setq blue-complete--debounce-timer
          (run-at-time blue-complete--debounce-delay nil
                       (lambda ()
                         (setq blue-complete--debounce-timer nil)
                         (blue-complete--spawn blueprint input update-fn)))))
  ;; Return cached candidates immediately so the UI is never blocked.
  blue-complete--candidates)

(defun blue-complete--refresh (buf)
  "Nudge the completion UI in BUF to redisplay with fresh candidates.

Flushes the sorted-completion cache so the next display pass re-calls
the collection function, then fires `post-command-hook' so UIs like
Corfu and Company notice the change without a keypress."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (completion--flush-all-sorted-completions)
      (run-hooks 'post-command-hook))))

(defun blue-complete--table (&rest _)
  "Dynamic completion table for `pcomplete/blue'.

Returns cached candidates immediately (non-blocking) and spawns an async
process in the background.  When the process finishes the completion UI
is nudged to redisplay with the updated candidate list."
  (when-let* ((blueprint (or blue--blueprint (blue--find-blueprint)))
              (prompt-start (line-beginning-position))
              (input (buffer-substring-no-properties prompt-start (point))))
    (let ((buf (current-buffer)))
      (blue-complete--async-table
       blueprint input
       (lambda (_candidates)
         (run-at-time 0 nil #'blue-complete--refresh buf)))))
  blue-complete--candidates)

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

(defun blue--get-options-completion-table (options bounds)
  "Generate an appropriate completion table from OPTIONS respecting BOUNDS."
  (let ((labels (mapcar #'blue--format-option-label options)))
    `( ,(car bounds) ,(cdr bounds)
       ,labels
       :company-doc-buffer
       (lambda (candidate)
         (when-let* ((label (string-trim candidate "--?" "="))
                     (option (blue--get-option-from-label label ',options))
                     (doc (alist-get 'doc option))
                     (arguments (alist-get 'arguments option))
                     (arg-name (alist-get 'name arguments))
                     (keywords `((,arg-name . 'blue-documentation))))
           (blue--create-doc-buffer doc keywords)))
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

(defun blue--create-group-fn (commands)
  "Create group function for COMMANDS."
  (lambda (candidate transform)
    (if transform
        candidate
      (when-let* ((invocation candidate)
                  (command (blue--get-command invocation commands)))
        (alist-get 'category command)))))

(defun blue--create-doc-buffer (content keywords)
  "Create or reuse documentation buffer with CONTENT.

KEYWORDS is a list of font-lock keyword specifications to apply to the
buffer. For more information read `font-lock-add-keywords'.

Returns the buffer containing the formatted documentation."
  (with-current-buffer (get-buffer-create "*blue-capf-doc*")
    (erase-buffer)
    (insert content)
    (when keywords
      (font-lock-add-keywords nil keywords)
      (font-lock-mode 1)
      (font-lock-ensure))
    (current-buffer)))

(defun blue--command-completion-properties (commands)
  "Create completion properties for COMMANDS and INVOCATIONS."
  (when-let* ((invocations (blue--get-command-invocations commands))
              (max-width (apply #'max (mapcar #'string-width invocations)))
              (annotation-function
               (lambda (candidate)
                 (when-let* ((invocation candidate)
                             (command (blue--get-command invocation commands))
                             (synopsis (alist-get 'synopsis command))
                             (padding (max (+ blue-annotation-padding
                                              (- max-width (string-width candidate)))
                                           2)))
                   (concat (make-string padding ?\s)
                           (propertize synopsis 'face 'blue-documentation)))))
              (options-doc-buffer-function
               (lambda (candidate)
                 (when-let* ((invocation candidate)
                             (command (blue--get-command invocation commands))
                             (help-msg (alist-get 'help command))
                             (keywords `(("\\[.+\\]\s*\\.\\{3\\}*"
                                          . 'blue-documentation))))
                   (blue--create-doc-buffer help-msg keywords)))))
    (list
     :annotation-function annotation-function
     :company-doc-buffer options-doc-buffer-function
     :company-kind (lambda (candidate)
                     (cond
                      ((string-prefix-p "--" candidate) 'property)
                      ((member candidate invocations) 'command)
                      (t 'event)))
     :exclusive 'no
     :group-function (blue--create-group-fn commands))))


;;; Interfaces.

(defun blue-completion-at-point ()
  "`completion-at-point' function for `blue-run-command'."
  (when blue--data
    (let* ((commands (car blue--data))
           (ui-options (caddr blue--data))
           (invocations (blue--get-command-invocations commands))
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
           (cmd-options (alist-get 'options cmd))
           (options (if cmd cmd-options ui-options)))
      (cond
       ;; Option value completion (from command or UI).
       ((and (looking-back
              (regexp-opt (ensure-list blue-complete--option-value-prefix) t)
              (pos-bol))
             (match-end 1))
        (let* ((thing (thing-at-point 'symbol))
               (label (string-trim thing "--?" "="))
               (option (blue--get-option-from-label label options)))
          (blue-completion--complete-autocompletable option)))
       ;; Option completion (from command or UI).
       ((and (looking-back "\\(^\\|\s\\|\t\\)-+[^\s]*" (pos-bol))
             (match-end 1))
        (blue--get-options-completion-table options bounds-at-pt))
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
