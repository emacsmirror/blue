;;; blue-replay.el --- BLUE build system replay interface -*- lexical-binding: t -*-
;;
;; Copyright © 2025 Sergio Pastor Pérez
;;
;; Author: Sergio Pastor Pérez <sergio.pastorperez@gmail.com>
;; Version: 0.0.5
;; Package-Requires: ((emacs "30.1"))
;; Keywords: blue, tools
;; URL: https://codeberg.org/lapislazuli/blue-replay.el

;;; Commentary:

;; Interface for the BLUE build system replay command.

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

(require 'magit-section)
(require 'blue)

(defface blue-replay--file-heading
  '((t :extend t :weight bold :foreground "#0031a9"))
  "Face for diff file headings."
  :group 'blue-faces)

;; FIXME: If the file is a hiden file (starts with a space), it's not properly
;; propertized.
(defvar blue-replay--buffer "*blue replay*"
  "Buffer to dump raw output of replay.")

;;; Utilities.

(defun blue-replay--get-buffer ()
  "Return `blue-replay--buffer', creating it if needed."
  (get-buffer-create blue-replay--buffer))

(defun blue-replay--parse-recutils-to-plist (recutils)
  "Parse a RECUTILS format string into a list of property lists.
Each record becomes a plist with field names as keywords."
  (let ((recs '())
        (current-record '())
        (lines (split-string recutils "\n"))
        (current-key nil))

    (dolist (line lines)
      (let ((original-line line))
        (setq line (string-trim line))
        ;; Debug: uncomment to see what's happening
        ;; (when (string-match "^[0-9]+\\." line)
        ;;   (message "Line: '%s' | Original: '%s' | Starts with space: %s"
        ;;            line original-line (string-match "^[ \t]" original-line)))
        (cond
         ;; Empty line marks end of record
         ((string-empty-p line)
          (when current-record
            (push current-record recs)
            (setq current-record '())
            (setq current-key nil)))

         ;; Line starting with + is a continuation/list item
         ((string-match "^\\+ " line)
          (when current-key
            (let* ((value (substring line 2))
                   (existing-value (plist-get current-record current-key)))
              (if existing-value
                  ;; If value already exists, make it a list or append to list
                  (if (listp existing-value)
                      (setq current-record
                            (plist-put current-record current-key
                                       (append existing-value (list value))))
                    (setq current-record
                          (plist-put current-record current-key
                                     (list existing-value value))))
                ;; First continuation item - replace the original value
                (setq current-record
                      (plist-put current-record current-key (list value)))))))

         ;; Multi-line value continuation - line starts with whitespace
         ((and current-key (string-match "^[ \t]" original-line))
          (let* ((existing-value (plist-get current-record current-key))
                 (continuation original-line)) ; Keep original indentation
            (if existing-value
                (setq current-record
                      (plist-put current-record current-key
                                 (concat existing-value "\n" continuation)))
              (setq current-record
                    (plist-put current-record current-key continuation)))))

         ;; Regular field: key: value (only if original line doesn't start with whitespace)
         ((and (not (string-match "^[ \t]" original-line))
               (string-match "^\\([^:]+\\):\\s-*\\(.*\\)" line))
          (let* ((key-name (match-string 1 line))
                 (key (intern (concat ":" key-name)))
                 (value (match-string 2 line)))
            (setq current-key key)
            ;; Build plist in order by appending to end
            (setq current-record
                  (append current-record
                          (list key (if (string-empty-p value) nil value)))))))))

    ;; Don't forget the last record if there's no trailing newline
    (when current-record
      (push current-record recs))

    (nreverse recs)))

(defun blue-replay--insert-record-section (rec)
  "Insert a single REC as a magit section."
  (let* ((replay (plist-get rec :replay))
         (replay-hash (car (last (string-split replay))))
         (origin (plist-get rec :origin))
         (class (plist-get rec :class))
         (error-msg (plist-get rec :error))
         (has-error (not (null error-msg)))
         (heading (concat "Record " replay-hash)))

    (magit-insert-section (blue-record rec)
      (magit-insert-heading (propertize heading 'font-lock-face 'blue-replay--file-heading))

      ;; Insert basic info
      (when origin
        (magit-insert-section (blue-field :origin)
          (insert (format "%-10s %s\n" "origin:" origin))))

      (when replay
        (magit-insert-section (blue-field :replay)
          (insert (format "%-10s %s\n" "replay:" replay))))

      (when class
        (magit-insert-section (blue-field :class)
          (insert (format "%-10s %s\n" "class:" class))))

      ;; Insert inputs section
      (let ((inputs (plist-get rec :inputs)))
        (when inputs
          (magit-insert-section (blue-inputs)
            (magit-insert-heading "Inputs:")
            (if (listp inputs)
                (dolist (input inputs)
                  (insert "  + " input "\n"))
              (insert "  " inputs "\n")))))

      ;; Insert outputs section
      (let ((outputs (plist-get rec :outputs)))
        (when outputs
          (magit-insert-section (blue-outputs)
            (magit-insert-heading "Outputs:")
            (if (listp outputs)
                (dolist (output outputs)
                  (insert "  + " output "\n"))
              (insert "  " outputs "\n")))))

      ;; Insert error section (collapsed by default if long)
      (when error-msg
        (let ((error-lines (split-string error-msg "\n")))
          (magit-insert-section (blue-error)
            (magit-insert-heading "Error:")
            (dolist (line error-lines)
              (insert line "\n")))))

      (insert "\n"))))

(defun blue-replay--display-recutils-magit (recs dir)
  "Display parsed recutils RECS in a 'magit-section' buffer.

DIR is the directory where the replay data has been taken from."
  (let ((buf-name (blue-replay--get-buffer)))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (magit-section-mode)
        (magit-insert-section (blue-root)
          (magit-insert-heading
            (concat "Build directory: " (propertize dir 'font-lock-face 'link) "\n\n"))
          (dolist (rec recs)
            (blue-replay--insert-record-section rec)))
        (goto-char (point-min)))
      (switch-to-buffer buf-name))))

(defun blue-replay--replay (blueprint dir)
  "Return the replay data for BLUEPRINT stored in DIR."
  (let* ((flags (when blueprint
                  (list "--file" blueprint "--build-directory" dir)))
         (output (blue--execute-serialize flags "replay" t)))
    (blue-replay--parse-recutils-to-plist output)))


;;; UI.

;;;###autoload
(defun blue-replay ()
  "Display and interactive replay buffer."
  (interactive)
  (let* ((blueprint (blue--find-blueprint))
         (rec-data (blue-replay--replay blueprint default-directory)))
    (blue-replay--display-recutils-magit rec-data default-directory)))

(provide 'blue-replay)
;;; blue-replay.el ends here.
