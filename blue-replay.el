;;; blue-replay.el --- BLUE build system replay interface -*- lexical-binding: t -*-
;;
;; Copyright © 2025 Sergio Pastor Pérez

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
(require 'cus-edit) ; Required for `custom-button' face.
(require 'blue)

;; FIXME: If the file is a hiden file (starts with a space), it's not properly
;; propertized.
(defvar blue-replay--buffer "*blue replay*"
  "Buffer to dump raw output of replay.")

;;; Utilities.

(defun blue-replay--get-buffer ()
  "Return `blue-replay--buffer', creating it if needed."
  (get-buffer-create blue-replay--buffer))

(defun blue-replay--exec-replay (replay)
  "Replay REPLAY item."
  (blue--compile replay))

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
        (cond
         ;; Empty line marks end of record
         ((string-empty-p line)
          (when current-record
            (push current-record recs)
            (setq current-record '())
            (setq current-key nil)))

         ;; Line starting with + is a continuation/list item.
         ((string-match "^\\+ " line)
          (when current-key
            (let* ((value (substring line 2))
                   (existing-value (plist-get current-record current-key)))
              (if existing-value
                  ;; If value already exists, make it a list or append to list.
                  (if (listp existing-value)
                      (setq current-record
                            (plist-put current-record current-key
                                       (append existing-value (list value))))
                    (setq current-record
                          (plist-put current-record current-key
                                     (list existing-value value))))
                ;; First continuation item - replace the original value.
                (setq current-record
                      (plist-put current-record current-key (list value)))))))

         ;; Multi-line value continuation - line starts with whitespace.
         ((and current-key (string-match "^[ \t]" original-line))
          (let* ((existing-value (plist-get current-record current-key))
                 (continuation original-line)) ; Keep original indentation
            (if existing-value
                (setq current-record
                      (plist-put current-record current-key
                                 (concat existing-value "\n" continuation)))
              (setq current-record
                    (plist-put current-record current-key continuation)))))

         ;; Regular field: key: value (only if original line doesn't start with
         ;; whitespace).
         ((and (not (string-match "^[ \t]" original-line))
               (string-match "^\\([^:]+\\):\\s-*\\(.*\\)" line))
          (let* ((key-name (match-string 1 line))
                 (key (intern (concat ":" key-name)))
                 (value (match-string 2 line)))
            (setq current-key key)
            ;; Build plist in order by appending to end
            (setq current-record
                  (append current-record
                          (list key (if (string-empty-p value)
                                        nil
                                      value)))))))))

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
      (magit-insert-heading
        (propertize heading
                    'font-lock-face
                    '(:inherit font-lock-constant-face :weight bold)))

      ;; Insert basic info
      (when origin
        (magit-insert-section (blue-field :origin)
          (insert (format "%-10s %s\n" "origin:" origin))))

      (when replay
        (magit-insert-section (blue-field :replay)
          (insert (format "%-10s %s\n" "replay:"
                          (propertize
                           (buttonize replay #'blue-replay--exec-replay replay)
                           'font-lock-face 'custom-button)))))

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
    (with-current-buffer buf-name
      (setq default-directory dir) ; Update directory of buffer.
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Reset cache before redisplaying data. This ensures that the
        ;; section visibility is properly updated.
        (setq magit-section-visibility-cache nil)
        (blue-replay-mode)
        (save-excursion
          (magit-insert-section (blue-root)
            (insert "Build directory: " dir "\n\n")
            (dolist (rec recs)
              (blue-replay--insert-record-section rec))))
        ;; NOTE: `magit-insert-section' does not automatically display the
        ;; visibility indicators.
        (magit-map-sections #'magit-section-maybe-update-visibility-indicator))
      (blue--set-search-path blue--blueprint))
    (display-buffer buf-name)))

(defun blue-replay--replay (blueprint dir)
  "Return the replay data for BLUEPRINT stored in DIR."
  (let* ((flags (when blueprint
                  (list "--file" blueprint "--build-directory" dir)))
         (output (blue--execute-serialize flags "replay" t))
         (data (car output))
         (exit-code (cdr output)))
    (if (zerop exit-code)
        (blue-replay--parse-recutils-to-plist data)
      (error (string-remove-suffix "\n" data)))))


;;; Fontification

(defun blue-replay--visit-location (file &optional line column)
  "Open FILE and move point to LINE and COLUMN if provided."
  (when-let* ((path (if (file-exists-p file)
                        file
                      (locate-file file blue--search-path))))
    (when (file-exists-p path)
      (find-file path)
      (when line
        (goto-line line))
      (when column
        (move-to-column column)))))

(defvar blue-replay--file-rx
  (rx (group
       (or
        (group (group (zero-or-more "/")
                      (one-or-more (any alnum "_./-")))
               ":"
               (group (one-or-more digit))
               ":"
               (group (one-or-more digit)))
        (group "/" (one-or-more (any alnum "_./-"))))))
  "Rx matcher for files.

We cannot try to mach relative file paths as that would be too
permissive.  So we match absolute paths or relative paths with
line:column information.")

(defvar blue-replay-font-lock-keywords
  `(;; Class names in angle brackets
    ("\\(#*<.*>\\)"
     (1 'font-lock-variable-use-face))

    ("\\(\"[^\"]+\"\\)"
     (1 'font-lock-string-face))

    ;; Make files clickable if they exist.
    (,blue-replay--file-rx
     (0
      (prog1 'button
        (let* ((path (or (match-string-no-properties 3)
                         (match-string-no-properties 6)))
               (line-str (match-string-no-properties 4))
               (line (when line-str
                       (string-to-number line-str)))
               (col-str (match-string-no-properties 5))
               (col (when col-str
                      (string-to-number col-str))))
          (make-text-button (match-beginning 0) (match-end 0)
                            'action `(lambda (_)
                                       (blue-replay--visit-location
                                        ,path ,line ,col))
                            'follow-link t
                            'help-echo (format "Click to open %s" path))))
      ;; Allow overriding previous fontification, eg. refontify strings.
      t))

    ;; Keywords with colons (#:log, #:trs, #:cov)
    ("\\(#:[a-zA-Z-]+\\)"
     (1 'font-lock-keyword-face))

    ;; ERROR: labels
    ("^\s*ERROR:" . 'error)

    ;; Numbered error items
    ("^\s*\\([0-9]+\\.\\)"
     (1 'font-lock-number-face))

    ;; Error types (&origin, &irritants, &message, etc.)
    ("\\(&[a-zA-Z-]+\\)"
     (1 'font-lock-type-face))

    ;; Plus signs for list items
    ("^\s*\\(\\+\\)" . 'font-lock-comment-face))
  "Font lock keywords for blue build output.")

;; Integration with blue-replay-mode
(defun blue-replay-setup-font-lock ()
  "Set up font-lock for `blue-replay-mode'."
  (setq font-lock-defaults '(blue-replay-font-lock-keywords t))
  (font-lock-mode 1))


;;; UI.

(defun blue-replay-revert ()
  "Revert BLUE replay buffer."
  (interactive)
  (let ((pt (point)))
    (blue-replay default-directory)
    (goto-char pt)))

(defvar-keymap blue-replay-mode-map
  :doc "Keymap for `blue-replay-mode'."
  :parent magit-section-mode-map
  "g" #'blue-replay-revert)

(define-derived-mode blue-replay-mode magit-section-mode "Blue-replay"
  "Mode for looking at BLUE replay data."
  :interactive nil
  :group 'blue
  (blue-replay-setup-font-lock))

;;;###autoload
(defun blue-replay (dir)
  "Interactively display replay data from DIR."
  (interactive
   (progn
     (blue--ensure-cache)
     (let* ((known (blue--cache-get-build-dirs default-directory))
            (cur-dir (directory-file-name (expand-file-name default-directory)))
            (cached (member cur-dir known))
            (prefix (car current-prefix-arg)))
       (list (if (and cached
                      (not prefix))
                 cur-dir
               (blue--prompt-dir))))))
  ;; TODO: homogenize dynamic binding reliance. Some functions take
  ;; `blue--blueprint' as an argument, other rely on the dynamic binding.
  (let* ((blue--blueprint (blue--find-blueprint)) ; Bounded dynamicaly.
         (rec-data (blue-replay--replay blue--blueprint dir)))
    (blue-replay--display-recutils-magit rec-data dir)))

(provide 'blue-replay)
;;; blue-replay.el ends here.
