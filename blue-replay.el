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
  '((t :extend t :weight bold :foreground "DodgerBlue"))
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
        (magit-map-sections #'magit-section-maybe-update-visibility-indicator)))
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


;;; UI.

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
  (let* ((blueprint (blue--find-blueprint))
         (rec-data (blue-replay--replay blueprint dir)))
    (blue-replay--display-recutils-magit rec-data dir)
    (blue--set-search-path blueprint)))

(defun blue-replay-revert ()
  "Revert BLUE replay buffer."
  (interactive)
  (let ((pt (point)))
    (blue-replay default-directory)
    (goto-char pt)))

;; --- Fontification

(defface blue-replay-header-face
  '((t :foreground "#4A90E2" :weight bold))
  "Face for build output headers."
  :group 'blue-faces)

(defface blue-replay-record-face
  '((t :foreground "#7B68EE" :weight bold))
  "Face for record identifiers."
  :group 'blue-faces)

(defface blue-replay-field-face
  '((t :foreground "#50C878" :weight bold))
  "Face for field names."
  :group 'blue-faces)

(defface blue-replay-path-face
  '((t :inherit link :mouse-face highlight))
  "Face for file paths."
  :group 'blue-faces)

(defface blue-replay-class-face
  '((t :foreground "#DA70D6" :weight bold))
  "Face for class names."
  :group 'blue-faces)

(defface blue-replay-error-face
  '((t :foreground "#FF6B6B" :weight bold))
  "Face for error indicators."
  :group 'blue-faces)

(defface blue-replay-error-detail-face
  '((t :foreground "#FF9999"))
  "Face for error details."
  :group 'blue-faces)

(defface blue-replay-keyword-face
  '((t :foreground "#20B2AA"))
  "Face for keywords and symbols."
  :group 'blue-faces)

(defface blue-replay-list-face
  '((t :foreground "#FFA500"))
  "Face for numbers."
  :group 'blue-faces)

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
  `(
    ;; Field names (origin, replay, class, etc.) - not at start of line to avoid headers
    ("^\\(\\(origin\\|replay\\|class\\):\\)"
     (1 'blue-replay-field-face))

    ;; Class names in angle brackets
    ("\\(#*<[^>]+>\\)"
     (1 'blue-replay-class-face))

    ;; Make files clickable if they exist.
    (,blue-replay--file-rx

     (0
      (prog1 'blue-replay-path-face
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
                                       (blue-replay--visit-location ,path ,line ,col))
                            'follow-link t
                            'help-echo (format "Click to open %s" path))))))

    ;; Keywords with colons (#:log, #:trs, #:cov)
    ("\\(#:[a-zA-Z-]+\\)"
     (1 'font-lock-keyword-face))

    ;; ERROR: labels
    ("^\s*ERROR:" . 'error)

    ;; Numbered error items
    ("^\s*\\([0-9]+\\.\\)"
     (1 'blue-replay-list-face))

    ;; Error types (&origin, &irritants, &message, etc.)
    ("\\(&[a-zA-Z-]+\\)"
     (1 'font-lock-type-face))

    ;; Quoted strings - check if they're file paths and make clickable
    ("\"\\([^\"]+\\)\""
     (1 (prog1 'blue-replay-path-face
          (let ((path (match-string 1)))
            (when (and path (file-exists-p path))
              (make-text-button (match-beginning 1) (match-end 1)
                                'action `(lambda (_button) (find-file ,path))
                                'follow-link t
                                'help-echo (format "Click to open %s" path)))))))

    ;; Plus signs for list items
    ("^\s*\\(\\+\\)" . 'blue-replay-list-face))
  "Font lock keywords for blue build output.")

;; Integration with blue-replay-mode
(defun blue-replay-setup-font-lock ()
  "Set up font-lock for `blue-replay-mode'."
  (setq font-lock-defaults '(blue-replay-font-lock-keywords t))
  (font-lock-mode 1))

;; --- Fontification

(defvar-keymap blue-replay-mode-map
  :doc "Keymap for `blue-replay-mode'."
  :parent magit-section-mode-map
  "g" #'blue-replay-revert)

(define-derived-mode blue-replay-mode magit-section-mode "Blue-replay"
  "Mode for looking at BLUE replay data."
  :interactive nil
  :group 'blue
  (blue-replay-setup-font-lock))

(provide 'blue-replay)
;;; blue-replay.el ends here.
