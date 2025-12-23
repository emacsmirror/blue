;;; blue-run.el --- BLUE build system completing read interface -*- lexical-binding: t -*-
;;
;; Copyright © 2025 Sergio Pastor Pérez

;;; Commentary:

;; Completing read interface for the BLUE.

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
(require 'blue-complete)


;;; Minibuffer Hints.

(defun blue--format-build-dir-hint (index build-dir current)
  "Format a single build directory hint line.

INDEX is the number prefixing the displayed known build directory in the
hint message.
BUILD-DIR is the directory of the known build directory.
If CURRENT is non-nil the entry will be highlighted."
  (let ((face (if (string-equal build-dir current)
                  'blue-hint-highlight
                'blue-hint-faded))
        (build-dir* (buttonize build-dir
                               (lambda (_)
                                 (execute-kbd-macro
                                  (kbd (concat "M-" (number-to-string index))))))))
    (concat
     (propertize (number-to-string index) 'face 'blue-hint-index)
     " "
     (propertize build-dir*
                 'face face
                 'help-echo "Click to select build directory"))))

(defun blue--create-hint-overlay (build-dirs current override)
  "Create hint overlay content from BUILD-DIRS and CURRENT build-dir.

If OVERRIDE is non nil disable CONFIGS."
  (when-let* (build-dirs
              (indices (number-sequence 1 (length build-dirs)))
              (formatted (seq-mapn (lambda (idx dir)
                                     (blue--format-build-dir-hint idx dir current))
                                   indices build-dirs))
              (lines (string-join formatted "\n"))
              (lines* (if override
                          (concat
                           (blue--format-build-dir-hint 0 override override)
                           "\n"
                           (propertize lines 'face 'blue-hint-separator))
                        lines)))
    (concat
     "Build directory (M-<num> to select):\n"
     lines*
     "\n"
     (propertize " " 'face 'blue-hint-separator
                 'display '(space :align-to right))
     "\n")))

(defun blue--show-hints (&rest _)
  "Display build directory hints in minibuffer overlay."
  (when-let* ((build-dirs (blue--cache-get-build-dirs blue--blueprint))
              (content (blue--create-hint-overlay build-dirs (blue--get-build-dir) blue--overiden-build-dir)))
    (unless blue--hint-overlay
      (setq blue--hint-overlay (make-overlay (point) (point))))
    (overlay-put blue--hint-overlay 'after-string content)
    (move-overlay blue--hint-overlay (point-min) (point-min) (current-buffer))))


;;; Minibuffer Setup.

(defun blue--bind-build-dir-key (index)
  "Setup keybinding for build directory INDEX."
  (let ((key (kbd (format "M-%d" index))))
    (define-key (current-local-map) key
                (lambda ()
                  (interactive)
                  (setq blue--build-dir
                        (nth (1- index) (blue--cache-get-build-dirs blue--blueprint))
                        default-directory blue--build-dir) ; Make completion work from selected build dir.
                  (blue--show-hints)))))

(defun blue--setup-minibuffer ()
  "Setup keybindings and completion for minibuffer prompt."
  ;; Work on a copy of the current minibuffer keymap so it doesn’t leak
  (use-local-map (copy-keymap (current-local-map)))
  (define-key (current-local-map) (kbd "SPC") nil)
  (unless blue--overiden-build-dir
    (let ((build-dirs (blue--cache-get-build-dirs blue--blueprint)))
      (seq-do #'blue--bind-build-dir-key (number-sequence 1 (length build-dirs)))))
  (add-hook 'completion-at-point-functions #'blue-completion-at-point nil t)
  (blue--show-hints))


;;; UI.

(defun blue--prompt-for-commands ()
  "Interactive prompt for BLUE commands."
  (blue--check-blue-binary)
  (blue--ensure-cache)
  (setq blue--blueprint (blue--find-blueprint))
  (let* ((prefix (car current-prefix-arg))
         (build-dirs (blue--cache-get-build-dirs blue--blueprint))
         (last-build-dir (car build-dirs))
         (prompt-dir-p (or (eql prefix 4) ; Single universal argument 'C-u'.
                           (and blue-require-build-directory
                                (not last-build-dir))))
         (comint-flip (eql prefix 16))) ; Double universal argument 'C-u C-u'.
    (setq blue--overiden-build-dir (when prompt-dir-p
                                     (blue--prompt-dir t))
          blue--build-dir (or blue--overiden-build-dir last-build-dir)
          blue--data (blue--get-data blue--blueprint))
    ;; Make completion work from selected build dir.
    (blue--set-default-directory blue--build-dir)
    (if-let* ((commands (car blue--data))
              (invocations (blue--get-command-invocations commands))
              (completion-extra-properties
               (blue--command-completion-properties commands))
              (crm-separator (propertize "[ \t]*--[ \t]+"
                                         'separator "--"
                                         'description "double-dash-separated list"))
              (crm-prompt "[%d] [CMR%s] Command: "))
        (list (minibuffer-with-setup-hook #'blue--setup-minibuffer
                (completing-read-multiple "Command: " invocations))
              comint-flip)
      (list nil))))

;;;###autoload
(defun blue-run-command (input &optional comint-flip)
  "Run a BLUE command interactively.

The prompt will hint for a directory where to run the BLUE command in a
directory.  The hinted directories are directories where BLUE has been
previously executed by `blue.el'.  The execution directory can be
changed using `M-<num>'.

Invoked with universal prefix argument '\\[universal-argument]', prompt
for a directory to use when running `blue'.

Invoked with double universal prefix argument '\\[universal-argument]
\\[universal-argument]', invert the interactive heuristics configured by
`blue-interactive-commands'.

INPUT is a list of command strings.
COMINT-FLIP inverts the interactive compilation logic."
  (interactive (blue--prompt-for-commands))

  (when input
    (let* ((options (blue--normalize-options blue-default-options))
           (tokens (mapcar #'string-split input))
           (is-interactive (blue--any-interactive-p tokens))
           (comint (xor is-interactive comint-flip)))
      (let ((command-string (string-join
                             (cons blue-binary
                                   (append (when options options)
                                           (list (string-join input " -- "))))
                             " ")))
        ;; Bring `blue--build-dir' to the from of the list so it's ordered by
        ;; usage.
        (when-let* ((build-dir (blue--get-build-dir)))
          (blue--cache-add build-dir))
        (blue--compile command-string comint)))))

(provide 'blue-run)
;;; blue-run.el ends here.
