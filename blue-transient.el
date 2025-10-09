;;; blue-transient.el --- BLUE build system transient interface -*- lexical-binding: t -*-
;;
;; Copyright © 2025 Sergio Pastor Pérez

;;; Commentary:

;; Transient interface for the BLUE.
;;
;; This module is based on:
;; https://github.com/gavv/transient-compile

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
(require 'cus-edit) ; For `custom-button' face.
(require 's)
(require 'seq)
(require 'transient)
(require 'wid-edit) ; For `widget-field' face.


;;; Configuration

(defcustom blue-transient-menu-columns-limit nil
  "If non-nil, limits maximum allowed number of menu columns.
Used by `blue-transient--menu-columns'."
  :group 'blue-compile
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Limit")))

(defcustom blue-transient-keychar-function nil
  "Custom function that chooses unique key character for a word.

The function should take 3 arguments:
  - name - group or target name for which we choose a key
  - all-names - list of all names, among which the key must be unique
  - key-map - hashtable of taken keys
  - group-p - whether it's group or target

The function should return character to be used as a key.
Character must not be taken by other words (other groups
or other targets in group), i.e. it must not be present
in the key-map.

The function can return nil if it doesn't have a good key.
In this case default algorithm is used for this word."
  :group 'blue-transient
  :type '(choice (const :tag "Default" nil)
                 function))

(defcustom blue-transient-keychar-unfold t
  "Whether to use upcase/downcase key characters.

If non-nil, allow using upcase and downcase variants of the original
character as the key character."
  :group 'blue-transient
  :type 'boolean)

(defcustom blue-transient-keychar-regexp "[[:alnum:]]"
  "Regexp for allowed key characters.

Only those characters in group and target names, which match this regex,
can become key characters."
  :group 'blue-transient
  :type 'regexp)


;;; Internal variables.
(defvar blue-transient-group-fallback 'unknown
  "The name of the fallback group for targets without group.")

(defvar blue-transient--command nil
  "List of BLUE command strings.")

(defconst blue-transient--keychar-table
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  "Valid characters used to form the keys to dispatch commands.

This is used to create the `blue-transient' menu.")

(defvar blue-transient--menu-expresion nil
  "Last evaluated transient.

This is used to prevent reevaluating the same transient, losing any
possible saved state.")


;;; Undo/Redo

(defvar blue-transient--undo-stack nil
  "Undo stack for `blue-transient--command'.")

(defvar blue-transient--redo-stack nil
  "Redo stack for `blue-transient--command'.")

(defun blue-transient--save-state ()
  "Save current state of `blue-transient--command' to the undo stack."
  (push (copy-tree blue-transient--command) blue-transient--undo-stack)
  (setq blue-transient--redo-stack nil)) ; Clear redo when new change happens.

(defun blue-transient-undo ()
  "Undo last change to `blue-transient--command'."
  (interactive)
  (when blue-transient--undo-stack
    (push (copy-tree blue-transient--command) blue-transient--redo-stack)
    (setq blue-transient--command (pop blue-transient--undo-stack))))

(defun blue-transient-redo ()
  "Redo last undone change to `blue-transient--command'."
  (interactive)
  (when blue-transient--redo-stack
    (push (copy-tree blue-transient--command) blue-transient--undo-stack)
    (setq blue-transient--command (pop blue-transient--redo-stack))))


;;; Utilities.

(defun blue-transient--del ()
  "Delete the last argument or command in `blue-transient--command'.

If the last command still has arguments, remove its last argument.
If it has none left, remove the entire command."
  (interactive)
  (when blue-transient--command
    (blue-transient--save-state)
    (let* ((last-cmd (car (last blue-transient--command)))
           (args (cdr last-cmd)))
      (if args
          ;; Remove last argument.
          (setcar (last blue-transient--command)
                  (butlast last-cmd))
        ;; Remove entire command.
        (setq blue-transient--command
              (butlast blue-transient--command))))))

(defun blue-transient--clear ()
  "Clean command prompt."
  (interactive)
  (when blue-transient--command
    (blue-transient--save-state)
    (setq blue-transient--command nil)))

(defun blue-transient--run ()
  "Run BLUE commands."
  (interactive)
  (transient-set)
  (let* ((args (transient-args (oref transient-current-prefix command)))
         (comint-flip (transient-arg-value "flip" args))
         (transient-options (transient-arg-value "options=" args))
         (options (or (when transient-options
                        (list transient-options))
                      (blue--normalize-options blue-default-options)))
         (is-interactive (blue--any-interactive-p blue-transient--command))
         (comint (xor is-interactive comint-flip))
         (known-build-dirs (blue--cache-get-build-dirs default-directory))
         (blue--build-dir (seq-find (lambda (dir)
                                      (transient-arg-value dir args))
                                    known-build-dirs))
         (commands (mapcar (lambda (token)
                             (string-join token " "))
                           blue-transient--command))
         (input (string-join commands " -- "))
         (full-input (string-join
                      (cons blue-binary
                            (append (when options options)
                                    (list input)))
                      " ")))
    (blue--compile full-input comint)))

(defun blue-transient--menu-heading ()
  "Dynamic header for BLUE transient."
  (let* ((header (propertize "Commands: " 'face 'bold))
         (commands (mapcar (lambda (token)
                             (string-join token " "))
                           blue-transient--command))
         (input (string-join commands " -- "))
         (padding (- (frame-width) (length header)))
         (formated-command (propertize (string-pad input padding)
                                       'face 'widget-field)))
    (concat header formated-command "\n")))

(defun blue-transient--menu-columns (items)
  "Return bounded menu column count from ITEMS.

Takes assoc list returned by `blue-transient--build-menu'."
  (let* ((categories (mapcar #'car items))
         (longest-category (apply #'max
                                  (mapcar #'length categories)))
         (command-invokes (mapcar #'cadr (seq-mapcat #'cdr items)))
         (longest-invoke (apply #'max
                                (mapcar #'length command-invokes)))
         (max-width (max longest-category longest-invoke))
         (max-columns (max (/ (frame-width) max-width)
                           1))) ; At least 1 column.
    (if (and blue-transient-menu-columns-limit
             (> blue-transient-menu-columns-limit 0))
        (min blue-transient-menu-columns-limit
             max-columns)
      max-columns)))

(defun blue-transient--keychar-p (char)
  "Check if CHAR can be used as a key."
  (string-match-p
   blue-transient-keychar-regexp (string char)))

(defun blue-transient--random-key (word key-map)
  "Generate random key for WORD, trying to return same results for same words.

The function ensures that the assigned key is not already present in KEY-MAP."
  (let ((counter 0)
        result)
    (while (not result)
      (cl-incf counter)
      (let* ((hash (abs (sxhash word)))
             (index (mod hash (length blue-transient--keychar-table)))
             (char (elt blue-transient--keychar-table index)))
        (if (and (blue-transient--keychar-p char)
                 (not (gethash char key-map)))
            ;; Hit!
            (setq result char)
          (if (< counter (length blue-transient--keychar-table))
              ;; Repeat with hash of hash, and so on.
              (setq word (number-to-string hash))
            ;; Give up.
            "_"))))
    result))

(defun blue-transient--assign-keys (words group-p)
  "Map WORDS to unique keys.

GROUP-P is passed to `blue-transient-keychar-function' in case it needs
to be specially handled."
  (let* ((key-map (make-hash-table :test 'equal))
         (shared-prefix (seq-reduce 's-shared-start
                                    words
                                    (car words)))
         (sorted-words (seq-sort
                        'string< words))
         (max-len (seq-max (seq-map (lambda (w) (length w))
                                    sorted-words)))
         word-keys)
    (while (< (length word-keys)
              (length words))
      (let (word
            word-index
            word-key)
        (unless (and blue-transient-keychar-function
                     (seq-find
                      (lambda (w)
                        (when-let* ((key (funcall blue-transient-keychar-function
                                                  w
                                                  words
                                                  key-map
                                                  group-p)))
                          ;; Special case: custom user-provided key.
                          (unless (characterp key)
                            (user-error
                             "Got non-char key %S from `blue-transient-keychar-function'"
                             key))
                          (when (gethash key key-map)
                            (user-error
                             "Got duplicate key %s from `blue-transient-keychar-function'"
                             (string key)))
                          (setq word w
                                word-index (seq-position word key)
                                word-key key)))
                      (seq-remove (lambda (w)
                                    (assoc w word-keys))
                                  sorted-words)))
          (if (and (seq-contains-p sorted-words shared-prefix)
                   (not (assoc shared-prefix word-keys)))
              ;; Special case: word = shared prefix.
              (setq word shared-prefix
                    word-index 0
                    word-key (elt word 0))
            ;; Normal case.
            (seq-find
             (lambda (prefer-first)
               (seq-find
                (lambda (casefn)
                  ;; If prefer-first is true:
                  ;;  - Find word with minimal N so that its Nth character is not taken.
                  ;; Else:
                  ;;  - Find word with minimal N so that its Nth character is not taken
                  ;;    AND is unique among Nth characters of all words.
                  (seq-find
                   (lambda (index)
                     (when (setq word
                                 (seq-find
                                  (lambda (word)
                                    (and (not (assoc word word-keys))
                                         (> (length word) index)
                                         (blue-transient--keychar-p (elt word index))
                                         (not (gethash
                                               (funcall casefn (elt word index)) key-map))
                                         (or prefer-first
                                             (not (seq-find
                                                   (lambda (other-word)
                                                     (and
                                                      (not (string= other-word word))
                                                      (> (length other-word) index)
                                                      (eq (funcall casefn (elt other-word index))
                                                          (funcall casefn (elt word index)))))
                                                   sorted-words)))))
                                  sorted-words))
                       (setq word-index index
                             word-key (funcall casefn (elt word index)))))
                   (number-sequence (length shared-prefix)
                                    max-len)))
                ;; Repeat above search a few times: first try characters as-is, then try
                ;; their upper-case and down-case variants.
                (if blue-transient-keychar-unfold
                    (list 'identity 'upcase 'downcase)
                  (list 'identity))))
             ;; If group-p is set, do above search once with prefer-first set to t.
             ;; Otherwise, first try it with prefer-first set to nil, then with t.
             ;; When prefer-first is nil, less matches are possible, but we have a
             ;; nice effect when keychars are placed in the same column or close,
             ;; so we try to maximize this effect.
             (if group-p
                 '(t)
               '(nil t)))))
        ;; Can't choose key char from word's letters, fallback to random key.
        ;; Randomness is based on word hash, so that we return same key
        ;; for same word, when possible.
        (unless word
          (setq word (seq-find (lambda (w)
                                 (not (assoc w word-keys)))
                               sorted-words)
                word-key (blue-transient--random-key word key-map)
                word-index (seq-position word word-key)))
        (let ((word-label (substring word 0)))
          (push (list word
                      word-label
                      (string word-key))
                word-keys)
          (puthash word-key t key-map))))
    word-keys))

(defun blue-transient--group-commands (categories category-keys)
  "Group commands by CATEGORIES and assign CATEGORY-KEYS."
  (mapcar
   (lambda (category)
     (let* ((category-name (symbol-name (car category)))
            (category-commands (cdr category))
            (category-command-names (mapcar (lambda (command)
                                              (alist-get 'invoke command))
                                            category-commands))
            (category-command-keys (blue-transient--assign-keys category-command-names nil)))
       (append
        (list (capitalize category-name))
        (mapcar
         (lambda (command)
           (let* ((command-invoke (alist-get 'invoke command))
                  (command-interactive-p (when (member command-invoke
                                                       blue-interactive-commands)
                                           t))
                  (command-key
                   (if (> (length category-commands) 1)
                       (s-concat (caddr (assoc category-name category-keys))
                                 (caddr (assoc command-invoke category-command-keys)))
                     (caddr (assoc command-invoke category-command-keys))))
                  (command-synopsis (alist-get 'synopsis command)))
             `(,command-key
               ,(capitalize command-invoke)
               (lambda () ,command-synopsis (interactive)
                 (blue-transient--save-state)
                 (setq blue-transient--command
                       (append blue-transient--command
                               (list (list ,command-invoke)))))
               :transient t)))
         category-commands))))
   categories))

(defun blue-transient--build-menu (commands)
  "Build transient menu for BLUE COMMANDS."
  ;; TODO: we don't really need category names, `blue-transient--assign-keys'
  ;; should be refactored to take a symbols instead of strings and the target
  ;; label creation can be removed since it's not needed.
  (let* ((categories (seq-uniq (mapcar (lambda (command)
                                         (alist-get 'category command))
                                       commands)))
         (category-names (mapcar (lambda (category)
                                   (symbol-name category))
                                 categories))
         (category-keys (blue-transient--assign-keys category-names t))
         (sorted-commands-by-category
          (mapcar (lambda (category)
                    (cons category
                          (seq-filter (lambda (command)
                                        (eq (alist-get 'category command)
                                            category))
                                      commands)))
                  categories))
         (grouped-commands
          (blue-transient--group-commands sorted-commands-by-category
                                          category-keys)))
    ;; Make each menu entry a vector.
    (mapcar (lambda (item)
              (apply #'vector item))
            grouped-commands)))


;;; UI.

;; TODO: Implement command arguments as a value transient option that always
;; affects the last inputed command.

(defun blue-transient--setup-minibuffer ()
  "Setup keybindings and completion for minibuffer prompt."
  (use-local-map (copy-keymap (current-local-map)))
  (define-key (current-local-map) (kbd "TAB")
              #'completion-at-point)
  ;; NOTE: `corfu--minibuffer-on` won't enable `corfu-mode'
  ;; if `completion-at-point-functions` isn't local.
  (add-hook 'completion-at-point-functions
            #'blue--completion-at-point nil t))

(defun blue-transient--prompt-args ()
  "Helper for prompting BLUE command arguments."
  (interactive)
  (when blue-transient--command
    (let* ((front (butlast blue-transient--command))
           (last-cmd (caar (last blue-transient--command)))
           (last-cmd-prompt (concat last-cmd " "))
           (initial-contents (when last-cmd
                               (propertize last-cmd-prompt
                                           'face 'shadow
                                           'read-only t
                                           'rear-nonsticky t)))
           ;; HACK: `blue-transient--setup-minibuffer' fills the completion table
           ;; from user input, this is why here we use `initial-contents'
           (input (minibuffer-with-setup-hook #'blue-transient--setup-minibuffer
                    (read-from-minibuffer "args=" initial-contents nil nil nil)))
           (args (string-trim-left input last-cmd-prompt))
           (cmd/args (cons last-cmd (list args))))
      (blue-transient--save-state)
      (setq blue-transient--command
            (if front
                (append front (list cmd/args))
              (list cmd/args))))))

;;;###autoload
(defun blue-transient ()
  "Open transient menu for BLUE.

Invoked with universal prefix argument '\\[universal-argument]', prompt
for a directory to use when running 'blue'.

The following steps are performed:

 - For each command, a unique key sequence is assigned.  See
   `blue-transient-keychar-function' and other related options.

 - Transient menu is built.  See `blue-transient--menu-heading' and
   `blue-transient--menu-columns' for altering its appearance.

 - Transient menu is opened.  Now we wait until selects target using its
   key sequence, or cancels operation.

After that, `blue-transient' closes menu and returns, while the command
keeps running in the compilation buffer."
  (interactive)
  (blue--ensure-cache)
  (let* ((blue--blueprint (blue--find-blueprint))
         (prefix (car current-prefix-arg))
         (build-dirs (blue--cache-get-build-dirs blue--blueprint))
         (last-build-dir (car build-dirs))
         (prefix (car current-prefix-arg))
         (prompt-dir-p (or (eql prefix 4) ; Single universal argument 'C-u'.
                           (and blue-require-build-directory
                                (not last-build-dir))))
         (build-dir (when prompt-dir-p
                      (blue--prompt-dir t))))
    (when build-dir
      (blue--cache-add build-dir)
      (setq build-dirs (cons build-dir build-dirs)
            last-build-dir (car build-dirs)))
    ;; Rebuild menu.
    (let* ((commands (blue--get-commands blue--blueprint))
           (indices (number-sequence 1 (length build-dirs)))
           (transient `(transient-define-prefix blue-transient--menu ()
                         :incompatible ',(list build-dirs)
                         :value '(,last-build-dir)
                         ;; Heading.
                         [:description
                          blue-transient--menu-heading
                          ("." "Blue options" "options="
                           :reader
                           (lambda (prompt initial-input history)
                             (minibuffer-with-setup-hook #'blue-transient--setup-minibuffer
                               (read-from-minibuffer prompt initial-input nil nil history))))
                          ("," "Last command args"
                           blue-transient--prompt-args
                           :transient t)]
                         ;; Build dirs.
                         ["Build directory"
                          ,@(seq-mapn (lambda (idx build-dir)
                                        (list (number-to-string idx) "" build-dir
                                              :format "%k %v"))
                                      indices build-dirs)]
                         ;; Commands.
                         ["Commands"
                          ,@(blue-transient--build-menu commands)
                          ;; Controls.
                          [("RET" ,(propertize "Run" 'face 'custom-button) blue-transient--run)
                           ("DEL" "Del" blue-transient--del :transient t)
                           ("C-l" "Clear" blue-transient--clear :transient t)
                           ("^" "Comint flip" "flip")
                           ("_" "Undo" blue-transient-undo :transient t)
                           ("M-_" "Redo" blue-transient-redo :transient t)]])))
      ;; Only evaluate menu if it has changed since the last invocation. This
      ;; ensures that any saved state (eg.: `transient-set') is preserved.
      (unless (equal transient blue-transient--menu-expresion)
        (setq blue-transient--menu-expresion transient)
        (eval transient))
      ;; Open menu.
      (blue-transient--menu))))

(provide 'blue-transient)
;;; blue-transient.el ends here.
