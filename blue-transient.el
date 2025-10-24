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
(require 'cl-generic)
(require 'cus-edit) ; For `custom-button' face.
(require 'eieio)
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

(defface blue-transient-selection
  '((t :inherit blue-hint-highlight))
  "Face used to highlight selections.")

(defface blue-transient-selection-suffix
  '((t :inherit transient-argument))
  "Face used to highlight suffixes.")

(defface blue-transient-selection-suffix-value
  '((t :inherit transient-value))
  "Face used to highlight suffix values.")


;;; Internal variables.
(defvar blue-transient-group-fallback 'unknown
  "The name of the fallback group for targets without group.")

(defvar blue-transient--command-chain nil
  "List of BLUE command strings.")

;; NOTE: -1 means no selection.
(defvar blue-transient--selected-index -1
  "Current element from `blue-transient--command-chain'.")

(defvar blue-transient--selected-argument-index 0
  "Current argument from `blue-transient--command-chain' selected command.")

(defconst blue-transient--keychar-table
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  "Valid characters used to form the keys to dispatch commands.

This is used to create the `blue-transient' menu.")

(defvar blue-transient--menu-expresion nil
  "Last evaluated transient.

This is used to prevent reevaluating the same transient, losing any
possible saved state.")


;;; History.

(defvar blue-transient--history nil
  "Command prompt history stack for `blue-transient--command-chain'.")

(defvar blue-transient--history-index 0
  "Current history element from `blue-transient--history'.")

(defvar blue-transient--undo-stack nil
  "Undo stack for `blue-transient--command-chain'.")

(defvar blue-transient--redo-stack nil
  "Redo stack for `blue-transient--command-chain'.")

(defun blue-transient--save-state ()
  "Save current state of `blue-transient--command-chain' to the undo stack."
  (push (copy-tree blue-transient--command-chain) blue-transient--undo-stack)
  (setq blue-transient--redo-stack nil)) ; Clear redo when new change happens.

(defun blue-transient--set-and-setup ()
  "Set and setup 'blue-transient--menu'.
This save the current transient state for future invocations
`transient-set'.  And refreshes the menu `transient-setup'."
  (transient-set)
  (transient-setup (oref transient-current-prefix command)))

(defun blue-transient-undo ()
  "Undo last change to `blue-transient--command-chain'."
  (interactive)
  (when blue-transient--undo-stack
    (push (copy-tree blue-transient--command-chain) blue-transient--redo-stack)
    (setq blue-transient--command-chain (pop blue-transient--undo-stack))
    ;; Adjust command selection.
    (setq blue-transient--selected-index (min blue-transient--selected-index
                                              (length blue-transient--command-chain)))
    (blue-transient--set-and-setup)))

(defun blue-transient-redo ()
  "Redo last undone change to `blue-transient--command-chain'."
  (interactive)
  (when blue-transient--redo-stack
    (push (copy-tree blue-transient--command-chain) blue-transient--undo-stack)
    (setq blue-transient--command-chain (pop blue-transient--redo-stack))
    (blue-transient--set-and-setup)))

(defun blue-transient-previous-history ()
  "Set `blue-transient--command-chain' to previous recorded command."
  (interactive)
  (let ((prev (1+ blue-transient--history-index))
        (hist-length (length blue-transient--history)))
    (when (> prev hist-length)
      (message (propertize "End of history; no successing item"
                           'face 'minibuffer-prompt))
      (setq prev hist-length))
    (setq blue-transient--history-index prev)
    (setq blue-transient--command-chain (nth blue-transient--history-index
                                             blue-transient--history))
    (blue-transient--set-and-setup)))

(defun blue-transient-next-history ()
  "Set `blue-transient--command-chain' to next recorded command."
  (interactive)
  (let ((next (1- blue-transient--history-index)))
    (when (< next 0)
      (message (propertize "Beginning of history; no preceding item"
                           'face 'minibuffer-prompt))
      (setq next 0))
    (setq blue-transient--history-index next)
    (setq blue-transient--command-chain (nth blue-transient--history-index
                                             blue-transient--history))
    (blue-transient--set-and-setup)))


;;; Utilities.

(defun blue-transient--command-index-1 (&optional skip-arguments)
  "Utility to remove 1 from  `blue-transient--selected-index' respecting bounds.

If SKIP-ARGUMENTS is non-nil, jump to next command."
  (let* ((selected-command (blue-transient--selected-command))
         (selected-command-length (length selected-command))
         (previous-command (blue-transient--previous-command))
         (previous-command-length (length previous-command)))
    (cond
     ((and (not skip-arguments)
           (> selected-command-length 1)
           (> blue-transient--selected-argument-index 0))
      (setq blue-transient--selected-argument-index
            (1- blue-transient--selected-argument-index)))
     (t
      (setq blue-transient--selected-index (max (1- blue-transient--selected-index)
                                                -1)
            blue-transient--selected-argument-index (1- previous-command-length))))))

(defun blue-transient--command-index+1 (&optional skip-arguments)
  "Utility to add 1 from `blue-transient--selected-index' respecting bounds.

If SKIP-ARGUMENTS is non-nil, jump to previous command."
  (let* ((selected-command (blue-transient--selected-command))
         (selected-command-length (length selected-command)))
    (cond
     ((and (not skip-arguments)
           (> selected-command-length 1)
           (< blue-transient--selected-argument-index (1- selected-command-length)))
      (setq blue-transient--selected-argument-index
            (1+ blue-transient--selected-argument-index)))
     (t
      (setq blue-transient--selected-index (min (1+ blue-transient--selected-index)
                                                (1- (length blue-transient--command-chain)))
            blue-transient--selected-argument-index 0)))))

(defun blue-transient--select-first ()
  "Select first command for argument operation from `blue-transient--command-chain'."
  (interactive)
  (when blue-transient--command-chain
    (setq blue-transient--selected-index 0
          blue-transient--selected-argument-index 0)
    (blue-transient--set-and-setup)))

(defun blue-transient--select-previous ()
  "Select previous command for argument operation from `blue-transient--command-chain'."
  (interactive)
  (when blue-transient--command-chain
    (blue-transient--command-index-1)
    (blue-transient--set-and-setup)))

(defun blue-transient--select-next ()
  "Select next command for argument operation from `blue-transient--command-chain'."
  (interactive)
  (when blue-transient--command-chain
    (blue-transient--command-index+1)
    (blue-transient--set-and-setup)))

(defun blue-transient--select-last ()
  "Select first command for argument operation from `blue-transient--command-chain'."
  (interactive)
  (when blue-transient--command-chain
    (let* ((selected-command (blue-transient--selected-command))
           (selected-command-length (length selected-command)))
      (setq blue-transient--selected-index (1- (length blue-transient--command-chain))
            blue-transient--selected-argument-index (1- selected-command-length)))
    (blue-transient--set-and-setup)))

(defun blue-transient--selected-command ()
  "Get the selected command from the `blue-transient' menu prompt."
  (unless (< blue-transient--selected-index 0)
    (nth blue-transient--selected-index blue-transient--command-chain)))

(defun blue-transient--previous-command ()
  "Get the previous command from the `blue-transient' menu prompt."
  (unless (< blue-transient--selected-index 1)
    (nth (1- blue-transient--selected-index) blue-transient--command-chain)))

(defun blue-transient--selected-command-args-initial-values ()
  "Get the selected command arguments initial values."
  (let* ((selected-command (blue-transient--selected-command))
         (selected-command-args (cdr selected-command)))
    selected-command-args))

(defun blue-transient--selected-command-suffixes-values ()
  "Get the selected command arguments suffixes values from the menu prompt."
  (let* ((blueprint (or blue--blueprint
                        (blue--find-blueprint)))
         (args (transient-get-value))
         (selected-command (blue-transient--selected-command))
         (selected-command-suffixes (mapcar #'caddr
                                            (blue-transient--arguments-menu
                                             blueprint
                                             (car selected-command)))))
    (seq-filter (lambda (arg)
                  (let ((arg-prefix (concat (string-trim-right arg "=.*") "=")))
                    (member arg-prefix selected-command-suffixes)))
                args)))

(defun blue-transient--insert-nth (n elem lst)
  "Return a new LST with ELEM inserted at position N (0-based).
If N is 0, insert at the front.  If N >= length of LIST, append ELEM at
the end."
  (cond
   ((null lst)
    (list elem))
   ((= n 0)
    (cons elem lst))
   (t
    (cons (car lst)
          (blue-transient--insert-nth (1- n) elem (cdr lst))))))

(defun blue-transient--update-selected-command-arguments ()
  "Sync `blue-transient--command-chain' with selected command arguments."
  (let* ((blueprint (or blue--blueprint
                        (blue--find-blueprint)))
         (selected-command (blue-transient--selected-command))
         (suffixes (blue-transient--arguments-menu blueprint (car selected-command)))
         (suffixes-values (blue-transient--selected-command-suffixes-values))
         (selected-command-args (cdr selected-command))
         ;; Remove the ones controled by suffixes.
         (selected-command-args* (seq-remove
                                  (lambda (arg)
                                    (seq-some (lambda (suffix)
                                                (let ((suffix-argument
                                                       (caddr suffix)))
                                                  (string-prefix-p suffix-argument
                                                                   arg)))
                                              suffixes))
                                  selected-command-args))
         (merged-args (seq-uniq (append suffixes-values selected-command-args*)))
         (selected-command* (cons (car selected-command) merged-args))
         (cleaned-chain (seq-remove-at-position blue-transient--command-chain
                                                blue-transient--selected-index)))
    (setq blue-transient--command-chain
          (blue-transient--insert-nth blue-transient--selected-index
                                      selected-command*
                                      cleaned-chain))))

(defun blue-transient--del ()
  "Delete the selected argument or command in `blue-transient--command-chain'."
  (interactive)
  (let ((selected-index blue-transient--selected-index)
        (selected-argument-index blue-transient--selected-argument-index))
    (unless (or (not blue-transient--command-chain)
                (< selected-index 0))
      (blue-transient--save-state)
      (blue-transient--command-index-1)
      (if (> selected-argument-index 0)
          ;; Remove selected argument.
          (let* ((selected-command (blue-transient--selected-command))
                 (selected-command* (seq-remove-at-position selected-command
                                                            selected-argument-index))
                 (cleaned-chain (seq-remove-at-position blue-transient--command-chain
                                                        selected-index)))
            (setq blue-transient--command-chain
                  (blue-transient--insert-nth selected-index
                                              selected-command*
                                              cleaned-chain)))
        ;; Remove entire command.
        (setq blue-transient--command-chain
              (seq-remove-at-position blue-transient--command-chain
                                      selected-index)))
      (blue-transient--set-and-setup))))

(defun blue-transient--kill ()
  "Delete `blue-transient--command-chain' chain from selection."
  (interactive)
  (let ((selected-index blue-transient--selected-index)
        (selected-argument-index blue-transient--selected-argument-index))
    (unless (not blue-transient--command-chain)
      (blue-transient--save-state)
      (blue-transient--command-index-1)
      (let ((head (seq-take blue-transient--command-chain (1+ selected-index))))
        (if (> selected-argument-index 0)
            ;; Remove from selected argument.
            (let* ((selected-command (blue-transient--selected-command))
                   (selected-command* (seq-take selected-command
                                                selected-argument-index))
                   (cleaned-head (seq-remove-at-position head
                                                         selected-index)))
              (setq blue-transient--command-chain
                    (blue-transient--insert-nth selected-index
                                                selected-command*
                                                cleaned-head)))
          ;; Remove from selected command.
          (setq blue-transient--command-chain (butlast head))))
      (blue-transient--set-and-setup))))

(defun blue-transient--clear ()
  "Clean command prompt."
  (interactive)
  (when blue-transient--command-chain
    (blue-transient--save-state)
    (setq blue-transient--command-chain nil
          blue-transient--selected-index -1)
    (blue-transient--set-and-setup)))

(defun blue-transient--get-option-arguments ()
  "Retrieve values of options for the current transient prefix."
  (let ((prefix (oref transient-current-prefix command))
        (i 0)
        results)
    (condition-case nil
        (while t
          (let* ((suffix (transient-get-suffix prefix `(1 0 ,i)))
                 (argument (plist-get (cdr suffix) :argument)))
            (push argument results))
          (setq i (1+ i)))
      (error)) ; Do nothing, just break the loop if there are no more keys.
    results))

(defun blue-transient--run ()
  "Run BLUE commands."
  (interactive)
  (transient-set) ; Save the current transient state for future invocations.
  (let* ((args (transient-args (oref transient-current-prefix command)))
         (comint-flip (transient-arg-value "flip" args))
         (default-options (blue--normalize-options blue-default-options))
         (option-args (blue-transient--get-option-arguments))
         ;; Remove command arguments from `args'.
         (options (append default-options
                          (seq-filter (lambda (arg)
                                        (seq-some (lambda (option)
                                                    (string-prefix-p option
                                                                     arg))
                                                  option-args))
                                      args)))
         (is-interactive (blue--any-interactive-p blue-transient--command-chain))
         (comint (xor is-interactive comint-flip))
         (known-build-dirs (blue--cache-get-build-dirs default-directory))
         (blue--build-dir (seq-find (lambda (dir)
                                      (transient-arg-value dir args))
                                    known-build-dirs))
         (commands (mapcar (lambda (token)
                             (string-join token " "))
                           blue-transient--command-chain))
         (input (string-join commands " -- "))
         (full-input (string-join
                      (cons blue-binary
                            (append (when options options)
                                    (list input)))
                      " ")))
    (let ((last-hist (car blue-transient--history)))
      ;; Avoid not pushing consecutive duplicates.
      (unless (equal blue-transient--command-chain last-hist)
        (push (copy-tree blue-transient--command-chain) blue-transient--history)))
    (blue--compile full-input comint)))

(defun blue-transient--propertize-value-arg (arg selected)
  "Helper to propertize a transient ARG of the form 'arg=val'.

SELECTED controls the face properties to apply."
  (let* ((split (string-split arg "="))
         (arg (concat (car split) "="))
         (val (string-join (cdr split)
                           "="))
         (properties (if selected
                         '(:underline t)
                       '(:weight regular))))
    (concat (propertize arg 'face `(:inherit blue-transient-selection-suffix ,@properties))
            (propertize val 'face `(:inherit blue-transient-selection-suffix-value ,@properties)))))

(defun blue-transient--menu-heading ()
  "Dynamic header for BLUE transient."
  (let* ((selected-command (blue-transient--selected-command))
         (selected-command-suffixes (blue-transient--selected-command-suffixes-values))
         (head (seq-take blue-transient--command-chain blue-transient--selected-index))
         (tail (seq-drop blue-transient--command-chain (1+ blue-transient--selected-index)))
         (propertized-head (mapcar (lambda (tokens)
                                     (propertize (string-join tokens " ")
                                                 'face 'widget-field))
                                   head))
         (propertized-selection
          (when selected-command
            (string-join
             (seq-map-indexed
              (lambda (token i)
                (if (= i blue-transient--selected-argument-index)
                    (if (member token selected-command-suffixes)
                        (blue-transient--propertize-value-arg token t)
                      (propertize token 'face '(:inherit blue-transient-selection :underline t)))
                  (if (member token selected-command-suffixes)
                      (blue-transient--propertize-value-arg token nil)
                    (propertize token 'face '(:inherit blue-transient-selection :weight regular)))))
              selected-command)
             (propertize " " 'face 'widget-field))))
         (propertized-tail (mapcar (lambda (tokens)
                                     (propertize (string-join tokens " ")
                                                 'face 'widget-field))
                                   tail))
         (commands (seq-remove #'null
                               (append propertized-head
                                       (list propertized-selection)
                                       propertized-tail)))
         (input (string-join commands (propertize " -- " 'face 'widget-field)))
         (header (propertize "Commands:" 'face 'bold))
         (header* (if propertized-selection
                      (concat header "  ")
                    (concat header
                            (propertize " >" 'face 'blue-transient-selection)))))
    (concat header* input
            (propertize "\n" 'face '(:inherit widget-field :extend t)))))

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
          (seq-find
           (lambda (prefer-first)
             (seq-find
              (lambda (casefn)
                ;; If prefer-first is true:
                ;; - Find word with minimal N so that its Nth character is not
                ;;   taken.
                ;; Else:
                ;; - Find word with minimal N so that its Nth character is not
                ;;   taken AND is unique among Nth characters of all words.
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
                 (number-sequence 0 max-len)))
              ;; Repeat above search a few times: first try characters as-is,
              ;; then try their upper-case and down-case variants.
              (if blue-transient-keychar-unfold
                  (list 'identity 'upcase 'downcase)
                (list 'identity))))
           ;; If group-p is set, do above search once with prefer-first set to
           ;; t.  Otherwise, first try it with prefer-first set to nil, then
           ;; with t.  When prefer-first is nil, less matches are possible, but
           ;; we have a nice effect when keychars are placed in the same column
           ;; or close, so we try to maximize this effect.
           (if group-p
               '(t)
             '(nil t))))
        ;; Can't choose key char from word's letters, fallback to random key.
        ;; Randomness is based on word hash, so that we return same key
        ;; for same word, when possible.
        (unless word
          (setq word (seq-find (lambda (w)
                                 (not (assoc w word-keys)))
                               sorted-words)
                word-key (blue-transient--random-key word key-map)
                word-index (seq-position word word-key)))
        (push (list word
                    (string word-key))
              word-keys)
        (puthash word-key t key-map)))
    word-keys))

(defun blue-transient--group-commands (categories category-keys)
  "Group commands by CATEGORIES and assign CATEGORY-KEYS."
  (mapcar
   (lambda (category)
     (let* ((category-name (car category))
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
                       (concat (cadr (assoc category-name category-keys))
                               (cadr (assoc command-invoke category-command-keys)))
                     (cadr (assoc command-invoke category-command-keys))))
                  (command-synopsis (alist-get 'synopsis command)))
             `(,command-key
               ,(capitalize command-invoke)
               (lambda () ,command-synopsis (interactive)
                 (blue-transient--save-state)
                 (let ((command-chain (if blue-transient--command-chain
                                          (blue-transient--insert-nth
                                           (1+ blue-transient--selected-index)
                                           '(,command-invoke)
                                           blue-transient--command-chain)
                                        '((,command-invoke)))))
                   (setq blue-transient--command-chain command-chain)
                   (blue-transient--command-index+1 t)
                   (blue-transient--set-and-setup))))))
         category-commands))))
   categories))

(defun blue-transient--build-menu (commands)
  "Build transient menu for BLUE COMMANDS."
  (let* ((category-names (seq-uniq (mapcar (lambda (command)
                                             (alist-get 'category command))
                                           commands)))
         (category-keys (blue-transient--assign-keys category-names t))
         (sorted-commands-by-category
          (mapcar (lambda (category)
                    (cons category
                          (seq-filter (lambda (command)
                                        (string-equal (alist-get 'category command)
                                                      category))
                                      commands)))
                  category-names))
         (grouped-commands
          (blue-transient--group-commands sorted-commands-by-category
                                          category-keys)))
    ;; Make each menu entry a vector.
    (mapcar (lambda (item)
              (apply #'vector item))
            grouped-commands)))


;;; Selected command arguments.

(defclass blue-transient--command-argument (transient-option) ()
  "Class used for BLUE command arguments that can take a value.")

(cl-defmethod transient-infix-set :after ((obj blue-transient--command-argument) value)
  "Setter for the 'blue-transient--command-argument' class.

This function is meant for sideffects, it is responsible of keeping
`blue-transient--command-chain' sync with the selected command argument
suffixes."
  (blue-transient--update-selected-command-arguments))

(blue--define-memoized blue-transient--arguments-menu (blueprint command)
  "Build transient menu for BLUE COMMAND arguments."
  (when-let* ((suffixes (seq-filter (lambda (suffix)
                                      (string-prefix-p "--" suffix))
                                    (blue--autocomplete blueprint (concat command " --"))))
              (suffixes-keys (blue-transient--assign-keys suffixes nil))
              (menu-entries (mapcar (lambda (pair)
                                      (let* ((suffix (car pair))
                                             (key (cadr pair))
                                             (msg (string-replace
                                                   "-" " " (string-trim-left suffix "--")))
                                             (msg* (capitalize msg)))
                                        `(,(concat "--" key)
                                          ,msg*
                                          ,(concat suffix "=")
                                          :class blue-transient--command-argument)))
                                    suffixes-keys)))
    menu-entries))


;;; UI.

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
  (if (< blue-transient--selected-index 0)
      (message (propertize "No command selected"
                           'face 'minibuffer-prompt))
    (let* ((selected-command (blue-transient--selected-command))
           (head (seq-take blue-transient--command-chain blue-transient--selected-index))
           (tail (seq-drop blue-transient--command-chain (1+ blue-transient--selected-index)))
           (selected-command-string (string-join selected-command " "))
           (selected-command-prompt (concat selected-command-string " "))
           (initial-contents (when selected-command-string
                               (propertize selected-command-prompt
                                           'face 'shadow
                                           'read-only t
                                           'rear-nonsticky t)))
           ;; HACK: `blue-transient--setup-minibuffer' fills the completion table
           ;; from user input, this is why here we use `initial-contents'
           (input (minibuffer-with-setup-hook #'blue-transient--setup-minibuffer
                    (read-from-minibuffer "args=" initial-contents nil nil nil)))
           (args (string-trim-left input selected-command-prompt))
           (selected-command* (append selected-command (list args))))
      (blue-transient--save-state)
      (setq blue-transient--command-chain
            (append head (list selected-command*) tail)))))

(defun blue-transient--free-type ()
  "Helper for prompting for input to add to BLUE commands."
  (interactive)
  (let* ((input (minibuffer-with-setup-hook #'blue-transient--setup-minibuffer
                  (read-from-minibuffer "Input: " nil nil nil nil))))
    (blue-transient--save-state)
    (setq blue-transient--command-chain
          (append blue-transient--command-chain
                  (list (list input))))))

(defun blue-transient--selected-command-suffix-arguments (_)
  "Helper function to group last command arguments in transient suffixes."
  (if-let* ((blueprint (or blue--blueprint
                           (blue--find-blueprint)))
            (selected-command (car (blue-transient--selected-command)))
            (suffixes (blue-transient--arguments-menu blueprint selected-command))
            (columns (seq-split suffixes
                                (/ (length suffixes) 2)))
            ;; Make each menu entry a vector. Each vector will be a column.
            (vector-columns (mapcar (lambda (item)
                                      (apply #'vector item))
                                    columns)))
      (transient-parse-suffixes
       'transient--prefix
       vector-columns)
    (transient-parse-suffixes
     'transient--prefix
     '([(:info (propertize "No arguments" 'face 'shadow) :format "%d")]))))

;; FIXME: blue switches get dissable when chaning selection.

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
  (blue--check-blue-binary)
  (blue--ensure-cache)
  (setq blue-transient--history-index 0
        blue-transient--selected-index -1
        blue-transient--selected-argument-index 0
        blue--blueprint (blue--find-blueprint)
        blue--store-dir (make-temp-file "blue-" t)
        blue--data (blue--get-data blue--blueprint))
  (let* ((prefix (car current-prefix-arg))
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
    (let* ((commands (car blue--data))
           (indices (number-sequence 1 (length build-dirs)))
           (transient `(transient-define-prefix blue-transient--menu ()
                         :incompatible '(,(cons "--build-directory="
                                                build-dirs))
                         :init-value
                         (lambda (obj)
                           (let ((transient-state
                                  (if-let ((saved (assq (oref obj command)
                                                        transient-values)))
                                      (cdr saved) ; Set state for the session.
                                    (transient-default-value obj)))) ; Default.
                             (oset obj value
                                   (append
                                    (cons ,last-build-dir
                                          (blue-transient--selected-command-args-initial-values))
                                    transient-state))))
                         [:description
                          blue-transient--menu-heading
                          :class
                          transient-columns
                          [("M-p" "Previous prompt" blue-transient-previous-history)]
                          [("M-n" "Next prompt" blue-transient-next-history)]]
                         ;; Heading.
                         [["Options"
                           ("-a" "Always build" "--always-build")
                           (5 "-c" "Color" "--color="
                              :choices ("auto" "always" "never"))
                           (5 "-C" "Compiled load path" "--compiled-load-path"
                              :prompt "Compiled load path: "
                              :reader transient-read-existing-directory)
                           (5 "-F" "File" "--file="
                              :prompt "Blueprint file: "
                              :reader transient-read-existing-file)
                           ("-f" "Fresh store" "--fresh-store")
                           ("-j" "Jobs"
                            "--jobs="
                            ;; NOTE: here we could use
                            ;; `transient-read-number-N+' but I prefer the
                            ;; prompt to have a default value when hitting
                            ;; 'RET', instead of an initial value which you have
                            ;; to erase.
                            :reader (lambda (&rest _)
                                      (let* ((cpus (num-processors))
                                             (input (read-number "Max parallel jobs: "
                                                                 (num-processors))))
                                        (if (not (zerop input))
                                            (number-to-string input)
                                          (message "Zero is not a valid value.")
                                          nil))))
                           (5 "-L" "Load path" "--load-path="
                              :prompt "Load path: "
                              :reader transient-read-existing-directory)
                           ("-l" "Log level" "--log-level="
                            :choices ("error" "warn" "info" "debug" "trace"))
                           ("-p" "Profile" "--profile="
                            :choices ("gc" "stat" "trace"))
                           (5 "-s" "Source directory" "--source-directory="
                              :prompt "Source directory: "
                              :reader transient-read-existing-directory)
                           (5 "-S" "Store directory" "--store-directory="
                              :prompt "Store directory: "
                              :reader transient-read-existing-directory)
                           ("-t" "Trace" "--trace")]
                          ["" ; Empty description for alignment.
                           ("," "Selected command args"
                            blue-transient--prompt-args
                            :transient t)
                           ("!" "Free-type" blue-transient--free-type :transient t)
                           ("^" "Comint flip" "flip")]]
                         ;; Build dirs.
                         ["Build directory"
                          ;; NOTE: this switch won't add the directory to
                          ;; `blue--cache-list' is this the desired behavior?
                          ("-b" "Build directory" "--build-directory="
                           :prompt "Build directory: "
                           :reader transient-read-existing-directory)
                          ,@(seq-mapn (lambda (idx build-dir)
                                        (list (concat " " (number-to-string idx)) "" build-dir
                                              :format "%k %v"))
                                      indices build-dirs)]
                         [:description
                          (lambda ()
                            (if-let* ((selected-command (car (blue-transient--selected-command)))
                                      (invisible-separator ?\u2063)
                                      (separator (make-string 2 invisible-separator))
                                      (propertized-selected-command
                                       (if (display-graphic-p)
                                           ;; HACK: This is done because we are
                                           ;; using the ':box' face attribute
                                           ;; with a negative value to prevent
                                           ;; the height of the line to jump
                                           ;; when switching from no selection
                                           ;; (which does not have a box) to
                                           ;; selection.  To the commands we are
                                           ;; concatenating invisible separators
                                           ;; to prevent the box from being too
                                           ;; close to the text.
                                           (propertize
                                            (format "%s%s%s" separator selected-command separator)
                                            'face '(:inherit blue-hint-highlight :box (-1 . -1)))
                                         ;; Boxes are not supported in terminal
                                         ;; so let's use ':inverse-video'.
                                         (propertize
                                          selected-command
                                          'face '(:inherit blue-hint-highlight :inverse-video t)))))
                                (concat propertized-selected-command
                                        (propertize " arguments" 'face 'bold))
                              "No selected command"))
                          :class
                          transient-columns
                          :setup-children
                          blue-transient--selected-command-suffix-arguments]
                         ;; Command selector.
                         [[("C-a" "        " blue-transient--select-first :transient t)
                           ("<home>" "First" blue-transient--select-first :transient t)]
                          [("C-b" "     " blue-transient--select-previous :transient t)
                           ("<left>" "<-" blue-transient--select-previous :transient t)]
                          [("C-f" "      " blue-transient--select-next :transient t)
                           ("<right>" "->" blue-transient--select-next :transient t)]
                          [("C-e" "" blue-transient--select-last :transient t)
                           ("<end>" "Last" blue-transient--select-last :transient t)]]
                         ;; Commands.
                         ["Commands"
                          ,@(blue-transient--build-menu commands)
                          ;; Controls.
                          [("DEL" "Del" blue-transient--del :transient t)
                           ("C-k" "Kill" blue-transient--kill :transient t)
                           ("C-l" "Clear" blue-transient--clear :transient t)
                           ("_" "  Undo" blue-transient-undo :transient t)
                           ("M-_" "Redo" blue-transient-redo :transient t)]
                          [("RET" ,(propertize "Run" 'face 'custom-button) blue-transient--run)]])))
      ;; Only evaluate menu if it has changed since the last invocation. This
      ;; ensures that any saved state (eg.: `transient-set') is preserved.
      ;; Reevaluating means creating a new object which will not have the stored
      ;; state.
      (unless (equal transient blue-transient--menu-expresion)
        (setq blue-transient--menu-expresion transient)
        (eval transient))
      ;; Open menu.
      (blue-transient--menu))))

(provide 'blue-transient)
;;; blue-transient.el ends here.
