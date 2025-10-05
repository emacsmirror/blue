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


;;; Configuration

(defcustom blue-transient-menu-columns-limit nil
  "If non-nil, limits maximum allowed number of menu columns.
Used by `blue-transient--menu-columns-function'."
  :package-version '(blue-compile . "0.1")
  :group 'blue-compile
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Limit")))

(defcustom blue-transient-menu-columns-spread nil
  "Whether to spread the columns so they span across the frame.

If non-nil, columns will have spacing between them and will
occupy the entire frame width.  Otherwise, columns will have
the minimum width needed to fit the contents."
  :package-version '(blue-transient . "0.4")
  :group 'blue-transient
  :type 'boolean)

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
  :package-version '(blue-transient . "0.1")
  :group 'blue-transient
  :type '(choice (const :tag "Default" nil)
                 function))

(defcustom blue-transient-keychar-unfold t
  "Whether to use upcase/downcase key characters.

If non-nil, allow using upcase and downcase variants of the original
character as the key character."
  :package-version '(blue-transient . "0.1")
  :group 'blue-transient
  :type 'boolean)

(defcustom blue-transient-keychar-regexp "[[:alnum:]]"
  "Regexp for allowed key characters.

Only those characters in group and target names, which match this regex,
can become key characters."
  :package-version '(blue-transient . "0.1")
  :group 'blue-transient
  :type 'regexp)


;;; Internal variables.
(defvar blue-transient-group-fallback 'unknown
  "The name of the fallback group for targets without group.")

(defvar blue-transient-menu-heading
  (propertize "Choose command" 'face 'bold)
  "Header for BLUE transient.")

(defconst blue-transient--keychar-table
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  "Valid characters used to form the keys to dispatch commands.

This is used to create the `blue-transient' menu.")


;;; Utilities.

(defun blue-transient--sort-function (categories)
  "Function to sort CATEGORIES and targets inside the categories.

The function is allowed to sort both categories and targets inside the
categories.

Default implementation sorts categories alphabetically, does not sort
targets, and places fallback group first."
  (let ((fallback-group (assoc blue-transient-group-fallback categories)))
    (append (when fallback-group
              (list fallback-group))
            (seq-sort (lambda (a b)
                        (string< (car a) (car b)))
                      (seq-remove (lambda (gr)
                                    (eq gr fallback-group))
                                  categories)))))

(defun blue-transient--menu-columns-function (items)
  "Return menu column count.

Takes assoc list returned by `blue-transient-split-function'.
Returns desired number of columns.

`blue-transient--build-grid' will arange ITEMS into N columns by
inserting a break after each Nth group."
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
                             "Got non-char key %S from blue-transient-keychar-function"
                             key))
                          (when (gethash key key-map)
                            (user-error
                             "Got duplicate key %s from blue-transient-keychar-function"
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
                  categories)))
    (mapcar
     (lambda (category)
       (let* ((category-name (symbol-name (car category)))
              (category-commands (cdr category))
              (category-command-names (mapcar (lambda (command)
                                                (alist-get 'invoke command))
                                              category-commands))
              (category-command-keys (blue-transient--assign-keys category-command-names nil)))
         (append
          (list category-name)
          (mapcar
           (lambda (command)
             (let* ((command-invoke (alist-get 'invoke command))
                    (command-key
                     (if (> (length commands) 1)
                         (s-concat (caddr (assoc category-name category-keys))
                                   (caddr (assoc command-invoke category-command-keys)))
                       (caddr (assoc command-invoke category-command-keys))))
                    ;; TODO: Make this the full blue command and think how
                    ;; to fit arguments.
                    (command-string command-invoke)
                    ;; TODO: use `blue--build-dir'.
                    (build-dir default-directory)
                    (command-synopsis (alist-get 'synopsis command)))
               `(,command-key
                 ,command-invoke
                 (lambda () ,command-synopsis (interactive)
                   ;; TODO: Handle properly the `comint-p' argument of
                   ;; `blue--compile'.
                   (blue--compile ,command-string nil)))))
           category-commands))))
     sorted-commands-by-category)))

(defun blue-transient--build-grid (menu-heading items)
  "Align menu items into a grid.

MENU-HEADING COLUMN-COUNT ITEMS."
  (let* ((column-count (blue-transient--menu-columns-function items))
         (columns (make-list column-count nil))
         (index 0))
    (dolist (item items)
      (setf (nth index columns) (append (nth index columns)
                                        (list item)))
      (setq index (% (1+ index) column-count)))
    (let* ((row-count (apply 'max (seq-map 'length columns)))
           (rows (make-list row-count nil)))
      (dotimes (row-index row-count)
        (dotimes (col-index column-count)
          (when (< row-index (length (nth col-index columns)))
            (setf (nth row-index rows) (append (nth row-index rows)
                                               (list (nth row-index (nth col-index columns))))))))
      (let ((grid (seq-map-indexed (lambda (row index)
                                     (vconcat
                                      (append (when (and menu-heading (eq index 0))
                                                `(:description ,(s-concat menu-heading "\n")))
                                              (list :class 'transient-columns)
                                              (seq-map 'vconcat row))))
                                   rows)))
        (when blue-transient-menu-columns-spread
          (setq grid (append `(:column-widths
                               ',(make-list column-count (/ (frame-width) column-count)))
                             grid)))
        grid))))


;;; UI.

;;;###autoload
(defun blue-transient ()
  "Open transient menu for BLUE.

The following steps are performed:

 - Available targets are collected according to the `:targets' function
   of the selected tool from `blue-transient-tool-alist'.

 - Targets are organized into groups.  See
   `blue-transient-group-function', `blue-transient-split-function',
   `blue-transient--sort-function' and other related options.

 - For each target, a unique key sequence is assigned.  See
   `blue-transient-keychar-function' and other related options.

 - Transient menu is built.  See `blue-transient-menu-heading-function'
   and `blue-transient--menu-columns-function' for altering its
   appearance.

 - Transient menu is opened.  Now we wait until selects target using its
   key sequence, or cancels operation.

 - After user have selected target, compilation command is formatted
   using `:command' function of the selected tool from
   `blue-transient-tool-alist'.

 - Formatted command is padded to `compile', or `project-compile', or
   other function.  See `blue-transient-function'.

After that, `blue-transient' closes menu and returns, while the command
keeps running in the compilation buffer."
  (interactive)
  (let* ((blue--blueprint (blue--find-blueprint))
         (commands (blue--get-commands blue--blueprint)))
    ;; Rebuild menu.
    (eval `(transient-define-prefix blue-transient--menu ()
             ,@(blue-transient--build-grid
                blue-transient-menu-heading
                (blue-transient--build-menu commands))))
    ;; Open menu.
    (blue-transient--menu)))

(provide 'blue-transient)
;;; blue-transient.el ends here.
