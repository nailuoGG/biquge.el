;;; biquge-ui.el --- UI components for biquge -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: nailuoGG
;; URL: https://github.com/nailuoGG/biquge.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; UI components for the biquge package.

;;; Code:

(require 'cl-lib)

(require 'biquge-types)

;; UI picker implementations

(defvar biquge-ui--current-picker nil
  "Current picker implementation.")

(defun biquge-ui--get-picker ()
  "Get the current picker implementation."
  (or biquge-ui--current-picker
      (setq biquge-ui--current-picker
            (pcase (bound-and-true-p biquge-picker)
              ('ivy (biquge-ui--ivy-picker))
              ('helm (biquge-ui--helm-picker))
              ('vertico (biquge-ui--vertico-picker))
              (_ (biquge-ui--builtin-picker))))))

;; Builtin picker (using completing-read)
(defun biquge-ui--builtin-picker ()
  "Create a builtin picker."
  (list
   :pick
   (lambda (items prompt display-fn confirm-fn &optional _actions)
     (let* ((candidates (mapcar (lambda (item)
                                  (cons (funcall display-fn item) item))
                                items))
            (selection (completing-read prompt candidates nil t)))
       (when selection
         (funcall confirm-fn (cdr (assoc selection candidates))))))

   :refresh nil))

;; Ivy picker
(defun biquge-ui--ivy-picker ()
  "Create an ivy picker."
  (if (not (require 'ivy nil t))
      (progn
        (message "Ivy is not available, falling back to builtin picker")
        (biquge-ui--builtin-picker))
    (list
     :pick
     (lambda (items prompt display-fn confirm-fn &optional actions)
       (let ((candidates (mapcar (lambda (item)
                                   (cons (funcall display-fn item) item))
                                 items)))
         (ivy-read prompt candidates
                   :action (lambda (x)
                             (funcall confirm-fn (cdr x)))
                   :caller 'biquge-ivy)))

     :refresh nil)))

;; Helm picker
(defun biquge-ui--helm-picker ()
  "Create a helm picker."
  (if (not (require 'helm nil t))
      (progn
        (message "Helm is not available, falling back to builtin picker")
        (biquge-ui--builtin-picker))
    (list
     :pick
     (lambda (items prompt display-fn confirm-fn &optional actions)
       (let ((candidates (mapcar (lambda (item)
                                   (cons (funcall display-fn item) item))
                                 items)))
         (helm :sources (helm-build-sync-source prompt
                                                :candidates candidates
                                                :action (lambda (candidate)
                                                          (funcall confirm-fn (cdr candidate))))
               :buffer "*helm biquge*")))

     :refresh nil)))

;; Vertico picker
(defun biquge-ui--vertico-picker ()
  "Create a vertico picker."
  (if (not (require 'vertico nil t))
      (progn
        (message "Vertico is not available, falling back to builtin picker")
        (biquge-ui--builtin-picker))
    (list
     :pick
     (lambda (items prompt display-fn confirm-fn &optional _actions)
       (let* ((candidates (mapcar (lambda (item)
                                    (cons (funcall display-fn item) item))
                                  items))
              (selection (completing-read prompt candidates nil t)))
         (when selection
           (funcall confirm-fn (cdr (assoc selection candidates))))))

     :refresh nil)))

;; Generic picker interface

(defun biquge-ui-pick (items prompt display-fn confirm-fn &optional actions)
  "Pick an item from ITEMS using the current picker.
PROMPT is the prompt to display.
DISPLAY-FN is a function that takes an item and returns a string.
CONFIRM-FN is a function that takes the selected item.
ACTIONS is an alist of additional actions."
  (let ((picker (biquge-ui--get-picker)))
    (funcall (plist-get picker :pick) items prompt display-fn confirm-fn actions)))

;; Specific picker interfaces

(defun biquge-ui-pick-book (books confirm-fn)
  "Pick a book from BOOKS.
CONFIRM-FN is called with the selected book."
  (biquge-ui-pick
   books
   "搜索结果"
   (lambda (book)
     (format "%s - %s" (biquge-book-title book) (biquge-book-author book)))
   confirm-fn))

(defun biquge-ui-pick-chapter (book chapters confirm-fn)
  "Pick a chapter from CHAPTERS for BOOK.
CONFIRM-FN is called with the selected chapter."
  (biquge-ui-pick
   chapters
   (format "%s - 目录" (biquge-book-title book))
   (lambda (chapter)
     (biquge-chapter-title chapter))
   confirm-fn))

(defun biquge-ui-pick-bookshelf (bookshelf confirm-fn unstar-fn)
  "Pick a book from BOOKSHELF.
CONFIRM-FN is called with the selected record.
UNSTAR-FN is called to unstar a book."
  (biquge-ui-pick
   bookshelf
   "书架"
   (lambda (record)
     (format "%s - %s"
             (biquge-book-title (biquge-record-info record))
             (biquge-book-author (biquge-record-info record))))
   confirm-fn
   (list (cons "unstar" unstar-fn))))

(provide 'biquge-ui)
;;; biquge-ui.el ends here
