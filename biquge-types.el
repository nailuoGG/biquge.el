;;; biquge-types.el --- Type definitions for biquge -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: nailuoGG
;; URL: https://github.com/nailuoGG/biquge.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Type definitions for the biquge package.

;;; Code:

(require 'cl-lib)

;; Book type
(cl-defstruct (biquge-book
               (:constructor biquge-book-create)
               (:copier nil))
  "A book structure.
AUTHOR is the author of the book.
LINK is the link to the book.
TITLE is the title of the book."
  (author "" :type string :read-only t)
  (link "" :type string :read-only t)
  (title "" :type string :read-only t))

;; Chapter type
(cl-defstruct (biquge-chapter
               (:constructor biquge-chapter-create)
               (:copier nil))
  "A chapter structure.
LINK is the link to the chapter.
TITLE is the title of the chapter."
  (link "" :type string :read-only t)
  (title "" :type string :read-only t))

;; Location type
(cl-defstruct (biquge-location
               (:constructor biquge-location-create)
               (:copier nil))
  "A location structure.
BUFFER is the buffer where the novel is displayed.
POSITION is the position in the buffer."
  (buffer nil :read-only t)
  (position 0 :type integer :read-only t))

;; Record type
(cl-defstruct (biquge-record
               (:constructor biquge-record-create)
               (:copier nil))
  "A record structure for the bookshelf.
INFO is the book information.
LAST-READ is the index of the last read chapter."
  (info nil :type biquge-book)
  (last-read 0 :type integer))

(provide 'biquge-types)
;;; biquge-types.el ends here
