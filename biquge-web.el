;;; biquge-web.el --- Web API for biquge -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: nailuoGG
;; URL: https://github.com/nailuoGG/biquge.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Web API functions for the biquge package.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'json)
(require 'url-util)

(require 'biquge-types)
(require 'biquge-async)

;; HTML parsing utilities

(defun biquge-web--normalize-html (html)
  "Normalize HTML string by replacing common entities and tags."
  (thread-last html
               (replace-regexp-in-string "&nbsp;" " ")
               (replace-regexp-in-string "<br\\s*/>" "\n")
               (replace-regexp-in-string "\r\n" "\n")))

(defun biquge-web--parse-html (html)
  "Parse HTML string into a DOM tree."
  (with-temp-buffer
    (insert (biquge-web--normalize-html html))
    (libxml-parse-html-region (point-min) (point-max))))

;; Content extraction functions

(defun biquge-web--extract-content (dom)
  "Extract content from DOM."
  (let ((content-div (dom-by-id dom "content")))
    (if content-div
        (split-string (dom-text content-div) "\n")
      nil)))

(defun biquge-web--extract-toc (dom)
  "Extract table of contents from DOM."
  (let ((chapters nil)
        (list-div (dom-by-id dom "list")))
    (when list-div
      (dolist (a (dom-by-tag list-div 'a))
        (let ((link (dom-attr a 'href))
              (title (dom-text a)))
          (when (and link title)
            (push (biquge-chapter-create :link link :title title) chapters)))))
    (nreverse chapters)))

(defun biquge-web--extract-books (dom)
  "Extract books from search results DOM."
  (let ((books nil)
        (grid-table (dom-elements dom 'class "grid")))
    (when grid-table
      (dolist (tr (dom-by-tag grid-table 'tr))
        (let* ((tds (dom-by-tag tr 'td))
               (title-td (car tds))
               (author-td (cadr tds)))
          (when (and title-td author-td)
            (let* ((a (dom-by-tag title-td 'a))
                   (link (dom-attr (car a) 'href))
                   (title (dom-text (car a)))
                   (author (dom-text author-td)))
              (when (and link title author)
                (push (biquge-book-create :link link :title title :author author) books)))))))
    (nreverse books)))

;; JSON handling for bookshelf

(defun biquge-web--json-encode-bookshelf (bookshelf)
  "Encode BOOKSHELF to JSON."
  (let ((json-array (mapcar
                     (lambda (record)
                       `((info . ((author . ,(biquge-book-author (biquge-record-info record)))
                                  (link . ,(biquge-book-link (biquge-record-info record)))
                                  (title . ,(biquge-book-title (biquge-record-info record)))))
                         (last_read . ,(biquge-record-last-read record))))
                     bookshelf)))
    (json-encode json-array)))

(defun biquge-web--json-parse-bookshelf (json-string)
  "Parse JSON-STRING into a bookshelf."
  (let ((json-array (json-read-from-string json-string))
        (bookshelf nil))
    (dolist (item json-array)
      (let* ((info (alist-get 'info item))
             (book (biquge-book-create
                    :author (alist-get 'author info)
                    :link (alist-get 'link info)
                    :title (alist-get 'title info)))
             (last-read (alist-get 'last_read item)))
        (push (biquge-record-create :info book :last-read last-read) bookshelf)))
    (nreverse bookshelf)))

;; API functions

(defun biquge-web-search (query)
  "Search for books with QUERY."
  (let ((result nil)
        (url (format "http://www.xbiquzw.com/modules/article/search.php?searchkey=%s"
                     (url-hexify-string query))))
    (biquge-async-shell-command
     (format "curl --compressed '%s'" url)
     (lambda (html)
       (setq result (biquge-web--extract-books (biquge-web--parse-html html)))))

    ;; Wait for the result (in a real async implementation, we would use callbacks)
    (while (null result)
      (sit-for 0.1))

    result))

(defun biquge-web-get-toc (url)
  "Get table of contents from URL."
  (let ((result nil))
    (biquge-async-shell-command
     (format "curl --compressed '%s'" url)
     (lambda (html)
       (setq result (biquge-web--extract-toc (biquge-web--parse-html html)))))

    ;; Wait for the result
    (while (null result)
      (sit-for 0.1))

    result))

(defun biquge-web-get-chapter-content (url)
  "Get chapter content from URL."
  (let ((result nil))
    (biquge-async-shell-command
     (format "curl --compressed '%s'" url)
     (lambda (html)
       (setq result (biquge-web--extract-content (biquge-web--parse-html html)))))

    ;; Wait for the result
    (while (null result)
      (sit-for 0.1))

    result))

(provide 'biquge-web)
;;; biquge-web.el ends here
