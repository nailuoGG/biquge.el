;;; biquge-web.el --- Web API for biquge -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: nailuoGG
;; URL: https://github.com/nailuoGG/biquge.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (dom-select "0.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Web API functions for the biquge package.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'json)
(require 'url-util)
(require 'dom-select)

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
  (biquge-debug "开始解析HTML，长度: %d" (length html))
  (with-temp-buffer
    (insert (biquge-web--normalize-html html))
    (biquge-debug "规范化后HTML长度: %d" (buffer-size))
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      (biquge-debug "DOM解析完成，结构: %s..."
               (substring (format "%S" dom) 0 (min 100 (length (format "%S" dom)))))
      dom)))

;; Content extraction functions

(defun biquge-web--extract-content (dom)
  "Extract content from DOM using CSS selectors."
  (biquge-debug "开始提取章节内容 DOM类型: %s" (type-of dom))
  (let ((content nil))
    (condition-case err
        (let ((content-div (car (dom-select dom "#content"))))
          (biquge-debug "content-div查找结果: %s" (if content-div "找到" "未找到"))
          (when content-div
            (biquge-debug "content-div结构: %s..."
                     (substring (format "%S" content-div) 0 (min 100 (length (format "%S" content-div)))))
            (let ((text (dom-text content-div)))
              (biquge-debug "提取到文本长度: %d" (length text))
              (setq content (split-string text "\n"))
              (biquge-debug "分割后行数: %d" (length content)))))
      (error
       (biquge-debug "dom-select 错误: %S" err)
       (setq content nil)))

    ;; 如果没有找到内容，尝试其他可能的容器
    (unless content
      (biquge-debug "未找到content-div，尝试查找其他可能的内容容器"))

    content))

(defun biquge-web--extract-toc (dom)
  "Extract table of contents from DOM using CSS selectors."
  (biquge-debug "开始提取目录 DOM类型: %s" (type-of dom))
  (let ((chapters nil))
    (condition-case err
        (let ((list-items (dom-select dom "#list a")))
          (biquge-debug "找到链接数量: %d" (length list-items))
          (dolist (a list-items)
            (let ((link (dom-attr a 'href))
                  (title (dom-text a)))
              (biquge-debug "处理链接: %s, 标题: %s" link title)
              (when (and link title)
                (push (biquge-chapter-create :link link :title title) chapters)))))
      (error
       (biquge-debug "dom-select 错误: %S" err)
       (setq chapters nil)))

    (biquge-debug "提取完成，章节数: %d" (length chapters))
    (nreverse chapters)))

(defun biquge-web--extract-books (dom)
  "Extract books from DOM using CSS selectors."
  (biquge-debug "开始提取书籍 DOM类型: %s" (type-of dom))
  (let ((books nil))
    (condition-case err
        (let ((rows (dom-select dom ".grid tr")))
          (biquge-debug "找到表格行数: %d" (length rows))
          (dolist (tr rows)
            ;; 跳过表头行
            (when (dom-select tr "td")
              (let* ((title-a (car (dom-select tr "td:first-child a")))
                     (author-td (car (dom-select tr "td:nth-child(2)")))
                     (link (and title-a (dom-attr title-a 'href)))
                     (title (and title-a (dom-text title-a)))
                     (author (and author-td (dom-text author-td))))
                (biquge-debug "提取书籍: 链接=%s, 标题=%s, 作者=%s"
                         (or link "nil") (or title "nil") (or author "nil"))
                (when (and link title author)
                  (push (biquge-book-create :link link :title title :author author) books))))))
      (error
       (biquge-debug "dom-select 错误: %S" err)
       (setq books nil)))

    (biquge-debug "提取完成，书籍数: %d" (length books))
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
             (author (or (alist-get 'author info) ""))  ;; 确保至少有空字符串
             (link (or (alist-get 'link info) ""))      ;; 确保至少有空字符串
             (title (or (alist-get 'title info) ""))    ;; 确保至少有空字符串
             (book (condition-case err
                       (biquge-book-create :author author :link link :title title)
                     (error
                      (biquge-debug "Error creating book: %S" err)
                      nil)))
             (last-read (or (alist-get 'last_read item) 0)))  ;; 确保有默认值
        (when book  ;; 只有当成功创建 book 时才添加记录
          (push (biquge-record-create :info book :last-read last-read) bookshelf))))
    (nreverse bookshelf)))

;; API functions

(defun biquge-web-search (query)
  "Search for books with QUERY."
  (biquge-debug "开始搜索: %s" query)
  (let ((result nil)
        (url (format "http://www.xbiquzw.com/modules/article/search.php?searchkey=%s"
                     (url-hexify-string query))))
    (biquge-debug "搜索URL: %s" url)
    (biquge-async-shell-command
     (format "curl --compressed '%s'" url)
     (lambda (html)
       (biquge-debug "获取搜索结果HTML长度: %d" (length html))
       ;; 保存HTML以便手动检查
       (when biquge-debug-mode
         (with-temp-file "/tmp/biquge-search-debug.html"
           (insert html))
         (biquge-debug "搜索HTML已保存到/tmp/biquge-search-debug.html"))
       (let ((dom (biquge-web--parse-html html)))
         (biquge-debug "搜索DOM解析完成")
         (setq result (biquge-web--extract-books dom)))))

    ;; Wait for the result (in a real async implementation, we would use callbacks)
    (biquge-debug "等待搜索结果...")
    (while (null result)
      (sit-for 0.1))

    (biquge-debug "搜索完成，找到书籍数: %d" (length result))
    result))

(defun biquge-web-get-toc (url)
  "Get table of contents from URL."
  (biquge-debug "开始获取目录: %s" url)
  (let ((result nil))
    (biquge-async-shell-command
     (format "curl --compressed '%s'" url)
     (lambda (html)
       (biquge-debug "获取HTML响应长度: %d" (length html))
       ;; 保存HTML以便手动检查
       (when biquge-debug-mode
         (with-temp-file "/tmp/biquge-debug.html"
           (insert html))
         (biquge-debug "HTML已保存到/tmp/biquge-debug.html"))

       ;; 使用DOM解析和CSS选择器提取
       (let ((dom (biquge-web--parse-html html)))
         (biquge-debug "DOM解析完成")
         (setq result (biquge-web--extract-toc dom)))))

    ;; 等待结果
    (biquge-debug "等待目录提取结果...")
    (while (null result)
      (sit-for 0.1))

    (biquge-debug "目录获取完成，章节数: %d" (length result))
    result))

(defun biquge-web-get-chapter-content (url)
  "Get chapter content from URL."
  (biquge-debug "开始获取章节内容: %s" url)
  (let ((result nil))
    (biquge-async-shell-command
     (format "curl --compressed '%s'" url)
     (lambda (html)
       (biquge-debug "获取章节HTML响应长度: %d" (length html))
       ;; 保存HTML以便手动检查
       (when biquge-debug-mode
         (with-temp-file "/tmp/biquge-chapter-debug.html"
           (insert html))
         (biquge-debug "章节HTML已保存到/tmp/biquge-chapter-debug.html"))
       (let ((dom (biquge-web--parse-html html)))
         (biquge-debug "章节DOM解析完成")
         (let ((content (biquge-web--extract-content dom)))
           (biquge-debug "章节内容提取完成，行数: %d" (length content))
           (setq result content)))))

    ;; Wait for the result
    (biquge-debug "等待章节内容提取结果...")
    (while (null result)
      (sit-for 0.1))

    (biquge-debug "章节内容获取完成")
    result))

(provide 'biquge-web)
;;; biquge-web.el ends here
