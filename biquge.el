;;; biquge.el --- Read novels in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: nailuoGG
;; URL: https://github.com/nailuoGG/biquge.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (request "0.3.0"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; An Emacs port of biquge.nvim, allowing you to read novels in Emacs.
;; This package displays novel content as overlays that look like comments,
;; making it perfect for discreet reading.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)

(require 'biquge-types)
(require 'biquge-async)
(require 'biquge-web)
(require 'biquge-ui)

;;; Customization

(defgroup biquge nil
  "Read novels in Emacs."
  :group 'convenience
  :prefix "biquge-")

(defcustom biquge-width 30
  "Width of the displayed text (in characters)."
  :type 'integer
  :group 'biquge)

(defcustom biquge-height 10
  "Height of the displayed text (in lines)."
  :type 'integer
  :group 'biquge)

(defcustom biquge-face 'font-lock-comment-face
  "Face used for displaying novel text."
  :type 'face
  :group 'biquge)

(defcustom biquge-bookshelf-file
  (expand-file-name "biquge_bookshelf.json" user-emacs-directory)
  "Path to the bookshelf file."
  :type 'file
  :group 'biquge)

(defcustom biquge-picker 'builtin
  "UI picker to use for selection.
Possible values are:
- 'builtin: Use Emacs' built-in completing-read
- 'ivy: Use ivy for selection
- 'helm: Use helm for selection
- 'vertico: Use vertico for selection"
  :type '(choice (const :tag "Built-in" builtin)
          (const :tag "Ivy" ivy)
          (const :tag "Helm" helm)
          (const :tag "Vertico" vertico))
  :group 'biquge)

;;; Variables

(defvar biquge--domain "http://www.xbiquzw.com"
  "Domain of the novel website.")

(defvar biquge--current-book nil
  "Current book being read.")

(defvar biquge--current-toc nil
  "Table of contents for the current book.")

(defvar biquge--current-chap nil
  "Current chapter being read.")

(defvar biquge--current-content nil
  "Content of the current chapter.")

(defvar biquge--current-location nil
  "Current location (buffer and position) where the novel is displayed.")

(defvar biquge--active nil
  "Whether the novel display is active.")

(defvar biquge--begin-index -1
  "Start index of the displayed content.")

(defvar biquge--end-index -1
  "End index of the displayed content.")

(defvar biquge--bookshelf nil
  "List of books in the bookshelf.")

(defvar biquge--overlay nil
  "Overlay used to display the novel content.")

;;; Utility functions

(defun biquge--notify (msg &optional level)
  "Display a notification with MSG at LEVEL."
  (message "[biquge] %s" msg))

(defun biquge--current-chap-index ()
  "Get the index of the current chapter in the table of contents."
  (when biquge--current-chap
    (cl-position biquge--current-chap biquge--current-toc
                 :test (lambda (a b)
                         (and (string= (biquge-chapter-link a)
                                       (biquge-chapter-link b))
                              (string= (biquge-chapter-title a)
                                       (biquge-chapter-title b)))))))

(defun biquge--save ()
  "Save the current reading progress."
  (when (and biquge--current-book biquge--current-chap)
    (let ((found nil)
          (index (biquge--current-chap-index)))
      (dolist (record biquge--bookshelf)
        (when (and (string= (biquge-book-link (biquge-record-info record))
                            (biquge-book-link biquge--current-book))
                   (string= (biquge-book-title (biquge-record-info record))
                            (biquge-book-title biquge--current-book)))
          (when index
            (setf (biquge-record-last-read record) index))
          (setq found t)))
      (unless found
        (when index
          (push (biquge-record-create :info biquge--current-book
                                      :last-read index)
                biquge--bookshelf))))))

(defun biquge--load-bookshelf ()
  "Load the bookshelf from file."
  (when (file-exists-p biquge-bookshelf-file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents biquge-bookshelf-file)
          (setq biquge--bookshelf (biquge-web--json-parse-bookshelf (buffer-string))))
      (error
       (message "Error loading bookshelf: %S" err)
       (setq biquge--bookshelf nil)))))

(defun biquge--save-bookshelf ()
  "Save the bookshelf to file."
  (biquge--save)
  (condition-case err
      (with-temp-file biquge-bookshelf-file
        (insert (biquge-web--json-encode-bookshelf biquge--bookshelf)))
    (error
     (message "Error saving bookshelf: %S" err))))

(defun biquge--pieces (line)
  "Split LINE into pieces of width `biquge-width'."
  (let ((result nil)
        (len (length line))
        (i 0))
    (while (< i len)
      (push (substring line i (min (+ i biquge-width) len)) result)
      (setq i (+ i biquge-width)))
    (nreverse result)))

(defun biquge--cook-content ()
  "Fetch and process the content of the current chapter."
  (when (and biquge--current-book biquge--current-chap)
    (biquge-async-run
     (lambda ()
       (let ((content (biquge-web-get-chapter-content
                       (concat biquge--domain
                               (biquge-book-link biquge--current-book)
                               (biquge-chapter-link biquge--current-chap)))))
         (setq biquge--current-content
               (cons (concat "# " (biquge-chapter-title biquge--current-chap))
                     (apply #'append
                            (mapcar #'biquge--pieces content))))
         (setq biquge--begin-index 0
               biquge--end-index (min (length biquge--current-content) biquge-height))
         (biquge-show))))))

(defun biquge--reset ()
  "Reset the current state."
  (biquge-hide)
  (biquge--save)

  (setq biquge--current-book nil
        biquge--current-toc nil
        biquge--current-chap nil
        biquge--current-content nil
        biquge--begin-index -1
        biquge--end-index -1
        biquge--current-location nil))

(defun biquge--fetch-toc ()
  "Fetch the table of contents for the current book."
  (when biquge--current-book
    (biquge-async-run
     (lambda ()
       (setq biquge--current-toc
             (biquge-web-get-toc
              (concat biquge--domain (biquge-book-link biquge--current-book))))
       t))))

(defun biquge--jump-chap (offset)
  "Jump to a chapter with OFFSET from the current one."
  (if (null biquge--current-toc)
      (biquge--notify "没有正在阅读的小说，请先搜索想要阅读的小说" 'warning)
    (let* ((index (or (biquge--current-chap-index) 0))
           (target (+ index offset)))
      (when (and (>= target 0) (< target (length biquge--current-toc)))
        (setq biquge--current-chap (nth target biquge--current-toc))
        (biquge--cook-content)))))

;;; Interactive functions

;;;###autoload
(defun biquge-setup ()
  "Setup biquge."
  (interactive)
  (biquge--load-bookshelf)
  (add-hook 'kill-emacs-hook #'biquge--save-bookshelf))

;;;###autoload
(defun biquge-show ()
  "Show the novel at the current point."
  (interactive)
  (if (or (= biquge--begin-index -1) (= biquge--end-index -1))
      (biquge--notify "没有正在阅读的章节，请先搜索想要阅读的章节" 'warning)
    (setq biquge--current-location
          (list :buffer (current-buffer)
                :position (point)))

    (when biquge--overlay
      (delete-overlay biquge--overlay))

    (let ((content (cl-subseq biquge--current-content
                              biquge--begin-index
                              (min biquge--end-index
                                   (length biquge--current-content))))
          (inhibit-read-only t))
      (setq biquge--overlay (make-overlay (point) (point)))
      (overlay-put biquge--overlay 'after-string
                   (propertize (concat "\n" (mapconcat #'identity content "\n"))
                               'face biquge-face))
      (overlay-put biquge--overlay 'biquge t))

    (setq biquge--active t)))

;;;###autoload
(defun biquge-hide ()
  "Hide the novel."
  (interactive)
  (when (and biquge--active biquge--overlay)
    (delete-overlay biquge--overlay)
    (setq biquge--overlay nil
          biquge--active nil)))

;;;###autoload
(defun biquge-toggle ()
  "Toggle showing/hiding the novel."
  (interactive)
  (if biquge--active
      (biquge-hide)
    (biquge-show)))

;;;###autoload
(defun biquge-scroll (offset)
  "Scroll the novel content by OFFSET lines."
  (interactive "p")
  (unless biquge--active
    (user-error "Novel display is not active"))

  (let* ((content-length (length biquge--current-content))
         (step (if (< offset 0)
                   (max (- 1 biquge--begin-index) offset)
                 (min (- content-length biquge--end-index) offset))))
    (unless (zerop step)
      (setq biquge--begin-index (+ biquge--begin-index step)
            biquge--end-index (+ biquge--end-index step))
      (biquge-show))))

;;;###autoload
(defun biquge-next-chap ()
  "Go to the next chapter."
  (interactive)
  (biquge--jump-chap 1))

;;;###autoload
(defun biquge-prev-chap ()
  "Go to the previous chapter."
  (interactive)
  (biquge--jump-chap -1))

;;;###autoload
(defun biquge-toc ()
  "Show the table of contents."
  (interactive)
  (if (null biquge--current-book)
      (biquge--notify "没有正在阅读的小说，请先搜索想要阅读的小说" 'warning)
    (biquge-async-run
     (lambda ()
       ;; 直接同步获取目录
       (setq biquge--current-toc
             (biquge-web-get-toc
              (concat biquge--domain (biquge-book-link biquge--current-book))))
       ;; 目录获取完成后再显示
       (biquge-ui-pick-chapter biquge--current-book biquge--current-toc
                              (lambda (chap)
                                (setq biquge--current-chap chap)
                                (biquge--cook-content)))))))

;;;###autoload
(defun biquge-search ()
  "Search for a novel."
  (interactive)
  (biquge--reset)
  (biquge-async-run
   (lambda ()
     (let ((input (read-string "书名: ")))
       (when input
         (let ((results (biquge-web-search input)))
           (biquge-ui-pick-book results
                                (lambda (book)
                                  (setq biquge--current-book book)
                                  (biquge-toc)))))))))

;;;###autoload
(defun biquge-star (&optional book)
  "Star the current book or BOOK."
  (interactive)
  (let ((book (or book biquge--current-book)))
    (if (null book)
        (biquge--notify "没有正在阅读的小说，无法收藏" 'warning)
      (let ((found nil)
            (index nil))
        (cl-loop for record in biquge--bookshelf
                 for i from 0
                 when (and (string= (biquge-book-link (biquge-record-info record))
                                    (biquge-book-link book))
                           (string= (biquge-book-title (biquge-record-info record))
                                    (biquge-book-title book)))
                 do (setq found t
                          index i))
        (if found
            (progn
              (biquge--notify (format "取消收藏 %s - %s"
                                      (biquge-book-title book)
                                      (biquge-book-author book))
                              'info)
              (setq biquge--bookshelf (append (cl-subseq biquge--bookshelf 0 index)
                                              (cl-subseq biquge--bookshelf (1+ index)))))
          (biquge--notify (format "收藏 %s - %s"
                                  (biquge-book-title book)
                                  (biquge-book-author book))
                          'info)
          (push (biquge-record-create :info book
                                      :last-read (or (biquge--current-chap-index) 0))
                biquge--bookshelf))))))

;;;###autoload
(defun biquge-bookshelf ()
  "Show the bookshelf."
  (interactive)
  (biquge--reset)
  (biquge-ui-pick-bookshelf biquge--bookshelf
                            (lambda (record)
                              (setq biquge--current-book (biquge-record-info record))
                              (biquge-async-run
                               (lambda ()
                                 ;; 直接同步获取目录
                                 (setq biquge--current-toc
                                       (biquge-web-get-toc
                                        (concat biquge--domain (biquge-book-link biquge--current-book))))
                                 ;; 目录获取完成后再处理章节
                                 (let ((index (biquge-record-last-read record)))
                                   (when (and (>= index 0)
                                              (< index (length biquge--current-toc)))
                                     (setq biquge--current-chap
                                           (nth index biquge--current-toc))
                                     (biquge--cook-content))))))
                            (lambda (record)
                              (biquge-star (biquge-record-info record)))))

(provide 'biquge)
;;; biquge.el ends here
