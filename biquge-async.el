;;; biquge-async.el --- Async utilities for biquge -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: nailuoGG
;; URL: https://github.com/nailuoGG/biquge.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Async utilities for the biquge package.

;;; Code:

(require 'url)

;; Simple async function runner
(defun biquge-async-run (fn &rest args)
  "Run FN with ARGS asynchronously."
  (run-with-timer 0 nil (lambda () (apply fn args))))

;; Async HTTP request
(defun biquge-async-http-request (url callback &optional errorback)
  "Make an asynchronous HTTP request to URL.
Call CALLBACK with the response body on success.
Call ERRORBACK with the error message on failure."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         '(("User-Agent" . "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36")
           ("Accept" . "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8")
           ("Accept-Language" . "en-US,en;q=0.5"))))
    (url-retrieve url
                  (lambda (status)
                    (if (plist-get status :error)
                        (when errorback
                          (funcall errorback (format "Error: %s" (plist-get status :error))))
                      (goto-char (point-min))
                      (re-search-forward "^$" nil t)
                      (let ((response (buffer-substring-no-properties (point) (point-max))))
                        (funcall callback response))))
                  nil t t)))

;; Async shell command
(defun biquge-async-shell-command (command callback &optional errorback)
  "Execute COMMAND asynchronously.
Call CALLBACK with the output on success.
Call ERRORBACK with the error message on failure."
  (let ((buffer (generate-new-buffer "*biquge-async*")))
    (set-process-sentinel
     (start-process-shell-command "biquge-async" buffer command)
     (lambda (process event)
       (with-current-buffer buffer
         (let ((output (buffer-string)))
           (if (string-match-p "finished" event)
               (when callback
                 (funcall callback output))
             (when errorback
               (funcall errorback output)))))
       (kill-buffer buffer)))))

;; Async input (with a small delay to ensure UI responsiveness)
(defun biquge-async-input (prompt)
  "Read input with PROMPT asynchronously."
  (let ((result nil))
    (while (null result)
      (setq result (read-string prompt))
      (sit-for 0.01))
    result))

(provide 'biquge-async)
;;; biquge-async.el ends here
