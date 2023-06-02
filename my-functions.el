;;; my-functions.el --- My independent functions file -*- lexical-binding: t; no-byte-compile: t -*-

;; reset all TODO to none then select only 10 as FROG from eat that frog book
(defun org-change-todo-keywords-in-directory (directory)
  "Change all TODO keywords in org headings to an empty string for all org files in DIRECTORY."
  (interactive "DDirectory: ")
  (let ((org-files (directory-files-recursively directory "\\.org$")))
    (dolist (file org-files)
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
         (lambda ()
           (when (string= (org-get-todo-state) "TODO")
             (org-set-property "TODO" "")))
         nil 'file)
        (save-buffer)
        (kill-buffer)))))


