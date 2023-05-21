;;; my-browse-url.el --- browse-url config file -*- lexical-binding: t; no-byte-compile: t -*-

;; set browse-url firefox var
(setq browse-url-firefox-program "C:\\Program Files\\Mozilla Firefox\\firefox.exe")

(defun my-search-google ()
  "Searches the web using the selected text."
  (interactive)
  (let ((search-text (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (thing-at-point 'symbol))))
    (browse-url (concat "https://www.google.com/search?q=" (url-hexify-string search-text)))))

(defun my-search-youtube ()
  "Searches the web using the selected text."
  (interactive)
  (let ((search-text (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (thing-at-point 'symbol))))
    (browse-url (concat "https://www.youtube.com/results?search_query=" (url-hexify-string search-text)))))
