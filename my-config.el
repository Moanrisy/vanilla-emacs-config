;;; my-config.el --- My own init config file -*- lexical-binding: t; no-byte-compile: t -*-

;;; automatically insert timestamp on new heading
(defun my/org-add-created-timestamp ()
  (interactive)
  (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %a]")))

(add-hook 'org-insert-heading-hook 'my/org-add-created-timestamp)

;;; open magit status in full screen
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

;;; launch emacs maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; make emacs font bigger
(set-face-attribute 'default nil :height 120)

;;; hide all org mode heading
(defun my-org-hide-sublevels ()
  "Hide all sublevels under the current heading."
  (interactive)
  (outline-hide-sublevels 1))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<C-tab>") 'my-org-hide-sublevels))

;; ignore windows error sound when press C-g
(setq ring-bell-function 'ignore)

;; prevent org-pomodoro and org-clock-in running for task without estimated effort
(defun my-org-pomodoro ()
  "Start a pomodoro timer if org-effort is set."
  (interactive)
  (if (org-entry-get (point) "Effort")
      (org-pomodoro)
    (message "Please set org-effort for this task before starting a pomodoro.")))

(defun my-org-clock-in ()
  "Start a clock-in timer if org-effort is set."
  (interactive)
  (if (org-entry-get (point) "Effort")
      (org-clock-in)
    (message "Please set org-effort for this task before starting doing clock-in.")))

;; fix indentation in org-mode
(add-hook 'org-mode-hook 'org-indent-mode)

;; ignore case when switch buffer
(setq read-buffer-completion-ignore-case t)

;; set initial visibility for all org-mode files
(setq org-startup-folded t)

;; set org-todo-keyword color
 (setq org-todo-keyword-faces
       '(("TODO" . (:foreground "red"))
         ("IN-PROGRESS" . "yellow")
         ("DONE" . (:foreground "green" :weight bold))))

;; (setq display-line-numbers-type 'visual) ;; Not working on nano theme
;; open this file on startup so org-capture work properly
(find-file "~/Dropbox/notes/notes.org")
(find-file "~/Dropbox/notes/inbox.org")
(find-file "~/Dropbox/notes/now.org")
;;;; run custom org-agenda at startup
(setq org-agenda-window-setup 'only-window)
(org-agenda nil "g")
