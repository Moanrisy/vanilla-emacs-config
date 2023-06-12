;;; packages.el --- Packages File -*- lexical-binding: t; no-byte-compile: t -*-

(use-package electric
  :straight (:type built-in)
  :init
  (electric-pair-mode +1) ;; automatically insert closing parens 
  (setq electric-pair-preserve-balance nil)) ;; more annoying than useful

(use-package deft
  :init
  (setq deft-extensions '("org" "txt" "tex"))
  (setq deft-directory "~/Dropbox/notes")
)

(use-package magit
  :general
  (patrl/leader-keys
    "g" '(:ignore t :wk "git")
    "gg" '(magit-status :wk "status")))

(use-package real-auto-save
  :init
  (require 'real-auto-save)
  (setq real-auto-save-interval 2) ;; in seconds
  (add-hook 'text-mode-hook 'real-auto-save-mode))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-pomodoro
  :ensure t
  :after org
  :config

  ;; Add the new keybinding to org-agenda-mode-map
  ;; BUG my-org-pomodoro didn't work in agenda mode
  (evil-define-key 'motion org-agenda-mode-map
    "i" 'my-org-pomodoro)

  ;; Hook for org-pomodoro
  (add-hook 'org-pomodoro-started-hook
          (lambda ()
            (interactive)
            (start-process-shell-command "feha" nil "cmd.exe /c C:/Users/moanr/Dropbox/Scripts/feha.bat")))

  (add-hook 'org-pomodoro-finished-hook
          (lambda ()
            (interactive)
            (start-process-shell-command "fehb" nil "cmd.exe /c C:/Users/moanr/Dropbox/Scripts/fehb.bat")))

  (add-hook 'org-pomodoro-break-finished-hook
          (lambda ()
            (interactive)
            (start-process-shell-command "fehc" nil "cmd.exe /c C:/Users/moanr/Dropbox/Scripts/fehc.bat")))

  (defun my-org-pomodoro-break-finished-hook ()
  (setq org-clock-heading "After a good break, Let's go clock-in another task"))

  (add-hook 'org-pomodoro-break-finished-hook 'my-org-pomodoro-break-finished-hook)
)

;; git gutter plus
(use-package git-gutter+
  :ensure t
  :init (global-git-gutter+-mode)
  :config (progn
            (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)
            (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
            (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
            (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
            (define-key git-gutter+-mode-map (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
            (define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer))
  :diminish (git-gutter+-mode . "gg"))

;; visual fill column
(use-package visual-fill-column
  :config
  (setq-default visual-fill-column-center-text t)
  (setq global-visual-fill-column-mode t))

(add-hook 'text-mode-hook #'visual-fill-column-mode)
(add-hook 'text-mode-hook #'visual-line-mode)

;; avy - jump to things
(use-package avy)

;; Always load theme as the last package! -----------------
;;; Nano theme can't shown org-pomodoro timer
 (straight-use-package
   '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))
 (require 'nano)

;; redisable menu bar mode after load nano theme
(menu-bar-mode -1)
