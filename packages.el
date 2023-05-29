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
  (add-hook 'org-mode-hook 'real-auto-save-mode))

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


;; Always load theme as the last package! -----------------
;;; Nano theme can't shown org-pomodoro timer
 (straight-use-package
   '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))
 (require 'nano)

;; redisable menu bar mode after load nano theme
(menu-bar-mode -1)
