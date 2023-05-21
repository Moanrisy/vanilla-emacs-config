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

;; Always load theme as the last package! -----------------
(straight-use-package
  '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))
(require 'nano)

;; redisable menu bar mode after load nano theme
(menu-bar-mode -1)
