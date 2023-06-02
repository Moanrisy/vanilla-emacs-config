;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t; -*-
;; NOTE: init.el is now generated from readme.org.  Please edit that file instead

(setq straight-use-package-by-default t) ;; have use-package use straight.el by default.

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package) ;; install use-package via straight


;;; Variables
(defvar patrl/library-path "~/Dropbox (MIT)/library/"
  "Directory .pdf collection lives.")

(defvar patrl/notes-path "~/notes/"
  "Notes.")

(defvar patrl/journal-path (concat patrl/notes-path "daily/")
  "Journal entries.")

(defvar patrl/global-bib-file "~/repos/bibliography/master.bib"
  "Bibliography.")

(defvar patrl/org-path "~/Dropbox (MIT)/org/"
  "Org path.")

;;; Defaults
(use-package emacs
  :init
  (setq user-full-name "Mohammad Anton") ;; my details
  (setq user-mail-address "moanrisy@gmail.com")

  (defalias 'yes-or-no-p 'y-or-n-p) ;; life is too short

  (setq indent-tabs-mode nil) ;; no tabs

  (setq make-backup-files nil) ;; keep everything under vc 
  (setq auto-save-default nil)

  ;; keep backup and save files in a dedicated directory
  (setq backup-directory-alist
        `((".*" . ,(concat user-emacs-directory "backups")))
        auto-save-file-name-transforms
        `((".*" ,(concat user-emacs-directory "backups") t)))

  (setq create-lockfiles nil) ;; no need to create lockfiles

  (set-charset-priority 'unicode) ;; utf8 in every nook and cranny
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  (global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; escape quits everything

  ;; Don't persist a custom file
  (setq custom-file (make-temp-file "")) ; use a temp file as a placeholder
  (setq custom-safe-themes t)            ; mark all themes as safe, since we can't persist now
  (setq enable-local-variables :all)     ; fix =defvar= warnings

  (setq delete-by-moving-to-trash t) ;; use trash-cli rather than rm when deleting files.

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq native-comp-async-report-warnings-errors nil)
  (setq load-prefer-newer t)

  ;; FIXME currently using tempel in org-mode triggers this warning
  ;; (setq warning-suppress-types (append warning-suppress-types '((org-element-cache))))

  (show-paren-mode t)

  ;; Hide commands in M-x which don't work in the current mode
  (setq read-extended-command-predicate #'command-completion-default-include-p))

;;; Load other config files
(load-file "~/.emacs.d/keybindings.el")
(load-file "~/.emacs.d/packages.el")
(load-file "~/.emacs.d/my-agenda.el")
(load-file "~/.emacs.d/my-browse-url.el")
(load-file "~/.emacs.d/my-functions.el")

;;; My-config for init.el

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
      '(("TODO" . org-warning)
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
