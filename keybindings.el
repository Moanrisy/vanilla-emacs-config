;;; keybindings.el --- Keybindings File -*- lexical-binding: t; no-byte-compile: t -*-

;;; General
(use-package general
  :config
  (general-evil-setup)
  ;; integrate general with evil

  ;; set up 'SPC' as the global leader key
  (general-create-definer patrl/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  ;; set up ',' as the local leader key
  (general-create-definer patrl/local-leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "," ;; set local leader
    :global-prefix "M-,") ;; access local leader in insert mode

  (general-define-key
   :states 'insert
   "C-g" 'evil-normal-state) ;; don't stretch for ESC

  ;; unbind some annoying default bindings
  (general-unbind
    "C-x C-r"   ;; unbind find file read only
    "C-x C-z"   ;; unbind suspend frame
    "C-x C-d"   ;; unbind list directory
    "<mouse-2>") ;; pasting with mouse wheel click


  (patrl/leader-keys
    ;;"SPC" '(execute-extended-command :wk "execute command") ;; an alternative to 'M-x'
    "SPC" '(switch-to-buffer :wk "switch buffer") ;; gets overridden by consult
    "TAB" '(:keymap tab-prefix-map :wk "tab")) ;; remap tab bindings

  (patrl/leader-keys
    "c" '(:ignore t :wk "code"))

  ;; help
  ;; namespace mostly used by 'helpful'
  (patrl/leader-keys
    "h" '(:ignore t :wk "help"))

  ;; file
  (patrl/leader-keys
    "f" '(:ignore t :wk "file")
    "ff" '(find-file :wk "find file") ;; gets overridden by consult
    "fs" '(save-buffer :wk "save file"))

  ;; buffer 
  ;; see 'bufler' and 'popper'
  (patrl/leader-keys
    "b" '(:ignore t :wk "buffer")
    "bk" '(kill-this-buffer :wk "kill this buffer")
    ; use ibuffer, M-x ibuffer t D y
    ;;"bK" '(kill-other-buffers :wk "kill other buffers")
    "br" '(revert-buffer :wk "reload buffer")
  ;; bookmark
    "bs" '(bookmark-set :wk "set bookmark")
    "bj" '(bookmark-jump :wk "jump to bookmark")
    )

  ;; universal argument
  (patrl/leader-keys
    "u" '(universal-argument :wk "universal prefix"))

  ;; notes
  ;; see 'citar' and 'org-roam'
  (patrl/leader-keys
    "n" '(:ignore t :wk "notes")
    ;; see org-roam and citar sections
    "na" '(org-todo-list :wk "agenda todos") ;; agenda
    "nd" '(deft :wk "deft notes") ;; deft notes
    "ne" '(org-set-effort :wk "org-set-effort")
    "ni" '(org-clock-in :wk "notes clock in")
    "nc" '(org-clock-goto :wk "currently clocked in notes")
    "no" '(org-clock-out :wk "notes clock out"))

  ;; code
  ;; see 'flymake'
  (patrl/leader-keys
    "c" '(:ignore t :wk "code"))

  ;; open
  (patrl/leader-keys
    "o" '(:ignore t :wk "open")
    "os" '(speedbar t :wk "speedbar")) ;; TODO this needs some love

  ;; run
  (patrl/leader-keys
    "r" '(:ignore t :wk "run")
    "rp" '(my-org-pomodoro t :wk "run pomodoro"))

  ;; search
  ;; see 'consult'
  (patrl/leader-keys
    "s" '(:ignore t :wk "search")
    "sg" '(my-search-google t :wk "google")
    "sy" '(my-search-youtube t :wk "youtube"))

  ;; agenda
  (patrl/leader-keys
    "a" '(:ignore t :wk "org-agenda")
    "ag" '(my-org-agenda-g :wk "open agenda g view"))

  ;; templating
  ;; see 'tempel'
  (patrl/leader-keys
    "t" '(:ignore t :wk "template")))


(defun me/last-buffer ()
  "Switch to the last buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

  ;; Switch last buffer
  (patrl/leader-keys 
    "ESC" 'me/last-buffer :wk "last buffer")

;; "c" '(org-capture :wk "capture")))

;;; Evil
(use-package evil
  :general
  (patrl/leader-keys
    "w" '(:keymap evil-window-map :wk "window")) ;; window bindings
  :init
  (setq evil-search-module 'isearch)

  (setq evil-want-C-u-scroll t) ;; allow scroll up with 'C-u'
  (setq evil-want-C-d-scroll t) ;; allow scroll down with 'C-d'

  (setq evil-want-integration t) ;; necessary for evil collection
  (setq evil-want-keybinding nil)

  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)

  (setq evil-want-C-i-jump nil) ;; hopefully this will fix weird tab behaviour

  (setq evil-undo-system 'undo-redo) ;; undo via 'u', and redo the undone change via 'C-r'; only available in emacs 28+.
  :config
  (evil-mode t) ;; globally enable evil mode
  ;; set the initial state for some kinds of buffers.
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  ;; buffers in which I want to immediately start typing should be in 'insert' state by default.
  (evil-set-initial-state 'eshell-mode 'insert)
  (evil-set-initial-state 'magit-diff-mode 'insert))

(use-package evil-collection ;; evilifies a bunch of things
  :after evil
  :init
  (setq evil-collection-outline-bind-tab-p t) ;; '<TAB>' cycles visibility in 'outline-minor-mode'
  ;; If I want to incrementally enable evil-collection mode-by-mode, I can do something like the following:
  ;; (setq evil-collection-mode-list nil) ;; I don't like surprises
  ;; (add-to-list 'evil-collection-mode-list 'magit) ;; evilify magit
  ;; (add-to-list 'evil-collection-mode-list '(pdf pdf-view)) ;; evilify pdf-view
  :config
  (evil-collection-init))
