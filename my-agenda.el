;;; my-agenda.el --- Agenda config file -*- lexical-binding: t; no-byte-compile: t -*-

;; function to launch agenda then oipen g view
(defun my-org-agenda-g ()
  "Launches the Org Agenda and switches to the 'g' view."
  (interactive)
  (org-agenda nil "g"))

;; Clock in task fyne
;; Hook for clock-in
(defun write-clock-in-title-hook ()
  "Write clock in title into a file"
  (let ((clock-in-file "C:\\Users\\moanr\\AppData\\Local\\Temp\\clock-in-title"))
    (message (symbol-value 'org-clock-heading))
    (message "clocked time %d." (org-clock-get-clocked-time))
    (append-to-file (number-to-string (org-clock-get-clocked-time)) nil clock-in-file)
    (append-to-file "/" nil clock-in-file)

    (when (symbol-value 'org-clock-effort)
      (append-to-file (symbol-value 'org-clock-effort) nil clock-in-file))

    (append-to-file " " nil clock-in-file)
    (append-to-file (symbol-value 'org-clock-heading) nil clock-in-file)
    (append-to-file "\n" nil clock-in-file)))

(add-hook 'org-clock-in-hook 'write-clock-in-title-hook)
(run-at-time nil 60 #'write-clock-in-title-hook)

(defun my-org-clock-out-hook ()
  (setq org-clock-heading "Let's go clock-in another task"))

(add-hook 'org-clock-out-hook 'my-org-clock-out-hook)

;; Emacs GTD
;; https://github.com/rougier/emacs-gtd
(setq org-directory "~/Dropbox/notes")
(setq org-agenda-files (list "now.org" "course.org" "inbox.org" "projects.org" "agenda.org" "notes.org" "gtd-mobile.org"))

;; todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "DAILY(y)" "NEXT(n)" "HOLD(h)" "BUG(b)" "|" "DONE(d)" "KILL(k)")))

;; custom agenda
(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ((agenda ""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline)
                    )
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'regexp "\\* DAILY")
                    )
                   (org-deadline-warning-days 0)
                   (org-agenda-start-day "0d")
                   (org-agenda-span 'day)
                   ))
;;          (todo "DAILY"
;;                ((org-agenda-skip-function
;;                  '(org-agenda-skip-entry-if 'deadline))
;;                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
;;                 (org-agenda-todo-ignore-scheduled 'future)
;;                 (org-agenda-overriding-header "\nDaily habits\n")))
          (todo "NEXT"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline))
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "\nTasks\n")))
          (agenda nil
                  ((org-agenda-entry-types '(:deadline))
                   (org-agenda-format-date "")
                   (org-deadline-warning-days 7)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                   (org-agenda-overriding-header "\nDeadlines")))
          (tags-todo "course"
                     ((org-agenda-prefix-format "  %?-12t% s")
                      (org-agenda-overriding-header "\nCourse\n")))
          (tags-todo "inbox"
                     ((org-agenda-prefix-format "  %?-12t% s")
                      (org-agenda-overriding-header "\nInbox\n")))
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "\nCompleted today\n")))))))
