;;;;;;;;;;;;;;
;; ORG MODE
;;;;;;;;;;;;;;

(require 'org)

;; The following lines are always needed.  Choose your own keys.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; Customizing org-modules:
;; M-x customize-variable RET org-modules

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; English locale: http://orgmode.org/worg/org-faq.html
(setq system-time-locale "C")

;; Enable speed keys: http://notesyoujustmightwanttosave.blogspot.se/2011/12/org-speed-keys.html
(setq org-use-speed-commands t)

;; Record the time when a task is changed to DONE
(setq org-log-done t)

;; Sets files to search for TODOs
(setq org-agenda-files (list "~/Dropbox/org"
			     "~/Dropbox/org/projects"))


(setq org-agenda-custom-commands
      '(("P" "Project List"
          ( (tags "PROJECT")
          )
        )
       )
)

;; Don't show things that are DONE
;; http://orgmode.org/manual/Global-TODO-list.html#Global-TODO-list
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-timestamp-if-done t)
(setq org-agenda-todo-ignore-scheduled t)
(setq org-agenda-todo-ignore-deadlines t)
(setq org-agenda-todo-ignore-timestamp t)
(setq org-agenda-todo-ignore-with-date t)
(setq org-agenda-start-on-weekday nil) ;; start on current day
;; http://orgmode.org/worg/agenda-optimization.html
(setq org-use-tag-inheritance nil)

;; org-capture config
(setq org-default-notes-file "~/Dropbox/org/action.org")

;; automatically revert (reload) files changed on disk: http://www.jefftk.com/p/emacs-auto-revert-mode
(global-auto-revert-mode t)
