;;; init.el --- Emacs configuration of Edu Zamora -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2016 Edu Zamora <edu.zasu@gmail.com>
;;
;; Author: Edu Zamora <edu.zasu@gmail.com>
;; URL: https://github.com/edu-zamora/emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Code:

;; Increase GC threshold to make init.el load faster
(defvar ez-default-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold 100000000)

;;; Debugging
(setq message-log-max 10000)

;;; Package management

;; Please don't load outdated byte code
(setq load-prefer-newer t)

;; https://stackoverflow.com/a/57172081/222900
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      ;; Package archives, the usual suspects
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      ;; Prefer MELPA Stable over GNU over MELPA
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Requires

(require 'use-package)
(require 'subr-x) ; Extra Lisp functions

;;; General config

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; No tabs: https://www.emacswiki.org/emacs/NoTabs
(setq-default indent-tabs-mode nil)

;; Fix tilde dead key issue: https://www.emacswiki.org/emacs/DeadKeys
(require 'iso-transl)

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Revert buffers automatically when underlying files are changed externally
;; http://www.jefftk.com/p/emacs-auto-revert-mode
(global-auto-revert-mode t)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Open links in Chrome
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "gnome-open")

;; Specific Mac OS X config
(when (equal system-type 'darwin)
  ;; Fix ctrl-left and ctrl-right
  (setq mac-option-key-is-meta t)
  (setq mac-right-option-modifier nil)
  ;; Open urls with the default browser
  (setq browse-url-browser-function 'browse-url-default-macosx-browser))

;;; Packages

(use-package gnu-elpa-keyring-update ; https://elpa.gnu.org/packages/gnu-elpa-keyring-update.html
  :ensure t)

(use-package validate ; Validate options (using validate-setq)
  :ensure t)

;; https://github.com/purcell/color-theme-sanityinc-tomorrow
;; Available themes:
;; - sanityinc-tomorrow-day
;; - sanityinc-tomorrow-night
;; - sanityinc-tomorrow-blue
;; - sanityinc-tomorrow-bright
;; - sanityinc-tomorrow-eighties
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-night t))

;;(use-package monokai-theme
;;  :ensure t
;;  :config
;;  (load-theme 'monokai t))

;;(use-package dracula-theme
;;  :ensure t
;;  :config
;;  (load-theme 'dracula t))

;;(use-package solarized-theme
;;  :ensure t
;;  :config
;;  (load-theme 'solarized-dark t))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (equal system-type 'darwin)
    (exec-path-from-shell-initialize)))

;; https://www.emacswiki.org/emacs/WhiteSpace
(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(use-package ido
  :ensure t
  :config
  (validate-setq
   ;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
   ido-enable-flex-matching t
   ;; Turn this behavior off because it's annoying
   ido-use-filename-at-point nil
   ;; Don't try to match file across all "work" directories; only match files
   ;; in the current directory displayed in the minibuffer
   ido-auto-merge-work-directories-length -1
   ;; Includes buffer names of recently open files, even if they're not open now
   ido-use-virtual-buffers t)
  (ido-mode +1))

;; Makes buffer names unique
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; Rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; Don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(use-package smex
  :ensure t
  :bind ("M-x" . smex))

;; Displays the key bindings following your currently entered incomplete command
;; (a prefix) in a popup
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :defer 5
  :config
  (which-key-mode +1))

;; Modular in-buffer completion framework for Emacs
;; http://company-mode.github.io/
(use-package company
  :ensure t
  :defer 5
  :config
  (global-company-mode))

;; Template system for Emacs.
;; It allows you to type an abbreviation and automatically expand it into
;; function templates
;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :ensure t
  :defer 5
  :config
  (yas-global-mode 1))

;; Keeping notes, maintaining TODO lists, planning projects, and authoring
;; documents with a fast and effective plain-text system
;; http://orgmode.org/
(use-package org
  :ensure t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-iswitchb))
  :config
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
                 '(("P" "Project List" ((tags "PROJECT")))))
  ;; Don't show things that are DONE
  ;; http://orgmode.org/manual/Global-TODO-list.html#Global-TODO-list
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-timestamp-if-done t)
  (setq org-agenda-todo-ignore-scheduled t)
  (setq org-agenda-todo-ignore-deadlines t)
  (setq org-agenda-todo-ignore-timestamp t)
  (setq org-agenda-todo-ignore-with-date t)
  ;; Start on current day
  (setq org-agenda-start-on-weekday nil)
  ;; http://orgmode.org/worg/agenda-optimization.html
  (setq org-use-tag-inheritance nil)
  ;; org-capture config
  (setq org-default-notes-file "~/Dropbox/org/action.org"))

;; Inserts an org-mode link with title of page
;; https://github.com/rexim/org-cliplink
(use-package org-cliplink
  :ensure t
  :bind (("C-x p i" . org-cliplink)))

;; A Git Porcelain inside Emacs
;; https://github.com/magit/magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package restclient
  :ensure t
  :defer t)

(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

;; Clojure Interactive Development Environment
(use-package cider
  :ensure t
  :defer t
  :bind (("C-c n" . cider-format-buffer)))

;; NOT FULLY WORKING
;; Refactoring support for Clojure projects
;; https://github.com/clojure-emacs/clj-refactor.el
(use-package clj-refactor
  :ensure t
  :bind (("C-c C-m" . cljr-add-keybindings-with-prefix))
  :config
  (clj-refactor-mode 1))

;; Perform structured editing of S-expression data
(use-package paredit
  :ensure t
  :defer t)

(use-package paren
  :config
  ;; Highlights matching parenthesis
  (show-paren-mode +1))

;; Highlight delimiters by depth
(use-package rainbow-delimiters
  :ensure t
  :defer t)

(use-package erlang
  :ensure t)

;; Erlang Development Tool Suite
;; https://github.com/tjarvstrand/edts
;; The installation from melpa does not fully work by itself.
;; After it is downloaded, go to edts' folder (under .emacs.d/elpa/)
;; and manually run `make`.
;; This package is also to blame for the next warning on startup:
;; "Package iswitchb is obsolete"
;(use-package edts
;  :ensure t
;  :pin "MELPA"
;  :config
;  (add-hook 'after-init-hook (lambda () (require 'edts-start))))

(use-package markdown-mode
  :ensure t
  :defer t)

;; On the fly markdown preview
(use-package flymd
  :ensure t
  :defer t)

;; For fly markdown, use a browser that works by default
;; https://github.com/mola-T/flymd/blob/master/browser.md
(defun my-flymd-browser-function (url)
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "firefox " url)
           nil
           "/usr/bin/open"
           (list "-a" "firefox" url))))
(setq flymd-browser-open-function 'my-flymd-browser-function)

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package coffee-mode
  :ensure t
  :defer t)

(use-package groovy-mode
  :ensure t
  :defer t)

;; Restore the default GC threshold
(setq gc-cons-threshold ez-default-gc-cons-threshold)
;; Clear the variable again
(makunbound 'ez-default-gc-cons-threshold)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(groovy-mode coffee-mode yaml-mode flymd markdown-mode edts erlang rainbow-delimiters paredit clj-refactor cider clojure-mode restclient magit org-cliplink yasnippet company which-key smex exec-path-from-shell color-theme-sanityinc-tomorrow validate use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
