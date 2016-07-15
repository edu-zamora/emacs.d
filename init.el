;;;;
;; Packages
;;;;

;; Define package repositories
;; http://ergoemacs.org/emacs/emacs_package_system.html
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives
;;             '("gnu" . "http://elpa.gnu.org/packages/") t)
;;(add-to-list 'package-archives
;;             '("org" . "http://orgmode.org/elpa/") t)

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(magit-popup . "melpa-stable") t)

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
;; To try:
;; - https://github.com/joaotavora/yasnippet
;; - https://github.com/justbur/emacs-which-key
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; text completion framework
    ;; http://company-mode.github.io/
    company

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/better-defaults.el line 47 for a description
    ;; of ido
    ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; git integration
    magit

    ;; incremental completion and selection narrowing framework
    ;; https://github.com/emacs-helm/helm
    helm

    ;; Use Dash docsets (documentation) in Emacs
    ;; https://github.com/areina/helm-dash
    ;; http://jwintz.me/blog/2014/02/16/helm-dash-makes-you-efficient/
    helm-dash

    ;; interface to the w3m text based browser
    w3m

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; erlang development tool suite
    ;; https://github.com/tjarvstrand/edts
    edts

    ;; http://jblevins.org/projects/markdown-mode/
    markdown-mode

    ;; inserts an org-mode link with title of page
    ;; https://github.com/rexim/org-cliplink
    org-cliplink

    ;; Typing game inspired in The House of the Dead
    ;; https://www.emacswiki.org/emacs/TypingOfEmacs
    typing

    ;; On OS X, an Emacs instance started from the graphical user
    ;; interface will have a different environment than a shell in a
    ;; terminal window, because OS X does not run a shell during the
    ;; login. Obviously this will lead to unexpected results when
    ;; calling external utilities like make from Emacs.
    ;; This library works around this problem by copying important
    ;; environment variables from the user's shell.
    ;; https://github.com/purcell/exec-path-from-shell
    exec-path-from-shell)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")

;;;;
;; Customization
;;;;

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; These customizations change the key bindings associated to certain
;; commands
(load "key-bindings.el")

;; Specific customizations for osx
(when is-mac
  (load "osx.el"))

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; Langauage-specific
(load "setup-erlang.el")
(load "setup-eqc.el")
(load "setup-clojure.el")
(load "setup-org.el")
;(load "setup-dmacro.el")
(load "setup-column-marker.el")
(load "setup-w3m.el")
