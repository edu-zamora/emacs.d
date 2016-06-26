;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.pxi$" . clojure-mode))

(setq nrepl-hide-special-buffers t)

(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'paredit-mode)

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)

(add-hook 'clojure-mode-hook #'cider-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

(add-hook 'minibuffer-setup-hook (lambda () (paredit-mode 0)))
