;; Column marker
(require 'column-marker)
(add-hook 'find-file-hook (lambda () (interactive) (column-marker-3 80)))
(global-font-lock-mode t)
(setq-default show-trailing-whitespace t)
