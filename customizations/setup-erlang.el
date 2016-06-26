;;;;;;;;;;;;;;
;; Erlang
;;;;;;;;;;;;;;

;; Erlang mode: http://erlang.org/doc/apps/tools/erlang_mode_chapter.html
(setq load-path (cons "/home/ezamora/.kerl/installations/r16b03-1/lib/tools-2.6.13/emacs"
load-path))
(setq erlang-root-dir "/home/ezamora/.kerl/installations/r16b03-1")
(setq exec-path (cons "/home/ezamora/.kerl/installations/r16b03-1/bin" exec-path))
(require 'erlang-start)

;; apply erlang mode to .erl, .hrl and .yaws
(autoload 'erlang-mode "erlang.el" "" t)
(add-to-list 'auto-mode-alist '("\\.[eh]rl$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.yaws$" . erlang-mode))

;; Edts
(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
 (require 'edts-start))
