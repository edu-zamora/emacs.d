;;;;;;;;;;;;;;
;; EQC Emacs Mode
;;;;;;;;;;;;;;

(add-to-list 'load-path "/home/ezamora/.kerl/installations/r16b03-1/lib/eqc-1.32.2/emacs/")
(autoload 'eqc-erlang-mode-hook "eqc-ext" "EQC Mode" t)
(add-hook 'erlang-mode-hook 'eqc-erlang-mode-hook)
(setq eqc-max-menu-length 30)
(setq eqc-root-dir "/home/ezamora/.kerl/installations/r16b03-1/lib/eqc-1.32.2")
