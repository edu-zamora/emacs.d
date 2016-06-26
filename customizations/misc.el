;;;;;;;;;;;;;;
;; OTHERS
;;;;;;;;;;;;;;

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      x-select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; Fix tilde dead key issue: https://www.emacswiki.org/emacs/DeadKeys
(require 'iso-transl)

;; Getting the git revision in Emacs vc when head is detached
(defadvice vc-git-working-revision (around vc-git-working-revision-detached activate)
  "Get the git working revision when detached"
  ad-do-it
  (when (string= ad-return-value "")
    (setq ad-return-value
          (with-output-to-string
            (with-current-buffer standard-output
              (vc-git--out-ok "describe" "--tags" "--exact-match" "HEAD")))))
  (when (string= ad-return-value "")
    (setq ad-return-value
          (with-output-to-string
            (with-current-buffer standard-output
              (vc-git--out-ok "rev-parse" "HEAD")))))
  (setq ad-return-value (replace-regexp-in-string "\n$" "" ad-return-value)))
(ad-activate 'vc-git-working-revision t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNKNOWN STUFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ctrl-z-map (make-keymap))
(global-set-key "\C-z" ctrl-z-map)
(global-set-key "\C-zf" 'kfind)
(global-set-key (kbd "<C-tab>") 'dabbrev-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keybindings
(global-set-key "\M-g" 'goto-line)  ; Goto line number


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language environment
;; use latin-1
(set-terminal-coding-system 'iso-8859-1)
(setq default-buffer-file-coding-system 'iso-8859-1)
(prefer-coding-system 'iso-8859-1)
(set-language-environment "Latin-1")
(setq file-buffer-coding 'iso-8859-1)
;; (let ((mode (current-input-mode)))
;;   (setcar (cdr (cdr mode)) 8)
;;   (apply 'set-input-mode mode))
; (iso-accents-mode) C-x 8 /a -> å
(let ((mode (current-input-mode)))
  (setcar (cdr (cdr mode)) 8)
  (apply 'set-input-mode mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Annoying stuff
;; start with beep rather than blink, blank page, scrollbars
(setq visible-bell nil)
(setq inhibit-startup-echo-area-message t
      inhibit-startup-message           t)
(if (> emacs-major-version 19)
    (tool-bar-mode nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global modes
;; col & line numbers, mouse scrolling
(column-number-mode t)
(line-number-mode t)
(transient-mark-mode t)
;(if (>= emacs-major-version 21) (iswitchb-mode t))
(mouse-wheel-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font lock mode
;; start with full syntax colors on
(require 'font-lock)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; frame (window) defaults - initial frame may differ
(setq default-frame-alist
      '((width . 80) (height . 63)
	(vertical-scroll-bars . right)
        ))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((allout-layout . t) (erlang-indent-level . 2))))
 '(version-control-system (quote Git)))
(setq erlang-indent-level 2)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :family "misc-fixed")))))


;; Resizing the frame horizontally (based on code by Darren S. Embry)
(defun fix-frame-width (width)
  "Set the frame's size to 80 (or prefix arg WIDTH) columns wide."
  (interactive "P")
  (if window-system
      (if (equal (frame-width) 80)
	  (set-frame-width (selected-frame) (or width 164))
	(set-frame-width (selected-frame) (or width 80)))
    (error "Cannot resize frame horizontally: is a text terminal")))
(global-set-key (kbd "C-x w") 'fix-frame-width)


;; Resize windows keyboard shortcuts
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(setq align-to-tab-stop nil)
(setq-default indent-tabs-mode nil)

(menu-bar-mode 0)
(scroll-bar-mode nil)

(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit
                                      exit-minibuffer
                                      keyboard-quit))
          (ding))))
