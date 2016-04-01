(require-package 'emmet-mode)

;; Basic settings
(add-hook 'html-mode-hook
  (lambda ()
    (setq
     ; Use tabs for indendation
     sgml-basic-offset 4
     tab-width 4
     indent-tabs-mode t)))

;; Emmet
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
;(add-hook 'css-mode-hook 'emmet-mode)

(eval-after-load "emmet-mode"
  '(progn
     (define-key emmet-mode-keymap (kbd "C-j") nil)
     (define-key emmet-mode-keymap (kbd "C-<tab>") 'emmet-expand-line)))
