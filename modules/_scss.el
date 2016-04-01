(require-package 'scss-mode)

;; Basic settings
(add-hook 'scss-mode-hook
  (lambda ()
    (setq indent-tabs-mode t
	  tab-width 4)))
