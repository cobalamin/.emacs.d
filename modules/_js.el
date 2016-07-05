;; Basic settings
(add-hook 'js-mode-hook
  (lambda ()
    ; Use tabs for indendation
    (setq indent-tabs-mode t
	  tab-width 2
	  js-indent-level 2)))
