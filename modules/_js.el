;; Basic settings
(add-hook 'js-mode-hook
  (lambda ()
    ; Use spaces for indendation
    (setq indent-tabs-mode nil
	  tab-width 2
	  js-indent-level 2)))

;; JSX
(add-hook 'rjsx-mode-hook
  '(lambda () (progn
    (setq indent-tabs-mode nil
	  tab-width 2
	  js2-basic-offset 2))))

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)
