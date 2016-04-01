(add-hook 'html-mode-hook
  (lambda ()
    ; set offset and tab width to 4
    (setq sgml-basic-offset 4
	  tab-width 4)
    ; Use tabs for indendation
    (setq indent-tabs-mode t)))
