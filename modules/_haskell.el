;; TODO Perhaps have a look at when deemed necessary:
; stylish-haskell

;; Install necessary packages
(setq my-packages
      (append my-packages
	      '(;; Modes
		haskell-mode ; The major mode
		shm ; structured-haskell-mode
		hindent-mode
		)))

(install-packages)


;; Use hindent minor mode for indenting with M-q
(add-hook 'haskell-mode-hook #'hindent-mode)

;; Generate tags on save
(setq haskell-tags-on-save t)

;; Some sensible settings
(setq
 ; Let GHC suggest to remove import lines that generate warnings
 haskell-process-suggest-remove-import-lines t
 ; Auto import the modules reported by GHC to have been loaded
 haskell-process-auto-import-loaded-modules t
 ; Debug logging to "*haskell-process-log*" buffer
 haskell-process-log t)

;; Some sensible keybindings

; haskell-mode
(my-bind-keys
 '(((kbd "C-c C-l") . 'haskell-process-load-file)
   ((kbd "C-c C-z") . 'haskell-interactive-switch)
   ((kbd "C-c C-n C-t") 'haskell-process-do-type)
   ((kbd "C-c C-n C-i") 'haskell-process-do-info)
   ((kbd "C-c C-n C-c") 'haskell-process-cabal-build)
   ((kbd "C-c C-n c") 'haskell-process-cabal))
 'haskell-mode
 haskell-mode-map)

; haskell-cabal
(my-bind-keys
 '(((kbd "C-c C-z") 'haskell-interactive-switch)
   ((kbd "C-c C-k") 'haskell-interactive-mode-clear)
   ((kbd "C-c C-c") 'haskell-process-cabal-build)
   ((kbd "C-c c") 'haskell-process-cabal))
 'haskell-cabal
 haskell-cabal-mode-map)
