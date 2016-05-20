;;; Install necessary packages
(require-package 'haskell-mode)
(require-package 'ghc)

;;; Require haskell-mode
(require 'haskell-mode)
(require 'haskell-interactive-mode)

;;; Some sensible settings
(setq
 ; Let GHC suggest to remove import lines that generate warnings
 haskell-process-suggest-remove-import-lines t
 ; Auto import the modules reported by GHC to have been loaded
 haskell-process-auto-import-loaded-modules t
 ; Debug logging to "*haskell-process-log*" buffer
 haskell-process-log t
 ; Use stack-ghci
 haskell-process-type 'stack-ghci
 ; Generate tags on save
 haskell-tags-on-save t)

;;; Load ghc-mod
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;;; Haskell-flycheck
(require-package 'flycheck-haskell)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
(add-hook 'haskell-mode-hook #'flycheck-mode)

;;; company-ghc
(require-package 'company-ghc)
(add-to-list 'company-backends 'company-ghc)

(add-hook 'haskell-mode-hook #'company-mode)
;(add-hook 'haskell-interactive-mode-hook #'company-mode)

;;; Some sensible keybindings

(my-bind-keys
 '(((kbd "C-c C-l") . haskell-process-load-file)
   ((kbd "C-c C-z") . haskell-interactive-switch)
   ((kbd "C-<tab>") . company-complete))
 'haskell-mode
 haskell-mode-map)

(my-bind-keys
 '(((kbd "C-<tab>") . company-complete)
   ((kbd "<up>") . haskell-interactive-mode-history-previous)
   ((kbd "<down>") . haskell-interactive-mode-history-next))
 'haskell-interactive-mode
 haskell-interactive-mode-map)
