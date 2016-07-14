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
   ((kbd "C-c C-x C-l") . haskell-process-load-hsc-file)
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

;;; HSC support
(require 's)
(defun haskell-process-load-hsc-file ()
  (interactive)
  (save-buffer)
  (let ((file-name (buffer-file-name)))
    (if (s-suffix? ".hsc" file-name)
	(let* ((out-buffer "*hsc2hs*")
	       (errcode (call-process "hsc2hs" nil out-buffer nil "-v" file-name)))
	  (if (= errcode 0)
	      (let* ((hs-name (substring (buffer-file-name) 0 -1))
		     (hs-buffer (find-file-noselect hs-name t)))
		(with-current-buffer hs-buffer
		  (haskell-process-load-file)))
	    (progn
	      (message "Error running hsc2hs (%s)" errcode)
	      (switch-to-buffer out-buffer))))
      (message "Can't load a non .hsc file with this command."))))
