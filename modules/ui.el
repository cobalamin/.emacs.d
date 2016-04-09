;;; Line numbers
(require 'linum)
(global-linum-mode t)
; But not in doc-view-mode...
(add-hook 'doc-view-mode-hook
  (lambda ()
    (linum-mode -1)))
; Quick toggle
(global-set-key (kbd "C-c C-l") 'linum-mode)

;;; Column numbers
(column-number-mode 1)

;;; Highlight Current Line
(require 'hl-line)
(global-hl-line-mode)

;;; Show matching paren
(require 'paren)
(setq show-paren-delay 0)
(show-paren-mode 1)

;;; Rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;; Hide menu bar, toolbar, and scroll bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Propagate dired-details
(setq dired-details-propagate-flag t)
