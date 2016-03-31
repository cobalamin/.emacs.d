;;; Theme
(load-theme 'solarized-dark t)

;;; Font settings
(defun screen-height-to-font-size ()
  (let ((sh (display-pixel-height)))
    (cond ((<= sh 1080) 105)
	  ((<= sh 1440) 150)
	  (t 170))))

(set-face-attribute 'default nil :height (screen-height-to-font-size))
(set-face-attribute 'default nil :family "Pragmata Pro")

;;; Line numbers
(require 'linum)
(add-hook 'prog-mode-hook 'linum-on)
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
