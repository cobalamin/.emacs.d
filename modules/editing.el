;;; Undo Tree
(global-undo-tree-mode t)
(global-set-key (kbd "C-x /") 'undo-tree-visualize)
(defalias 'redo 'undo-tree-redo)

;;; Fix whitespace with C-c C-w
(global-set-key (kbd "C-c C-w") 'fixup-whitespace)

;;; Keybinding for whitespace-mode toggle
(global-set-key (kbd "C-c C-x C-w") 'whitespace-mode)

;;; Immediately show register preview
(setq register-preview-delay 0)

;;; C-w to kill current line if region is inactive
(require 'whole-line-or-region)
(whole-line-or-region-mode)

;;; Commenting
;; Dwim, or comment out line if not at end of line
;; https://www.emacswiki.org/emacs/CommentingCode
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
   If no region is selected and current line is not blank and we are not at the end of the line, then comment current line.
   Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key (kbd "C-;") 'comment-dwim-line)

(setq-default comment-column 0) ; I don't want a comment column for inline comments

;;; Paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

(defvar paredit-mode-hooks
  '(emacs-lisp-mode-hook
    eval-expression-minibuffer-setup-hook
    ielm-mode-hook
    lisp-mode-hook
    lisp-interaction-mode-hook
    scheme-mode-hook))

(dolist (hook paredit-mode-hooks)
  (add-hook hook #'enable-paredit-mode))

;;; Incredibly important: Emojis
(global-emojify-mode t)
(setq
 emojify-emoji-styles '(unicode)
 emojify-program-contexts '(comments string))

;;; Use hippie-expand instead of dabbrev-expand
;(global-set-key [remap dabbrev-expand] 'hippie-expand)

;;; Use C-tab for company-complete
(global-set-key (kbd "C-<tab>") 'company-complete)

;;; Use zop-to-char, which excludes the "zopped" char
(global-set-key [remap zap-to-char] 'zop-to-char)

;;; Bind C-M-backspace to backward-kill-sexp
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)

;;; Use british dictionary and aspell for spell correction
(setq ispell-program-name "aspell")
(setq ispell-dictionary "british")

;;; Flycheck
(require-package 'flycheck)
; don't show errors at point, use mouse hover functionality instead
(setq flycheck-display-errors-function nil)
; enabling it is up to each mode config file

;;; Company
(require-package 'company)
(require 'company)

;;; Command to clear buffer and ignore readonly text properties
(defun force-erase-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))
