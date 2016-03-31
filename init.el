;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PACKAGES
(require 'package)

;;; Repositories
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(defun every (p list)
  "Check predicate for every element in a list. Should probably move this some place else."
  (let ((value t))
    (dolist (x list value)
      (setq value (and (funcall p x) value)))))

(defun all-packages-installed-p ()
  "Check if all my packages are installed."
  (every #'package-installed-p my-packages))

(defun require-package (package)
  "Install package, unless already installed."
  (unless (memq package my-packages)
    (add-to-list 'my-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun require-packages (packages)
  "Ensure PACKAGES are installed. Missing packages are installed automatically."
  (mapc #'require-package packages))

(defun install-packages ()
  "Install all packages listed in 'my-packages'."
  (unless (all-packages-installed-p)
    ;; Refresh package database
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    (require-packages my-packages)))

(defvar my-packages
  '(;; Themes
    base16-theme
    zenburn-theme
    solarized-theme
    ;; UI
    rainbow-delimiters
    dired-details+
    ;; Editing
    undo-tree
    whole-line-or-region
    paredit
    emojify
    zop-to-char
    ;; Navigation
    smooth-scrolling
    smex
    neotree)
  "A list of packages to ensure are installed at launch.")

(install-packages)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NAVIGATION

;;; Ido mode
(require 'ido)

(setq
 ; Flex/fuzzy matching
 ido-enable-flex-matching t
 ; Ido everywhere!
 ido-everywhere t
 ; Emphasize regularly used file extensions in ido
 ido-file-extensions-order '(".js" ".clj" ".hs" ".py" ".rb" ".erb" ".css" ".html")
 ; Ignore some files (regex), kept for reference for now
 ido-ignore-files (append ido-ignore-files
			  '()))

(ido-mode 1)

;;; Window navigation/management
; Use M-o and M-O for quick forward/backward window cycling
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") (kbd "C-- C-x o")) ; don't know how to do `negative-argument`

; Use windmove keybindings (S-<left>, S-<right>, S-<up>, S-<down>)
(windmove-default-keybindings)

;;; Smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)

;;; Smooth scrolling with C-p/C-n
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

;;; Imenu
(global-set-key (kbd "M-g M-o") 'imenu)

;;; I don't want no electric-indent
(electric-indent-mode -1)

;;; Edit the current file as root
(defun sudo ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
	     buffer-file-name))))

;; Moar dired
(require 'dired-x)

;; Neotree for directory tree navigation
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EDITING

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

;;; Use hippie-expand instead of dabbrev-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;;; Use zop-to-char, which excludes the "zopped" char
(global-set-key [remap zap-to-char] 'zop-to-char)

;;; Use british dictionary and aspell for spell correction
(setq ispell-program-name "aspell")
(setq ispell-dictionary "british")

;;; Spell-correct (La)TeX with the proper parser
(add-hook 'tex-mode-hook
  #'(lambda ()
      (setq ispell-parser 'tex)
      (flyspell-mode)))

(dolist (mh '(html-mode-hook clojure-mode-hook haskell-mode-hook))
  (add-hook mh 'flyspell-prog-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UI

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MISCELLANEOUS

;;; Sort Apropos by relevancy score
(setq apropos-sort-by-scores t)

;;; Where to save customize settings
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;;; Where to store backup files
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;;; Sunrise/Sunset
(setq
 calendar-latitude 49.4532115
 calendar-longitude 11.0743073
 calendar-location-name "Nürnberg, DE")

;;; No splash screen
(setq inhibit-splash-screen t)
