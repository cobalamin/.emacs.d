;;;;; PACKAGES
(require 'package)

;;; Repositories
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(defvar my-packages
  '(;; Themes
    color-theme-sanityinc-tomorrow
    ;; Editing
    undo-tree
    ;; Navigation
    smex)
  "A list of packages to ensure are installed at launch.")

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

(install-packages)



;;;;; NAVIGATION

;;; Ido mode
(require 'ido)
; Flex/fuzzy matching
(setq ido-enable-flex-matching t)
; Emphasize regularly used file extensions in ido
(setq ido-file-extensions-order '(".js" ".clj" ".hs" ".py" ".rb" ".erb" ".css" ".html"))
; Ignore some files (regex), kept for reference for now
(setq ido-ignore-files
      (append ido-ignore-files
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



;;;;; EDITING

;;; Undo Tree
(global-undo-tree-mode t)
(global-set-key (kbd "C-x /") 'undo-tree-visualize)
(defalias 'redo 'undo-tree-redo)

;;; Fix whitespace with C-c C-w
(global-set-key (kbd "C-c C-w") 'fixup-whitespace)



;;;;; UI

;;; Theme
(load-theme 'sanityinc-tomorrow-eighties t)

;;; Font settings
(defun screen-height-to-font-size ()
  (let ((sh (display-pixel-height)))
    (cond ((<= sh 1080) 120)
	  ((<= sh 1440) 150)
	  (t 170))))

(set-face-attribute 'default nil :height (screen-height-to-font-size))
(set-face-attribute 'default nil :family "Pragmata Pro")



;;;;; MISCELLANEOUS

;;; Sort Apropos by relevancy score
(setq apropos-sort-by-scores t)

;;; Where to save customize settings
(setq custom-file "~/.emacs.d/custom.el")
(load "~/.emacs.d/custom.el")

;;; Sunrise/Sunset
(setq
 calendar-latitude 49.4532115
 calendar-longitude 11.0743073
 calendar-location-name "Nürnberg, DE")
