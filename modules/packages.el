(require 'package)

;;; Repositories
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa-stable" . "http://melpa.milkbox.net/packages/")))

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
    neotree

    ;; Useful libs for Elisp
    s)
  "A list of packages to ensure are installed at launch.")

(install-packages)
