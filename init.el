;;;;; PACKAGES
(require 'package)
(package-initialize)

;; Repositories
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

;;; Installed packages
; Themes
(package-install 'color-theme-sanityinc-tomorrow)


;;;;; UI
(load-theme 'sanityinc-tomorrow-eighties t)


;;;;; MISCELLANEOUS

; Sort Apropos by relevancy score
(setq apropos-sort-by-scores t)

; Where to save custom settings
(setq custom-file "~/.emacs.d/custom.el")
(load "~/.emacs.d/custom.el")

; Sunrise/Sunset
(setq calendar-latitude 49.4532115)
(setq calendar-longitude 11.0743073)
(setq calendar-location-name "Nürnberg, DE")
