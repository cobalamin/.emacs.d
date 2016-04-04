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

;;; Add /usr/local/bin to execution path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
