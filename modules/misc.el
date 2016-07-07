;;; Sort Apropos by relevancy score
(setq apropos-sort-by-scores t)

;;; Where to save customize settings
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;;; Where to store backup files
(defvar backup-dir (concat user-emacs-directory "backups"))
(setq backup-directory-alist (list (cons "." backup-dir)))

;;; Don't create .# lock files
(setq create-lockfiles nil)

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

;;; Get environment vars
(require-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;;; Set language in environment, to not get encoding errors (especially from python in a shell process)
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")

;;; Automatically reload TAGS files
(setq tags-revert-without-query 1)
