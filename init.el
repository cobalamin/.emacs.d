;; Initialise packages
(package-initialize)

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defconst config-modules-subdir "modules")

(defun load-user-file-required (file)
  (interactive "f")
  "Load a file in current user's configuration directory. Fails with an error if the file does not exist."
  (load-file
   (expand-file-name
    file
    (concat user-init-dir config-modules-subdir))))

(defun load-user-file-if-exists (file)
  (interactive "f")
  "Load a file in current user's configuration directory, if it exists. Otherwise, do nothing."
  (let ((fname
	 (expand-file-name
	  file
	  (concat user-init-dir config-modules-subdir)))
	)
    (if (file-exists-p fname)
	(load-file fname))))

;; Pulling newest config from GitHub
(defun pull-emacs-d ()
  (interactive)
  (call-process-shell-command
   (concat "cd "
	   user-init-dir
	   " && git pull")))

;; Loading all modules
(load-user-file-required "packages.el")
(load-user-file-required "funs.el")

(load-user-file-required "editing.el")
(load-user-file-required "navigation.el")
(load-user-file-required "ui.el")
(load-user-file-required "misc.el")

;; Language specific config
;(load-user-file "_haskell.el")

(load-user-file-required "_html.el")
(load-user-file-required "_js.el")
(load-user-file-required "_scss.el")

;(load-user-file "_clojure.el")

(load-user-file-required "_elm.el")

;; Per-system overrides (file in .gitignore)
(load-user-file-if-exists "__overrides__.el")
