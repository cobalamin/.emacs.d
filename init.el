(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defconst config-modules-subdir "modules")

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file
   (expand-file-name file
		     (concat user-init-dir config-modules-subdir))))

;; Pulling newest config from GitHub
(call-process-shell-command
 (concat "cd "
	 user-init-dir
	 " && git pull"))

;; Loading all modules
(load-user-file "packages.el")
(load-user-file "funs.el")

(load-user-file "editing.el")
(load-user-file "navigation.el")
(load-user-file "ui.el")
(load-user-file "misc.el")

;; Language specific config
(load-user-file "_haskell.el")

(load-user-file "_html.el")
(load-user-file "_js.el")
(load-user-file "_scss.el")

(load-user-file "_clojure.el")
