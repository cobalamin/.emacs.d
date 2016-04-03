;; Based on https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md

;; Update cabal
(message "Updating the cabal package lists...")
(shell-command "cabal update")
(message "Done.")

;; Install prerequisite commands if they're not known yet
;; This is very rudimentary but it works for what we need
(message "Installing prerequisite cabal packages...")
(dolist (pkg
	 '("hindent"
	   "happy"
	   "hasktags"
	   "hoogle"
	   "ghc-mod"
	   "hlint"))
  (if (eq 127 (call-process-shell-command pkg))
    (if (eq 0 (call-process-shell-command (concat "cabal install " pkg)))
	(message (concat "Installed cabal package " pkg))
	(message (concat "Error installing cabal package " pkg)))))
(message "Done.")

(message "Generating initial hoogle data...")
(shell-command "hoogle data")
(message "Done.")
