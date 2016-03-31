(defun my-bind-keys (bindings mode mode-map)
  "Set keybindings given BINDINGS, a key-to-command alist, MODE, a quoted mode variable, and a mode map"
  (eval-after-load mode
    '(progn
       (dolist (binding bindings)
	 (define-key
	   mode-map
	   (eval (car binding))
	   (cdr binding))))))

(defun escape-cabal-hell ()
  "Try to escape cabal hell with the bulldozer method: Just rm -rf ALL THE FILES"
  (interactive)
  (message "Escaping...")
  (dolist (cmd
	   '("rm -rf `find ~/.ghc -maxdepth 1 -type d`"
	     "rm -rf ~/.cabal/lib"
	     "rm -rf ~/.cabal/packages"
	     "rm -rf ~/.cabal/share"))
    (shell-command cmd))
  (message "(Possibly) escaped from cabal hell."))
