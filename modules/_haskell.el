;;; Install/require necessary packages
(require-package 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)

;;; HSC support
(require 's)
(defun haskell-process-load-hsc-file ()
  (interactive)
  (save-buffer)
  (let ((file-name (buffer-file-name)))
    (if (s-suffix? ".hsc" file-name)
	(let* ((out-buffer "*hsc2hs*")
	       (errcode (call-process "hsc2hs" nil out-buffer nil "-v" file-name)))
	  (if (= errcode 0)
	      (let* ((hs-name (substring (buffer-file-name) 0 -1))
		     (hs-buffer (find-file-noselect hs-name t)))
		(with-current-buffer hs-buffer
		  (haskell-process-load-file)))
	    (progn
	      (message "Error running hsc2hs (%s)" errcode)
	      (switch-to-buffer out-buffer))))
      (message "Can't load a non .hsc file with this command."))))

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
