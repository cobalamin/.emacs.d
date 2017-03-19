(defun my-bind-keys (bindings mode mode-map)
  "Set keybindings given BINDINGS, a key-to-command alist, MODE, a quoted mode variable, and a mode map"
  (eval-after-load mode
    '(progn
       (dolist (binding bindings)
	 (define-key
	   mode-map
	   (eval (car binding))
	   (cdr binding))))))
