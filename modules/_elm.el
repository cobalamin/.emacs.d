;; elm-mode
(require-package 'elm-mode)
(require 'elm-mode)

;; Indentation
(setq elm-indent-offset 2)

;; Auto-completion
(add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
(add-hook 'elm-mode-hook #'company-mode)
(add-to-list 'company-backends 'company-elm)

;; Generate TAGS
(setq elm-tags-on-save t)

;; Proper C-c C-l binding, saving file before loading
(defun my-elm-repl-reload ()
  (interactive)
  (save-buffer)
  (elm-repl-load))

(my-bind-keys
 '(((kbd "C-c C-l") . my-elm-repl-reload))
 'elm-mode
 elm-mode-map)
