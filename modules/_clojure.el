;; clojure-mode
(require-package 'clojure-mode)
(require 'clojure-mode)

;; Use subword navigation
(add-hook 'clojure-mode-hook #'subword-mode)
;; Use paredit
(add-hook 'clojure-mode-hook #'paredit-mode)
;; Use rainbow delimiters
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)


;; CIDER
(require-package 'cider)
