;;; Ido mode
(require 'ido)

(setq
 ; Flex/fuzzy matching
 ido-enable-flex-matching t
 ; Ido everywhere!
 ido-everywhere t
 ; Emphasize regularly used file extensions in ido
 ido-file-extensions-order '(".js" ".clj" ".hs" ".py" ".rb" ".erb" ".css" ".html")
 ; Ignore some files (regex), kept for reference for now
 ido-ignore-files (append ido-ignore-files
			  '()))

(ido-mode 1)

;;; Window navigation/management
; Use M-o and M-O for quick forward/backward window cycling
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") (kbd "C-- C-x o")) ; don't know how to do `negative-argument`

; Use windmove keybindings (S-<left>, S-<right>, S-<up>, S-<down>)
(windmove-default-keybindings)

;;; Smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)

;;; Smooth scrolling with C-p/C-n
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

;;; Imenu
(global-set-key (kbd "M-g M-o") 'imenu)

;;; I don't want no electric-indent
(electric-indent-mode -1)

;;; Edit the current file as root
(defun sudo ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
	     buffer-file-name))))

;;; Moar dired
(require 'dired-x)

;;; Neotree for directory tree navigation
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;;; Save configuration
(setq desktop-path `(,(concat user-emacs-directory "saved-desktops")))
(desktop-save-mode t)

;;; Set C-c C-x C-q as general shortcut if q is not bound to quit-window
(global-set-key (kbd "C-c C-x C-q") 'quit-window)

;;; Show full file name in minibuffer
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(global-set-key [C-f1] 'show-file-name)
