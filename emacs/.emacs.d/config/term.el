;;; Terminal settings.
;;;
;;; Gabriele Rastello

;; Bash as default shell.
(defvar term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list term-shell)))
(ad-activate 'ansi-term)

;; Keybinding for new terminal.
(global-set-key (kbd "C-c t") #'ansi-term)
