;;; Terminal settings.
;;;
;;; Gabriele Rastello

;; Bash as default shell.
(defvar term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list term-shell)))
(ad-activate 'ansi-term)

