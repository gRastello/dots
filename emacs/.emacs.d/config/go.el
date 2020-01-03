;;; Golang setup.
;;;
;;; Gabriele Rastello

(use-package go-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook (lambda () (setq tab-width 4))))
