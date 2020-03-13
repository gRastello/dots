;;; Haskell configs.
;;;
;;; Gabriele Rastello

(use-package haskell-mode
  :ensure t
  :config
  (eval-after-load 'haskell-mode '(progn
				    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
				    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch))))

