;;; Haskell configs.
;;;
;;; Gabriele Rastello

(use-package haskell-mode
  :ensure t
  :config
  ;; (setq haskell-process-suggest-remove-import-lines t )
  ;; (setq	haskell-process-auto-import-loaded-modules  t)
  ;; (setq haskell-process-type                        'stack-ghci)
  ;; (setq haskell-process-log                         t)
  (eval-after-load 'haskell-mode '(progn
				    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
				    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch))))

