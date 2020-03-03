;;; Company completion engine.
;;;
;;; Gabriele Rastello

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 3)
  (setq company-backends (delete 'company-semantic company-backends))
  
  (add-hook 'prog-mode-hook 'company-mode))
