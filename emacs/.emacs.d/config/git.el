;;; Git configuration.
;;;
;;; Gabriele Rastello.


(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c g") #'magit-status))
