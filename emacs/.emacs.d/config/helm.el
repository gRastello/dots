;;; Helm stuff
;;;
;;; Gabriele Rastello

(use-package helm
  :ensure t
  :bind
  ("C-x C-f" . 'helm-find-files)
  ("C-x C-b" . 'helm-buffers-list)
  ("M-x"     . 'helm-M-x)
  :config
  (setq helm-autoresize-max-height 0
        helm-autoresize-min-height 30
	helm-mode-fuzzy-match t
        helm-move-to-line-cycle-in-source nil)
  (setq helm-external-programs-associations '(("pdf" . "evince")))
  :init
  (helm-mode 1))

(require 'helm-config)    
(helm-autoresize-mode 1)
