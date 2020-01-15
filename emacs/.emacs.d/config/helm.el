;;; Helm stuff
;;;
;;; Gabriele Rastello

(use-package helm
  :ensure t
  :bind
  ("C-x C-f" . 'helm-find-files)
  ("C-x C-b" . 'helm-buffers-list)
  ("C-x b"   . 'helm-buffers-list)
  ("M-x"     . 'helm-M-x)
  ("C-s"     . 'helm-occur)
  ("M-y"     . 'helm-show-kill-ring)

  ("C-c h" . 'helm-command-prefix)
  :config
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)

  (setq helm-autoresize-max-height 0
        helm-autoresize-min-height 30
	helm-split-window-in-side-p t

	helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match t
	helm-mode-fuzzy-match t
	helm-semantic-fuzzy-match t
	helm-imenu-fuzzy-match t
	helm-completion-in-region-fuzzy-match t
	
        helm-move-to-line-cycle-in-source nil)
  (setq helm-external-programs-associations '(("pdf"  . "evince")
					      ("djvu" . "evince")))
  :init
  (helm-mode 1))

(semantic-mode 1)
(require 'helm-config)    
(helm-autoresize-mode 1)
