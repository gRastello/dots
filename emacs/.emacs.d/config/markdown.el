;;; Markdown stuff
;;;
;;; Gabriele Rastello

(use-package markdown-mode
  :ensure t
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'"       . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  (add-hook 'markdown-mode-hook (lambda ()
                                  (flyspell-mode)
                                  (visual-line-mode 1))))
