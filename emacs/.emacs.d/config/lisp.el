;;; LISP stuff.
;;;
;;; Gabriele Rastello

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package slime-company
  :ensure t
  :init
    (require 'company)
    (slime-setup '(slime-fancy slime-company)))

(use-package paredit
  :ensure t
  :config
  (add-hook 'lisp-mode-hook (lambda () (paredit-mode 1)))
  (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode))))
