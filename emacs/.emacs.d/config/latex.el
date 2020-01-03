;;; Latex configs
;;;
;;; Gabriele Rastello

(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'yas-minor-mode))

;; Flymake latex.
(require 'flymake)

(defun flymake-get-tex-args (file-name)
  (list "pdflatex"
	(list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

(add-hook 'LaTeX-mode-hook 'flymake-mode)
