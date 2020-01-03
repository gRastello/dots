;;; Quality of life improvements
;;;
;;; Gabriele Rastello

;;; Fix the most annoying thing about Emacs.
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Better buffer killing.
(defun murder-current-buffer ()
  "directly kill the current buffer"
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") #'murder-current-buffer)

;;; Better frame splitting.
(defun split-and-follow-horizontally ()
  "split current window horizontally and move the cursor in it"
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-and-follow-vertically ()
  "split current window vertically and move the cursor in it"
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;;; Backups
(setq backup-directory-alist '(("." . "~/emacs-backups"))
      backup-by-copying      t
      delete-old-versions    t
      kept-new-versions      10
      kept-old-versions      3)

