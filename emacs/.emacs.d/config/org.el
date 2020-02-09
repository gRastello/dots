;;; ORG-mode config.
;;;
;;; Gabriele Rastello

(require 'org)

(setq org-directory "~/org")

(add-hook 'org-mode-hook '(lambda ()
                            (visual-line-mode 1)
                            (flyspell-mode)))

;; Global keybindings.
(global-set-key (kbd "C-c C-a") 'org-agenda)
(global-set-key (kbd "C-c a")   #'(lambda ()
				    (interactive)
				    (org-agenda nil "a")
				    (org-agenda-day-view)))
(global-set-key (kbd "C-c c") 'org-capture)

;; Org-files
(setq org-default-inbox-file    "~/org/Inbox.org"
      org-default-incubate-file "~/org/Incubate.org"
      org-default-tasks-file    "~/org/Tasks.org"
      org-default-martin-file   "~/org/Martin.org")

(setq org-agenda-files (list org-default-tasks-file
			     org-default-martin-file))

;; TODO options.
(setq org-log-done 'time)

;; Capture templates.
(setq org-capture-templates `(("i" "Inbox entry"        entry
			       (file ,org-default-inbox-file)
			       "* %?\n%i"
			       :empty-lines 1)))

;; Refiling.
(setq org-refile-use-outline-path        'file
      org-outline-path-complete-in-steps nil)

(setq org-refile-targets '((org-default-incubate-file :level . 0)
                           (org-default-tasks-file :level . 0)))
;; Agenda options
(setq org-agenda-skip-scheduled-if-done t
      org-agenda-todo-ignore-scheduled  'future)

;; Gnuplot for plotting some tables.
(use-package gnuplot
  :ensure t)

(defun refile-to (file heading)
  "Refile current heading to `file` `header`."
  (let ((pos (save-excursion
	       (find-file file)
	       (org-find-exact-headline-in-buffer heading))))
    (org-refile nil nil (list heading file nil pos))))

(defun org-todo-and-tomorrow ()
  "Mark the entry at point as TODO, set a deadline for tomorrow and move it to the Task.org file."
  (interactive)
  (org-mark-ring-push)
  (org-todo)
  (org-deadline t "+1d")
  (refile-to org-default-tasks-file "Tasks")
  (org-mark-ring-goto))

(define-key org-mode-map (kbd "C-c t") #'org-todo-and-tomorrow)
