;;; ORG-mode config.
;;;
;;; Gabriele Rastello

(require 'org)

(setq org-directory "~/org")

(add-hook 'org-mode-hook '(lambda ()
                            (visual-line-mode 1)
                            (flyspell-mode)))

;; Global keybindings.
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Org-files
(setq org-default-inbox-file    "~/org/Inbox.org"
      org-default-incubate-file "~/org/Incubate.org"
      org-default-tasks-file    "~/org/Tasks.org"
      org-default-media-file    "~/org/Media.org"
      org-default-notes-file    "~/org/Notes.org")

(setq org-agenda-files (list org-default-tasks-file))

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
			   (org-default-notes-file :level . 1)
                           (org-default-tasks-file :level . 0)))
;; Agenda options
(setq org-agenda-skip-scheduled-if-done t
      org-agenda-todo-ignore-scheduled  'future)

;; Gnuplot for plotting som tables.
(use-package gnuplot
  :ensure t)

;; Mark the entry at point as TODO and set a deadline for tomorrow.
(defun org-todo-and-tomorrow ()
  (interactive)
  (org-todo)
  (org-deadline t "+1d"))

(define-key org-mode-map (kbd "C-c t") #'org-todo-and-tomorrow)
