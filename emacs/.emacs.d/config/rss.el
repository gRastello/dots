;;; RSS feeds
;;;
;;; Gabriele Rastello

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds *feeds*)

  ;; Tags
  (add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "youtube\\.com"
                              :add '(youtube)))

  ;; Youtube streaming.
  (defun elfeed-youtube-stream ()
    (interactive)
    
    (let* ((entry (elfeed-search-selected :single))
	   (link  (elfeed-entry-link entry)))
	(start-process "elfeed youtube streaming" nil "mpv" link)))

  ;; Keybindings.
  (global-set-key (kbd "C-c f") (lambda () (interactive)
				  (progn (elfeed)
					 (elfeed-update))))
  (define-key elfeed-search-mode-map "y" #'elfeed-youtube-stream))
