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

  ;; Youtube streaming and downloading.
  (defun elfeed-youtube-stream ()
    (interactive)
    (let* ((entry (elfeed-search-selected :single))
	   (link  (elfeed-entry-link entry)))
      (start-process "elfeed youtube streaming" nil "mpv" link)))

  (defun elfeed-youtube-download ()
    (interactive)
    (let* ((entry (elfeed-search-selected :single))
	   (link  (elfeed-entry-link entry)))
      (youtube-dl link)))
  
  ;; Keybindings.
  (global-set-key (kbd "C-c f") (lambda () (interactive)
				  (progn (elfeed)
					 (elfeed-update))))
  (define-key elfeed-search-mode-map "y" #'elfeed-youtube-stream)
  (define-key elfeed-search-mode-map "d" #'elfeed-youtube-download))

(defun youtube-dl (url)
  (let* ((dir (read-directory-name "Download to: "))
	 (type (completing-read "Format: " '("music" "video"))))
    (start-process "youtube-dl" "*youtube-dl*" "youtube-dl"
		   (if (string-equal type "music") "-xic" "")
		   "-o" (concat dir "%(title)s.%(ext)s")
		   url)))
