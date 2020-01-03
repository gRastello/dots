;;; Dired and filesystem stuff.
;;;
;;; Gabriele Rastello

(require 'dired)

;; aesthetic choices
(setq dired-listing-switches "-lh")
(add-hook 'dired-mode-hook (lambda ()
                             (hl-line-mode 1)
                             (toggle-truncate-lines)
                             (show-paren-mode 0)))

;; toggle hidden files
(defun dired-toggle-hidden-files ()
  "toggle hidden files visibility"
  (interactive)

  ;; set ls switches accordigly
  (if (string= dired-listing-switches "-lh")
      (setq dired-listing-switches "-lha")
    (setq dired-listing-switches "-lh"))

  ;; kill current buffer and re-open the directory
  ;; will probably if current dired instance is listing more
  ;; than a single directory
  (let ((current-directory dired-directory))
    (kill-buffer (current-buffer))
    (dired current-directory)))

(define-key dired-mode-map (kbd ".") #'dired-toggle-hidden-files)

;; open the file at point in an external program
(setq dired-ext-open-alist '(("mkv"  . "mpv")
                             ("mp4"  . "mpv")
                             ("webm" . "mpv")
			     ("exe"  . "wine")
                             ("djvu" . "evince")
			     ("pdf"  . "evince")))

(defun dired-xdg-open ()
  "open the file at point with `xdg-open'"
  (interactive)
  (let* ((filename  (file-truename (dired-get-file-for-visit)))
         (extension (file-name-extension filename))
         (program   (cdr (assoc-string extension dired-ext-open-alist))))
    (if program
        (start-process "dired xdg open" nil program filename)
      (dired-find-file))))

(define-key dired-mode-map (kbd "C-m") #'dired-xdg-open)

;; Mount command.
(setq mount-devices-mask "^sd[b-z][0-9]")
(setq mount-default-mount-point "/mnt")

(defun mount ()
  "Mount a device to `mnt`."
  (interactive)
  (let* ((all-drives (directory-files "/dev" t mount-devices-mask))
	 (drive (ivy-read "Select drive to mount:" all-drives))
	 (password (read-passwd "sudo: "))
	 (process (start-process "mount" "*mount*"
				 "sudo" "mount" drive mount-default-mount-point)))
    (process-send-string process password)
    (process-send-string process "\n")
    (process-send-eof process)))

(defun umount ()
  "Umount whatever is mounted at `mnt`."
  (interactive)
  (let ((password (read-passwd "sudo: "))
	(process (start-process "umount" "*umount*"
				"sudo" "umount" mount-default-mount-point)))
    (process-send-string process password)
    (process-send-string process "\n")
    (process-send-eof)))

