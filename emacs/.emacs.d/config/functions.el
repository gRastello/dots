;; Useful functions.
;;
;; Gabriele Rastello.

(defun better-compile ()
  "Travel up the path until a Makefile is found and the use it to."
  (interactive)
  (when (locate-dominating-file default-directory "Makefile")
  (with-temp-buffer
    (cd (locate-dominating-file default-directory "Makefile"))
    (compile "make -k"))))

(global-set-key (kbd "C-c m") #'better-compile)

(setq mount-devices-mask "^sd[b-z][0-9]")
(setq mount-default-mount-point "/mnt")

(defun mount ()
  "Mount a device to `mnt`."
  (interactive)
  (let* ((all-drives (directory-files "/dev" t mount-devices-mask))
	 (drive (completing-read "Select drive to mount:" all-drives))
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
