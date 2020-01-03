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
