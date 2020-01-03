;;; UI settings
;;;
;;; Gabriele Rastello

;;; UI elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 0)

(setq scroll-conservatively 100)
(setq inhibit-startup-message t)
(setq ringbell-function 'ignore)

;;; Mode-line.
(setq-default mode-line-format
              (list
               ;; Front stuff.
               "%e"
               mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote

	       ;; Version control.
	       '(vc-mode vc-mode)

               ;; Frame and buffer.
               mode-line-frame-identification
               mode-line-buffer-identification

               ;; Position in the Buffer.
               mode-line-position

               ;; Major mode.
               mode-line-modes

               ;; Date, time and battery
               mode-line-misc-info

               ;; Final spaces.
               mode-line-end-spaces
               ))


(column-number-mode 1)

(setq display-time-format "%F %H:%M ")
(setq display-time-default-load-average nil)
(display-time-mode 1)

(display-battery-mode 1)

