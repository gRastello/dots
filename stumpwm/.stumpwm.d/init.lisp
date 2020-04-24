;; -*-lisp-*-

(in-package :stumpwm)

(set-prefix-key (kbd "C-q"))

;; Visual stuff.
(setf *message-window-gravity* :center
      *input-window-gravity*   :center
      *startup-message*        nil
      *window-format*          "%n %s %c"
      *window-border-style*    :thin)

(set-focus-color "violet")

(setf *mode-line-border-color*     "black"
      *mode-line-background-color* "black"
      *mode-line-foreground-color* "white")

;; Modeline
(load-module "battery-portable")

(setf stumpwm:*screen-mode-line-format*
      (list "BAT %B | %d"))

(define-key *root-map* (kbd "b") "mode-line")

;; Saves some time.
(setf *timeout-wait* 2)

;; Brightness control.
(defcommand brightness-up () ()
  "Raises brightness."
  (run-shell-command "light -A 5"))

(defcommand brightness-down () ()
  "Decreases brightness."
  (run-shell-command "light -U 5"))

(define-key *root-map* (kbd "Up") "brightness-up")
(define-key *root-map* (kbd "Down") "brightness-down")

;; Volume control
(defcommand volume-up () ()
  "Raises volume."
  (run-shell-command "pactl set-sink-volume @DEFAULT_SINK@ +10%"))

(defcommand volume-down () ()
  "Decreases volume."
  (run-shell-command "pactl set-sink-volume @DEFAULT_SINK@ -10%"))

(define-key *root-map* (kbd "Right") "volume-up")
(define-key *root-map* (kbd "Left")  "volume-down")

;; Autostart.
(run-shell-command "setxkbmap -option ctrl:nocaps -option compose:menu")
(run-shell-command "transmission-daemon")
(run-shell-command "xsetroot -cursor_name left_ptr")

;; Update colors just in case.
(update-color-map (current-screen))
