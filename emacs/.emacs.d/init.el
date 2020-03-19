;;; Basic repositories.
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("elpa"  . "http://tromey.com/elpa/")
			 ("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)

;;; Secure the `use-package` macro.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load theme and config.
(load-theme 'Witchmacs t)
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
