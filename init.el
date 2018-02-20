(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t)
 '(cua-mode t nil (cua-base))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))

;; Remove startup screen
;;
(setq inhibit-startup-screen t)

;;  Init UI Themes load path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Load the zenburn theme
;; (load-theme 'zenburn t)

;; Load the monokai theme
(load-theme 'monokai t)


;; Turn on Text Mode and auto-fill mode automatically
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; when using make-frame (C-x 5 2), use other position
(setq initial-frame-alist '((top .10) (left .30)
			    (width .90) (height .50)))
(setq default-frame-alist '((width .80) (height .45)))
