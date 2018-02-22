;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
;;(require 'diminish)
(require 'bind-key)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 '(package-selected-packages
   (quote
    (ace-window dracula-theme projectile use-package magit)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
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


;; Remember cursor position...
(save-place-mode 1)


;;
;;Lookup in WikiPedia
;;
;;
(require 'browse-url) ; part of gnu-emacs

(defun my-lookup-wikipedia()
  "Lookup the word under the cursor in wikipedia.
  If there is a text selection (a phrase), use that.

This command switches to browser."
  (interactive)
  (let (word)
    (setq word
	  (if (use-region-p)
	      (buffer-substring-no-properties (region-beginning) (region-end))
	    (current-word)))
    (setq word (replace-regexp-in-string " " "_" word))
    (browse-url (concat "http://en.wikipedia.org/wiki/" word))
    ;; (eww myUrl) ; emacs's own browser
    ))

;;
;; Projectile settings
;;
(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)

;;
;; reformat xml
;;

(require 'sgml-mode)

(defun my/reformat-xml ()
  (interactive)
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))
;;
;; ace-window 
;; Set the swith key to M-o and make the home row the access keys
;;(global-set-key (kbd "M-o") 'ace-window)
;;(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(use-package ace-window
  :ensure t
  :defer t
  :init
  (progn
    (global-set-key (kbd "M-o") 'ace-window)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
     ;;more info at https://github.com/abo-abo/ace-window
    )
  )
