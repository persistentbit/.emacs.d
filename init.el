;;
;; PACKAGE MANAGEMENT
;;
(require 'package)

(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; Add marmalade to package repos
(setq package-archives nil)
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)

(package-initialize)

(unless (and (file-exists-p "~/.emacs.d/elpa/archives/gnu")
             (file-exists-p "~/.emacs.d/elpa/archives/melpa")
             (file-exists-p "~/.emacs.d/elpa/archives/melpa-stable"))
  (package-refresh-contents))

(defun packages-install (&rest packages)
  (message "running packages-install")
  (mapc (lambda (package)
          (let ((name (car package))
                (repo (cdr package)))
            (when (not (package-installed-p name))
              (let ((package-archives (list repo)))
                (package-initialize)
                (package-install name)))))
        packages)
  (package-initialize)
  (delete-other-windows))

;; Install extensions if they're missing
(defun init--install-packages ()
  (message "Lets install some packages")
  (packages-install
   ;; Since use-package this is the only entry here
   ;; ALWAYS try to use use-package!
   (cons 'use-package melpa)
   ))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))


;;
;;  BOOKMARKS
;;
(use-package bm
  :ensure t
  :bind (("C-c =" . bm-toggle)
         ("C-c [" . bm-previous)
         ("C-c ]" . bm-next)))
(use-package counsel :ensure t)

(use-package swiper
  :ensure t
  :bind*
  (("C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("M-a" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c h f" . counsel-describe-function)
   ("C-c h v" . counsel-describe-variable)
   ("C-c i u" . counsel-unicode-char)
   ("M-i" . counsel-imenu)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-c l" . scounsel-locate))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (define-key read-expression-map (kbd "C-r") #'counsel-expression-history)
    (ivy-set-actions
     'counsel-find-file
     '(("d" (lambda (x) (delete-file (expand-file-name x)))
        "delete"
        )))
    (ivy-set-actions
     'ivy-switch-buffer
     '(("k"
        (lambda (x)
          (kill-buffer x)
          (ivy--reset-state ivy-last))
        "kill")
       ("j"
        ivy--switch-buffer-other-window-action
        "other window")))))


;;
;; ENVIRONMENT
;;

(if (or
     (eq system-type 'darwin)
     (eq system-type 'berkeley-unix))
    (setq system-name (car (split-string system-name "\\."))))

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(push "/usr/local/bin" exec-path)

;; /usr/libexec/java_home
;;(setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/jdk1.8.0_05.jdk/Contents/Home")


;;
;;  IVY MODE
;;
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (defun couns-git ()
    "Find file in the current Git repository."
    (interactive)
    (let* ((default-directory (locate-dominating-file
                               default-directory ".git"))
           (cands (split-string
                   (shell-command-to-string
                    "git ls-files --full-name --")
                   "\n"))
           (file (ivy-read "Find file: " cands)))
      (when file
        (find-file file))))
  :bind ("M-o" . couns-git)
  )

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t)









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
    (ace-jump-mode emmet-mode less-css-mode web-mode clj-refactor rainbow-delimiters highlight-parentheses paredit-everywhere paredit cider slime macrostep elisp-slime-nav org-bullets which-key ace-window dracula-theme projectile use-package magit)))
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

;;
(global-prettify-symbols-mode 1)



;; Turn on Text Mode and auto-fill mode automatically
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; when using make-frame (C-x 5 2), use other position
;;(setq initial-frame-alist '((top .10) (left .30)
;;
;;(setq default-frame-alist '((width .80) (height .45)))


;; Remember cursor position...
;;(save-place-mode 1)


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

(use-package ace-jump-mode
  :ensure t
  :config
  (define-key global-map (kbd "C-c SPC") 'ace-jump-mode))



;;  frames
(setq default-frame-alist
      '(
        (width . 80) (height . 53)
        
        ))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (java . t)
   (python . t)
   (sql . t)
   (js . t)
   (haskell .t)
   (plantuml .t)
   (sh .t)
   ))

(setf org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)

;;
;; Recent Files
;;

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Enable to autosave recent files every 5 minutes
;;(run-at-time nil (* 5 60) 'recentf-save-list)

;;
;; Recent files auto complete: Bound to M-<f1>
;;
(defun recentf-open-files-compl ()
      (interactive)
      (let* ((all-files recentf-list)
        (tocpl (mapcar (function 
           (lambda (x) (cons (file-name-nondirectory x) x))) all-files))
        (prompt (append '("File name: ") tocpl))
        (fname (completing-read (car prompt) (cdr prompt) nil nil)))
        (find-file (cdr (assoc-ignore-representation fname tocpl)))))
(global-set-key (kbd "<f1>") 'recentf-open-files-compl)

;;
;; which-key
;;

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;
;; magit GIT
;;
;; Magit is an Emacs interface to Git.
;; (It's awesome)
;; https://github.com/magit/magit

(use-package magit
  :commands magit-get-top-dir
  :bind (("C-c g" . magit-status)
         ("C-c C-g l" . magit-file-log)
         ("C-c f" . magit-grep))
  :init
  (progn
    ;; magit extensions
    (use-package magit-blame
      :bind ("C-c C-g b" . magit-blame-mode))

    ;; we no longer need vc-git
    (delete 'Git vc-handled-backends)
    ;; make magit status go full-screen but remember previous window
    ;; settings
    ;; from: http://whattheemacsd.com/setup-magit.el-01.html
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    ;; Close popup when commiting - this stops the commit window
    ;; hanging around
    ;; From: http://git.io/rPBE0Q
    (defadvice git-commit-commit (after delete-window activate)
      (delete-window))

    (defadvice git-commit-abort (after delete-window activate)
      (delete-window))

    ;; these two force a new line to be inserted into a commit window,
    ;; which stops the invalid style showing up.
    ;; From: http://git.io/rPBE0Q
    (defun magit-commit-mode-init ()
      (when (looking-at "\n")
        (open-line 1)))

    (add-hook 'git-commit-mode-hook 'magit-commit-mode-init))
  :config
  (progn
    ;; restore previously hidden windows
    (defadvice magit-quit-window (around magit-restore-screen activate)
      (let ((current-mode major-mode))
        ad-do-it
        ;; we only want to jump to register when the last seen buffer
        ;; was a magit-status buffer.
        (when (eq 'magit-status-mode current-mode)
          (jump-to-register :magit-fullscreen))))

    (defun magit-maybe-commit (&optional show-options)
      "Runs magit-commit unless prefix is passed"
      (interactive "P")
      (if show-options
          (magit-key-mode-popup-committing)
        (magit-commit)))

    (define-key magit-mode-map "c" 'magit-maybe-commit)

    ;; major mode for editing `git rebase -i` files
    (use-package rebase-mode)

    ;; magit settings
    (setq
     ;; use ido to look for branches
     magit-completing-read-function 'magit-ido-completing-read
     ;; don't put "origin-" in front of new branch names by default
     magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
     ;; open magit status in same window as current buffer
     magit-status-buffer-switch-function 'switch-to-buffer
     ;; highlight word/letter changes in hunk diffs
     magit-diff-refine-hunk t
     ;; ask me if I want to include a revision when rewriting
     magit-rewrite-inclusive 'ask
     ;; ask me to save buffers
     magit-save-some-buffers t
     ;; pop the process buffer if we're taking a while to complete
     magit-process-popup-time 10
     ;; ask me if I want a tracking upstream
     magit-set-upstream-on-push 'askifnotset
     )))

(provide 'init-magit)


;;
;; org-bullets
;;
(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;
;; set org indent mode
(setq org-startup-indented t)

;;
;; Set up slime
;;
;;(use-package slime
;;  :ensure t
;;  :init(add-hook 'lisp-mode-hook(lambda () (slime-mode t)))
;;  )

;;
;;  AUTO COMPLETE
;;
(use-package company
  :ensure t
  :bind (("C-c /". company-complete))
  :config
  (global-company-mode)
  )


;;
;; Set up paredit
;;
(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  )

;; Ensure paredit is used EVERYWHERE!
(use-package paredit-everywhere
  :ensure t
  :diminish paredit-everywhere-mode
  :config
  (add-hook 'prog-mode-hook #'paredit-everywhere-mode))

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda()
              (highlight-parentheses-mode)
              )))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'lisp-mode-hook
            (lambda()
              (rainbow-delimiters-mode)
              )))

(global-highlight-parentheses-mode)



;; set up closure
;;
(use-package cider
  :ensure t
  :pin melpa-stable

  :config
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook #'cider-hydra-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-display-help-banner nil)
  (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

  :bind (("M-r" . cider-namespace-refresh)
         ("C-c r" . cider-repl-reset)
         ("C-c ." . cider-reset-test-run-tests))
  )

(use-package clj-refactor
  :ensure t
  :config
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 ;; insert keybinding setup here
                                 ))
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (setq cljr-warn-on-evaql nil)
  :bind ("C-c '" . hydra-cljr-help-menu/body)
)

;;
;;   WEB Editing
;;

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.xhtml?\\'" . web-mode))

  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2))

  (add-hook 'web-mode-hook  'my-web-mode-hook))

(use-package less-css-mode
  :ensure t)

(use-package emmet-mode
  :ensure t)

;;
;;  C-k kills and put in buffer
;;
(setq kill-whole-line t)
