;; init.el - tjbw

(setq debug-on-error t)
;; Start Emacs in server mode
(load "server")
(unless (server-running-p) (server-start))
(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 4 1024 1024))
(setq frame-resize-pixelwise t)
;; set browser to emacs default
(setq browse-url-browser-function 'browse-url-default-browser)

;; Begin package installation and configuarion
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))
 
;;(load-theme 'doom-spacegrey t)
;;(load-theme 'doom-xcode t)

;; (use-package timu-macos-theme
;;   :ensure t
;;   :config
;;   (customize-set-variable 'timu-macos-flavour "light")
;;   (load-theme 'timu-macos t))

(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'latte) ;; or 'latte, 'macchiato, or 'mocha
(catppuccin-reload))

;; Set some built-in minor modes
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(show-paren-mode t)
(prettify-symbols-mode)
;;(fringe-mode 0)

(setq backup-directory-alist `(("." . "~/.emacs.d/.backups"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200
      kept-new-versions 10
      kept-old-versions 2)

;; Always split windows vertically
; (setq
; split-width-threshold 0
; tsplit-height-threshold nil) 

;; Set sensible margins
(setq left-margin-width 1)
(setq right-margin-width 1)
(set-window-buffer (selected-window) (current-buffer))
(set-window-buffer nil (current-buffer))



(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

(setq ring-bell-function 'ignore)
(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      tab-bar-show nil)
(setq dired-kill-when-opening-new-dired-buffer t)
(setq diredp-hide-details-initially-flag nil)
(setq switch-to-buffer-obey-display-actions t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq scroll-margin 5 scroll-conservatively 9999 scroll-step 5)
(setq scroll-conservatively 10000)
(setq highlight-indentation-mode nil)
(setq find-name-arg "-iname")
;;(setq comint-prompt-read-only t)
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format "   (%s/%s)")
(setq backup-directory-alist '(("." . "~/.emacs.d/Backups")))
(setq backup-by-copying t)
(setq user-full-name '"Timothy Welch")
(setq user-mail-address '"t.welch2@exeter.ac.uk")
(setq gnus-select-method '(nntp "news.eternal-september.org"))
(setq dired-listing-switches "-lahF")
;;(setq dired-listing-switches "-lahF --group-directories-first")
;;(setq dired-listing-switches "-lahFog --group-directories-first")
(setq dired-dwim-target t)
(setq-default indent-tabs-mode nil)
(setq toggle-diredp-find-file-reuse-dir t)
(fset 'yes-or-no-p 'y-or-n-p)

;; Set custom key bindings
(global-set-key (kbd "C-x C-x") 'avy-goto-char-timer)
(global-set-key (kbd "C-c f") 'tw/dired-filter-files)
(global-set-key (kbd "C-c b") 'ivy-switch-buffer-other-window)
(global-set-key (kbd "C-c d s") 'dired-mark-files-regexp)
(global-set-key (kbd "C-c c c") 'org-capture)
;(global-set-key (kbd "C-c C-d C-n") 'denote-create-note)
(global-set-key (kbd "C-c C-d C-s") 'consult-notes)
;; (global-set-key (kbd "C-c d d") 'eyebrowse-switch-to-window-config)
;;(global-set-key (kbd "C-c d d") 'tab-switch)
;;(global-set-key (kbd "C-c c d") 'consult-bookmark)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c y") 'clipboard-yank)
(global-set-key (kbd "C-c c w") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-x C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-x a") 'org-agenda)
(global-set-key (kbd "C-x k") 'kill-buffer)
(global-set-key (kbd "M-n") 'scroll-up-command)
(global-set-key (kbd "M-p") 'scroll-down-command)
(global-set-key (kbd "M-,") 'beginning-of-buffer)
(global-set-key (kbd "M-.") 'end-of-buffer)
(global-set-key (kbd "M-g M-g") 'goto-line)
(global-set-key (kbd "C-c C-f") 'find-name-dired)
(global-set-key (kbd "C-c C-o") 'browse-url-of-dired-file)
(global-set-key (kbd "C-x r e") 'eval-region)
(global-set-key (kbd "C-c C-l") 'package-list-packages)
(global-set-key (kbd "C-c h") 'dired-dotfiles-toggle)
(global-set-key (kbd "C-c y") 'popup-kill-ring)
(global-set-key (kbd "C-c w") 'make-frame)
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch-backward)
(global-set-key (kbd "C-x w t") 'tw/toggle-window-dedication)
(global-set-key (kbd "C-<return>") (lambda () (interactive) (tw/smart-open-line-below)))
(global-set-key (kbd "M-RET") 'tw/smart-open-line-above)
(global-set-key (kbd "C-c m") 'tw/set-margins)
;;(global-set-key (kbd "C-c s s") 'consult-notes)
;;(global-set-key (kbd "C-c l") 'linum-mode)
;;(global-set-key (kbd "C-x n") 'tab-next)
;;(global-set-key (kbd "C-x <right>") 'windmove-right)
;;(global-set-key (kbd "C-x <left>"q) 'windmove-left)
;;(global-set-key (kbd "C-x <up>")    'windmove-up)
;;(global-set-key (kbd "C-x <down>")  'windmove-down)
;;(global-set-key (kbd "C-x l") 'windmove-right)
;;(global-set-key (kbd "C-x h") 'windmove-left)
;;(global-set-key (kbd "C-x k") 'windmove-up)
;;(global-set-key (kbd "C-x j") 'windmove-down)
 
;; Hook some modes
(add-hook 'dired-mode-hook 'auto-revert-mode) ;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "w") #'tw/dired-find-file-other-application)))
(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "o") #'tw/dired-find-file-other-window)))
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
;;(add-hook 'prog-mode-hook (setq display-line-numbers 'absolute)'display-line-numbers-mode)
(add-hook 'elfeed-mode-hook (lambda () (local-set-key (kbd "g") #'elfeed-update)))
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))
(add-hook 'org-agenda-mode 'hl-line-mode)
(add-hook 'org-mode 'hl-line-mode)

;;(makunbound display-line-numbers)

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (load-theme 'doom-nord t)
;;   (setq custom-safe-themes t)
;;   (set-face-attribute 'default nil :family "Ubuntu Mono" :height 100)
;;   (set-face-attribute 'mode-line nil :family "Ubuntu Mono" :height 100)

(use-package swiper
  :ensure t)

(use-package olivetti
  :ensure t)

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode))

(use-package denote
  :ensure t
  :custom
  (denote-directory "~/Dropbox/Denote"))

(use-package helpful
  :ensure t)

;; (use-package god-mode
;;   :ensure t
;;   :config
;;   (god-mode)
;;   (global-set-key (kbd "C-<escape>") 'god-local-mode))

(use-package ace-window
  :ensure t
  :bind
  (("C-x C-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?q ?w ?z ?x))
  (setq aw-ignore-current t)
  (custom-set-faces
 '(aw-leading-char-face
   ((t (:inherit ace-jump-face-foreground :height 3.0))))))

(use-package activities
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  :bind
  (("C-c C-a C-n" . activities-new)
   ("C-c C-a C-d" . activities-define)
   ("C-c C-a C-a" . activities-resume)
   ("C-c C-a C-s" . activities-suspend)
   ("C-c C-a C-k" . activities-kill)
;;   ("C-c C-a RET" . activities-switch)
   ("C-c C-a b" . activities-switch-buffer)
   ("C-c d d"  . activities-switch)
   ("C-c C-a g" . activities-revert)
   ("C-c C-a l" . activities-list)))

(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t)
  :config
  (ivy-mode t))

(use-package ivy-posframe
  :ensure t
  :config
  ;; display at `ivy-posframe-style'
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  (setq ivy-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))
  (ivy-posframe-mode 1))

(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package which-key
  :ensure t
  :config
  (setq which-key-popup-type 'minibuffer)
  (which-key-mode t))

(use-package winner
  :ensure t)

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))



;;(use-package org-side-tree
;;  :ensure t)

(load-file "~/.emacs.d/org-mode-config.el")

(use-package denote
  :ensure t
  :custom
  (denote-directory "~/Dropbox/Org/Denote/")
;;  (denote-rename-buffer-function 'dw/denote-rename-buffer-with-prefixed-title)
  :config
  (denote-rename-buffer-mode)
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  (require 'denote-org-dblock)

  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 '("n" "New note (with Denote)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured nil))))

(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.25)))) ;
   '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
  (setq org-superstar-leading-bullet ?\s)
  (org-superstar-mode t))

(use-package smartparens
  :ensure t  ;; install the package
  :hook (prog-mode text-mode markdown-mode org-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  :config
  (smartparens-global-mode))

;; (use-package org-notify
;;   :ensure t
;;   :config
;;   (org-notify-start))

(use-package nerd-icons
  :ensure t)
;; 
;; (use-package nerd-icons-ibuffer
;;   :ensure t
;;   :config
;;   (nerd-icons-ibuffer-mode t))

;; (use-package all-the-icons-ibuffer
;;   :ensure t
;;   :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

;; (setq ibuffer-saved-filter-groups
;;       (quote (("default"
;;                ("Dired" (mode . dired-mode))
;;                ("Org" (mode . org-mode))
;;                ("System" (name . "^\*.*\*$"))))))
;; (add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :config
  (set-face-attribute 'region nil :background "#666")
  (delete-selection-mode 1))

(use-package helpful
  :ensure t)

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode t))

(use-package popup-kill-ring
  :ensure t)

(use-package vterm
  :config
  (defun turn-off-chrome ()
    (global-hl-line-mode -1))
  :hook
  (vterm-mode . turn-off-chrome)
  (tw/toggle-window-dedication))

(use-package multi-vterm
  :ensure t)

(use-package paredit
  :ensure t
  :config
  (add-hook 'ielm-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'paredit-mode)
  (define-key paredit-mode-map (kbd "RET") nil)
  (define-key paredit-mode-map (kbd "C-j") 'paredit-newline))
;  (add-hook 'ielm-mode-hook 'g-ielm-init-history))

;;(load (expand-file-name "~/.quicklisp/slime-helper.el"))
;;(setq inferior-lisp-program "sbcl")

;; (defun tw/dired-ediff-marked-files ()
;;   "Run ediff-files on a pair of files marked in dired buffer"
;;   (interactive)
;;   (let ((marked-files (dired-get-marked-files nil)))
;;     (if (not (= (length marked-files) 2))
;;         (message "mark exactly 2 files")
;;       (ediff-files (nth 0 marked-files)
;;                    (nth 1 marked-files)))))

(defun tw/set-margins ()
(interactive)
(setq left-margin-width 1)
(setq right-margin-width 1)
(set-window-buffer (selected-window) (current-buffer))
(set-window-buffer nil (current-buffer)))


(defun tw/dired-find-file-other-application ()
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)
    (message "Opening %s done" file)))

(defun tw/dired-find-file-other-window ()
  "Open the file in a vertical split to the right."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (select-window (split-window-right))
    (find-file file)))

(defun tw/dired-filter-files (string)
  (interactive "Filter Dired for files containing string: ")
  (dired-mark-files-regexp string)
  (dired-toggle-marks)
  (dired-do-kill-lines))

(defun tw/toggle-window-dedication ()
  (interactive)
  (set-window-dedicated-p (selected-window)
                          (not (window-dedicated-p (selected-window)))))

(defun tw/smart-open-line-above ()
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun tw/smart-open-line-below ()
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

;; Dired custom functions and configuration

(defun tw/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

 
(defun dired-dotfiles-toggle ()
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
	(progn 
	  (set (make-local-variable 'dired-dotfiles-show-p) nil)
	  (message "h")
	  (dired-mark-files-regexp "^\\\.")
	  (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
	     (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(defun elfeed-mark-all-as-read ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" default))
 '(package-selected-packages
   '(timu-macos-theme selected-window-accent-mode pdf-tools magit true malyon org-side-tree all-the-icons-ibuffer listen dashboard zygospore yequake which-key visual-fill-column vertico ts telephone-line taxy sr-speedbar smartparens slime sicp ripgrep rainbow-delimiters quelpa-use-package popup-kill-ring pkg-info perspective peg paredit page-break-lines ov org-roam org-bullets openwith nerd-icons-ibuffer nerd-icons-dired multi-vterm marginalia ivy-posframe ivy-explorer imenu-anywhere ht helpful golden-ratio eyebrowse exwm expand-region erc-hl-nicks erc-colorize erc equake eat dracula-theme doom-themes doom-modeline dirvish dired-single dired+ dimmer denote-menu counsel consult-notes company-box auto-dim-other-buffers activities ace-window 0blayout)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
 '(hl-line ((t (:extend t :background "gray27"))))
 '(org-agenda-done ((t (:foreground "gray42"))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.25))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
