;; init-macbook.el / called from init.el 2024-September-10

;; Look and feel
;; (use-package cloud-theme
;;   :ensure t)
;; (load-theme 'cloud)

;; (use-package catppuccin-theme
;;   :ensure t
;;   :config
;;   (setq catppuccin-flavor 'latte) ;; or 'latte, 'macchiato, or 'mocha, 'frappe
;;   (catppuccin-reload))

;; (use-package timu-macos-theme
;;   :ensure t)
;; (load-theme 'timu-macos)

  (custom-set-faces
   '(default ((t (:height 125 :family "Iosevka" :foundry "nil"
                          :slant normal :weight medium :width normal)))))
(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-italic-constructs t)

  (global-set-key (kbd "C-c l l")
		  (lambda ()
		    (interactive)
		    (custom-set-faces
		     '(org-agenda-date-today ((t (:weight bold :italic t :foreground "Olive")))))
		    (load-theme 'modus-operandi)))

  (global-set-key (kbd "C-c l d")
		  (lambda ()
		    (interactive)
		    (custom-set-faces
		     '(org-agenda-date-today ((t (:weight bold :italic t :foreground "yellow")))))
		    (load-theme 'modus-vivendi)))
  )

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0) 
(show-paren-mode t)
(prettify-symbols-mode)
(global-prettify-symbols-mode 1)
;; (fringe-mode 0)
;; Always split windows vertically
; (setq split-width-threshold nil)
; tsplit-height-threshold nil) 

;; Set sensible margins
(setq left-margin-width 1)
(setq right-margin-width 1)
(set-window-buffer (selected-window) (current-buffer))
(set-window-buffer nil (current-buffer))
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

;; Some Macos nonsense
(setq mac-command-modifier 'meta)
(define-key key-translation-map (kbd "M-3") (kbd "#"))
(define-key key-translation-map (kbd "M-£") (kbd "#"))
(define-key key-translation-map (kbd "H-3") (kbd "#"))
(define-key key-translation-map (kbd "H-£") (kbd "#"))
(define-key key-translation-map (kbd "S-3") (kbd "#"))
(define-key key-translation-map (kbd "S-£") (kbd "#"))
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
;; (setq backup-directory-alist `(("." . "~/.emacs.d/.backups"))
;;       backup-by-copying t
;;       version-control t
;;       delete-old-versions t
;;       auto-save-default t
;;       auto-save-timeout 20
;;       auto-save-interval 200
;;       kept-new-versions 10
;;       kept-old-versions 2)
;; (setq backup-directory-alist '(("." . "~/.emacs.d/Backups")))
;; (setq backup-by-copying t)
;; (setq make-backup-files t)
;; (setq auto-save-default nil)


(global-set-key (kbd "C-x C-h") 'tw/highlight-line)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c f") 'tw/dired-filter-files)
(global-set-key (kbd "C-c b") 'ivy-switch-buffer-other-window)
(global-set-key (kbd "C-c d s") 'dired-mark-files-regexp)
(global-set-key (kbd "C-c c c") 'org-capture)
;;(global-set-key (kbd "C-c C-d C-s") 'consult-notes)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c y") 'clipboard-yank)
(global-set-key (kbd "C-c c w") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-x C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-x k") 'kill-buffer)
(global-set-key (kbd "M-n") 'scroll-up-command)
(global-set-key (kbd "M-p") 'scroll-down-command)
(global-set-key (kbd "M-,") 'beginning-of-buffer)
(global-set-key (kbd "M-.") 'end-of-buffer)
(global-set-key (kbd "C-x C-g") 'avy-goto-line)
(global-set-key (kbd "C-x C-x") 'avy-goto-char-timer)
(global-set-key (kbd "C-c C-f") 'find-name-dired)
(global-set-key (kbd "C-c C-o") 'browse-url-of-dired-file)
(global-set-key (kbd "C-x r e") 'eval-region)
;(global-set-key (kbd "C-c C-l") 'package-list-packages)
(global-set-key (kbd "C-c h") 'dired-dotfiles-toggle)
(global-set-key (kbd "C-c y") 'popup-kill-ring)
(global-set-key (kbd "C-c w") 'make-frame)
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch-backward)
;(global-set-key (kbd "C-x w t") 'tw/toggle-window-dedication)
(global-set-key (kbd "C-<return>") (lambda () (interactive) (tw/smart-open-line-below)))
(global-set-key (kbd "M-RET") 'tw/smart-open-line-above)
(global-set-key (kbd "C-c t h") 'tw/hide-org-tags)
(global-set-key (kbd "C-c i d") 'tw/insert-current-date)
(global-set-key (kbd "C-x w") 'tw/ivy-switch-to-window-by-buffer)
 
;; Hook some modes
(add-hook 'dired-mode-hook 'auto-revert-mode) ;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "w") #'tw/dired-find-file-other-application)))
(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "o") #'dired-find-file-other-window)))
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'org-agenda-mode 'hl-line-mode)
(add-hook 'org-mode-hook 'hl-line-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)
;;(add-hook 'prog-mode-hook (setq display-line-numbers 'absolute)'display-line-numbers-mode)
(add-hook 'elfeed-mode-hook (lambda () (local-set-key (kbd "g") #'elfeed-update)))
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))

(use-package zygospore
  :ensure t)

(use-package magit
  :ensure t)

(use-package noflet
  :ensure t)

(use-package xclip
  :ensure t
  :config
  (xclip-mode t))

(use-package swiper
  :ensure t)

;; (use-package olivetti
;;   :ensure t)

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-c m") 'mc/edit-lines)
  (global-set-key (kbd "C-c >") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c <") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode t))

(use-package helpful
  :ensure t)

(use-package ace-window
  :ensure t
  :bind
  (("C-x C-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?q ?w ?z ?x))
  (setq aw-ignore-current t)
  (custom-set-faces
 '(aw-leading-char-face
   ((t (:inherit ace-jump-face-foreground :height 3.0 :foreground "yellow"))))))

(use-package activities
  :ensure t
  :init
  (activities-mode)
;  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  :bind
  (("C-c C-a C-n" . activities-new)
   ("C-c C-a C-d" . activities-define)
   ("C-c C-a C-a" . activities-resume)
   ("C-c C-a C-s" . activities-suspend)
   ("C-c C-a C-k" . activities-kill)
   ("C-c C-a b" . activities-switch-buffer)
   ("C-c d d"  . activities-switch)
   ("C-c C-a g" . activities-revert)
   ("C-c C-a l" . activities-list)))

(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode t))

(use-package ivy-posframe
  :ensure t
  :config
  ;; display at `ivy-posframe-style'
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters
      '((left-fringe . 1)
        (right-fringe . 4)))
;;  (set-face-attribute 'ivy-posframe nil :foreground "#3f8c9b" :background "#000000")
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
  :ensure t
  :config
  (winner-mode t))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))



(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :config
  (nerd-icons-dired-mode t))
 
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

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
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))
  
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

(use-package treemacs
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

;; Custom functions
(defun tw/raycast-show-agenda ()
  (interactive)
  (let ((agenda-frame (make-frame-command)))
    (select-frame agenda-frame)
    (org-agenda-list)
    (x-focus-frame agenda-frame)))

; (defun tw/dired-ediff-marked-files ()
;   "Run ediff-files on a pair of files marked in dired buffer"
;   (interactive)
;   (let ((marked-files (dired-get-marked-files nil)))
;     (if (not (= (length marked-files) 2))
;         (message "mark exactly 2 files")
;       (ediff-files (nth 0 marked-files)
;                    (nth 1 marked-files)))))

(defun tw/hide-org-tags ()
  (interactive)
  (setq org-hide-tags t)
  (org-set-tags nil))

(defun tw/highlight-line ()
  (interactive)
  (move-beginning-of-line 1)
  (set-mark-command nil)
  (move-end-of-line 1))

(defun tw/ivy-switch-to-window-by-buffer ()
  "Use ivy to switch to a window displaying a selected buffer."
  (interactive)
  (let* ((window-buffer-alist
          (mapcar (lambda (w)
                    (cons (buffer-name (window-buffer w)) w))
                  (window-list))))
    (ivy-read "Switch to window displaying buffer: "
              (mapcar #'car window-buffer-alist)
              :action (lambda (buffer-name)
                        (select-window (cdr (assoc buffer-name window-buffer-alist)))))))

(defun tw/set-margins ()
(interactive)
(setq left-margin-width 1)
(setq right-margin-width 1)
(set-window-buffer (selected-window) (current-buffer))
(set-window-buffer nil (current-buffer)))

(add-hook 'window-configuration-change-hook 'tw/set-margins)

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

(defun tw/dired-find-file-split ()
  "Open the file in the right-hand vertical split."
  (interactive)
  (let ((buffer (dired-get-file-for-visit)))
    ;; Check if the window split is valid
    (let ((new-window (split-window-right)))
      (when (window-live-p new-window)
        (select-window new-window)))  ; Split vertically and move to the new window if it's live
    (find-file buffer)))              ; Open the file in the new window

(defun tw/dired-filter-files (string)
  "Filter Dired for files containing string: "
  (interactive "sFilter by substring: ")
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

(defun tw/insert-current-date ()
  "Insert the current date in the format YYYY-MM-DD."
  (interactive)
  (insert (format-time-string "%Y-%B-%d")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2eca138bb4bd21c5de6f2f271038ae562a1c79ccfc006b9aa4f1d31139c8824d" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" default))
 '(package-selected-packages
   '(xclip gptel dired-hacks-utils denote-explore timu-macos-theme selected-window-accent-mode pdf-tools magit true malyon org-side-tree all-the-icons-ibuffer listen dashboard zygospore yequake which-key visual-fill-column vertico ts telephone-line taxy sr-speedbar smartparens slime sicp ripgrep rainbow-delimiters quelpa-use-package popup-kill-ring pkg-info perspective peg paredit page-break-lines ov org-roam org-bullets openwith nerd-icons-ibuffer nerd-icons-dired multi-vterm marginalia ivy-posframe ivy-explorer imenu-anywhere ht helpful golden-ratio eyebrowse exwm expand-region erc-hl-nicks erc-colorize erc equake eat dracula-theme doom-themes doom-modeline dirvish dired-single dired+ dimmer counsel consult-notes company-box auto-dim-other-buffers activities ace-window 0blayout)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
 '(hl-line ((t (:inherit nil :extend t))))
 '(org-agenda-date-today ((t (:weight bold :italic t :foreground "LightGoldenRod2"))))
 '(org-agenda-done ((t (:foreground "gray42"))))
 '(org-agenda-overriding-header ((t (:weight bold :foreground "green"))))
 '(org-document-title ((t (:foreground "gray53" :weight bold :height 1.0))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(org-level-8 ((t (:extend nil :foreground "#e64553" :weight normal))))
 '(org-tag ((t (:foreground "light steel blue" :weight bold))))
 '(org-todo ((t (:foreground "DarkOrange3"))))
 '(swiper-line-face ((t (:background "gray70")))))

;; (set-face-attribute 'mode-line nil
;; ;;                    :foreground "#ffffff"  ;; Text color
;;                     :background "#684B71"  ;; Background color
;;                     :box nil)              ;; Remove the box if desired

;; (define-skeleton 1-journal-skeleton
;;   "A journal skeleton" nil
;;   "** Check Org-agenda tasks
;; ** Check ToListen/Today's Queue
;; ** Notes
;; *** ")

(load-file "~/.emacs.d/init-org-mode.el")
