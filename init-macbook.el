;; Init-Macbook.el / called from init.el 2024-September-10

  (custom-set-faces
   '(default ((t (:height 125 :family "Iosevka" :foundry "nil"
                          :slant normal :weight medium :width normal)))))

(set-frame-parameter nil 'alpha-transparency 50)
(set-frame-parameter (selected-frame) 'alpha '(96 96))

(set-frame-parameter nil 'alpha-transparency 0)
(set-frame-parameter (selected-frame) 'alpha '(100 100))

;; Custom functions
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
 
(defun tw/display-async-shell-output-in-active-window (buffer _action)
  "Display BUFFER by splitting the active window below."
  (let ((win (selected-window))) ; Get the active window
    (with-selected-window win
      (let ((new-win (split-window win nil 'below))) ; Split the active window below
        (set-window-buffer new-win buffer)
        (set-window-text-height new-win (max 10 (/ (frame-height) 3)))))))

(setq display-buffer-alist
      (cons '("\\*Async Shell Command\\*"
              (tw/display-async-shell-output-in-active-window))
            display-buffer-alist))

;; (defun tw/display-async-shell-output-in-active-window (buffer _action)
;;   "Display BUFFER by splitting the active window below and switching to it."
;;   (let ((win (selected-window))) ; Get the active window
;;     (with-selected-window win
;;       (let ((new-win (split-window win nil 'below))) ; Split the active window below
;;         (set-window-buffer new-win buffer)
;;         (set-window-text-height new-win (max 10 (/ (frame-height) 3)))
;;         (run-at-time 0.1 nil #'select-window new-win)))))  ; Delay switch slightly

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

(defun tw/list-files-changed-on-disk ()
  "Display all files that have changed on disk but not in the buffer."
  (interactive)
  (let ((changed-files
         (seq-filter
          (lambda (buf)
            (and (buffer-file-name buf) ; The buffer is visiting a file
                 (not (verify-visited-file-modtime buf)))) ; File changed on disk
          (buffer-list))))
    (if changed-files
        (with-current-buffer (get-buffer-create "*Files Changed on Disk*")
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert "Files changed on disk but not in buffer:\n\n")
          (dolist (buf changed-files)
            (insert (format "%s\n" (buffer-file-name buf))))
          (setq buffer-read-only t)
          (switch-to-buffer (current-buffer)))
      (message "No files have changed on disk."))))

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





(defun tw/ef-themes-org-todo-faces (&rest _)
  "Apply colors from the active Ef theme, while ignoring all arguments.
Add this to `ef-themes-post-load-hook', `enable-theme-functions',
or related, to make changes apply to another Ef theme."
  (ef-themes-with-colors
    (setq org-todo-keyword-faces
          `(("TODO" . ,red-cooler) ; different colours for demo purposes
            ("DONE" . ,blue-cooler)
	    ("IN-PROGRESS" . ,green-warmer)
	    ("NEXT" . ,yellow-warmer)
	    ("PROJECT" . ,blue-warmer)))))
(add-hook 'ef-themes-post-load-hook #'tw/ef-themes-org-todo-faces)

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

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0) 
(show-paren-mode t)
(prettify-symbols-mode)
(global-prettify-symbols-mode 1)
;; (fringe-mode 0)
;; Always split windows vertically
(setq split-width-threshold nil)
(setq tsplit-height-threshold nil)


(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer buffer '(display-buffer-same-window))))

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

(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/Backups/"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/Backups/" t)))

(global-set-key (kbd "C-x a s") 'async-shell-command)
(global-set-key (kbd "C-x v t") 'multi-vterm)
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
;(global-set-key (kbd "C-x r e") 'eval-region)
;(global-set-key (kbd "C-x r b") 'eval-buffer)
;(Global-Setkey (kbd "C-c C-l") 'package-list-packages)
(global-set-key (kbd "C-c h") 'dired-dotfiles-toggle)
(global-set-key (kbd "C-c y") 'popup-kill-ring)
(global-set-key (kbd "C-c w") 'make-frame)
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch-backward)
;(global-set-key (kbd "C-x w t") 'tw/toggle-window-dedication)
(global-set-key (kbd "C-<return>") (lambda () (interactive) (tw/smart-open-line-below)))
(global-set-key (kbd "M-<return>") 'tw/smart-open-line-above)
(global-set-key (kbd "C-c l c f") 'tw/list-files-changed-on-disk)
(global-set-key (kbd "C-c v") 'visual-line-mode)
(global-set-key (kbd "C-c g d") 'find-grep-dired)
(global-set-key (kbd "C-c =") 'balance-windows-area)

;; (global-set-key (kbd "C-c t") (lambda () (interactive) (unless (derived-mode-p 'org-mode) (call-interactively 'tw/smart-open-line-above))))
;;(global-unset-key (kbd "M-<return>"))

(global-set-key (kbd "C-c t h") 'tw/hide-org-tags)
(global-set-key (kbd "C-c i d") 'tw/insert-current-date)
(global-set-key (kbd "C-x w") 'tw/ivy-switch-to-window-by-buffer)

(setq dired-listing-switches "-lha")
(setq large-file-warning-threshold 50000000)
(setq dired-kill-when-opening-new-dired-buffer t)

;; Hook some modes
;(add-hook 'dired-mode-hook 'auto-revert-mode) ;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "w") #'tw/dired-find-file-other-application)))
(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "o") #'dired-find-file-other-window)))
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(setq electric-pair-inhibit-predicate
      (lambda (c)
        (if (char-equal c ?\") t (electric-pair-default-inhibit c))))
(add-hook 'org-agenda-mode 'hl-line-mode)
(add-hook 'org-mode-hook 'hl-line-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)
;;(add-hook 'prog-mode-hook (setq display-line-numbers 'absolute)'display-line-numbers-mode)
(add-hook 'elfeed-mode-hook (lambda () (local-set-key (kbd "g") #'elfeed-update)))
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))
(add-hook 'ibuffer-mode-hook (lambda () (hl-line-mode 1)))
(add-hook 'text-mode-hook #'visual-line-mode)

;; (use-package cloud-theme
;;   :ensure t
;;   :config
;;   (setq tw-light-theme 'cloud-theme))

;; (use-package catppuccin-theme
;;   :ensure t
;;   :config
;;   (setq catppuccin-flavor 'latte) ;; or 'latte, 'macchiato, or 'mocha, 'frappe
;;   (catppuccin-reload))

;; (use-package timu-macos-theme
;;   :ensure t
;;   :config
;;   (setq tw-light-theme 'timu-macos-theme))

;; Define light and dark color themes
(setq tw-dark-theme 'ef-owl
      tw-light-theme 'ef-frost)

(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-italic-constructs t)
  (global-set-key (kbd "C-c l l")
		  (lambda ()
		    (interactive)
		    (custom-set-faces
		     '(org-agenda-date-today ((t (:weight bold :italic t :foreground "Olive"))))
		     '(aw-leading-char-face
		       ((t (:inherit ace-jump-face-foreground :height 3.0 :foreground "DarkMagenta")))))
		    (disable-theme (car custom-enabled-themes))
  		    (load-theme tw-light-theme)))

  (global-set-key (kbd "C-c l d")
		  (lambda ()
		    (interactive)
		    (custom-set-faces
		     '(org-agenda-date-today ((t (:weight bold :italic t :foreground "steelblue"))))
				     '(aw-leading-char-face
		       ((t (:inherit ace-jump-face-foreground :height 3.0 :foreground "DarkOrange")))))
		    (disable-theme (car custom-enabled-themes))
		    (load-theme tw-dark-theme))))
;; End of custom fuctions

(use-package zygospore
  :ensure t)

(use-package casual-suite
  :ensure t
  :config
  (keymap-set dired-mode-map "C-o" #'casual-dired-tmenu)
  (keymap-set isearch-mode-map "C-o" #'casual-isearch-tmenu)
  (keymap-set ibuffer-mode-map "C-o" #'casual-ibuffer-tmenu)
  (keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
  (keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu)
  (keymap-set Info-mode-map "C-o" #'casual-info-tmenu)
  (keymap-set bookmark-bmenu-mode-map "C-o" #'casual-bookmarks-tmenu)
  (keymap-set org-agenda-mode-map "C-o" #'casual-agenda-tmenu)
  (keymap-global-set "M-g" #'casual-avy-tmenu)
  (keymap-set symbol-overlay-map "C-o" #'casual-symbol-overlay-tmenu)
  (keymap-global-set "C-o" #'casual-editkit-main-tmenu))

(use-package orderless
  :ensure t)

(use-package magit
  :ensure t
  :config
    (global-set-key (kbd "C-x g") 'magit))

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

(use-package visual-replace
   :defer t
   :bind (("C-c r" . visual-replace)
          :map isearch-mode-map
          ("C-c r" . visual-replace-from-isearch)))

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
  (setq ivy-use-virtual-buffers nil)
  (setq enable-recursive-minibuffers t)
  :custom
  (setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order)))
  :config
  (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
  (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight))
  (ivy-mode t))

(use-package counsel
  :ensure t
  :config
  (defun tw/counsel-org-todo ()
    "Use `counsel-org-goto-all` to search only TODO items."
  (interactive)
  (let ((ivy-re-builders-alist '((t . ivy--regex-ignore-order))))
    (counsel-org-goto-all)))
  :bind (("C-c c f t" . tw/counsel-org-todo)))

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
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters
      '((left-fringe . 10)
        (right-fringe . 10)))
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
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
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
  :ensure t
  :config
  (treemacs))

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

(load-file "~/.emacs.d/init-org-mode.el")
