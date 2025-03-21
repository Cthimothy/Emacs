; (setq debug-on-error t)

(load "server")
(unless (server-running-p)
  (server-start))
(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 4 1024 1024))
(setq frame-resize-pixelwise t)
(setq browse-url-browser-function 'browse-url-default-browser)

(setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin"))
(setq exec-path (append exec-path '("/opt/homebrew/bin")))

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

(setq custom-file (locate-user-emacs-file "custom.el"))  
(load custom-file :no-error-if-file-is-missing)  

(setq lock-file-name-transforms
      '((".*" "~/.emacs.d/lockfiles/" t)))

;; (let ((host (system-name)))
;;   (cond
;;    ((string-equal host "azathoth")
;;     (load "~/.emacs.d/init-azathoth.el"))
;;    ((string-equal host "DHW392J4FQ")
;;     (load "~/.emacs.d/init-macbook.el"))
;;    (t
;;     (message "No specific configuration for this host"))))

(setq inhibit-startup-screen t)
;; (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
;;(add-hook 'after-init-hook (lambda () (kill-buffer "*scratch*")))
(add-hook 'after-init-hook (lambda () (kill-buffer "*Messages*")))

  (custom-set-faces
   '(default ((t (:height 125 :family "Iosevka" :foundry "nil"
                          :slant normal :weight medium :width normal)))))

(set-frame-parameter nil 'alpha-transparency 50)
(set-frame-parameter (selected-frame) 'alpha '(95 95))
;(set-frame-parameter nil 'alpha-transparency 0)
;(set-frame-parameter (selected-frame) 'alpha '(100 100))

;; -----------------------------------------------------------------------------
;; Custom functions

;; Read in Journelly to Denote code
(load-file "~/Projects/Code/Elisp/journelly-to-denote.el")

(defun tw/toggle-fill-column-indicator ()
  "Enable `display-fill-column-indicator-mode` only if the current line exceeds `fill-column`."
  (if (> (current-column) fill-column)
      (display-fill-column-indicator-mode 1)
    (display-fill-column-indicator-mode -1)))
(add-hook 'post-command-hook #'tw/toggle-fill-column-indicator)
(setq-default fill-column 80)

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
	  (Set-Window-buffer (selected-window) this-win-buffer)
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
  (Let ((File (dired-get-file-for-visit)))
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
	    ("PROJECT" . ,blue-warmer)
	    ("UNSCHEDULED" . ,grey-warmer)))))
(add-hook 'ef-themes-post-load-hook #'tw/ef-themes-org-todo-faces)

;; Custom functions

(defun tw/create-jekyll-post ()
"Create a new Jekyll blog post in ~/Projects/cthimothy.github.io/_posts/."
  (interactive)
  (let* ((title (read-string "Post title: "))
         (slug (replace-regexp-in-string "[^a-z0-9-]" "" (downcase (replace-regexp-in-string " " "-" title))))
         (date (format-time-string "%Y-%m-%d"))
         (filename (expand-file-name (format "%s-%s.md" date slug)
                                     "~/Projects/cthimothy.github.io/_posts/")))
    (if (file-exists-p filename)
        (message "File already exists: %s" filename)
      (find-file filename)
      (insert (format
               "---\n\
layout: post\n\
title: \"%s\"\n\
date: %s\n\
categories: blog\n\
tags: \n\
---\n\n"
               title date))
      (save-buffer)
      (message "Created new Jekyll post: %s" filename))))

(defun Tt/raycast-show-agenda ()
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

(setq column-number-mode t)

;; ----------------------------------------------------------------------------
(global-set-key (kbd "C-c t l") (lambda ()
				  (interactive)
				  (set-frame-parameter nil 'alpha-transparency 50)
				  (set-frame-parameter (selected-frame) 'alpha '(95 95))))

(global-set-key (kbd "C-c t d") (lambda ()
				  (interactive)
				  (set-frame-parameter nil 'alpha-transparency 0)
				  (set-frame-parameter (selected-frame) 'alpha '(100 100))))

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
(global-set-key (kbd "C-x C-l") 'avy-goto-line)
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
(global-set-key (kbd "C-c e") 'forward-sexp)
(global-set-key (kbd "C-c a") 'backward-sexp)
(global-set-key (kbd "C-c t h") 'tw/hide-org-tags)
(global-set-key (kbd "C-c i d") 'tw/insert-current-date)
(global-set-key (kbd "C-x w") 'tw/ivy-switch-to-window-by-buffer)

;; (global-set-key (kbd "C-c t") (lambda () (interactive) (unless (derived-mode-p 'org-mode) (call-interactively 'tw/smart-open-line-above))))
;;(global-unset-key (kbd "M-<return>"))


(setq insert-directory-program "/opt/homebrew/bin/gls")
(setq dired-listing-switches "-lhtgo")
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
;(add-hook 'org-mode-hook 'display-line-numbers-mode)
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
;;   (setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha, 'frappe
;;   (catppuccin-reload))

;; (use-package timu-macos-theme
;;   :ensure t
;;   :config
;;   (setq tw-light-theme 'timu-macos-theme))

(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-italic-constructs t)
  (global-set-key (kbd "C-c l l")
		  (lambda ()
		    (interactive)
;		    (custom-set-faces
;		     '(org-agenda-date-today ((t (:weight bold :italic t :foreground "Olive"))))
;		     '(aw-leading-char-face
;		       ((t (:inherit ace-jump-face-foreground :height 3.0 :foreground "DarkMagenta")))))
		    (disable-theme (car custom-enabled-themes))
  		    (load-theme tw-light-theme)))

  (global-set-key (kbd "C-c l d")
		  (lambda ()
		    (interactive)
;		    (custom-set-faces
;		     '(org-agenda-date-today ((t (:weight bold :italic t :foreground "steelblue"))))
;				     '(aw-leading-char-face
;		       ((t (:inherit ace-jump-face-foreground :height 3.0 :foreground "DarkOrange")))))
		    (disable-theme (car custom-enabled-themes))
		    (load-theme tw-dark-theme))))
;; End of custom fuctions

;; Define light and dark color themes
(setq tw-dark-theme 'ef-owl
      tw-light-theme 'ef-frost)

(load-theme 'ef-owl)

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
  (setq doom-modeline-workspace-name t)
  (setq doom-modeline-modal t)
  (doom-modeline-mode t))

(use-package helpful
  :ensure t
  :config
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  ;; Look up *F*unctions (excludes macros).
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  (global-set-key (kbd "C-h F") #'helpful-function)
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))

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
;  (setq ivy-posframe-parameters
;      '((left-fringe . 0)
;        (right-fringe . 0)))

  (setq ivy-posframe-width-relative t)
  (setq ivy-posframe-height-relative t)

  (setq ivy-posframe-width 180
	ivy-posframe-height 10)
;(setq ivy-posframe-width-relative-factor 0.62)
;(setq ivy-posframe-height-relative-factor 0.1)

  
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
  (setq-default pdf-view-display-size 'fit-height)
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
  (setq treemacs-width 75)
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

(use-package elfeed
  :ensure t)

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure t
  :init

  ;; --- Org-agenda functions
    (defun tw/org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.
  PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
          subtree-end
        nil)))

  (defun tw/org-agenda-skip-high-priority ()
    "Skip all tasks with priority 'A'."
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (member (org-entry-get nil "PRIORITY") '("A"))
          next-headline
        nil)))

  (defun tw/archive-done-tasks-in-agenda-files ()
  "Archive all DONE tasks in all files listed in `org-agenda-files`."
  (interactive)
  (dolist (file org-agenda-files)
    (with-current-buffer (find-file-noselect file)
      (org-map-entries
       (lambda ()
         (org-archive-s/-ubtree))
       "/DONE" 'file)
      (save-buffer))))
  
  (defadvice org-capture-finalize 
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (if (equal "capture" (frame-parameter nil 'name))
        (delete-frame)))
  
  (defadvice org-capture-destroy
      (after delete-capture-frame activate)
    "Advise capture-destroy to close the frame"
    (if (equal "capture" (frame-parameter nil 'name))
        (delete-frame)))
 
  (defun make-capture-frame ()
    "Create a new frame and run org-capture."
    (interactive)
    (make-frame '((name . "capture")))
    (select-frame-by-name "capture")
    (delete-other-windows)
    (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
      (org-capture)))
  
  ;; This is run from MacOS Automator
  ;; socketfile=$(lsof -c Emacs | grep server | /usr/local/opt/coreutils/libexec/gnubin/tr -s " " |
  ;; /usr/local/opt/coreutils/libexec/gnubin/cut -d' ' -f8)gg
  ;; /usr/local/bin/emacsclient -ne "(make-capture-frame)" -s $socketfile

  ;;  (run-at-time "24:00" 86400
  ;;               (lambda () (tw/org-archive-done-tasks-in-agenda-files)))
  
  :config
  (custom-set-faces
     '(org-agenda-structure
     ((t (:foreground "LightSkyBlue" :weight bold :height 1.2 :underline nil :height 1.1))))
     '(org-agenda-date-today ((t (:weight bold :height 1.4 :foreground "LightSkyBlue")))))

  (add-hook 'org-agenda-mode-hook 'hl-line-mode)
  (add-hook 'org-mode-hook 'hl-line-mode)
  (add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
  (add-hook 'org-mode-hook (lambda () (local-set-key (kbd "M-<return>") 'org-meta-return)))
  (global-set-key (kbd "C-S-<up>") 'org-move-subtree-up) 
  (global-set-key (kbd "C-S-<down>") 'org-move-subtree-down)
  
  (require 'color)
  (set-face-attribute 'org-block nil :background
                    (color-darken-name
                     (face-attribute 'default :background) 6))
  
  (setq org-todo-keyword-faces
	'(
	  ("NEXT" . (:foreground "#f3e16b" :background ""))
	  ("TODO" . (:foreground "#F0C455" :background ""))
	  ("IN-PROGRESS" . (:foreground "#c44d56" :background "" :weight bold))
	  ("UNSCHEDULED" . (:foreground "orange" :background ""))
	  ("PROJECT" . (:foreground "#65BC8F" :background "" :weight bold))
	  ("DONE" . (:foreground "#8f9C55" :background ""))))

  (setq org-todo-keywords
      '((sequence "NEXT(n)" "TODO(t)" "UNSCHEDULED(u)" "PROJECT(p)" "|" "DONE(d)")))

  (setq org-adapt-indentation t
      org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-pretty-entities t
	  org-ellipsis "  ·")
  ;;  

(setq org-adapt-indentation t
      org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-pretty-entities t
	  org-ellipsis " ❱") ;;;  · ❱ ❯ ⃕ ↴  ̬ ➥ ➧⤸ ⤵ 🠻 🠺 🡃 🡫 🡮 🡻 🡾 ▼ ⬎ ⤷
  
; (setq org-agenda-start-with-follow-mode t)
; (setq org-base-path "/Users/t.welch2/Library/CloudStorage/Dropbox")
; (setq org-directory (concat org-base-path "/Org/"))
  (setq org-directory "~/Org/")
  (setq org-agenda-window-setup 'current-window)
  (setq org-adapt-xindentation t)
  (setq org-adapt-indentation t)
  ;(setq org-agenda-show-current-time-in-grid t)
  (setq org-agenda-time-grid '((daily today remove-match require-timed)
                             (800 900 1000 1100  1200 1300 1400 1500 1600 1700 1800)
                             " - " ""))
  (setq org-agenda-show-all-dates nil)
  (setq org-agenda-compact-blocks t)

  (setq org-tag-alist
        '(("@Emacs" . ?e)
          ("@Tasks" . ?t)
          ("@Work" . ?w)
          ("@ADA" . ?a)
          ("@RPG" . ?r)
          ("@INBOX" . ?i)))

  (setq org-archive-location "~/Org/Org-Archive/Archive.org::")
  
;; (setq org-agenda-files
;;  	(append
;;  	 '(
;;  	   "~/Org/Emacs.org"
;;  	   "~/Org/Inbox.org"
;;  	   "~/Org/Projects.org"
;;  	   "~/Org/RPG.org"
;;  	   "~/Org/Tasks.org"
;;  	   "~/Org/Work.org")
;; 	 (directory-files-recursively "~/Denote/" "\\.org$")
;; 	 (directory-files-recursively "~/Denote/Journal/" "__journal\\.org$")
;; 	 (directory-files "~/Denote/" t "\\.org$")
;; 	 (directory-files "~/Denote/Journal" t "__journal\\.org$")))))

  (setq org-agenda-files
	(directory-files-recursively "~/Denote/" "\\.org$"))
  
  ;; (setq org-refile-targets
  ;;       '(("~/Org/Projects.org" :maxlevel . 1)
  ;;         ("~/Org/Tasks.org" :maxlevel . 1)
  ;;         ("~/Org/Atheism.org" :maxlevel . 1)
  ;;         ("~/Org/Emacs.org" :maxlevel . 1)
  ;;         ("~/Org/RPG.org" :maxlevel . 1)
  ;;         ("~/Org/Work.org" :maxlevel . 3)
;;         ("~/Org/Inbox.org" :maxlevel . 1)))

(setq org-refile-targets
      `((,(denote-journal-extras--entry-today) . (:maxlevel . 1))
	("~/Denote/journal/" :maxlevel . 1)
	("~/Denote/20231016T101943--atheism.org" :maxlevel . 1)
	("~/Denote/20250305T141314--emacs.org" :maxlevel . 1)
	("~/Denote/20250304T152326--rpg.org" :maxlevel . 1)
	("~/Denote/20250305T141315--projects.org" :maxlevel . 1)
	("~/Denote/20250305T073302--work.org" :maxlevel . 1)))

  (setq org-capture-templates `(
                                ("i" "INBOX" entry
                                 (file+headline "~/Denote/20250304T122024--inbox.org"
                                                "") "* TODO %i%?")
                                ("t" "General Task" entry
                                 (file+headline "~/Denote/20250304T113200--tasks.org"
                                                "To Do") "* TODO %i%?")
                                ("A" "Atheism Note" entry
                                 (file+headline "~/Denote/20250212T142617--atheism.org"
						"INBOX") "** %i%?")
                                ("w" "Work Task" entry
                                 (file+headline "~/Denote/20250305T073302--work.org"
                                                "INBOX") "* TODO %i%?")))

  (setq org-agenda-custom-commands
        '(
          ("g" "Agenda for week and all tasks"
           ((agenda "" ((org-agenda-overriding-header "Agenda")
                        (setq org-agenda-span 'week)
			(setq org-agenda-sorting-strategy
			      '((agenda todo-state-up)
				(todo todo-state-up)
				;;;(todo todo-state-down priority-down category-keep)
				(tags todo-state-up)
				(search todo-state-up)))
			(setq org-agenda-start-day "+0d")
			(setq org-agenda-start-on-weekday t)
			;;(setq org-agenda-start-on-weekday nil)
			(setq org-deadline-warning-days 0)
			;;(makunbound 'org-agenda-prefix-format)
			;;(makunbound 'org-prefix-format)
			(setq org-agenda-prefix-format '((agenda  . " %-12T")))

			(setq org-agenda-prefix-format
      '((agenda . " %i %?-12t% s")
        (todo . " %i ")
        (tags . " %i ")
        (search . " %i ")))

;; (setq org-prefix-format
;;       '(
;;         (agenda . "  %-12:c%?-12t% s")
;; 	(timeline . "  %?-12t% s")
;; 	(todo . "  %-12:c")
;; 	(search . "  %-12:c")
;; 	(tags . "  %-12:c")
;; 	(tags-todo . "  %-12:c")))

(setq org-agenda-prefix-format
      '((agenda . "  %i %?-12t% s")  ;; Remove `%c`
        (todo . "  %i ")
        (tags . "  %i ")
        (search . "  %i ")))

(setq org-agenda-remove-tags nil)

(org-agenda-entry-types '(:deadline :scheduled :timestamp))
(org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'any))
(org-agenda-block-separator "\n")))
	    
	    (tags-todo "+PRIORITY=\"A\"" ((org-agenda-overriding-header "\nPriority Tasks")))
	    (tags-todo "@INBOX" ((org-agenda-overriding-header "\nInbox")))
	    (todo "NEXT" ((org-agenda-overriding-header "\nNext Tasks")))
            (tags-todo "@Work"  ((org-agenda-overriding-header "\nWork Tasks")))
            (tags-todo "@Emacs" ((org-agenda-overriding-header "\nEmacs Tasks")))
            (tags-todo "@Projects" ((org-agenda-overriding-header "\nProjects Tasks")))
            (tags-todo "@Personal" ((org-agenda-overriding-header "\nPersonal Tasks")))))

	  ;; Summary for small frame
	  ("s" "Today's Summary"
	   ((agenda "" (org-agenda-overriding-header "Today's Summary")))
	   (tags-todo "+PRIORITY=\"A\"" ((org-agenda-overriding-header "\nPriority Tasks")))
	   (tags-todo "@INBOX" ((org-agenda-overriding-header "\nInbox")))
	    (todo "NEXT" ((org-agenda-overriding-header "\nNext Tasks"))))
	  
          ("w" "Work Tasks"
           ((agenda "" ((org-agenda-overriding-header "Work Tasks")))
            (tags-todo "@Work")))
	  
          ("p" "Personal Tasks"
           ((tags-todo "@Personal" ((org-agenda-overriding-header "Personal Tasks")))
            (agenda "")))
	  
	  ("r" "RPG Tasks"
           ((tags-todo "@RPG" ((org-agenda-overriding-header "RPG Tasks")))
            (agenda "")))
	  
          ("e" "Emacs"
           ((tags-todo "@Emacs" ((org-agenda-overriding-header "Emacs Tasks")))
            (agenda "")))
	  
          ("i" "Inbox"
           ((tags-todo "@INBOX" ((org-agenda-overriding-header "INBOX")))
            (agenda "")))))
  )
;;End of org-mode

;; (use-package org-notify
;;   :ensure t
;;   :config
;;   (org-notify-start))

(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.0 :underline t))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.0 :weight bold))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
  (setq org-superstar-leading-bullet ?\s)
  (org-superstar-mode t))

(use-package denote
  :ensure t
  :custom
  (denote-directory "~/Denote/")
  (denote-journal-directory "~/Denote/Journal/")
  :config
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  ;; (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
  (setq denote-known-keywords (list "journal" "atheism" "work" "rpg" "radio" "family" "music" "books"))
  ;; (denote-dired-mode t)
  (global-set-key (kbd "C-c d n") 'denote-create-note)
  (global-set-key (kbd "C-c d f") 'consult-notes)
    (global-set-key (kbd "C-c d s") 'denote-sort-dired)
  (global-set-key (kbd "C-c d j n") 'tw/denote-journal)
  (global-set-key (kbd "C-c d j j") 'tw/journelly-to-denote-journal)
  ;; (global-set-key (kbd "C-c d j n") 'denote-journal-extras-new-or-existing-entry)
  (global-set-key (kbd "C-c d o") (lambda ()
				    (interactive)
				    (dired denote-directory)))

    (global-set-key (kbd "C-c d j o") (lambda ()
				    (interactive)
				    (dired (concat denote-directory "/Journal/"))))
;;				    (dired denote-journal-extras-directory)))

  (denote-rename-buffer-mode)

  (require 'denote-org-extras)
  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 '("n" "New note (with Denote)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured nil)))

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
               '("j" "Journal" entry
                 (file denote-journal-extras-path-to-new-or-existing-entry)x
                 "* %U %?\n%i\n%a"
                 :kill-buffer t
                 :empty-lines 1)))
  
  ;; (defun tw/denote-journal ()
;;     "Create an entry tagged 'journal' with the date as its title."
;;     (interactive)
;;     (denote (format-time-string "%A %e %B %Y") '("journal"))
;;     (insert "* Daily Morning Routine
;; - [ ] Review yesterday's journal (c-c d j o)
;; - [ ] Review task list and refile in to journal (c-c o a t)
;; - [ ] Check Beorg for tasks
;; - [ ] Check for changed files (C-c l c f)
;; - [ ] Review Outlook calendar
;; - [ ] Review Org INBOX
;; - [ ] Review Raindrop INBOX   https://app.raindrop.io/my/-1

;; * Tasks

;; * Notes
;; "))

(defun tw/denote-journal ()
  "Create an entry tagged 'journal' with the date as its title in `~/Denote/journal/`."
  (interactive)
  (let ((denote-directory "~/Denote/journal/"))  ;; Temporarily set the Denote directory
    (denote (format-time-string "%A %e %B %Y") '("journal"))
    (goto-char (point-max))  ;; Move to the end of the metadata
    (insert "\n* Daily Morning Routine
- [ ] tw/journelly-to-denote
- [ ] Review yesterday's journal (C-c d j o)
- [ ] Review task list and refile into journal (C-c o a t)
- [ ] Check Beorg for tasks
- [ ] Check for changed files (C-c l c f)
- [ ] Review Outlook calendar
- [ ] Review Org INBOX
- [ ] Review Raindrop INBOX   https://app.raindrop.io/my/-1

* Tasks

* Notes
"))
  (setq org-refile-targets
	`((,(denote-journal-extras--entry-today) . (:maxlevel . 1))
	  ("~/Denote/journal/" :maxlevel . 1)
	  ("~/Denote/20231016T101943--atheism.org" :maxlevel . 1)
	  ("~/Denote/20250305T141314--emacs.org" :maxlevel . 1)
	  ("~/Denote/20250304T152326--rpg.org" :maxlevel . 1)
	  ("~/Denote/20250305T141315--projects.org" :maxlevel . 1)
	  ("~/Denote/20250305T073302--work.org" :maxlevel . 1))))

  
;; (let ((heading "Tasks")
;;       (case-fold-search t)) ; Make search case-insensitive
;;   (goto-char (point-min)) ; Start from the beginning of the buffer
;;   (if (re-search-forward (format org-complex-heading-regexp-format (regexp-quote heading)) nil t)
;;       (progn
;;         (end-of-line)          ; Move to the end of the heading line
;;         (newline)              ; Insert a newline if needed
;;         (message "Moved to the line under heading: %s" heading))
;;     (message "Heading '%s' not found" heading)))

  (defun tw/denote-find-file ()
  "Use Ivy to finda Denote file."
  (interactive)
  (let ((default-directory denote-directory))
    (ivy-read "Find Denote file: "
              (directory-files denote-directory nil "^[^.].*\\.org$")
              :action (lambda (file)
                        (find-file (expand-file-name file denote-directory)))))))

(use-package denote-search
  :ensure t
  :bind
  ;; Customize keybindings to your liking
  (("C-c d g" . denote-search)
   ("C-c s d" . denote-search-marked-dired-files)
   ("C-c s r" . denote-search-files-referenced-in-region))
  :custom
  ;; Disable help string (set it once you learn the commands)
  ;; (denote-search-help-string "")
  ;; Display keywords in results buffer
  (denote-search-format-heading-function #'denote-search-format-heading-with-keywords))

(use-package consult-notes
  :ensure t
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :config
  (consult-notes-denote-mode t))

;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (setq dashboard-week-agenda t)
;;   (setq dashboard-startup-banner 'official)
;;   (setq dashboard-center-content t)
;;   (setq dashboard-items '(
;;                           (agenda    . 5)
;; 			  (recents   . 5)
;;                           (bookmarks . 5)
;;                           (projects  . 5)
;;                           (registers . 5)))
;;   (dashboard-setup-startup-hook))
;; (dashboard-open)

(use-package calfw
  :ensure t)

;; (delete-window (get-buffer-window "*scratch*"))
