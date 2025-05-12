;; -----------------------------------------------------------------------------
;; Initial configuration
;; -----------------------------------------------------------------------------
(setq debug-on-error t)
(load "server")
(unless (server-running-p)
  (server-start))
(setq default-frame-alist '((ns-transparent-titlebar . t) (ns-appearance . dark)))
(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 4 1024 1024))
(setq frame-resize-pixelwise t)
(setq org-element-cache-persistent t)
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

(setq inhibit-startup-screen t)

(custom-set-faces
 '(default ((t (:height 125 :family "Iosevka" :foundry "nil"
                        :slant normal :weight medium :width normal)))))

(with-eval-after-load 'eww
  (dolist (face '(eww-form-checkbox
                  eww-form-file
                  eww-form-select
                  eww-form-submit
                  eww-form-text
                  eww-form-textarea
                  eww-invalid-certificate
                  eww-valid-certificate
                  eww-button
                  eww-link
                  eww-heading-1
                  eww-heading-2
                  eww-heading-3
                  eww-heading-4
                  eww-heading-5
                  eww-heading-6))
    (when (facep face)
      (set-face-attribute face nil :inherit 'default))))

(defun my/eww-use-iosevka-font ()
  "Force EWW and shr to use Iosevka."
  (let ((font "Iosevka")
        (height 135)) ;; Adjust height as you like
    (set-face-attribute 'shr-text nil :family font :height height)
    (set-face-attribute 'shr-link nil :family font :height height)
    (set-face-attribute 'eww-form-text nil :family font :height height)
    (set-face-attribute 'eww-form-submit nil :family font :height height)
    (set-face-attribute 'eww-form-select nil :family font :height height)
    (set-face-attribute 'eww-form-textarea nil :family font :height height)
    (set-face-attribute 'eww-form-checkbox nil :family font :height height)))

(add-hook 'eww-after-render-hook #'my/eww-use-iosevka-font)

(set-frame-parameter nil 'alpha-transparency 50)
(set-frame-parameter (selected-frame) 'alpha '(100 100))


;;Make Ielm Great Again
(add-hook 'ielm-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'paredit-mode)
(define-key paredit-mode-map (kbd "RET") nil)
(define-key paredit-mode-map (kbd "C-j") 'paredit-newline)

(defun g-ielm-init-history ()
  (let ((path (expand-file-name "ielm/history" user-emacs-directory)))
    (make-directory (file-name-directory path) t)
    (setq-local comint-input-ring-file-name path))
  (setq-local comint-input-ring-size 10000)
  (setq-local comint-input-ignoredups t)
  (comint-read-input-ring))

(add-hook 'ielm-mode-hook 'g-ielm-init-history)

(defun g-ielm-write-history (&rest _args)
  (with-file-modes #o600
    (comint-write-input-ring)))
  
(advice-add 'ielm-send-input :after 'g-ielm-write-history)

(define-key inferior-emacs-lisp-mode-map (kbd "C-l")
            'comint-clear-buffer)

(define-key inferior-emacs-lisp-mode-map (kbd "C-r")
            'helm-comint-input-ring)


;; -----------------------------------------------------------------------------
;; Load custom code from external files
;; -----------------------------------------------------------------------------
;(add-to-list 'load-path "~/Projects/Code/Elisp/tag-explorer/")
(add-to-list 'load-path "~/Projects/Code/Elisp/journelly-to-denote/")
(add-to-list 'load-path "~/Projects/Code/Elisp/denote-tag-find-dired/")
;(require 'tag-explorer)
(require 'journelly-to-denote-journal)
(require 'denote-tag-find-dired)

;; -----------------------------------------------------------------------------
;; Define custom functions
;; NOTE: All org-mode related functions defined within (use-package org-mode)
;; -----------------------------------------------------------------------------
(defun unfuck-all ()
  (interactive)
  (setq org-refile-targets
	(mapcar (lambda (f)
                  (cons f '(:maxlevel . 2)))
		(seq-filter (lambda (file)
                              (not (string-match-p "@" (file-name-nondirectory file))))
                            (directory-files-recursively "~/Org/" "\\.org$"))))

  (setq org-agenda-files
  	(directory-files-recursively "~/Org/" "\\.org$"))

  (setq org-adapt-indentation t
	org-hide-leading-stars t
	org-hide-emphasis-markers t
	org-pretty-entities t
	org-ellipsis " ❱") 

  (setq ivy-posframe-width-relative t)
  (setq ivy-posframe-height-relative t)
  (setq ivy-posframe-border-width 1)
  (set-face-attribute 'ivy-posframe-border nil :background "#666666"))


(defun search-web (start end)
  "Search the web for the selected region."
  (interactive "r")
  (let ((query (buffer-substring-no-properties start end)))
    (browse-url (concat "https://www.duckduckgo.com/search?q=" (url-hexify-string query)))))
;; (global-set-key (kbd "C-c g") #'search-web-at-point)

(defun tw/toggle-transparency ()
  "Toggle between light and dark frame transparency."
  (interactive)
  (let* ((current-alpha (frame-parameter nil 'alpha))
         (current-alpha (if (consp current-alpha) (car current-alpha) current-alpha)))
    (if (equal current-alpha 95)
        (set-frame-parameter nil 'alpha '(100 100))
      (set-frame-parameter nil 'alpha '(95 95)))))
;; Keyboard shortcut: C-c t


;; (defun tw/toggle-fill-column-indicator ()
;;   "Toggle `display-fill-column-indicator-mode` in the current buffer."
;;   (interactive)
;;   (setq fill-column 80) ;; Optional: sets fill-column to 80 for this buffer
;;   (if display-fill-column-indicator-mode
;;       (display-fill-column-indicator-mode -1)
;;     (display-fill-column-indicator-mode 1)))
;; Hooks and advice referenced:
;(add-hook 'post-command-hook #'tw/toggle-fill-column-indicator)


(defun tw/denote-search-by-tag-dired-ivy ()
  "Search Denote files by tag using Ivy, then open Dired on the matching files."
  (interactive)
  (let* ((tags (delete-dups (mapcan #'denote-extract-keywords-from-path
                                    (directory-files-recursively denote-directory "\\.org$"))))
         (selected-tag (ivy-completing-read "Tag: " tags))
         (matching-files (seq-filter
                          (lambda (file)
                            (member selected-tag (denote-extract-keywords-from-path file)))
                          (directory-files-recursively denote-directory "\\.org$"))))
    (if matching-files
        (dired (cons denote-directory matching-files))
      (message "No files found with tag: %s" selected-tag))))


(defun tw/close-old-denote-journal-buffers ()
  "Close all open Denote journal buffers except today's and non-journals."
  (interactive)
  (let* ((denote-dir (expand-file-name "~/Org//"))
         (today (format-time-string "%Y%m%d"))
         (today-regex (concat "\\b" today "\\b")))
    (dolist (buf (buffer-list))
      (let ((file (buffer-file-name buf)))
        (when (and file
                   (string-prefix-p denote-dir file)
                   (string-match-p "__journal" file)
                   (not (string-match-p today-regex file)))
          (kill-buffer buf))))))
;; Hooks and advice referenced:
;(add-hook 'emacs-startup-hook #'tw/close-old-denote-journal-buffers)
;(advice-add 'org-agenda :after #'tw/close-old-denote-journal-buffers)


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


(defun tw/reload-all-buffers-changed-on-disks ()
  "Revert all file buffers without confirmation.
Buffers visiting files that have changed on disk are reloaded."
  (interactive)
  (let ((reverted-count 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name buf)
                   (file-exists-p (buffer-file-name buf))
                   (not (buffer-modified-p buf))
                   (not (verify-visited-file-modtime buf)))
          (revert-buffer :ignore-auto :noconfirm)
          (cl-incf reverted-count))))
    (message "Reverted %d buffer(s)." reverted-count)))


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
;; Keyboard shortcut: C-c h


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
;; Keyboard shortcut C-x w


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
;; Keyboard shortcut C-c j b


(defun tw/highlight-line ()
  (interactive)
  (move-beginning-of-line 1)
  (set-mark-command nil)
  (move-end-of-line 1))
;; Keyboard shortcut C-x C-h


(defun tw/insert-current-date ()
  "Insert the current date in the format YYYY-MM-DD."
  (interactive)
  (insert (format-time-string "%Y-%B-%d")))


;(defun tw/toggle-window-dedication ()
;  (interactive)
;  (set-window-dedicated-p (selected-window)
;                          (not (window-dedicated-p (selected-window)))))
;; Keyboard shortcut C-c w t


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


;; Dired specific functions
(defun tw/dired-filter-files (string)
  "Filter Dired for files containing string: "
  (interactive "sFilter by substring: ")
  (dired-mark-files-regexp string)
  (dired-toggle-marks)
  (dired-do-kill-lines))
;; Keyboard shortcut C-c f

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

;; (defun tw/dired-find-file-split-below ()
;;   "Open the file at point in another window, split below the Dired buffer."
;;   (interactive)
;;   (let ((window (split-window-below)))
;;     (select-window window)
;;     (dired-find-file)))
;; (with-eval-after-load 'dired
;;   (define-key dired-mode-map (kbd "C-c d j o") #'tw/dired-find-file-split-below))

;; -----------------------------------------------------------------------------
;; Set personal and Macbook specific defaults 
;; -----------------------------------------------------------------------------
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0) 
(show-paren-mode t)
(prettify-symbols-mode)
(global-prettify-symbols-mode 1)
(electric-indent-mode -1)
;; (fringe-mode 0)

(setq ibuffer-default-sorting-mode 'recency)
(setq split-width-threshold nil)
(setq split-height-threshold nil)
(setq use-short-answers t)
(setq confirm-nonexistent-file-or-buffer nil)
(setq scroll-conservatively 10
      scroll-margin 15)
(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer buffer '(display-buffer-same-window))))
(setq left-margin-width 1)
(setq right-margin-width 1)
(set-window-buffer (selected-window) (current-buffer))
(set-window-buffer nil (current-buffer))
(setq insert-directory-program "/opt/homebrew/bin/gls")
(setq dired-use-ls-dired t)
(setq dired-listing-switches "-lGh1v --reverse --group-directories-first")
(setq large-file-warning-threshold 50000000)
(setq dired-kill-when-opening-new-dired-buffer t)
(setq org-use-sub-superscripts nil)
(setq column-number-mode t)
(setq electric-pair-inhibit-predicate
      (lambda (c)
        (if (char-equal c ?\") t (electric-pair-default-inhibit c))))
;; Some Macos nonsense
(setq mac-command-modifier 'meta)
(define-key key-translation-map (kbd "M-3") (kbd "#"))
; (define-key key-translation-map (kbd "M-£") (kbd "#"))
(define-key key-translation-map (kbd "H-3") (kbd "#"))
; (define-key key-translation-map (kbd "H-£") (kbd "#"))
; (define-key key-translation-map (kbd "S-3") (kbd "#"))
; (define-key key-translation-map (kbd "S-£") (kbd "#"))
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

;; Configure backups
(setq backup-by-copying t ;; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/Backups/"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/Backups/" t)))

;; Configure some dired filters called with '/ n'
(require 'ibuf-ext)
(add-to-list 'ibuffer-saved-filters
    '("non-denote-journals"
      (not (filename . "__journal\\.org$"))))
(add-to-list 'ibuffer-saved-filters
    '("all-denote-journals"
        (filename . "__journal\\.org$")))

;; -----------------------------------------------------------------------------
;; Configure themes
;; -----------------------------------------------------------------------------
;; Alternative themes: cloud-theme, modus-themes, timu-macos-theme

(use-package spacemacs-theme
  :ensure t
  :config
  (setq tw-light-theme 'spacemacs-light))

(use-package doom-themes
  :ensure t
  :config
  (setq tw-dark-theme 'doom-spacegrey))

;; FIXME: Combine following function into single toggle function
(defun tw/toggle-theme-light ()
		    (interactive)
		    (custom-set-faces
		     '(aw-leading-char-face
		       ((t (:inherit ace-jump-face-foreground :height 3.0 :foreground "DarkBlue")))))
		    (disable-theme (car custom-enabled-themes))
  		    (load-theme tw-light-theme))

(defun tw/toggle-theme-dark ()
		    (interactive)
		    (custom-set-faces
		     '(aw-leading-char-face
		       ((t (:inherit ace-jump-face-foreground :height 3.0 :foreground "DarkOrange")))))
		    (disable-theme (car custom-enabled-themes))
		    (load-theme tw-dark-theme))

(global-set-key (kbd "C-c l d") 'tw/toggle-theme-dark)
(global-set-key (kbd "C-c l l") 'tw/toggle-theme-light)
(load-theme tw-light-theme)

;; -----------------------------------------------------------------------------
;; Configure mode hooks
;; -----------------------------------------------------------------------------
(add-hook 'after-init-hook (lambda () (kill-buffer "*Messages*")))
(add-hook 'messages-buffer-mode-hook
          (lambda () (setq-local scroll-conservatively 101)))
;(add-hook 'post-command-hook #'tw/toggle-fill-column-indicator)
;(remove-hook 'post-command-hook #'tw/toggle-fill-column-indicator)
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'org-agenda-mode 'hl-line-mode)
(add-hook 'org-mode-hook 'hl-line-mode)
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))
(add-hook 'ibuffer-mode-hook (lambda () (hl-line-mode 1)))
(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'emacs-startup-hook #'tw/close-old-denote-journal-buffers)
(advice-add 'org-agenda :after #'tw/close-old-denote-journal-buffers)
(remove-hook 'prog-mode-hook 'display-line-numbers-mode)
;(remove-hook 'prog-mode-hook (setq display-line-numbers 'absolute) 'display-line-numbers-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq display-line-numbers 'relative)
            (display-line-numbers-mode 1)))
;(add-hook 'dired-mode-hook 'auto-revert-mode) ;; Auto-refresh dired on file change
;(add-hook 'elfeed-mode-hook (lambda () (local-set-key (kbd "g") #'elfeed-update)))
;(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "w") #'tw/dired-find-file-other-application)))
;(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "o") #'dired-find-file-other-window)))

;; -----------------------------------------------------------------------------
;; Configure custom keyboard shortcuts
;; -----------------------------------------------------------------------------
;; NOTE: Any keyboard shortcuts that are bound to external packages
;;       are defined within the (use-package) definition.
;;       Those keyboard shortcuts should also be referenced here for clarity
(global-set-key (kbd "C-c w") #'search-web)
(global-set-key (kbd "C-x ]") 'enlarge-window)
(global-set-key (kbd "C-c c f") 'global-display-fill-column-indicator-mode)
(global-set-key (kbd "C-c d t") 'tw/denote-search-by-tag-dired-ivy)
(global-set-key (kbd "C-c g") 'elpher)
(global-set-key (kbd "C-c j b") 'tw/create-jekyll-post)
(global-set-key (kbd "C-c t t") 'tw/toggle-transparency)
(global-set-key (kbd "C-x a s") 'async-shell-command)
(global-set-key (kbd "C-x v t") 'multi-vterm)
(global-set-key (kbd "C-x C-h") 'tw/highlight-line)
(global-set-key (kbd "C-c o a") 'org-agenda) ;; FIXME: move to org use-package
(global-set-key (kbd "C-c f") 'tw/dired-filter-files)
(global-set-key (kbd "C-c b") 'ivy-switch-buffer-other-window)
;; This isn't needed due to moving to Denote
;; Denote analogue is to simply call 'denote (denote-create-note) or 'denote-create-journal
;(global-set-key (kbd "C-c c c") 'org-capture) ;; FIXME :move to org use-package
(global-set-key (kbd "C-c c c") 'dneote)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x k") 'kil-buffer)
(global-set-key (kbd "M-n") 'scroll-up-command)
(global-set-key (kbd "M-p") 'scroll-down-command)
(global-set-key (kbd "M-,") 'beginning-of-buffer)
(global-set-key (kbd "M-.") 'end-of-buffer)
(global-set-key (kbd "C-x C-l") 'avy-goto-line)
(global-set-key (kbd "C-x C-x") 'avy-goto-char-timer)
;; Make M-f and M-b behave like Vim's w and b
(global-set-key (kbd "M-f") #'forward-to-word)
(global-set-key (kbd "M-b") (lambda () (interactive) (backward-word) (forward-to-word 0)))
(global-set-key (kbd "C-c C-o") 'browse-url-of-dired-file)
(global-set-key (kbd "C-c h") 'dired-dotfiles-toggle)
;(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch-backward)
(global-set-key (kbd "C-c l c f") 'tw/list-files-changed-on-disk)
(global-set-key (kbd "C-c v") 'visual-line-mode)
(global-set-key (kbd "C-c =") 'balance-windows-area)
(global-set-key (kbd "C-c e") 'forward-sexp)
(global-set-key (kbd "C-c a") 'backward-sexp)
(global-set-key (kbd "C-c t h") 'tw/hide-org-tags) ;; FIXME: move to org use-package
(global-set-key (kbd "C-c i d") 'tw/insert-current-date)
(global-set-key (kbd "C-x w") 'tw/ivy-switch-to-window-by-buffer)
;(global-set-key (kbd "C-c t") (lambda () (interactive) (unless (derived-mode-p 'org-mode) (call-interactively 'tw/smart-open-line-above))))
;(global-unset-key (kbd "M-<return>"))
;(global-set-key (kbd "C-c d s") 'dired-mark-files-regexp)
;(global-set-key (kbd "C-c y") 'clipboard-yank)
;(global-set-key (kbd "C-c c w") 'clipboard-kill-ring-save)
;(global-set-key (kbd "C-x C-a") 'mark-whole-buffer)
;(global-set-key (kbd "C-c C-f") 'find-name-dired)
;(global-set-key (kbd "C-c C-d C-s") 'consult-notes)
;(global-set-key (kbd "C-x r e") 'eval-region)
;(global-set-key (kbd "C-x r b") 'eval-buffer)
;(global-setkey (kbd "c-c c-l") 'package-list-packages)
;(global-set-key (kbd "C-c y") 'popup-kill-ring)
;(global-set-key (kbd "C-c w") 'make-frame)
;(global-set-key (kbd "C-x w t") 'tw/toggle-window-dedication)
;; Uncomment after adding hook to disable in org-mode
; (global-set-key (kbd "C-<return>") (lambda () (interactive) (tw/smart-open-line-below)))
; (global-set-key (kbd "M-<return>") 'tw/smart-open-line-above)

;; -----------------------------------------------------------------------------
;; Configure external packages
;; -----------------------------------------------------------------------------
(use-package mastodon
    :ensure t
    :config
    (setq mastodon-instance-url "https://dice.camp"
          mastodon-active-user "Cthimothy")
    (mastodon-discover))

 
(use-package denote-menu
  :ensure t
  :config
  (define-key denote-menu-mode-map (kbd "c") #'denote-menu-clear-filters)
  (define-key denote-menu-mode-map (kbd "/ r") #'denote-menu-filter)
  (define-key denote-menu-mode-map (kbd "/ k") #'denote-menu-filter-by-keyword)
  (define-key denote-menu-mode-map (kbd "/ o") #'denote-menu-filter-out-keyword)
  (define-key denote-menu-mode-map (kbd "e") #'denote-menu-export-to-dired))


(use-package ivy-posframe
  :ensure t
  :config
  ;; display at `ivy-posframe-style'
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;  (setq ivy-posframe-parameters
;	'((left-fringe . 0)
;	  (right-fringe . 0)))
  (setq ivy-posframe-width-relative t)
  (setq ivy-posframe-height-relative t)
  (setq ivy-posframe-border-width 1)
;  (set-face-attribute 'ivy-posframe-border nil :background "#666666")
  (set-face-attribute 'ivy-posframe-border nil :background "#666666")
  (setq ivy-posframe-width 180
	ivy-posframe-height 10)
;  (setq ivy-posframe-width-relative-factor 0.62)
;  (setq ivy-posframe-height-relative-factor 0.1)
  (ivy-posframe-mode 1))

(use-package elpher
  :ensure t
  :config
  (add-hook 'elpher-mode 'hl-line-mode))


(use-package easysession
  :ensure t
  :commands (easysession-switch-to
             easysession-save-as
             easysession-save-mode
             easysession-load-including-geometry)

  :custom
  (easysession-mode-line-misc-info t))  ; Display the session in the modeline
;  (easysession-save-interval (* 10 60))  ; Save every 10 minutes

  :init
  (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 102)
  (add-hook 'emacs-startup-hook #'easysession-save-mode 103)
  (global-set-key (kbd "C-c s s") 'easysession-switch-to))


(use-package orderless
  :ensure t)


(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit))


(use-package visual-replace
  :defer t
  :bind (("C-c r" . visual-replace)
         :map isearch-mode-map
         ("C-c r" . visual-replace-from-isearch)))


(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-workspace-name t)
  (setq doom-modeline-modal t)
  (setq doom-modeline-buffer-file-name-style 'truncate-all)
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
;   '(aw-leading-char-face
;   ((t (:inherit ace-jump-face-foreground :height 3.0 :foreground "yellow"))))
   ))


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


(use-package noflet
  :ensure t)


(use-package xclip
  :ensure t
  :config
  (xclip-mode t))


(use-package swiper
  :ensure t)


(use-package counsel
  :ensure t)


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
  :hook
  (dired-mode . nerd-icons-dired-mode))


(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))


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
  :defer t
  :config
  (defvar tw/treemacs-denote-root (expand-file-name "~/Org/"))

  ;; Define the transformer to remove the Denote identifier
  (defun tw/treemacs-denote-transformer (filename full-path)
    "Strip Denote identifier prefix from filenames in ~/Org/."
    (if (and (string-prefix-p tw/treemacs-denote-root full-path)
             (string-match "\\`[0-9]\\{8\\}T[0-9]\\{6\\}--\$begin:math:text$.*\\$end:math:text$" filename))
        (match-string 1 filename)
      filename))

  (treemacs-modify-theme "Default"
    :files '((".*" tw/treemacs-denote-transformer)))

  (setq treemacs-width 85)
  (setq treemacs-follow-mode t)
  (setq treemacs-tag-follow-delay 0.5)

  (unless (treemacs-current-visibility)
    (treemacs)))


(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))


(use-package paredit
  :ensure t
  :config
  (add-hook 'ielm-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'paredit-mode)
  (define-key paredit-mode-map (kbd "RET") nil))
;  (define-key paredit-mode-map (kbd "C-j") 'paredit-newline))
;  (add-hook 'ielm-mode-hook 'g-ielm-init-history)



(use-package dashboard
  :ensure t
  :config
;;  (setq dashboard-week-agenda t)
  (setq dashboard-startup-banner 'official)
  (setq dashboard-center-content t)
  (setq dashboard-items '(
                          (agenda    . 10)
			  (recents   . 10)
                          (bookmarks . 5)
                          (projects  . 5)
                          (registers . 5)))
  (dashboard-setup-startup-hook))


(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure t
  :init

  :config
  ;(custom-set-faces
  ; '(org-agenda-structure
  ;   ((t (:foreground "LightSkyBlue" :weight bold :height 1.2 :underline nil :height 1.1))))
  ; '(org-agenda-date-today ((t (:weight bold :height 1.4 :foreground "LightSkyBlue")))))
  
  (add-hook 'org-agenda-mode-hook 'hl-line-mode)
  (add-hook 'org-mode-hook 'hl-line-mode)
  (add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
  (add-hook 'org-agenda-before-read-hook
            (lambda ()
              (setq org-agenda-files
                    (directory-files-recursively "~/Org/" "\\.org$"))))
  ;(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "M-<return>") 'org-meta-return)))
  (global-set-key (kbd "C-S-<up>") 'org-move-subtree-up) 
  (global-set-key (kbd "C-S-<down>") 'org-move-subtree-down)

(setq org-adapt-indentation t)

  (setq org-agenda-window-setup 'current-window)

  (setq org-agenda-files
	(directory-files-recursively "~/Org/" "\\.org$"))

  (setq org-archive-location "~/Projects/Backups/Archive.org::")

  (setq org-todo-keywords
	'((sequence "NEXT(n)" "TODO(t)" "IN-PROGRESS(i)" "UNSCHEDULED(u)" "PROJECT(p)" 
		    "|" 
		    "CANCELLED(c)" "DONE(d)")))

  (setq org-todo-keyword-faces
	'(("TODO" . (:foreground "#CB7E65" :weight bold))
          ("NEXT" . (:foreground "#ECBE7B" :weight bold))
	  ("IN-PROGRESS" . (:foreground "#97B277" :weight bold))
	  ("UNSCHEDULED" . (:foreground "#44b9b1" :weight bold))
	  ("PROJECT" . (:foreground "#c678dd" :weight bold))
          ("DONE" . (:foreground "2257A0" :weight bold))
          ("CANCELLED" . (:foreground "#2257A0" :weight bold))))
  
  (setq org-agenda-sorting-strategy
	'((todo todo-state-up)))

  (setq org-agenda-prefix-format
	'((agenda . " %i %?-12t% s")
	  (todo . " %i ")
	  (tags . " %i ")
	  (search . " %i ")))

  (setq org-adapt-indentation t
	org-hide-leading-stars t
	org-hide-emphasis-markers t
	org-pretty-entities t
	org-ellipsis " ❱")


  (setq org-tag-alist
	'(("atheism" . ?A) ("ada" . ?a) ("adhd" . ?d) ("emacs" . ?e)
	  ("workflow" . ?f) ("programming" . ?g) ("thoughts" . ?h)
	  ("house" . ?H) ("inbox" . ?I) ("blog" . ?L) ("meeting" . ?m)
          ("homelab" . ?o) ("personal" . ?p) ("occult" .?l )
          ("rpg" . ?R) ("timesheet" . ?s) ("tasks" . ?T) ("to_read" . ?r)
          ("to_install" . ?i) ("to_listen" . ?l) ("to_buy" . ?b)	  	  	  
          ("to_watch" . ?w) ("work" . ?W)))

;  (setq denote-known-keywords (list "journal" "atheism" "work" "rpg" "radio" "emacs" 
;				    "family" "music" "books" "blog" "workflow"))

  (setq denote-known-keywords (list "atheism" "ada" "adhd" "emacs" "workflow" "programming""thoughts"
				    "house" "inbox" "blog" "meeting" "homelab" "personal" "occult" "retro"
				    "rpg" "timesheet" "tasks" "tech" "to_read" "to_install" "to_listen" 
				    "to_buy" "to_watch" "work"))

;;   (setq org-refile-targets
;;         '(("~/Org/20250305T141315--projects.org" :maxlevel . 1)
;;           ("~/Org/20250304T113200--tasks.org" :maxlevel . 1)
;;           ("~/Org/20250212T142617--atheism__atheism.org" :maxlevel . 1)
;;           ("~/Org/20250305T141314--emacs.org" :maxlevel . 1)
;;           ("~/Org/20250304T152326--rpg.org" :maxlevel . 1)
;;           ("~/Org/20250305T073302--work.org" :maxlevel . 2)))
;;   ;; NOTE: Refile Target for current daily journal is set in Denote use-package section
;;   ;; FIXME: It's still not working

;; (add-to-list 'org-refile-targets
;;              (let ((latest-journal (car (last (denote-journal-extras--entry-today)))))
;;                (cons latest-journal '(:maxlevel . 1))))

(setq org-refile-targets
      (mapcar (lambda (f)
                (cons f '(:maxlevel . 2)))
              (seq-filter (lambda (file)
                            (not (string-match-p "@" (file-name-nondirectory file))))
                          (directory-files-recursively "~/Org/" "\\.org$"))))
)
;; End of Org configuration
;; -----------------------------------------------------------------------------


(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.0 :weight bold))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
  (setq org-superstar-leading-bullet ?\s)
  (org-superstar-mode t))


(use-package denote
  :ensure t
  :init
  (setq denote-directory "~/Org/")
  (setq denote-journal-directory "~/Org/Journal/")
  :config
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  ;; (add-hook 'find-file-hook #'denote-link-buttonize-buffer)

  (setq denote-known-keywords (list "atheism" "ada" "adhd" "emacs" "workflow" "programming""thoughts"
				    "house" "inbox" "blog" "meeting" "homelab" "personal" "occult" "retro"
				    "rpg" "timesheet" "tasks" "tech" "to_read" "to_install" "to_listen" 
				    "to_buy" "to_watch" "work"))

  (setq denote-sort-dired-extra-prompts '(sort-by-component reverse-sort))

;  (global-set-key (kbd "C-c d n") 'denote-create-note)
  (global-set-key (kbd "C-c d n") 'denote)
  (global-set-key (kbd "C-c d f") 'consult-notes)
  (global-set-key (kbd "C-c d m f") 'denote-menu-filter-by-keyword)
  (global-set-key (kbd "C-c d g") 'find-grep-dired)
  (global-set-key (kbd "C-c d j o") 'denote-sort-dired)
  (global-set-key (kbd "C-c d j n") 'tw/denote-journal)
  ; (global-set-key (kbd "C-c d j o") 'tw/dired-find-file-split-below)
  (global-set-key (kbd "C-c d j j") 'tw/journelly-to-denote-journal)
  ; (global-set-key (kbd "C-c d j n") 'denote-journal-extras-new-or-existing-entry)
  (global-set-key (kbd "C-c d o") (lambda ()
				    (interactive)
				    (dired denote-directory)))

  (global-set-key (kbd "C-c d j o") (lambda ()
				      (interactive)
				      (dired (concat denote-directory "/Journal/"))))

  (denote-rename-buffer-mode)
  (require 'denote-journal-extras)
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

(defun tw/denote-journal ()
  "Open today's Denote journal if it exists, otherwise create it with a template."
  (interactive)
  (let* ((denote-directory "~/Org/Journal/")
         (today (format-time-string "%Y%m%d"))
         (files (directory-files denote-directory t "\\.org$"))
         (existing-file (seq-find (lambda (file)
                                    (and (string-match-p today file)
                                         (string-match-p "journal" file)))
                                  files)))
    (if existing-file
        (find-file existing-file)
      (let ((denote-directory denote-directory)) ; set locally for denote
        (denote (format-time-string "%A %e %B %Y") '("journal"))
        (goto-char (point-max))
        (insert "* Daily Morning Routine
- [ ] tw/journelly-to-denote (C-c d j j)
- [ ] Review yesterday's journal (C-c d j o)
- [ ] Review and re-file today's scheduled tasks (C-c o a a)
- [ ] Review and re-file all other tasks (C-c o a t)
- [ ] Review [[denote:20250413T135725][daily workflow]]
- [ ] Check for changed files (C-c l c f)
- [ ] Review Outlook calendar
- [ ] Review Raindrop INBOX https://app.raindrop.io/my/-1
- [ ] Check 2025 Reading List
- [ ] Add anything to To Read/Listen/Watch etc.
- [ ] Set two hour Pomodoro


* Tasks


* Notes
")
;        (add-to-list 'org-refile-targets
;                     `((,(denote-journal-extras--entry-today)) . (:maxlevel . 2)))
))))
    ;; FIXME: This isn't working


  (defun tw/denote-find-file ()
    "Use Ivy to finda Denote file."
    (interactive)
    (let ((default-directory denote-directory))
      (ivy-read "Find Denote file: "
		(directory-files denote-directory nil "^[^.].*\\.org$")
		:action (lambda (file)
                          (find-file (expand-file-name file denote-directory))))))
)
;; End of Denote configuration
;; -----------------------------------------------------------------------------


(unless (get-buffer "*dashboard*")
  (dashboard-open))
