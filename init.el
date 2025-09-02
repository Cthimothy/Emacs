;; -----------------------------------------------------------------------------
;; Initial Configuration
;; -----------------------------------------------------------------------------
(setq debug-on-error nil)
(setq debug-on-quit nil)

(load "server")
(unless (server-running-p)
  (server-start))
(setq default-frame-alist '((ns-transparent-titlebar . t) (ns-appearance . dark)))
(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 4 1024 1024))
(setq frame-resize-pixelwise t)
(setq org-element-cache-persistent t)
(setq browse-url-browser-function 'browse-url-default-browser)
(setq ns-use-proxy-icon nil) ;; Remove icon in centre of title bar

(setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin"))
(setq exec-path (append exec-path '("/opt/homebrew/bin")))

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
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

(when (member "Iosevka" (font-family-list))
  (set-face-attribute 'default nil
                      :family "Iosevka"
                      :height 125
                      :weight 'medium
                      :slant 'normal
                      :width 'normal))

;; (set-face-attribute 'default nil
;;                     :family "Maple Mono"
;;                     :height 124
;;                     :weight 'normal
;;                     :slant 'normal
;;                     :width 'normal)


;; (set-face-attribute 'default nil
;;                     :family "Geist Mono"
;;                     :height 124
;;                     :weight 'normal
;;                     :slant 'normal
;;                     :width 'normal)

(custom-set-faces
 '(region ((t (:background "#FFEFD5" :foreground nil)))))

;;(add-to-list 'custom-theme-load-path "~/Projects/Code/Elisp/Themes/")


;; Orange link style
; (set-face-attribute 'link nil :foreground "#ee6600" :underline t)
;; Soft red link style
; (set-face-attribute 'link nil :foreground "#C45B54" :underline t)

(set-frame-parameter (selected-frame) 'alpha '(100 100))

;; -----------------------------------------------------------------------------
;; Make Ielm Great Again
;; -----------------------------------------------------------------------------
(with-eval-after-load 'ielm
(add-hook 'ielm-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'paredit-mode)
(define-key paredit-mode-map (kbd "RET") nil)
(define-key paredit-mode-map (kbd "C-j") 'paredit-newline))

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

(with-eval-after-load 'ielm
  (define-key inferior-emacs-lisp-mode-map (kbd "C-l") 'comint-clear-buffer))

(with-eval-after-load 'elm
  (define-key inferior-emacs-lisp-mode-map (kbd "C-r")
              'helm-comint-input-ring))

;; -----------------------------------------------------------------------------
;; Load custom code from external files
;; -----------------------------------------------------------------------------
;(add-to-list 'load-path "~/Projects/Code/Elisp/tag-explorer/")
;(require 'tag-explorer)
;(add-to-list 'load-path "~/Projects/Code/Elisp/journelly-to-denote/")
;(require 'journelly-to-denote)

;(add-to-list 'load-path "~/Projects/Code/Elisp/denote-tag-find-dired/")
;(require 'denote-tag-find-dired)

;(add-to-list 'load-path "~/Projects/Code/Elisp/gptel/")
;(require 'gptel-setup)

;(add-to-list 'load-path "~/Projects/Code/Elisp/key-logger/")
;(require 'key-logger)

;; (add-to-list 'load-path "~/Projects/Code/Elisp/denote-keyword-browser/")
;; (require 'denote-keyword-browser)
;; (global-set-key (kbd "C-c d k") #'tw/denote-keyword-browser)

;; -----------------------------------------------------------------------------
;; Define custom functions
;; NOTE: All org-mode related functions defined within (use-package org-mode)
;; -----------------------------------------------------------------------------

;; (defun tw/org-end-then-insert-subheading ()
;;   "Move to end of line, then insert an Org subheading."
;;   (interactive)
;;   (end-of-line)
;;   (org-insert-subheading nil))
;; (define-key org-mode-map (kbd "C-c <return>") #'tw/org-end-then-insert-subheading)


(defun tw/find-grep-dired-ignore-case (dir pattern)
  "Run find-grep-dired in DIR searching for PATTERN, ignoring case."
  (interactive
   (list (read-directory-name "Find-grep (directory): " default-directory "")
         (read-string "Search for (pattern): " (thing-at-point 'symbol))))
  (let ((find-grep-options "-exec grep -i -nH -e "))
    (find-grep-dired dir (concat "grep -i -nH -e " (shell-quote-argument pattern)))))


;; (defun tw/org-checkbox-reading-cycle ()
;;   "Cycle checkbox between [ ] (not read), [.] (reading), and [X] (finished)."
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-line)
;;     (when (re-search-forward "\\[[ ~X]\\]" (line-end-position) t)
;;       (let ((current-state (match-string 0)))
;;         (replace-match 
;;          (cond
;;  ((string= current-state "[ ]") "[~]")
;;  ((string= current-state "[~]") "[X]")
;;  ((string= current-state "[X]") "[ ]")
;;           (t "[ ]"))
;;          t t)
;;         t))))
;; ;;(add-hook 'org-ctrl-c-ctrl-c-hook 'tw/org-checkbox-reading-cycle)
(remove-hook 'org-ctrl-c-ctrl-c-hook 'tw/org-checkbox-reading-cycle)


(defun tw/insert-src-block (&optional language)
  "Insert an Org source block with LANGUAGE and position cursor inside.
If LANGUAGE is not provided, prompt for it with completion."
  (interactive 
   (list (completing-read "Language: " 
                         '("elisp" "python" "bash" "sh" "C" "C++" "rust" "java" "javascript" "typescript"
                           "css" "scss" "html" "xml" "yaml" "json" "sql" "org" "text" "markdown")
                         nil nil nil nil "elisp")))
  (let ((start (point)))
    (insert (format "#+begin_src %s\n\n#+end_src\n" language))
    (goto-char (+ start 13 (length language)))
    (recenter)))
; (global-set-key (kbd "C-c i c b") 'tw/insert-src-block)


(defun unfuck-all ()
  (interactive)
  (setq org-agenda-files
	(list 
	 (expand-file-name "Personal.org" org-directory)
	 (expand-file-name "Work.org"     org-directory)
	 (expand-file-name "Journal.org"  org-directory)))

  ;; (set-face-attribute 'org-special-keyword nil :foreground "#cBc5c4" :slant 'italic)
  ;; (set-face-attribute 'org-drawer         nil :foreground "LightSlateGray" :slant 'italic)
  ;; (set-face-attribute 'region             nil :background "#FFEFD5" :foreground 'unspecified)
  ;; (set-face-attribute 'org-tag            nil :foreground "gray60" :height 0.85 :weight 'normal)
  ;; (custom-set-faces
  ;;  '(org-level-1  ((t (:foreground "#2f72b8" :weight normal :underline nil)))) ; blue
  ;;  '(org-level-2  ((t (:foreground "#b35860" :weight normal))))              ; muted rose
  ;;  '(org-level-3  ((t (:foreground "#7a5c8e" :weight normal))))              ; eggplant
  ;;  '(org-level-4  ((t (:foreground "#b37544" :weight normal))))              ; amber brown
  ;;  '(org-level-5  ((t (:foreground "#a0675a" :weight normal))))
  ;;  '(org-level-6  ((t (:foreground "#56749f" :weight normal))))
  ;;  '(org-level-7  ((t (:foreground "#6c5e75" :weight normal))))
  ;;  '(org-level-8  ((t (:foreground "#7d8484" :weight normal))))
  ;;  '(org-level-9  ((t (:foreground "#435470" :weight normal))))
  ;;  '(org-level-10 ((t (:foreground "#aa5d45" :weight normal)))))



  ;; (setq org-refile-targets
  ;; 	(mapcar (lambda (f)
  ;;                 (cons f '(:maxlevel . 2)))
  ;; 		(seq-filter (lambda (file)
  ;;                             (not (string-match-p "@" (file-name-nondirectory file))))
  ;;                           (directory-files-recursively "~/Org/" "\\.org$"))))

;  (setq org-agenda-files
;  	(directory-files-recursively "~/Org/" "\\.org$"))

  ;; (setq org-adapt-indentation t
  ;; 	org-hide-leading-stars t
  ;; 	org-hide-emphasis-markers t
  ;; 	org-pretty-entities t
  ;; 	org-ellipsis " ❱") 

  (vertico-mode)
  (vertico-posframe-cleanup)

  ;; (set-face-attribute 'link nil
  ;;                     :foreground "#C45B54"
  ;;                     :underline t)
 (custom-set-faces
  '(font-lock-comment-face ((t (:foreground "#9a9a9a" :slant italic :background nil))))))

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
;; Keyboard shortcut: C-c t]


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


(defun tw/vertico-switch-to-window-by-buffer ()
  "Use Vertico to switch to a window displaying a selected buffer."
  (interactive)
  (let* ((window-buffer-alist
          (mapcar (lambda (w)
                    (cons (buffer-name (window-buffer w)) w))
                  (window-list)))
         (buffer-names (mapcar #'car window-buffer-alist))
         (selected-buffer (completing-read "Switch to window displaying buffer: " buffer-names)))
    (when selected-buffer
      (select-window (cdr (assoc selected-buffer window-buffer-alist))))))
; (global-set-key (kbd "C-x w") 'tw/vertico-switch-to-window-by-buffer)


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
;  (insert (format-time-string "%Y-%m-%d %a")))
  (insert (format-time-string "%Y-%m-%d-%A")))

(defun tw/toggle-window-dedication ()
  (interactive)
  (set-window-dedicated-p (selected-window)
                          (not (window-dedicated-p (selected-window)))))
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
;; Keyboard shortcut C-c d f

(defun tw/dired-filter-out-files (string)
  "Filter Dired to hide files containing STRING."
  (interactive "sFilter out files containing: ")
  (dired-mark-files-regexp string)
  (dired-do-kill-lines)
  (message "Hiding files containing '%s' (use g to refresh)" string))
;;(global-set-key (kbd "C-c d F") 'tw/dired-filter-out-files)

(defun tw/dired-find-file-other-application ()
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)
    (message "Opening %s done" file)))

(defun tw/dired-find-file-other-window ()
  "Open the file in a vertical split to the right."
  (interactive)
  (let ((File (dired-get-file-for-visit)))
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

(defun tw/dired-open-in-middle-window ()
  "In Dired, open file in the middle window (window 2 of 3)."
  (interactive)
  (let* ((windows (window-list nil 'nomini))  ;; skip minibuffer window
         (middle-window (nth 1 windows))      ;; 0: left, 1: middle, 2: right
         (file (dired-get-file-for-visit)))
    (when (window-live-p middle-window)
      (select-window middle-window)
      (find-file file))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "]") #'tw/dired-open-in-middle-window))

;; -----------------------------------------------------------------------------
;; Set personal and Macbook specific defaults 
;; -----------------------------------------------------------------------------
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0) 
(show-paren-mode t)
;(prettify-symbols-mode)
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


(defvar tw/current-theme 'spacemacs-light
  "Current theme, either 'spacemacs-light or 'spacemacs-dark.")

(defun tw/apply-theme (theme)
  "Load THEME and apply any customizations."
  (load-theme theme t)
  (setq tw/current-theme theme)
  (tw/apply-theme-extras theme))


;; (defun tw/apply-theme-extras (theme)
;;   "Apply extras depending on THEME."
;;   (when (eq theme 'spacemacs-light)
;;     (set-face-attribute 'font-lock-comment-face nil
;;                         :foreground "#9a9a9a"
;;                         :background 'unspecified
;;                         :slant italic))
;;   (when (eq theme 'spacemacs-dark)
;;     ;; Reset to theme default
;;     (set-face-attribute 'font-lock-comment-face nil
;;                         :foreground 'unspecified
;;                         :background 'unspecified
;;                         :slant 'unspecified)))


(defun tw/apply-theme-extras (theme)
  "Apply extras depending on THEME."
  (when (eq theme 'spacemacs-light)
    (set-face-attribute 'font-lock-comment-face nil
                        :foreground "#9a9a9a"
                        :background 'unspecified
                        :slant 'italic))  ;; quote here
  (when (eq theme 'spacemacs-dark)
    (set-face-attribute 'font-lock-comment-face nil
                        :foreground 'unspecified
                        :background 'unspecified
                        :slant 'unspecified)))

(defun tw/toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (tw/apply-theme (if (eq tw/current-theme 'spacemacs-light)
                      'spacemacs-dark
                    'spacemacs-light)))

;; Set up the initial theme at startup
(mapc #'disable-theme custom-enabled-themes)
(tw/apply-theme tw/current-theme)

;; Optional: Bind C-c l t to toggle theme
(global-set-key (kbd "C-c t h") #'tw/toggle-theme)




;; (defvar tw/light-theme 'spacemacs-light)
;; (defvar tw/dark-theme 'doom-spacegrey)
;; (defvar tw/current-theme tw/light-theme)

;; (defun tw/apply-custom-faces ()
;;   ;; Comments
;;   (set-face-attribute 'font-lock-comment-face nil
;;                       :foreground "#9a9a9a"
;;                       :background nil
;;                       :slant 'italic)
;;   ;; Mode line (active)
;;   (set-face-attribute 'mode-line nil
;;                       :background "#4b3b61" :foreground "white" :box nil)
;;   ;; Mode line (inactive)
;;   (set-face-attribute 'mode-line-inactive nil
;;                       :background "#eaeaea" :foreground "#888888" :box nil)
;;   ;; Ace window leading char
;;   (set-face-attribute 'aw-leading-char-face nil
;;                       :inherit 'ace-jump-face-foreground
;;                       :height 10.0
;;                       :foreground (if (eq tw/current-theme tw/dark-theme)
;;                                       "DarkOrange"
;;                                     "DarkBlue")))

;; (add-hook 'after-load-theme-hook #'tw/apply-custom-faces)

;; (defun tw/toggle-theme ()
;;   (interactive)
;;   (disable-theme (car custom-enabled-themes))
;;   (setq tw/current-theme (if (eq tw/current-theme tw/light-theme)
;;                              tw/dark-theme
;;                            tw/light-theme))
;;   (load-theme tw/current-theme t))  ;; triggers after-load-theme-hook

;; (global-set-key (kbd "C-c l t") #'tw/toggle-theme)

;; (load-theme tw/current-theme t)









;; (use-package spacemacs-theme
;;   :ensure t
;;   :config
;;   (set-face-attribute 'font-lock-comment-face nil
;; 		      :foreground "#9a9a9a"  ;; soft neutral grey
;; 		      ;;:foreground "#a0847c"  ;; subtle warm brownish-grey
;; 		      :background nil
;; 		      :slant 'italic)
;;   (setq tw-light-theme 'spacemacs-light))

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (setq tw-dark-theme 'doom-spacegrey))

;; ;; FIXME: Combine following function into single toggle function
;; (defun tw/toggle-theme-light ()
;; 		    (interactive)
;; 		    (custom-set-faces
;; 		     '(aw-leading-char-face
;; 		       ((t (:inherit ace-jump-face-foreground :height 10.0 :foreground "DarkBlue")))))
;; 		    (disable-theme (car custom-enabled-themes))
;; 		    (load-theme tw-light-theme))


;; (defun tw/toggle-theme-dark ()
;; 		    (interactive)
;; 		    (custom-set-faces
;; 		     '(aw-leading-char-face
;; 		       ((t (:inherit ace-jump-face-foreground :height 10.0 :foreground "DarkOrange")))))
;; 		    (disable-theme (car custom-enabled-themes))
;; 		    (load-theme tw-dark-theme))

;; (global-set-key (kbd "C-c l d") 'tw/toggle-theme-dark)
;; (global-set-key (kbd "C-c l l") 'tw/toggle-theme-light)
;; (load-theme tw-light-theme)

;; -----------------------------------------------------------------------------
;; Configure mode hooks
;; -----------------------------------------------------------------------------
(add-hook 'shell-mode-hook
          (lambda ()
            (when (string= (buffer-name) "*Async Shell Command*")
              (add-hook 'compilation-finish-functions
                        (lambda (buf msg)
                          (message "Async command finished: %s" msg))
                        nil t))))

;(add-hook 'after-init-hook (lambda () (kill-buffer "*Messages*")))
(add-hook 'messages-buffer-mode-hook
          (lambda () (setq-local scroll-conservatively 101)))
;(add-hook 'post-command-hook #'tw/toggle-fill-column-indicator)
;(remove-hook 'post-command-hook #'tw/toggle-fill-column-indicator)
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
;(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; Use relative line numbers in programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (setq display-line-numbers-type 'absolute)
            (display-line-numbers-mode 1)))

(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'org-agenda-mode 'hl-line-mode)
(add-hook 'org-mode-hook 'hl-line-mode)
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))
(add-hook 'ibuffer-mode-hook (lambda () (hl-line-mode 1)))
(add-hook 'text-mode-hook #'visual-line-mode)
;(add-hook 'emacs-startup-hook #'tw/close-old-denote-journal-buffers)
;(advice-add 'org-agenda :after #'tw/close-old-denote-journal-buffers)
;(advice-remove 'org-agenda #'tw/close-old-denote-journal-buffers)
;(remove-hook 'prog-mode-hook 'display-line-numbers-mode)
;(remove-hook 'prog-mode-hook (setq display-line-numbers 'absolute) 'display-line-numbers-mode)
;(add-hook 'dired-mode-hook 'auto-revert-mode) ;; Auto-refresh dired on file change
;(add-hook 'elfeed-mode-hook (lambda () (local-set-key (kbd "g") #'elfeed-update)))
;(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "w") #'tw/dired-find-file-other-application)))
;(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "o") #'dired-find-file-other-window)))

;; -----------------------------------------------------------------------------
;; Configure custom global key bindings keyboard shortcuts
;; -----------------------------------------------------------------------------
;; NOTE: Any keyboard shortcuts that are bound to external packages:
;;       are defined within the (use-package) definition.
;;       Those keyboard shortcuts should also be referenced here for clarity
(global-set-key (kbd "C-c s l") 'org-store-link)
(global-set-key (kbd "C-\\") 'undo-redo)
;(global-set-key (kbd "C-c d k") #'tw/denote-keyword-dired)
(global-set-key (kbd "C-c d j o")
                (lambda ()
                  (interactive)
                  (find-file "~/Library/Mobile Documents/iCloud~com~xenodium~Journelly/Documents/Journelly.org")))
(global-set-key (kbd "C-c d i g") #'tw/find-grep-dired-ignore-case)
(global-set-key (kbd "C-x w") 'tw/vertico-switch-to-window-by-buffer)
;(global-set-key (kbd "C-c d k") 'tw/denote-show-filename-keywords)
;(add-hook 'org-ctrl-c-ctrl-c-hook 'tw/org-checkbox-reading-cycle)
;(add-hook 'org-ctrl-c-ctrl-c-hook 'tw/org-checkbox-reading-cycle)
(global-set-key (kbd "C-c j") (lambda () (interactive) (info "/usr/local/share/info/jargon.info.gz")))
(global-set-key (kbd "C-c i c b") 'tw/insert-src-block)
(global-set-key (kbd "C-c w") #'search-web)
(global-set-key (kbd "C-x ]") 'enlarge-window)
(global-set-key (kbd "C-c c f") 'global-display-fill-column-indicator-mode)
;(global-set-key (kbd "C-c d t") 'tw/denote-search-by-tag-dired-ivy)
;(global-set-key (kbd "C-c d m") #'denote-menu-list-notes)
;(global-set-key (kbd "C-c g") 'elpher)
(global-set-key (kbd "C-c c b") 'tw/create-jekyll-post)
(global-set-key (kbd "C-c t t") 'tw/toggle-transparency)
(global-set-key (kbd "C-x a s") 'async-shell-command)
(global-set-key (kbd "C-x v t") 'multi-vterm)
;(global-set-key (kbd "C-x v t") 'eshell)
(global-set-key (kbd "C-x C-h") 'tw/highlight-line)
(global-set-key (kbd "C-c o a") 'org-agenda) ;; FIXME: move to org use-package
(global-set-key (kbd "C-c d f") 'tw/dired-filter-files)
(global-set-key (kbd "C-c d F") 'tw/dired-filter-out-files)
;(global-set-key (kbd "C-c b") 'ivy-switch-buffer-other-window)
;; This isn't needed due to moving to Denote
;; Denote analogue is to simply call 'denote (denote-create-note) or 'denote-create-journal
;(global-set-key (kbd "C-c c c") 'org-capture) ;; FIXME :move to org use-package
;(global-set-key (kbd "C-c c c") 'dneote)
;(global-set-key (kbd "C-c C-b") 'ibuffer)
(global-set-key (kbd "C-x b") #'switch-to-buffer)
(global-set-key (kbd "C-x C-b") #'nil)
(global-set-key (kbd "C-x k") 'kill-buffer)
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
;(global-set-key (kbd "C-x w") 'tw/ivy-switch-to-window-by-buffer)
;(global-set-key (kbd "C-c t") (lambda () 
;				(interactive) (unless (derived-mode-p 'org-mode) 
;						(call-interactively 'tw/smart-open-line-above))))
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
                
(global-set-key (kbd "C-c i j")
  (lambda ()
    (interactive)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (let ((date (format-time-string "%d-%A"))) ; e.g., "15-Tuesday"
      (insert (concat "***** _" date "_\n"

"****** 🌞 Morning Routine
- [ ] Review Journelly (C-c d j j)
- [ ] Review Inbox.org
- [ ] Review yesterday's Daily Journal (C-c d j o)
- [ ] Review and re-file today's scheduled tasks (C-c o a a)
- [ ] Review and re-file all other tasks (C-c o a t)
- [ ] Review Daily [[*Workflows][Workflows]]
- [ ] Check for changed files (C-c l c f)
- [ ] Review Outlook calendar
- [ ] Review Raindrop Inbox https://app.raindrop.io/my/-1
- [ ] Update and Review 2025 Reading List
- [ ] Review To Read/Listen/Watch etc.
- [ ] Review [[file:20250510T113031--personal-development-goals__personal.org][Personal Development Goals]]
- [ ] Set two hour Pomodoro
****** 🌙 Evening Routine
- [ ] Review today's journal
******* Closing thoughts
****** ✍️ Notes
****** 🧠 Thoughts  :thoughts:
****** 🌀 Mood
****** ☑️ Tasks")))))


;; ;; ------------------------------
;; ;; Configure eshell
;; ;; ------------------------------
;; (require 'ansi-color)
;; (add-hook 'eshell-preoutput-filter-functions #'ansi-color-apply)
;; (setq eshell-term-name "xterm-256color")

;; ;; === PATH Setup ===
;; (defun tw/eshell-setup-path ()
;;   "Set PATH and exec-path to match your .bashrc."
;;   (let ((my-paths
;;          '("/opt/homebrew/bin"
;;            "/usr/local/bin"
;;            "/System/Cryptexes/App/usr/bin"
;;            "/usr/bin"
;;            "/bin"
;;            "/usr/sbin"
;;            "/sbin"
;;            "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin"
;;            "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin"
;;            "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin"
;;            "/Applications/Privileges.app/Contents/Resources"
;;            "/Applications/iTerm.app/Contents/Resources/utilities"
;;            "/opt/homebrew/opt/node@18/bin")))
;;     (dolist (p my-paths)
;;       (add-to-list 'exec-path p))
;;     (setenv "PATH" (string-join (reverse my-paths) ":"))))

;; ;; === Prompt Faces ===
;; (defface my-eshell-paren-face
;;   '((t :foreground "#556B2F")) ;; dark olive green
;;   "Face for parentheses in prompt.")

;; (defface my-eshell-cwd-face
;;   '((t :foreground "#4682B4")) ;; steel blue
;;   "Face for current directory in prompt.")

;; (defface my-eshell-prompt-face
;;   '((t :foreground "#DAA520")) ;; goldenrod
;;   "Face for prompt symbol.")

;; ;; === Prompt Format ===
;; (setq eshell-prompt-function
;;       (lambda ()
;;         (let ((cwd (abbreviate-file-name (eshell/pwd))))
;;           (concat
;;            "\n"
;;            (propertize "(" 'face 'my-eshell-paren-face)
;;            (propertize cwd 'face 'my-eshell-cwd-face)
;;            (propertize ")" 'face 'my-eshell-paren-face)
;;            "\n"
;;            (propertize "λ " 'face 'my-eshell-prompt-face)
;;            (propertize " " 'face 'default)))))

;; (setq eshell-prompt-regexp "λ ")

;; ;; Use Emacs Lisp ls instead of system ls
;; (setq eshell-use-ls-lisp t)

;; ;; Configure ls-lisp for better output
;; (setq ls-lisp-use-insert-directory-program nil)
;; (setq ls-lisp-dirs-first t)
;; (setq ls-lisp-use-color t)


;; ;; === Aliases ===

;; (fmakunbound 'tw/eshell-set-aliases)
;; (defun tw/eshell-set-aliases ()
;;   "Set up aliases similar to your .bash_aliases."
;;   (eshell/alias "ll" "ls -lhF --color=always")
;;   (eshell/alias "ls" "ls -F --color=always")
;;   (eshell/alias "less" "bat"))

;; ;; === Eshell Init ===
;; (defun tw/eshell-setup ()
;;   (tw/eshell-setup-path)
;;   (tw/eshell-set-aliases))

;; (add-hook 'eshell-first-time-mode-hook #'tw/eshell-setup)

;; (with-eval-after-load 'eshell
;;   (define-key eshell-mode-map (kbd "C-p") #'eshell-previous-input)
;;   (define-key eshell-mode-map (kbd "C-n") #'eshell-next-input))

;; (with-eval-after-load 'eshell
;;   (bind-keys*
;;    :map eshell-mode-map
;;    ("C-l" . (lambda ()
;;               (interactive)
;;               (let ((inhibit-read-only t))
;;                 (erase-buffer)
;;                 (eshell-insert-prompt))))))

;; ;; === Eat Integration ===
;; (use-package eat
;;   :ensure t
;;   :config
;;   ;; Activate Eat modes inside eshell
;;   (add-hook 'eshell-first-time-mode-hook #'eat-eshell-mode)
;;   (add-hook 'eshell-first-time-mode-hook #'eat-eshell-visual-command-mode)

;;   ;; Clear screen in Eat mode and force a new prompt
;;   (with-eval-after-load 'eat
;;     (define-key eat-mode-map (kbd "C-l")
;;       (lambda ()
;;         (interactive)
;;         (let ((inhibit-read-only t))
;;           (erase-buffer)
;;           (eat-send "\n"))))))



;; -----------------------------------------------------------------------------
;; Configure external packages
;; ----------------------------------------------------------------------------
(use-package elfeed
  :ensure t
  :config
  (elfeed-load-opml "~/.emacs.d/elfeed.opml"))

(unload-feature 'elfeed t)


(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

(use-package gptel
  :ensure t
  :config
  (setq gptel-backend
        (gptel-make-ollama "Ollama"
          :host "localhost:11434"
          :stream t
          :models '(llama3:8b)))
  (setq gptel-model 'llama3:8b)
;;  (setq gptel-default-mode 'org-mode)
  (setq gptel-default-mode 'markdown-mode)

  (setq display-buffer-alist
	(cons
	 '("\\*Ollama\\*"  ;; Match your GPTel buffer
           (display-buffer-same-window))
	 display-buffer-alist))

;; (defun tw/gptel-ret-smart ()
;;   "Send prompt with RET if point is at end of buffer, else insert newline."
;;   (interactive)
;;   (if (eobp)
;; 	(gptel-send)
;;     (org-return)))


;; (add-hook 'gptel-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "RET") #'tw/gptel-ret-smart)))

(add-hook 'gptel-mode-hook
          (lambda () (local-unset-key (kbd "RET"))))

(remove-hook 'gptel-mode-hook #'tw/gptel-set-ret-key)

;; (defun tw/gptel-buffer-setup (_start _end)
;;   "Force Org-mode visual customizations in the GPTel buffer after response."
;;   (when (and (derived-mode-p 'org-mode)
;;              (string-match-p "\\*Ollama\\*" (buffer-name)))
;;     ;; Force org-superstar-mode
;;     (setq org-hide-leading-stars t)
;;     (org-superstar-mode 1)

;;     (org-indent-mode 1)
;;     (visual-line-mode 1)
;;     (setq-local line-spacing 0.2)

;;     ;; Ensure font settings are applied
;;     (face-remap-add-relative 'default :family "Iosevka" :height 130)))



(defun tw/gptel-buffer-setup ()
  "Force Org-mode visual customizations in the GPTel buffer after response."
  (when (and (derived-mode-p 'org-mode)
             (string-match-p "\\*Ollama\\*" (buffer-name)))
    (setq org-hide-leading-stars t)
    (org-superstar-mode 1)
    (org-indent-mode 1)
    (visual-line-mode 1)
    (setq-local line-spacing 0.2)
    (face-remap-add-relative 'default :family "Iosevka" :height 130)))

(defun tw/gptel-flatten-org-headings (_start _end)
  "Demote all headings in GPTel buffer to level 1."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*\\{2,\\} " nil t)
        (replace-match "* ")))))

(add-hook 'gptel-post-response-hook #'tw/gptel-flatten-org-headings)

;; (defun tw/gptel-jump-to-next-input (_start _end)
;;   "After response, jump to the next input prompt (Org heading) and position cursor after it."
;;   (when (derived-mode-p 'org-mode)
;;     (goto-char (point-max))
;;     (when (re-search-forward org-heading-regexp nil t)
;;       (end-of-line)
;;       ;; Move to beginning of next line, creating one if needed
;;       (unless (eobp)
;;         (forward-line))
;;       (when (eobp)
;;         (insert "\n")))))

;; For org-mode
(defun tw/gptel-jump-to-next-input (_start _end)
  "After response, jump to the next input prompt and position cursor inline after '* USER '."
  (when (derived-mode-p 'org-mode)
    (goto-char (point-max))
    (when (re-search-backward org-heading-regexp nil t)
      (end-of-line) ;; Go to the end of '* USER'
      (insert " ")  ;; Add a space so typing starts after heading
      )))
;(fmakunbound tw/gptel-jump-to-next-input)

;; For markdown mode
(defun tw/gptel-jump-to-next-input-md (_start _end)
  "After response, jump to the next input prompt in Markdown and position cursor inline after '### USER '."
  (when (derived-mode-p 'markdown-mode)
    (goto-char (point-max))
    (when (re-search-backward "^### " nil t)
      (end-of-line)
      (insert " "))))

  (defun tw/gptel-send-region-with-prompt (start end prompt)
    "Send selected region with a custom prompt to the *Ollama* GPTel buffer."
    (interactive
     (list (region-beginning)
           (region-end)
           (read-string "Ask GPT about the region: " " ")))
    (let* ((text (buffer-substring-no-properties start end))
           (message (concat prompt "\n\n" text))
           (buf-name "*Ollama*"))
      (let ((buf (get-buffer buf-name)))
        (if buf
            (with-current-buffer buf
              (goto-char (point-max))
              (insert message)
              (gptel-send)
;              (pop-to-buffer buf))
;              (switch-to-buffer buf))
              (pop-to-buffer buf '(display-buffer-reuse-window . ((inhibit-same-window . nil)))))
          (message "No GPTel buffer found named %s" buf-name)))))

  (define-prefix-command 'tw/gptel-prefix)
  (global-set-key (kbd "C-c g") 'tw/gptel-prefix)
  (define-key tw/gptel-prefix (kbd "p") #'tw/gptel-send-region-with-prompt)
  (define-key tw/gptel-prefix (kbd "s") #'tw/gptel-summarise-region)
  (add-hook 'gptel-mode-hook #'tw/gptel-buffer-setup)
  (add-hook 'gptel-post-response-hook #'tw/gptel-jump-to-next-input)
  (add-hook 'gptel-post-response-hook #'tw/gptel-jump-to-next-input-md))


(use-package hackernews
  :ensure t)

(use-package consult
  :ensure t)


(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 3
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))


  ;; (use-package mastodon
  ;;   :ensure t
  ;;   :config
  ;;   (setq mastodon-instance-url "https://dice.camp"
  ;;         mastodon-active-user "Cthimothy")
  ;;   (mastodon-discover))
 

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
  (easysession-mode-line-misc-info nil)  ; Display the session in the modeline
;  (easysession-save-interval (* 10 60))  ; Save every 10 minutes

  :init
  (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 102)
  (add-hook 'emacs-startup-hook #'easysession-save-mode 103)
  (global-set-key (kbd "C-c s s") 'easysession-switch-to))


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
  (setq doom-modeline-Buffer-File-name-style 'truncate-all)
  (setq doom-modeline-total-line-number t)
  (setq doom-modeline-buffer-file-name-style 'auto)
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
  :bind (("C-x C-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?q ?w ?e ?z ?x))
;  (setq aw-keys '(?d ?x ?z ?w ?q ?s ?a))
;  (setq aw-keys '(?w ?z ?q ?s ?a))
  (setq aw-scope 'frame)
  (setq aw-ignore-current t)
  (setq aw-background t)
  (setq aw-leading-char-style 'char)     ; ← centers the letter in window
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:foreground "#b35860" :weight bold :box nil :height 3.0))))))


(use-package vertico
  :ensure t
  :init
  (vertico-mode))


(use-package vertico-posframe
  :after vertico
  :hook (vertico-mode . vertico-posframe-mode)
  :config
  (setq vertico-posframe-width 200)
  (setq vertico-posframe-border-width 1)
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  :init
  (vertico-posframe-mode))


(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))


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

(remove-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode)

(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :config
  (set-face-attribute 'region nil :background "#666")
  (delete-selection-mode 1))

  
(use-package popup-kill-ring
  :ensure t)


(use-package vterm
  :config
  (defun turn-off-chrome ()
    (global-hl-line-mode -1))
  :hook
  (vterm-mode . turn-off-chrome))


;(use-package multi-vterm
;  :ensure t)


;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :config
;;   (defvar tw/treemacs-denote-root (expand-file-name "~/Org/"))

  ;; ;; Define the transformer to remove the Denote identifier
  ;; (defun tw/treemacs-denote-transformer (filename full-path)
  ;;   "Strip Denote identifier prefix from filenames in ~/Org/."
  ;;   (if (and (string-prefix-p tw/treemacs-denote-root full-path)
  ;;            (string-match "\\`[0-9]\\{8\\}T[0-9]\\{6\\}--\$begin:math:text$.*\\$end:math:text$" filename))
  ;;       (match-string 1 filename)
  ;;     filename))

  ;; (treemacs-modify-theme "Default"
  ;;   :files '((".*" tw/treemacs-denote-transformer)))

  ;; (setq treemacs-width 85)
  ;; (setq treemacs-follow-mode t)
  ;; (setq treemacs-tag-follow-delay 0.5)

  ;; (unless (treemacs-current-visibility)
  ;;   (treemacs)))


(use-package paredit
  :ensure t
  :config
  (add-hook 'ielm-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'paredit-mode)
  (define-key paredit-mode-map (kbd "RET") nil))
;  (define-key paredit-mode-map (kbd "C-j") 'paredit-newline))
;  (add-hook 'ielm-mode-hook 'g-ielm-init-history)


;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (setq dashboard-week-agenda t)
;;   (setq dashboard-startup-banner 'official)
;;   (setq dashboard-center-content t)
;;   (setq dashboard-items '(
;;                           (agenda    . 10)
;; 			  (recents   . 10)
;;                           (bookmarks . 5)
;;                           (projects  . 5)
;;                           (registers . 5)))
;;   (dashboard-setup-startup-hook))

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Org Select\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))



;; ────────────────────────────────
;; Org config
;; ────────────────────────────────
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :ensure t
  :after org
  ;; :bind
  ;; (:map org-agenda-mode-map
  ;;       ("d" . my-org-agenda-today-day-view)
  ;;       ("D" . org-agenda-day-view)
  ;; 	("C-c t f" . my/org-find-tagged-headings)
  ;; 	("C-c t v" . org-tags-view))

  :config
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 20)   ;; adjust column where graph appears
  (setq org-habit-show-habits-only-for-today t) ;; show streaks across agenda
  (setq org-habit-today-glyph ?○)
  (setq org-habit-completed-glyph ?●)
  (setq org-habit-missed-glyph ?✘)
  (setq org-habit-graph-glyph ?⎯)
  (setq org-habit-preceding-days 2)
  (setq org-habit-following-days 0)
;  (setq org-habit-show-habits-only-for-today t)
;  (setq org-habit-following-days 3)

  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "d") #'my-org-agenda-today-day-view)
    (define-key org-agenda-mode-map (kbd "D") #'org-agenda-day-view)
    (define-key org-agenda-mode-map (kbd "C-c t f") #'my/org-find-tagged-headings)
    (define-key org-agenda-mode-map (kbd "C-c t v") #'org-tags-view))
  (defvar tw/org-last-agenda-command "c"
    "Stores the last used org-agenda custom command key. Defaults to 'c'.")

  (defun tw/org-store-last-agenda-command (orig-fun &rest args)
    "Advice to store the last custom agenda command key."
    (setq tw/org-last-agenda-command (or (nth 1 args) "c"))
    (apply orig-fun args))
  (advice-add 'org-agenda :around #'tw/org-store-last-agenda-command)

  (defun tw/org-agenda-refresh-custom-view ()
    "Rebuild the Org Agenda in the background if visible, then switch to day view."
    (let ((buf (get-buffer "*Org Agenda*")))
      (when (and buf (get-buffer-window buf))
	(let ((inhibit-message t)
              (win (get-buffer-window buf)))
          (with-selected-window win
            (org-agenda nil tw/org-last-agenda-command)
            (org-agenda-day-view)   ;; collapse to day view
            (org-agenda-goto-today)
            (beginning-of-line))))))
  (run-with-timer 60 60 #'tw/org-agenda-refresh-custom-view)

  (defun tw/org-todo-done-when-checkboxes-complete ()
    "Switch heading to DONE when all checkboxes are checked, else back to TODO."
    (when (and (org-get-todo-state)       ;; only act if heading has a TODO keyword
	       (org-get-checkbox-statistics))
      (let* ((stats (org-get-checkbox-statistics))
             (all-done (= (car stats) (cdr stats))))
        (org-todo (if all-done 'done 'todo)))))
  (add-hook 'org-checkbox-statistics-hook #'tw/org-todo-done-when-checkboxes-complete)

  (defun my-org-agenda-today-day-view ()
    "Jump to today in the agenda and show only today's view."
    (interactive)
    (org-agenda-goto-today)
    (org-agenda-day-view))

  (defun my/org-agenda-hide-cursor ()
    "Hide the cursor in org-agenda buffers."
    (setq cursor-type nil))
  (add-hook 'org-agenda-mode-hook #'my/org-agenda-hide-cursor)


  ;; (defun tw/org-find-tagged-headings ()
  ;;   "Prompt for a tag and show all matching headings in org-agenda-files using consult-grep, without vertico-posframe."
  ;;   (interactive)
  ;;   (require 'consult)
  ;;   (let ((tag (completing-read "Tag: " (org-global-tags-completion-table) nil t))
  ;;         (files (org-agenda-files))
  ;;         (default-directory "~/")
  ;;         (was-posframe-enabled vertico-posframe-mode))
  ;;     (when was-posframe-enabled (vertico-posframe-mode -1))
  ;;     (unwind-protect
  ;;         (consult-grep files (format "^\\*+ .*:%s:" tag))
  ;;       (when was-posframe-enabled (vertico-posframe-mode 1)))))


  ;; Leave the element cache ON (default since Org 9.6)
  (setq org-fold-core-style 'overlays)   ;; default; fastest redraw
  (setq org-fontify-whole-heading-line t) ;; cosmetic, no cost
  (setq org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Org/")
  (setq mouse-highlight nil)

  ;; Defer slow prettifiers:
;  (add-hook 'org-mode-hook #'so-long-minor-mode)


  ;; ────────────────────────────────
  ;; ✨ Appearance
  ;; ────────────────────────────────
  ;; General look and feel

  ;; Set face for code snippets in org
  (custom-set-faces
   '(org-code ((t (:family "Courier" :height 1.0 :foreground "#006400")))))

  (with-eval-after-load 'org
    (custom-set-faces
     ;; Studio Oils theme colours
     '(org-level-1  ((t (:foreground "#2f72b8" :height 1.0 :weight normal :underline nil)))) ; blue
     '(org-level-2  ((t (:foreground "#b35860" :height 1.0 :weight normal))))              ; muted rose
     '(org-level-3  ((t (:foreground "#7a5c8e" :height 1.0 :weight normal))))              ; eggplant
     '(org-level-4  ((t (:foreground "#b37544" :height 1.0 :weight normal))))              ; amber brown
     '(org-level-5  ((t (:foreground "#a0675a" :height 1.0 :weight normal))))
     '(org-level-6  ((t (:foreground "#56749f" :height 1.0 :weight normal))))
     '(org-level-7  ((t (:foreground "#6c5e75" :height 1.0 :weight normal))))
     '(org-level-8  ((t (:foreground "#7d8484" :height 1.0 :weight normal))))
     '(org-level-9  ((t (:foreground "#435470" :height 1.0 :weight normal))))
     '(org-level-10 ((t (:foreground "#aa5d45" :height 1.0 :weight normal))))


     ;; Other Org faces
     '(org-special-keyword ((t (:foreground "#cBc5c4" :slant italic))))
     '(org-drawer         ((t (:foreground "LightSlateGray" :slant italic))))
     '(org-tag            ((t (:foreground "#c6a5a3" :height 0.9 :weight normal))))
     '(region             ((t (:background "#FFEFD5" :foreground unspecified))))))

  (setq org-hide-leading-stars        t
        org-hide-emphasis-markers     t
        org-pretty-entities           t
        org-ellipsis                  " ❱"
        org-adapt-indentation         t
	org-tags-column               0
        org-auto-align-tags           nil
        org-blank-before-new-entry    '((heading . nil) (plain-list-item . nil)))

  (custom-set-faces
   '(org-document-title ((t (:slant italic :height 1.0 :underline nil)))))

  (add-hook 'org-mode-hook #'hl-line-mode)
  (add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))

  ;; ────────────────────────────────
  ;; Capture Templates               
  ;; ────────────────────────────────
  (setq org-default-notes-file (expand-file-name "Inbox.org" org-directory))

  (setq org-capture-templates
	`(("i" "Inbox" entry
           (file ,org-default-notes-file)
           "\n* TODO %?\n  Captured: %U\n")


          ("h" "New Habit" entry
           ;; Use a lambda to dynamically evaluate the file path
           (file+headline
            (lambda () (expand-file-name "Personal.org" org-directory))
            "Habits")
           "* HABIT %?\n  SCHEDULED: <%<%Y-%m-%d> +1d>\n  :PROPERTIES:\n  :STYLE:    habit\n  :END:\n"
           :empty-lines 1)


          ("p" "Personal")

          ("pn" "New Note" entry
           (file+olp ,(expand-file-name "Personal.org" org-directory) "Notes")
           "* %?\n  Captured on: %U\n")

          ("pt" "New Task" entry
           (file+olp ,(expand-file-name "Personal.org" org-directory) "Tasks")
           "* UNSCHEDULED %?\n  Captured on: %U\n")

          ("w" "Work")

          ("wn" "New Note" entry
           (file+olp ,(expand-file-name "Work.org" org-directory) "Notes")
           "* %?\n  Captured on: %U\n")

          ("wt" "New Task" entry
           (file+olp ,(expand-file-name "Work.org" org-directory) "Tasks")
           "* UNSCHEDULED %?\n  Captured on: %U\n")

	  ("wm" "Meeting with Outstanding Meeting Actions" entry
           (file+olp "~/Org/Work.org" "INBOX")
           "** %<%Y-%m-%d-%A> - %^{Meeting Title}\n*** Notes\n*** TODO Outstanding Actions [/]:actions:\nSCHEDULED: <%<%Y-%m-%d %a>>\n- [ ] \n%?")))

  ;; ────────────────────────────────
  ;; Capture in a new frame called from Raycast using:
  ;;
  ;; #!/bin/bash
  ;;
  ;; # Required parameters:
  ;; # @raycast.schemaVersion 1
  ;; # @raycast.title Org Capture
  ;; # @raycast.mode compact
  ;; 
  ;; # Optional parameters:
  ;; # @raycast.icon 🧠
  ;; # @raycast.description Capture notes to Org mode
  ;; 
  ;; emacsclient -e '(tw/org-capture-frame)' > /dev/null
  ;; ────────────────────────────────

  (defun tw/org-capture-frame ()
    "Create a clean, isolated frame for Org capture, centered on screen."
    (interactive)
    (let* ((frame-width 120)
           (frame-height 50)
           ;; Get screen dimensions
           (screen-width (display-pixel-width))
           (screen-height (display-pixel-height))
           ;; Calculate character dimensions (approximate)
           (char-width (frame-char-width))
           (char-height (frame-char-height))
           ;; Calculate pixel dimensions of the frame
           (frame-pixel-width (* frame-width char-width))
           (frame-pixel-height (* frame-height char-height))
           ;; Calculate centered position
           (left (/ (- screen-width frame-pixel-width) 2))
           (top (/ (- screen-height frame-pixel-height) 2))
           (frame (make-frame `((name . "org-capture")
				(width . ,frame-width)
				(height . ,frame-height)
				(left . ,left)
				(top . ,top)
				(minibuffer . t)
				(internal-border-width . 12)
				(font . "Iosevka")
				(auto-raise . t)
				(z-group . above)))))
      ;; Use select-frame instead of select-frame-set-input-focus for cleaner focus management
      (select-frame frame)
      (raise-frame frame)
      (delete-other-windows)
      (org-capture nil "i")
      (delete-other-windows)))

  (defun tw/delete-org-capture-frame ()
    "Close the capture frame after finishing or aborting."
    (when (equal "org-capture" (frame-parameter nil 'name))
      (delete-frame)))

  (add-hook 'org-capture-after-finalize-hook #'tw/delete-org-capture-frame)





  ;; ────────────────────────────────
  ;; 📅 Agenda Setup
  ;; ────────────────────────────────
  (setq org-agenda-files
	(list 
	 (expand-file-name "Personal.org" org-directory)
	 (expand-file-name "Work.org"     org-directory)
	 (expand-file-name "Journal.org"  org-directory)))

  (setq org-agenda-prefix-format
	'((agenda . "│ %?-12t% s")
          (todo . "│ ")
          (tags . "│ ")
          (search . "│ ")))

  (setq org-agenda-format-date
	(lambda (date)
          (concat "\n" (org-agenda-format-date-aligned date))))

  (setq org-agenda-window-setup                         'current-window
	org-agenda-start-with-entry-text-mode            nil
	org-agenda-group-by-todo-state                   t
	org-agenda-sorting-strategy                     '((todo todo-state-up))
	org-agenda-block-separator                       nil
	org-agenda-move-date-from-past-immediately       nil
        org-agenda-inhibit-startup                       t
        org-agenda-entry-text-maxlines                   0
        org-agenda-entry-text-leaders                    ""
        org-agenda-entry-text-cleanup-hook               nil
	org-agenda-skip-deadline-prewarning-if-scheduled t
	org-agenda-skip-scheduled-if-done                t
	org-agenda-skip-deadline-if-done                 t
	org-agenda-start-on-weekday                      1
	org-agenda-start-hour                            8
	org-deadline-warning-days                        0
	org-agenda-time-grid
	'((daily today require-timed)
          (800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800) ;; <-- hours visible
          "  "
	  " ﹒﹒﹒﹒﹒﹒﹒﹒﹒﹒﹒﹒﹒﹒﹒﹒﹒﹒﹒﹒﹒﹒﹒﹒﹒﹒﹒﹒﹒ "))


  (setq org-agenda-prefix-format
	'((agenda . " %i %?-12t% s")
          (todo   . " %i ")
          (tags   . " %i ")
          (search . " %i ")))

  (add-hook 'org-agenda-mode-hook #'hl-line-mode)

  ;; ────────────────────────────────
  ;; ✅ TODO States and Faces
  ;; ────────────────────────────────
  (setq org-todo-keywords
        '((sequence
           "NEXT(n)" "TODO(t)" "IN-PROGRESS(i)" "UNSCHEDULED(u)" "PROJECT(p)" "WAITING(w)"
           "|"
           "CANCELLED(c)" "DONE(d)")))

  (setq org-todo-keyword-faces
	'(("TODO"        . (:foreground "#2f72b8" :weight bold))     ; blue
          ("NEXT"        . (:foreground "#b35860" :weight bold))     ; muted rose
          ("IN-PROGRESS" . (:foreground "#7a5c8e" :weight bold))     ; eggplant
          ("UNSCHEDULED" . (:foreground "#b37544" :weight bold))     ; amber brown
          ("PROJECT"     . (:foreground "#a0675a" :weight bold))
          ("WAITING"     . (:foreground "#56749f" :weight bold))
          ("DONE"        . (:foreground "#435470" :weight bold :strike-through t))
          ("CANCELLED"   . (:foreground "#aa5d45" :weight bold :slant italic))))


  ;; ────────────────────────────────
  ;; 🧠 Agenda Custom Commands
  ;; ────────────────────────────────

  ;; Complete agenda view
  (setq org-super-agenda-header-separator
	(concat "└" (make-string 65 ?─) "┐\n"))

 


  (setq org-agenda-custom-commands
	'(("c" "Scheduled Today + Weekly Agenda + Grouped TODOs"
           ((agenda ""
                    ((org-agenda-span 'week)
                     (org-agenda-start-on-weekday 1)
                     (org-agenda-overriding-header
                      (propertize "📅 This Week’s Agenda"
                                  'face '(:height 1.5 :weight bold :inherit default)))))

            (alltodo ""
                     ((org-agenda-overriding-header
                       (propertize ""
                                   'face '(:height 1.5 :weight bold :inherit default)))
                      (org-super-agenda-groups
                       '((:name "🌐 Work"
				:and (:tag "work"
					   :not (:todo ("TODO" "IN-PROGRESS" "NEXT" "WAITING")))
				:order 0)
			 (:name "🏡 Personal"
				:and (:tag "personal"
					   :not (:tag "emacs"))
				:order 1)
			 (:name "𝝺 Emacs"
				:and (:tag "personal"
					   :tag "emacs")
				:order 2)
			 (:discard (:anything t))))))))))







;; (setq org-agenda-custom-commands
;;       '(("c" "Scheduled Today + Weekly Agenda + Grouped TODOs + Habits"
;;          ((agenda ""
;;                   ((org-agenda-span 'week)
;;                    (org-agenda-start-on-weekday 1)
;;                    (org-agenda-overriding-header
;;                     (propertize "📅 This Week’s Agenda"
;;                                 'face '(:height 1.5 :weight bold :inherit default)))))

;;           (alltodo ""
;;                    ((org-agenda-overriding-header
;;                      (propertize ""
;;                                  'face '(:height 1.5 :weight bold :inherit default)))
;;                     (org-super-agenda-groups
;;                      '((:name "🌐 Work"
;;                               :and (:tag "work"
;;                                          :not (:todo ("TODO" "IN-PROGRESS" "NEXT" "WAITING")))
;;                               :order 0)
;;                        (:name "🏡 Personal"
;;                               :and (:tag "personal"
;;                                          :not (:tag "emacs"))
;;                               :order 1)
;;                        (:name "𝝺 Emacs"
;;                               :and (:tag "personal"
;;                                          :tag "emacs")
;;                               :order 2)
;;                        (:discard (:anything t))))))

;;           Habits block at the bottom
;;           (agenda ""
;;                   ((org-agenda-span 1)
;;                    (org-agenda-overriding-header
;;                     (propertize "🔥 Habits"
;;                                 'face '(:height 1.3 :weight bold :inherit default)))
;;                    (org-agenda-show-log nil)
;;                    (org-agenda-skip-function
;;                     '(org-agenda-skip-entry-if 'notregexp ":STYLE:.*habit"))))))))

  ;; ────────────────────────────────
  ;; ️ Tag a-list
  ;; ────────────────────────────────
  (setq org-tag-alist
        '(("atheism"     . ?A) ("ada"        . ?a) ("adhd"      . ?D)
          ("discuss"     . ?d) ("emacs"      . ?e) ("workflow"  . ?f)
          ("programming" . ?g) ("thoughts"   . ?h) ("habit"     . ?i)
	  ("house"     . ?H)
          ("inbox"       . ?I) ("blog"       . ?L) ("meeting"   . ?m)
          ("homelab"     . ?o) ("personal"   . ?p) ("project"   . ?P)
          ("occult"      . ?c) ("rpg"        . ?R) ("timesheet" . ?s)
          ("tasks"       . ?T) ("to_read"    . ?r) ("to_install". ?i)
          ("to_listen"   . ?l) ("to_buy"     . ?b) ("to_watch"  . ?w)
          ("music_prod"  . ?M) ("work"       . ?W)
	  ("@MB") ("@SC")))

  ;; ────────────────────────────────
  ;; 📂 Refiling
  ;; ────────────────────────────────
  (setq org-refile-targets
	`((,(expand-file-name "Personal.org" org-directory) :maxlevel . 3)
          (,(expand-file-name "Work.org"     org-directory) :maxlevel . 3)
          (,(expand-file-name "Journal.org"  org-directory) :maxlevel . 3)))

  (setq org-refile-use-outline-path              'file
        org-outline-path-complete-in-steps       nil
        org-refile-allow-creating-parent-nodes   'confirm
        org-refile-use-cache                     nil
;        org-log-refile                           'note
        org-log-refile                           nil
        org-reverse-note-order                   nil)

  ;; ────────────────────────────────
  ;; ⚙️ Miscellaneous
  ;; ────────────────────────────────
  (setq org-goto-interface 'outline-path-completion
	org-archive-location (concat org-directory "Archive.org::"))

  ;; ────────────────────────────────
  ;; ⌨️ Keybindings
  ;; ────────────────────────────────

(with-eval-after-load 'org
  (global-set-key (kbd "C-S-<up>")   #'org-move-subtree-up)
  (global-set-key (kbd "C-S-<down>") #'org-move-subtree-down)
  (global-set-key (kbd "C-c c c")    #'org-capture)
  (global-set-key (kbd "C-c C-j")    #'consult-org-heading)
  (global-set-key (kbd "C-c C-i")    #'consult-imenu))

  ;; Refresh font-lock in all Org buffers (useful after theme change)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'org-mode)
        (font-lock-flush)
        (font-lock-ensure))))

  (custom-set-faces
   '(org-agenda-structure       ((t (:inherit org-level-1))))
   '(org-agenda-date            ((t (:inherit org-level-3))))
   '(org-agenda-date-today      ((t (:inherit org-level-3))))
   '(org-agenda-date-weekend    ((t (:inherit org-level-4))))
   '(org-agenda-date-header     ((t (:inherit org-level-5))))))



(use-package org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode 1))


;; (use-package org-super-agenda
;;   :ensure t
;;   :init
;;   (setq org-super-agenda-header-separator
;;         (propertize (make-string 60 ?─) 'face 'shadow)) ; 60-char grey line
;;   :config
;;   (custom-set-faces
;;    `(org-super-agenda-header ((t (:height 1.3 :weight bold
;; 					  :foreground ,(face-foreground 'org-level-1))))))
;;   (org-super-agenda-mode 1))


(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :config

  (setq org-superstar-leading-bullet ?\s)
  (setq org-superstar-special-todo-items nil)
  (setq org-superstar-configure-like-org-bullets nil)
;  (setq org-superstar-headline-bullets-list
;	'("¶" "α" "β" "γ" "δ" "ε" "ζ" "η" "θ" "ι" "κ"))
  (setq org-superstar-headline-bullets-list
        '("§" "◉" "○" "•" "◦" "∘" "⋅" "·")))

;; ------------------------------
;; End of init.el
;; ------------------------------
