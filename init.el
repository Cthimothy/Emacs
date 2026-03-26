; -----------------------------------------------------------------------------
;; Initial Configuration
;; -----------------------------------------------------------------------------
					;(setq debug-on-error nil)
(setq debug-on-error t)
(add-hook 'emacs-startup-hook (lambda () (setq debug-on-error nil)))
(setq debug-on-quit nil)

(setq ring-bell-function 'ignore
      visible-bell 'ignore)

(setq custom-file (make-temp-file "emacs-custom"))
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0) 
(show-paren-mode t)
(prettify-symbols-mode)
(global-prettify-symbols-mode 1)
(electric-indent-mode -1)

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
(setq cursor-in-non-selected-windows nil)
(with-eval-after-load 'org-agenda
  (setq org-agenda-highlight-mouse-over nil))


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("nongnu"   . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;; (setq backup-directory-alist
;;       `(("." . "~/.emacs.d/Backups/")))
;; (setq auto-save-file-name-transforms
;;       `((".*" "~/.emacs.d/Backups/" t)))
;; (setq create-lockfiles t)


;; Backups (versioned) in one place
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/Backups/"))
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

(setq vc-make-backup-files t)

;; Auto-saves in one place
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/Backups/" t))) 

;; Optional: keep lockfiles ON unless you have a specific reason
;; (setq create-lockfiles nil)


(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
;;(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;(when (file-exists-p custom-file)
;;  (load custom-file))
;;(setq custom-file (locate-user-emacs-file "custom.el"))  
;;(load custom-file :no-error-if-file-is-missing)

;;
;;Always split compialtion output in buffer below current
;;
(add-to-list
 'display-buffer-alist
 '("\\*compilation\\*"
   (display-buffer-reuse-window display-buffer-below-selected)
   (window-height . 0.33)))

(require 'auth-source)
(require 'epa-file)
(epa-file-enable)
;;(setq auth-sources '("~/.authinfo.gpg"))
(setq auth-sources '("~/.authinfo" "~/.netrc"))



(setq lock-file-name-transforms
      '((".*" "~/.emacs.d/lockfiles/" t)))

(setq inhibit-startup-screen t)

;; Some Macos nonsense
(setq mac-command-modifier 'meta)
(define-key key-translation-map (kbd "M-3") (kbd "#"))
(define-key key-translation-map (kbd "H-3") (kbd "#"))
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

;;
;; Look & Feel
;;

;; (when (member "Iosevka" (font-family-list))
;;   (set-face-attribute 'default nil
;;                       :family "JetBrains Mono"
;;                       :height 115
;;                       :weight 'medium
;;                       :slant 'normal
;;                       :width 'normal)
;;    (add-to-list 'default-frame-alist '(font . "Iosevka")))

;; Use JetBrains Mono as the default font
(when (member "JetBrains Mono" (font-family-list))
  (set-face-attribute 'default nil
                      :family "JetBrains Mono"
                      :height 115
                      :weight 'normal
                      :slant 'normal
                      :width 'normal)

  ;; Ensure new frames also use JetBrains Mono (daemon support)
  (add-to-list 'default-frame-alist '(font . "JetBrains Mono-13")))

;;

;;
;;; Define custom functions
(defun my/hl-line-on ()
  (hl-line-mode 1))

(defun my/hl-line-off ()
  (hl-line-mode -1))

(add-hook 'buffer-list-update-hook #'my/hl-line-on)
(add-hook 'window-selection-change-functions
          (lambda (_)
            (walk-windows
             (lambda (w)
               (with-current-buffer (window-buffer w)
                 (if (eq w (selected-window))
                     (hl-line-mode 1)
                   (hl-line-mode -1))))
             nil t)))

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

(defun tw/toggle-transparency ()
  (interactive)
  (let* ((alpha (frame-parameter nil 'alpha))
         (alpha (cond
                 ((numberp alpha) alpha)
                 ((consp alpha) (car alpha))
                 (t 100))))
    (set-frame-parameter
     nil 'alpha
     (if (<= alpha 95)
         '(100 . 100)
       '(95 . 95)))))

(defun tw/auth-get-openai-key ()
  "Return OpenAI API key from auth-source."
  (let* ((match (car (auth-source-search
                      :host "api.openai.com"
                      :user "openai"
                      :require '(:secret)
                      :max 1)))
         (secret (when match (plist-get match :secret))))
    (when secret
      (if (functionp secret) (funcall secret) secret))))

(defun tw/insert-current-date ()
  "Insert the current date in the format YYYY-MM-DD."
  (interactive)
  (insert (format-time-string "%Y-%m-%d-%A")))

(defun tw/insert-current-time ()
  "Insert time in hh:mm format"
  (interactive)
  (insert (format-time-string "%H:%M ")))

;;Use a bar cursor when mark is active and a region exists.
(defun th-activate-mark-init ()
  (setq cursor-type 'bar))
(add-hook 'activate-mark-hook 'th-activate-mark-init)

(defun th-deactivate-mark-init ()
  (setq cursor-type 'box))
(add-hook 'deactivate-mark-hook 'th-deactivate-mark-init)

;; (defun tw/insert-todays-journal-entry ()
;;   (interactive)
;;   (goto-char (point-max))
;;   (unless (bolp) (insert "\n"))
;;   (let ((date (format-time-string "%d-%A"))) ; e.g., "15-Tuesday"
;;     (insert (concat "*** _" date "_\n"
;; 		    "**** Memoranda
;; - 

;; **** Habits
;; - [ ] Workout/Weights

;; **** Tasks"))))

(defun tw/highlight-line ()
  (interactive)
  (move-beginning-of-line 1)
  (set-mark-command nil)
  (move-end-of-line 1))

(defun tw/consult-org-heading-with-initial (initial)
  "Call `consult-org-heading' with INITIAL inserted in the minibuffer."
  (minibuffer-with-setup-hook
      (lambda ()
        (insert initial))
    (consult-org-heading)))

(defun tw/org-jump ()
  "Jump to an Org heading without narrowing or expanding the whole subtree."
  (interactive)
  (when (tw/consult-org-heading-with-initial "^")
    (org-show-context)
    (org-show-entry)))

(defun tw/org-jump-and-focus ()
  "Jump to a heading and show only its direct children.
Keeps the rest of the file visible as an outline."
  (interactive)
  (widen)
  (when (tw/consult-org-heading-with-initial "^")
    (widen)
    (org-overview)
    (org-show-context)
    (org-show-children)))

(defun tw/org-jump-and-narrow ()
  "Jump to an Org heading and narrow to its subtree."
  (interactive)
  (widen)
  (when (tw/consult-org-heading-with-initial "^")
    (org-narrow-to-subtree)
    (org-show-subtree)))

(with-eval-after-load 'org
  ;; move org-goto to C-c j
  (define-key org-mode-map (kbd "C-c j") #'org-goto)
  ;; put your main jump on the stronger key
  (define-key org-mode-map (kbd "C-c C-j") #'tw/org-jump-and-focus)
  ;; keep your others
  (define-key org-mode-map (kbd "C-c J")   #'tw/org-jump)
  (define-key org-mode-map (kbd "C-c M-j") #'tw/org-jump-and-narrow))


(setq ibuffer-default-sorting-mode 'recency)
(setq split-width-threshold nil)
(setq split-height-threshold nil)
(setq use-short-answers t)
(setq confirm-nonexistent-file-or-buffer nil)
					;(setq scroll-conservatively 10
					;      scroll-margin 15)
					;(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-large-scroll-height 40)

(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer buffer '(display-buffer-same-window))))
(setq left-margin-width 1)
(setq right-margin-width 1)
(setq insert-directory-program "/opt/homebrew/bin/gls")
(setq dired-use-ls-dired t)
;;(setq dired-listing-switches "-lGh1v --reverse --group-directories-first")
(setq dired-listing-switches "-lGh1v --group-directories-first")
;;(setq dired-listing-switches "-alh --group-directories-first --ignore-case")
(setq large-file-warning-threshold 50000000)
(setq dired-kill-when-opening-new-dired-buffer t)
(setq org-use-sub-superscripts nil)
(setq column-number-mode t)
(setq electric-pair-inhibit-predicate
      (lambda (c)
        (if (char-equal c ?\") t (electric-pair-default-inhibit c))))
(set-frame-parameter (selected-frame) 'alpha '(100 100))

(setq org-hide-leading-stars     nil
      org-hide-emphasis-markers  t
      org-pretty-entities        t
      org-ellipsis               " ❱"
      org-adapt-indentation      t
      org-startup-indented       t
      org-tags-column            0
      org-auto-align-tags        nil
      org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

(add-hook 'shell-mode-hook
          (lambda ()
            (when (string= (buffer-name) "*Async Shell Command*")
              (add-hook 'compilation-finish-functions
                        (lambda (buf msg)
                          (message "Async command finished: %s" msg))
                        nil t))))

(add-hook 'messages-buffer-mode-hook
          (lambda () (setq-local scroll-conservatively 101)))
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'org-agenda-mode-hook 'hl-line-mode)
(add-hook 'org-mode-hook 'hl-line-mode)
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))
(add-hook 'ibuffer-mode-hook (lambda () (hl-line-mode 1)))
(add-hook 'text-mode-hook #'visual-line-mode)

;;
;; External required code files
(setq gnus-window-configuration 'current
      gnus-use-adaptive-windows nil
      gnus-article-browse-delete-window t
      gnus-save-newsrc-file t
      gnus-read-newsrc-file t
      gnus-fetch-old-headers 500
      gnus-summary-display-arrow t
      gnus-fetch-old-headers 300
      gnus-large-newsgroup 500
      gnus-fetch-old-headers 300
      gnus-auto-select-first nil
      gnus-configure-windows 'horizontal)

(require 'gnus-usenet)

;; (require 'imenu-list)
;; (setq imenu-list-position 'right
;;       imenu-list-size 40)

;; (imenu-list-minor-mode t)

;;
;;;External packages

(global-set-key (kbd "C-x C-l") 'avy-goto-line)
(global-set-key (kbd "C-x C-x") 'avy-goto-char-timer)

(use-package avy
  :ensure t
  :bind
  (("C-x C-l" . avy-goto-line)
   ("C-x C-x" . avy-goto-char-timer))
  :config
  ;; sensible defaults
  (setq avy-background t
        avy-style 'at-full
        avy-timeout-seconds 0.3
        avy-all-windows t
	avy-keys '(?a ?s ?d ?c ?e))

(use-package org-modern
  :ensure t
  :hook
  (org-mode-hook . org-modern-mode)
  (org-agenda-finalize-hook . org-modern-agenda)
  :config
  (setq org-modern-star nil))
  (setq org-agenda-tags-column 0)

;; (use-package olivetti
;;   :ensure t
;;   :hook
;;   (text-mode . olivetti-mode)
;;   (org-mode . olivetti-mode)
;;   :custom
;;   (olivetti-body-width 162)
;;   (olivetti-style 'margins))

(use-package easysession
  :ensure t
  :demand t
  :custom
  (easysession-save-interval (* 1 60))  ; Save every 10 minutes

  ;; Save the current session when using `easysession-switch-to'
  (easysession-switch-to-save-session t)

  ;; Do not exclude the current session when switching sessions
  (easysession-switch-to-exclude-current nil)

  ;; Display the active session name in the mode-line lighter.
  (easysession-save-mode-lighter-show-session-name t)

  ;; Optionally, the session name can be shown in the modeline info area:
  ;; (easysession-mode-line-misc-info t)

  :config
  ;; Key mappings
  (global-set-key (kbd "C-c ss") #'easysession-switch-to) ; Load session
  (global-set-key (kbd "C-c sS") #'easysession-save) ; Save session
  (global-set-key (kbd "C-c sL") #'easysession-switch-to-and-restore-geometry)
  (global-set-key (kbd "C-c sr") #'easysession-rename)
  (global-set-key (kbd "C-c sR") #'easysession-reset)
  (global-set-key (kbd "C-c su") #'easysession-unload)
  (global-set-key (kbd "C-c sd") #'easysession-delete)

  ;; non-nil: Make `easysession-setup' load the session automatically.
  ;; (nil: session is not loaded automatically; the user can load it manually.)
  (setq easysession-setup-load-session t)

  ;; The `easysession-setup' function adds hooks:
  ;; - To enable automatic session loading during `emacs-startup-hook', or
  ;;   `server-after-make-frame-hook' when running in daemon mode.
  ;; - To save the session at regular intervals, and when Emacs exits.
  (easysession-setup))

(use-package org-journal
  :ensure t
  :defer t
  :config
  (setq org-journal-dir "~/Org/Journal/"
	org-journal-enable-entry-properties nil
;;        org-journal-file-type 'yearly
        org-journal-file-type 'monthly
        org-journal-date-format "%Y-%m-%d, %A"
        org-journal-file-format "Journal-%Y.org"
	org-journal-find-file #'find-file
        org-journal-file-header "#+TITLE: Journal %Y\n#+STARTUP: folded\n")

  (defun tw/org-journal-indent ()
    (setq-local org-indent-indentation-per-level 3)
    (org-indent-mode 1))
  (add-hook 'org-journal-mode-hook #'tw/org-journal-indent)

  (defun my-dired-org-journal ()
    (interactive)
    (dired org-journal-dir)))

(define-prefix-command 'tw/journal-map)
(global-set-key (kbd "C-c o j") 'tw/journal-map)
(define-key tw/journal-map (kbd "n") #'org-journal-new-entry)
;; (define-key tw/journal-map (kbd "d") #'my-dired-org-journal)


(setq spacemacs-theme-org-height nil)
(use-package auto-dark
  :ensure t
  :custom
  (auto-dark-themes '((gruvbox-dark-medium) (doric-earth)))
  ;; (auto-dark-themes '((base16-gruvbox-dark-medium) (doric-earth)))
  (auto-dark-polling-interval-seconds 5)
  (auto-dark-allow-osascript nil)
  (auto-dark-allow-powershell nil)
  :hook
  ((auto-dark-dark-mode
    . (lambda ()
	(with-eval-after-load 'org
	  (set-face-attribute 'org-tag nil :slant 'italic :weight 'normal :inherit 'shadow))
	(set-face-attribute 'aw-leading-char-face nil
                            :height 6.0
                            :weight 'bold
                            :foreground "#83a598")
	;;(set-face-attribute 'aw-posframe-face nil
        ;;                    :height 5.0
        ;;                    :weight 'bold
        ;;                    :foreground "#83a598")
	))
   (auto-dark-light-mode
    . (lambda ()
	(with-eval-after-load 'org
	  (set-face-attribute 'org-tag nil :slant 'italic :weight 'normal :inherit 'shadow))


    	(set-face-attribute 'aw-leading-char-face nil
                            :height 10.0
                            :weight 'bold
                            :foreground "#af3a03")
	;;(set-face-attribute 'aw-posframe-face nil
        ;;                    :height 5.0
        ;;                    :weight 'bold
        ;;                    :foreground "#af3a03")
	)))
  :init
  (auto-dark-mode))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-align 'right)
  (marginalia-field-width 100)
  (marginalia-annotator #'marginalia-annotators-heavy)
  :init
  (marginalia-mode))

(use-package vertico
  :ensure t
  :init
  (setq vertico-resize nil)
  (setq vertico-count 30)
  (vertico-mode))

;; (use-package vertico-posframe
;;   :after vertico
;;   :hook (vertico-mode . vertico-posframe-mode)
;;   :config
;;   ;; make the popup wide enough
;;   (setq vertico-posframe-width 150   ;; nil = full frame width
;;         vertico-posframe-height 32
;;         vertico-posframe-border-width 10
;;         vertico-posframe-parameters
;;         '((left-fringe . 5)
;;           (right-fringe . 5)))
;;   :init
;;   (vertico-posframe-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  ;;(completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package erc
  :commands (erc erc-tls)
  :init
  ;; Load the hide module early
  (setq erc-modules
        '(autojoin button completion fill
		   hide irccontrols match menu
		   move-to-prompt netsplit
		   networks readonly ring
		   stamp track))

  :config
  ;; Hide noisy channel events
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))

  ;; Don’t let joins/leaves trigger activity tracking
  (setq erc-track-exclude-types '("JOIN" "PART" "QUIT"))

  ;; Optional but recommended: cleaner prompt handling
  (erc-move-to-prompt-mode 1)

  ;; Timestamps without clutter
  (setq erc-stamp-format "[%H:%M] "))

(use-package ace-window
  :ensure t
  :bind (("C-x C-o" . ace-window))
  :config
  (ace-window-posframe-mode t)
  (setq aw-leading-char-style 'char)     ; ← centers the letter in window
  (setq aw-keys '(?a ?s ?q ?w ?e ?z ?x))
					;  (setq aw-keys '(?d ?x ?z ?w ?q ?s ?a))
					;  (setq aw-keys '(?w ?z ?q ?s ?a))
  (setq aw-scope 'frame)
  (setq aw-ignore-current t)
  (setq aw-background t))
;;(ace-window-posframe-mode)

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
        '("☰" "◉" "○" "•" "◦" "·" "⋅")))
;        '("§" "◉" "○" "•" "◦" "·" "⋅")))
; ▷
; ▶

(use-package chatgpt-shell
  :ensure t
  :commands (chatgpt-shell chatgpt-shell-prompt-compose)
  :init
  ;; Supply key via function → avoids load-order problems
  (setq chatgpt-shell-openai-key #'tw/auth-get-openai-key)

  ;; Optional — only if you actually use these
  ;; (setq chatgpt-shell-openai-project "proj_XXXX")
  ;; (setq chatgpt-shell-openai-organization "org_XXXX")

  :config
  ;; Fail loudly if key missing (prevents silent half-loads)
  (unless (funcall chatgpt-shell-openai-key)
    (warn "chatgpt-shell: No OpenAI key found in auth-source")))


(use-package deadgrep
  :ensure t
  :bind (("C-c d g" . deadgrep))
  :config
  ;; Default search root: current project or ~/Org
  (setq deadgrep-project-root-function
        (lambda ()
          (or (ignore-errors (project-root (project-current)))
              (expand-file-name "~/Org"))))
  ;; Always open results in the current window
  (defun tw/deadgrep-in-current-window (orig-fun &rest args)
    "Force `deadgrep' results to appear in the current window."
    (let ((display-buffer-overriding-action
           '((display-buffer-same-window)
             (inhibit-same-window . nil))))

      (apply orig-fun args)))
  (advice-add 'deadgrep :around #'tw/deadgrep-in-current-window))

(use-package visual-fill-column
  :ensure t
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 120)
  (visual-fill-column-center-text nil))

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
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-height)
  (define-key pdf-view-mode-map (kbd "C-s") #'isearch-forward)
  
  ;; Modified navigation
  (defun tw/pdf-view-next-page-top ()
    "Go to the next PDF page and show it from the top."
    (interactive)
    (pdf-view-goto-page
     (min (pdf-cache-number-of-pages)
          (1+ (pdf-view-current-page))))
    (run-at-time
     0 nil
     (lambda (window)
       (when (window-live-p window)
         (with-selected-window window
           (set-window-vscroll window 0 t)
           (set-window-hscroll window 0))))
     (selected-window)))

  (defun tw/pdf-view-previous-page-bottom ()
    "Go to the previous PDF page and show it from the bottom."
    (interactive)
    (pdf-view-goto-page
     (max 1
          (1- (pdf-view-current-page))))
    (run-at-time
     0 nil
     (lambda (window)
       (when (window-live-p window)
         (with-selected-window window
           (image-eob)
           (set-window-hscroll window 0))))
     (selected-window)))

  (with-eval-after-load 'pdf-view
    (define-key pdf-view-mode-map (kbd "<right>") #'tw/pdf-view-next-page-top)
    (define-key pdf-view-mode-map (kbd "<left>")  #'tw/pdf-view-previous-page-bottom)
    (define-key pdf-view-mode-map (kbd "<down>")  #'image-next-line)
    (define-key pdf-view-mode-map (kbd "<up>")    #'image-previous-line)))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure nil
  :disabled t
  :hook
;;  (dired-mode . nerd-icons-dired-mode))
)
 
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))
;; (remove-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode)

(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :config
  ;;(set-face-attribute 'region nil :background "#666")
  (delete-selection-mode 1))

(use-package popup-kill-ring
  :ensure t)

(use-package vterm
  :config
  (defun turn-off-chrome ()
    (global-hl-line-mode -1))
  :hook
  (vterm-mode . turn-off-chrome))

(use-package paredit
  :ensure t
  :config
  (add-hook 'ielm-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'paredit-mode)
  (define-key paredit-mode-map (kbd "RET") nil))

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

(use-package noflet
  :ensure t)

(use-package xclip
  :ensure t
  :config
  (xclip-mode t))

(use-package recentf
  :init
  (recentf-mode 1)
  :config)

;;(setq recentf-exclude
;;      (delete "\\.org_archive\\'" recentf-exclude))

(use-package consult
  :ensure t)

(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101
        scroll-margin 0
	scroll-step 1
	line-move-visual t)
  :config
  (ultra-scroll-mode 1))

(use-package elpher
  :ensure t
  :config
  (add-hook 'elpher-mode 'hl-line-mode))

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

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode t)
  (setq nyan-wavy-trail nil
	nyan-animate-nyancat nil))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package diredc
  :load-path "~/.emacs.d/lisp"
  :commands (diredc))

(use-package undo-tree
  :ensure t
  :init
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-history")))
  (setq undo-tree-auto-save-history t)
  :config
  (global-undo-tree-mode 1))



;; Configure package archives

;; Install dependencies and org-social
(use-package request)
(use-package emojify)

(use-package org-social
  :config
  ;; Option 1: Local file + your own hosting
  (setq org-social-file "~/social.org")
  (setq org-social-relay "https://relay.org-social.org/")
  ;;  (setq org-social-my-public-url "https://example.com/social.org")

  ;; Option 2: Using Org Social Host (uncomment and use your credentials)
  ;; (setq org-social-file "http://host.org-social.org/vfile?token=YOUR_TOKEN&ts=TIMESTAMP&sig=SIGNATURE")
  ;; (setq org-social-relay "https://relay.org-social.org/")
  ;; (setq org-social-my-public-url "http://host.org-social.org/your-nick/social.org")

  ;; Optional: Add global keybindings
  (keymap-global-set "C-c s t" #'org-social-timeline)
  (keymap-global-set "C-c s n" #'org-social-new-post))


(use-package elfeed
  :ensure t
  :config
  ;; pick a db dir you can actually see/control
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  (setq elfeed-search-align-right t
	elfeed-search-title-min-width 50
	elfeed-search-title-max-width 100
	;; elfeed-search-trailing-width
        shr-use-fonts nil
        elfeed-search-filter "@3-days-ago +unread"))

(use-package elfeed-org
  :ensure t
  :after elfeed
  :config
  (setq rmh-elfeed-org-files
        (list (expand-file-name "elfeed.org" user-emacs-directory)))
  ;; This is the important bit: actually parse the org file into elfeed-feeds
  (elfeed-org))

;;
;;; Org-mode config
(require 'org)
(require 'org-capture)

(setq org-todo-keywords
      '((sequence
         "TODO(t)"
         "PLANNED(p)"
         "|"
         "DONE(d)")))

(setq org-use-speed-commands t)

(setq org-agenda-todo-ignore-scheduled nil)

(with-eval-after-load 'org
  ;; Reuse the built-in outline repeat map for Org navigation commands.
  (dolist (cmd '(org-next-visible-heading
                 org-previous-visible-heading
                 org-forward-heading-same-level
                 org-backward-heading-same-level
                 outline-up-heading))
    (put cmd 'repeat-map 'outline-navigation-repeat-map)))

;; (with-eval-after-load 'org
;;   (let ((bg (face-background 'default nil t)))
;;     (set-face-attribute 'org-hide   nil :foreground bg :background bg)
;;     (set-face-attribute 'org-indent nil :foreground bg :background bg)))

;; (with-eval-after-load 'org
;;   (let ((bg (or (face-background 'default nil t)
;;                 (face-background 'default))))
;;     (set-face-attribute 'org-indent nil :foreground bg :background 'unspecified)))

(setq diary-file "~/Org/diary")
(setq org-agenda-include-diary t)
(setq diary-show-holidays-flag nil)
(with-eval-after-load 'org-agenda
  (set-face-attribute 'org-agenda-diary nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'default))
(setq org-agenda-scheduled-leaders
      '("Schd:" "Schd %2dx: "))

(setq org-agenda-deadline-leaders
      '("Due: " "Due in %3d d: " "Over %3d d: "))

;; (setq org-agenda-prefix-format
;;       '((agenda . " %?-12t% s")
;;         (todo   . " ")
;;         (tags   . " ")
;;         (search . " ")))

;; (setq org-agenda-prefix-format
;;       '((agenda . " %-10:c %?-5t ")
;;         (todo   . " %-10:c ")
;;         (tags   . " %-10:c ")
;;         (search . " %-10:c ")))

;; (setq org-agenda-prefix-format
;;       '((agenda . " %i %-12:c%?-12t% s")
;;         (todo   . " %i %-12:c")
;;         (tags   . " %i %-12:c")
;;         (search . " %i %-12:c")))

(setq org-agenda-prefix-format
      '((agenda . " %-10:c%?-10t% s")
        (todo   . " %-10:c")
        (tags   . " %-10:c")
        (search . " %-10:c")))

;(setq org-tags-exclude-from-inheritance
;      '("meeting" "project" "work" "personal" "rpg" "emacs"))

(setq org-tags-exclude-from-inheritance
      '("meeting" "project" "work" "personal" "rpg" "emacs"))

(setq org-agenda-window-setup 'current-window)
(setq org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Org/")
(setq org-agenda-files
      (append
       (list 
	(expand-file-name "Personal.org" org-directory)
	(expand-file-name ".org"         org-directory)
	(expand-file-name "Work.org"     org-directory)
	(expand-file-name "Emacs.org"    org-directory)
;;	(expand-file-name "Journal.org"  org-directory)
	(expand-file-name "middle-earth-timeline.org" org-directory)
	(expand-file-name "middle-earth-personae.org" org-directory)
	(expand-file-name "middle-earth-places.org" org-directory)
	(expand-file-name "middle-earth-timeline.org" org-directory)
	(expand-file-name "british-calendar.org" org-directory))
       (directory-files "~/Org/Journal/" t "\\.org$")))

(setq org-agenda-window-setup 'current-window
      org-agenda-inhibit-startup t
      org-agenda-entry-text-maxlines 0
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-agenda-skip-scheduled-if-done nil
      org-agenda-skip-deadline-if-done nil
      org-agenda-move-date-from-past-immediately nil
      org-agenda-start-on-weekday 1
      org-agenda-start-hour 8
      org-deadline-warning-days 0
      org-agenda-block-separator ?─
      org-agenda-sorting-strategy '((todo priority-down todo-state-up alpha-up))
      org-agenda-time-grid
      '((daily today require-timed)
        (800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800)
        "  "
        " ················"))

(setq org-tag-alist
      '(("atheism"     . ?A) ("ada"        . ?a) ("adhd"      . ?D)
        ("discuss"     . ?d) ("emacs"      . ?e) ("workflow"  . ?f)
        ("programming" . ?g) ("thoughts"   . ?h) ("habit"     . ?i)
	("house"       . ?H) ("inbox"      . ?I)
        ("λ:ideation" . ?j)  ("blog"       . ?L) ("meeting"   . ?m)
        ("homelab"     . ?o) ("personal"   . ?p) ("project"   . ?P)
        ("occult"      . ?c) ("rpg"        . ?R) ("timesheet" . ?s)
        ("tasks"       . ?T) ("to_read"    . ?r) ("to_install". ?i)
        ("to_listen"   . ?l) ("to_buy"     . ?b) ("to_watch"  . ?w)
        ("music_prod"  . ?M) ("work"       . ?W)
	("@MB") ("@SC") ("@SM") ("@TB") ("@SS")))

(setq org-refile-targets
      `((,(expand-file-name "Personal.org" org-directory) :maxlevel . 3)
        (,(expand-file-name "Work.org"     org-directory) :maxlevel . 3)
        (,(expand-file-name "Emacs.org"     org-directory) :maxlevel . 3)
        (,(expand-file-name "RPG.org"     org-directory) :maxlevel . 3)
        (,(expand-file-name "Journal.org"  org-directory) :maxlevel . 3)))

(setq org-refile-use-outline-path              'file
      org-outline-path-complete-in-steps       nil
      org-refile-allow-creating-parent-nodes   'confirm
      org-refile-use-cache                     nil
;     org-log-refile                           'note
      org-log-refile                           nil
      org-reverse-note-order                   nil)

(setq org-default-notes-file (expand-file-name "Inbox.org" org-directory))

(remove-hook 'org-agenda-finalize-hook
          (lambda ()
            (with-current-buffer org-agenda-buffer-name
              (goto-char (point-min)))))

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
	 ((agenda ""
		  ((org-agenda-span 1)
		   (org-agenda-overriding-header
		    "DASHBOARD - Work + Personal + Emacs")))

	  (tags "PRIORITY=\"A\""
		((org-agenda-overriding-header "HIGH PRIORITY")))

	  (alltodo ""
		   ((org-agenda-files
		     '("~/Library/Mobile Documents/com~apple~CloudDocs/Org/Inbox.org"))
		    (org-agenda-overriding-header "INBOX")))

	  (todo "PLANNED"
		((org-agenda-overriding-header "BACKLOG")))))

        ("D" "Deadlines in next 30 days"
         ((agenda ""
                  ((org-agenda-span 30)
                   (org-agenda-start-day "+0d")
                   (org-agenda-entry-types '(:deadline))
                   (org-deadline-warning-days 30)
                   (org-agenda-overriding-header
                    "Upcoming deadlines (next 30 days)")))))

	("i" "Inbox"
	 ((alltodo ""
		  ((org-agenda-files
		    '("~/Library/Mobile Documents/com~apple~CloudDocs/Org/Inbox.org"))
		   (org-agenda-overriding-header "INBOX")))))

        ("n" "Agenda + Planned"
         ((agenda "")
          (todo "PLANNED")))

        ("p" "Personal agenda + tasks"
         ((agenda ""
                  ((org-agenda-span 7)
                   (org-agenda-files
                    '("~/Library/Mobile Documents/com~apple~CloudDocs/Org/Personal.org"))
                   (org-agenda-overriding-header
                    "PERSONAL DASHBOARD - Week Agenda")))
          (todo "PLANNED"
                ((org-agenda-files
                  '("~/Library/Mobile Documents/com~apple~CloudDocs/Org/Personal.org"))
                 (org-agenda-overriding-header "Personal Tasks")))))

        ("w" "Work agenda + tasks"
         ((agenda ""
                  ((org-agenda-span 7)
                   (org-agenda-files
                    '("~/Library/Mobile Documents/com~apple~CloudDocs/Org/Work.org"))
                   (org-agenda-overriding-header
                    "WORK DASHBOARD - Week Agenda")))
          (todo "PLANNED"
                ((org-agenda-files
                  '("~/Library/Mobile Documents/com~apple~CloudDocs/Org/Work.org"))
                 (org-agenda-overriding-header "Work Tasks")))))

        ("e" "Emacs agenda + tasks"
         ((agenda ""
                  ((org-agenda-span 7)
                   (org-agenda-files
                    '("~/Library/Mobile Documents/com~apple~CloudDocs/Org/Emacs.org"))
                   (org-agenda-overriding-header
                    "EMACS DASHBOARD - Week Agenda")))
          (todo "PLANNED"
                ((org-agenda-files
                  '("~/Library/Mobile Documents/com~apple~CloudDocs/Org/Emacs.org"))
                 (org-agenda-overriding-header "Emacs Tasks")))))))

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
         (file+olp ,(expand-file-name "Personal.org" org-directory) "Miscellaneous Notes")
         "* %?\n  Captured on: %U\n")

        ("pt" "New Task" entry
         (file+olp ,(expand-file-name "Personal.org" org-directory) "Tasks")
         "* %?\n  Captured on: %U\n")

	("pa"
         "Daily Tarot" entry
         (file+headline "~/Org/Personal.org" "Occult")  ; target under * Occult
         "**** %<%Y-%m-%d-%A>\n- RWS: \n- Thoth: \n- Reflection: " ; template content
         :empty-lines 1)

	("pj" "Journal entry" plain (function org-journal-find-location)
                               "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
                               :jump-to-captured t :immediate-finish t)

	("pf" 
	 "Film log" entry
         (file+olp ,(expand-file-name "Personal.org" org-directory) "Media" "Films" "Watched")
	 ;;         (file+olp+datetree "~/Org/Personal.org" "Media" "Films" "Watched")
         "****** %^{Film title}\n:PROPERTIES:\n:rating: %^{Rating}\n:director: %^{Director}\n:year: %^{Year}\n:url: %^{URL}\n:weather: %^{Weather}\n:tags: %^{Tags}\n:END:\n- %?"
         :empty-lines 1)

	("pr" "Read later" entry
         (file+olp ,(expand-file-name "Personal.org" org-directory) "Media" "Blogs")
         "* %? %a\n")

        ("w" "Work")

        ("wn" "New Note" entry
         (file+olp ,(expand-file-name "Work.org" org-directory) "Notes")
         "* %?\n  Captured on: %U\n")

        ("wt" "New Task" entry
         (file+olp ,(expand-file-name "Work.org" org-directory) "Tasks")
         "* TODO %?\n  Captured on: %U\n")

	("wm" "Meeting with Outstanding Meeting Actions" entry
         (file+olp "~/Org/Work.org" "Meetings")
         "** %<%Y-%m-%d-%A> - %^{Meeting Title}\n*** Notes\n*** TODO Outstanding Actions [/]:actions:\nSCHEDULED: <%<%Y-%m-%d %a>>\n- [ ] \n%?")))

(require 'org)
(require 'org-capture)

(defvar tw/org-capture-frame-name "org-capture"
  "Name used for the temporary Org capture frame.")

(defun tw/org-capture-delete-frame ()
  "Delete the current frame if it's the Org capture frame."
  (when (equal tw/org-capture-frame-name (frame-parameter nil 'name))
    (delete-frame)))

(defun tw/org-capture-force-single-window ()
  "Ensure capture frame shows only the capture buffer (no splits)."
  (when (equal tw/org-capture-frame-name (frame-parameter nil 'name))
    (delete-other-windows)))

(with-eval-after-load 'org-capture
  ;; When the capture buffer is created, force the frame to one window.
  (add-hook 'org-capture-mode-hook #'tw/org-capture-force-single-window)

  ;; Close the frame after successfully finalizing capture (C-c C-c).
  (add-hook 'org-capture-after-finalize-hook #'tw/org-capture-delete-frame)

  ;; Close the frame after aborting capture (C-c C-k).
  (advice-add 'org-capture-kill :after #'tw/org-capture-delete-frame))

(defun tw/org-capture-frame ()
  "Create a centered, isolated frame for Org capture, then prompt for template."
  (interactive)
  (let* ((frame-width 120)
         (frame-height 50)
         (screen-width (display-pixel-width))
         (screen-height (display-pixel-height))
         (char-width (frame-char-width))
         (char-height (frame-char-height))
         (frame-pixel-width (* frame-width char-width))
         (frame-pixel-height (* frame-height char-height))
         (left (/ (- screen-width frame-pixel-width) 2))
         (top (/ (- screen-height frame-pixel-height) 2))
         (frame (make-frame `((name . ,tw/org-capture-frame-name)
                              (width . ,frame-width)
                              (height . ,frame-height)
                              (left . ,left)
                              (top . ,top)
                              (minibuffer . t)
                              (internal-border-width . 1)
                              (font . "JetBrains Mono-12")
                              (auto-raise . t)
                              (z-group . above)))))
    (select-frame-set-input-focus frame)
    (raise-frame frame)
    (delete-other-windows)
    ;; No key here => Org prompts for template.
    (org-capture)))


;;
;; Configure custom global key bindings keyboard shortcuts
;; -----------------------------------------------------------------------------
;; NOTE: Any keyboard shortcuts that are bound to external packages:
;;       are defined within the (use-package) definition.
;;       Those keyboard shortcuts should also be referenced here for clarity

(global-set-key (kbd "C-c c r g") 'consult-ripgrep)
(global-set-key (kbd "C-c o t") 'org-tags-view)
;(global-set-key (kbd "M-o") 'dired-omit-mode)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-c i b") 'ibuffer)
(global-set-key (kbd "<M-S-up>") 'text-scale-increase)
(global-set-key (kbd "<M-S-down>") 'text-scale-decrease)
(global-set-key (kbd "C-c s l") 'org-store-link)
(global-set-key (kbd "C-\\") 'undo-redo)
					;(global-set-key (kbd "C-c j") (lambda () (interactive) (info "/usr/local/share/info/jargon.info.gz")))
					;(global-unset-key (kbd "C-c j"))
(global-set-key (kbd "C-c i c b") 'tw/insert-src-block)
(global-set-key (kbd "C-c w") #'search-web)
(global-set-key (kbd "C-x ]") 'enlarge-window)
(global-set-key (kbd "C-c c f") 'global-display-fill-column-indicator-mode)
(global-set-key (kbd "C-c c b") 'tw/create-jekyll-post)
(global-set-key (kbd "C-c t t") 'tw/toggle-transparency)
(global-set-key (kbd "C-x a s") 'async-shell-command)
(global-set-key (kbd "C-x v t") 'multi-vterm)
(global-set-key (kbd "C-x C-h") 'tw/highlight-line)
(global-set-key (kbd "C-c o a") 'org-agenda) ;; FIXME: move to org use-package
(global-set-key (kbd "C-c d f") 'tw/dired-filter-files)
(global-set-key (kbd "C-c d F") 'tw/dired-filter-out-files)
(global-set-key (kbd "C-x b") #'consult-buffer)
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
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-r") 'consult-line)
(global-set-key (kbd "C-c l c f") 'tw/list-files-changed-on-disk)
(global-set-key (kbd "C-c v") 'visual-line-mode)
(global-set-key (kbd "C-c =") 'balance-windows-area)
(global-set-key (kbd "C-c e") 'forward-sexp)
(global-set-key (kbd "C-c a") 'backward-sexp)
(global-set-key (kbd "C-c t h") 'tw/hide-org-tags) ;; FIXME: move to org use-package
(global-set-key (kbd "C-c i d") 'tw/insert-current-date)
(global-set-key (kbd "C-c i t") 'tw/insert-current-time)
(global-set-key (kbd "C-c i j") #'tw/insert-todays-journal-entry)
(global-set-key (kbd "C-c i r") #'string-insert-rectangle)
(global-set-key (kbd "C-c c h") #'consult-org-heading)
(global-set-key (kbd "C-c c c") #'org-capture)
(global-set-key (kbd "C-c C-i") #'consult-imenu)
(global-set-key (kbd "C-x c") #'compile)

;;(global-set-key (kbd "C-c d") #'diredc)
;;(global-set-key (kbd "C-c d i g") #'tw/find-grep-dired-ignore-case)
;;(global-set-key (kbd "C-x w") 'tw/vertico-switch-to-window-by-buffer)
;;(global-set-key (kbd "C-x b") #'switch-to-buffer)

;;
;; Sanity check init comletion
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "✅ init completed in %s" (emacs-init-time))))
