l(setq debug-on-error t)

;; Start Emacs in server mode
(load "server")
(unless (server-running-p)
  (server-start))
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



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3597154b88f370281a5d73f889190e1a561de1f1220237c813b6928b3db83014" "562791da8dabd79ef61d4a497f708d92c77546e39251b6f4f64bd7c12a7466df" "ec3a821777acea226811efaa9fdff5e2e99e77973c4686afdd43a5fa2e55f267" "b0cbcb2fa0c69ab36f4505fec9967969b73327f1b8564f9afface8afd216bc93" "1d6b446390c172036395b3b87b75321cc6af7723c7545b28379b46cc1ae0af9e" "8d146df8bd640320d5ca94d2913392bc6f763d5bc2bb47bed8e14975017eea91" "e410458d3e769c33e0865971deb6e8422457fad02bf51f7862fa180ccc42c032" "9a977ddae55e0e91c09952e96d614ae0be69727ea78ca145beea1aae01ac78d2" "2eca138bb4bd21c5de6f2f271038ae562a1c79ccfc006b9aa4f1d31139c8824d" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" default))
 '(package-selected-packages
   '(modus-themes xclip gptel dired-hacks-utils denote-explore timu-macos-theme selected-window-accent-mode pdf-tools magit true malyon org-side-tree all-the-icons-ibuffer listen dashboard zygospore yequake which-key visual-fill-column vertico ts telephone-line taxy sr-speedbar smartparens slime sicp ripgrep rainbow-delimiters quelpa-use-package popup-kill-ring pkg-info perspective peg paredit page-break-lines ov org-roam org-bullets openwith nerd-icons-ibuffer nerd-icons-dired multi-vterm marginalia ivy-posframe ivy-explorer imenu-anywhere ht helpful golden-ratio eyebrowse exwm expand-region erc-hl-nicks erc-colorize erc equake eat dracula-theme doom-themes doom-modeline dirvish dired-single dired+ dimmer counsel consult-notes company-box auto-dim-other-buffers activities ace-window 0blayout)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 125 :family "Iosevka" :foundry "nil" :slant normal :weight medium :width normal))))
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
 '(swiper-line-face ((t (:background "gray35")))))
 

(let ((host (system-name)))
  (cond
   ((string-equal host "azathoth")
    (load "~/.emacs.d/init-azathoth.el"))
   ((string-equal host "DHW392J4FQ")
    (load "~/.emacs.d/init-macbook.el"))
   (t
    (message "No specific configuration for this host"))))
