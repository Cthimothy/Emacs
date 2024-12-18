(setq debug-on-error t)

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

(let ((host (system-name)))
  (cond
   ((string-equal host "azathoth")
    (load "~/.emacs.d/init-azathoth.el"))
   ((string-equal host "DHW392J4FQ")
    (load "~/.emacs.d/init-macbook.el"))
   (t
    (message "No specific configuration for this host"))))
