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
  
  ;; ;; --- Org-agenda auto-refresh routines
  ;;   (defvar refresh-agenda-time-seconds 3000000)
  ;;   (defvar refresh-agenda-timer 3000000
  ;;     "Timer for `refresh-agenda-timer-function' to reschedule itself, or nil.")

  ;;   (defun refresh-agenda-timer-function ()
  ;;     ;; If the user types a command while refresh-agenda-timer
  ;;     ;; is active, the next time this function is called from
  ;;     ;; its main idle timer, deactivate refresh-agenda-timer.
  ;;     (when refresh-agenda-timer
  ;;       (cancel-timer refresh-agenda-timer))

  ;; ;;    (org-agenda nil "w")

  ;;     (setq refresh-agenda-timer
  ;;           (run-with-idle-timer
  ;;            ;; Compute an idle time break-length
  ;;            ;; more than the current value.
  ;;            (time-add (current-idle-time) refresh-agenda-time-seconds)
  ;;            nil
  ;;            'refresh-agenda-timer-function)))
  ;;   (run-with-idle-timer refresh-agenda-time-seconds t 'refresh-agenda-timer-function)

  ;;--- External org-capture routines using Automator
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
  ;; /usr/local/opt/coreutils/libexec/gnubin/cut -d' ' -f8)
  ;; /usr/local/bin/emacsclient -ne "(make-capture-frame)" -s $socketfile
  
  :config
  (setq org-hide-emphasis-markers t)
  (add-hook 'org-agenda-mode-hook 'hl-line-mode)
  (add-hook 'org-mode-hook 'hl-line-mode)
  (global-set-key (kbd "C-S-<up>") 'org-move-subtree-up)
  (global-set-key (kbd "C-S-<down>") 'org-move-subtree-down)
  
  (require 'color)
  (set-face-attribute 'org-block nil :background
                    (color-darken-name
                     (face-attribute 'default :background) 6))
    
  ;;  (setq org-agenda-start-with-follow-mode t)
  (setq org-base-path "/Users/t.welch2/Library/CloudStorage/Dropbox")
  (setq org-directory (concat org-base-path "/Org/"))
  (setq org-agenda-window-setup 'current-window)
  (setq org-adapt-xindentation t)
  (setq org-agenda-show-current-time-in-grid nil)
  (setq org-agenda-time-grid '((daily today remove-match require-timed)
                             (800 900 1000 1100  1200 1300 1400 1500 1600 1700 1800)
                             " -----" ""))
  ;; (add-hook 'org-mode-hook (lambda ()
  ;;                            (olivetti-mode 1)
  ;;                            (setq olivetti-body-width 20)))
  ;;  (remove-hook 'org-hook-mode 'olivetti-mode)
  ;;  (add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))

  (setq org-tag-list
        '(("@Emacs" . ?e)
          ("@Tasks" . ?t)
          ("@Work" . ?w)
          ("@ADA" . ?a)
          ("@RPG" . ?r)
          ("@INBOX" . ?i)))

  (setq org-archive-location "/Users/t.welch2/Library/CloudStorage/Dropbox/Org/Archive.org::")
  
  (setq org-agenda-files '(
        "/Users/t.welch2/Library/CloudStorage/Dropbox/Org/Emacs.org"
        "/Users/t.welch2/Library/CloudStorage/Dropbox/Org/Tasks.org"
        "/Users/t.welch2/Library/CloudStorage/Dropbox/Org/RPG.org"
        "/Users/t.welch2/Library/CloudStorage/Dropbox/Org/Work.org"
        "/Users/t.welch2/Library/CloudStorage/Dropbox/Org/Inbox.org"
        ;;        "/Users/t.welch2/Library/CloudStorage/Dropbox/Org/tjbw-gcal.org")
        )

  (setq org-refile-targets
        '(("/Users/t.welch2/Library/CloudStorage/Dropbox/Org/Projects.org" :maxlevel . 1)
          ("/Users/t.welch2/Library/CloudStorage/Dropbox/Org/Tasks.org" :maxlevel . 1)
          ("/Users/t.welch2/Library/CloudStorage/Dropbox/Org/Atheism.org" :maxlevel . 1)
          ("/Users/t.welch2/Library/CloudStorage/Dropbox/Org/Emacs.org" :maxlevel . 1)
          ("/Users/t.welch2/Library/CloudStorage/Dropbox/Org/RPG.org" :maxlevel . 1)
          ("/Users/t.welch2/Library/CloudStorage/Dropbox/Org/Work.org" :maxlevel . 3)
          ("/Users/t.welch2/Library/CloudStorage/Dropbox/Org/Inbox.org" :maxlevel . 1)))
        
  (setq org-capture-templates `(
                                ("i" "INBOX" entry
                                 (file+headline "~/Dropbox/Org/Inbox.org"
                                                "Inbox") "* TODO %i%?")
                                ("t" "General Task" entry
                                 (file+headline "~/Dropbox/Org/Tasks.org"
                                                "To Do") "* TODO %i%?")
                                ("w" "Work Task" entry
                                 (file+headline "~/Dropbox/Org/Work.org"
                                                "Tasks") "* TODO %i%?")))
  (setq org-agenda-custom-commands
        '(
          ("g" "Agenda for week and all tasks"
           ((agenda "" ((org-agenda-overriding-header "Agenda and all tasks")
                        (org-agenda-span 'week)
                        (org-agenda-start-day "0")))
            (alltodo "")))
           
           ("w" "Work Tasks"
            ((agenda "" ((org-agenda-overriding-header "Work Tasks")))
             (tags-todo "@Work")))
           
          ("p" "Personal Tasks"
           ((tags-todo "@Personal" ((org-agenda-overriding-header "Personal Tasks")))
            (agenda "")))
          
          ("e" "Emacs"
           ((tags-todo "@Emacs" ((org-agenda-overriding-header "Emacs Tasks")))
            (agenda "")))
          
           ("i" "INBOX"
            ((tags-todo "@INBOX" ((org-agenda-overriding-header "INBOX")))
             (agenda "")))))

  (setq org-insert-heading-respect-content t)
  (setq org-goto-interface 'outline-path-completion)
  (setq org-blank-before-new-entry '((heading) (plain-list-item)))
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo   . " ")
          (tags   . " %i %-12:c")
          (search . " %i %-12:c")))
;;  (makunbound 'org-agenda-prefix-format)

  (setq org-todo-keyword-faces
        '(("PROJECT" . "#4A90E2")
          ("org-headline-done" . "#ababab")))
  (set-face-attribute 'org-headline-done nil :foreground "olive drab" :strike-through "indian red")
  (setq org-fontify-done-headline t)
;; (set-face-foreground 'org-done "Red")

  (setq org-agenda-span 'day)
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-start-day "+0d")
;;   (setq org-agenda-custom-commands '())
  
;; 
;;   (setq org-agenda-custom-commands
;;        '(
;;          ("c" "Two weeks and tasks  agenda view"
;;           (
;;            (tags "PRIORITY=\"A\""
;;                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
;;                   (org-agenda-overriding-header "High-priority tasks:")))

;;            (agenda "")

;;            (alltodo ""
;;                     ((org-agenda-skip-function
;;                       '(or (tw/org-skip-subtree-if-priority ?A)
;;                            (org-agenda-skip-if nil '(scheduled deadline))))))))))

(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
  (setq org-superstar-leading-bullet ?\s)
  (org-superstar-mode t))

;; (use-package org-notify
;;   :ensure t
;;   :config
;;   (org-notify-start))
)
