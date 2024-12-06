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

  (defun tw/archive-all-done-tasks-in-agenda-files ()
  "Archive all DONE tasks in all files listed in `org-agenda-files`."
  (interactive)
  (dolist (file org-agenda-files)
    (with-current-buffer (find-file-noselect file)
      (org-map-entries
       (lambda ()
         (org-archive-subtree))
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
  ;; /usr/local/opt/coreutils/libexec/gnubin/cut -d' ' -f8)
  ;; /usr/local/bin/emacsclient -ne "(make-capture-frame)" -s $socketfile
  
  :config
  (run-at-time "24:00" 86400
               (lambda () (my-org-archive-done-tasks-in-agenda-files)))

  (setq org-hide-emphasis-markers t)
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


  (setq org-tag-list
        '(("@Emacs" . ?e)
          ("@Tasks" . ?t)
          ("@Work" . ?w)
          ("@ADA" . ?a)
          ("@RPG" . ?r)
          ("@INBOX" . ?i)))

(setq org-archive-location "~/Dropbox/Org-Archive/Archive.org::")
  
  (setq org-agenda-files
	(append
	 '(
	   "/Users/t.welch2/Library/CloudStorage/Dropbox/Org/Emacs.org"
	   "/Users/t.welch2/Library/CloudStorage/Dropbox/Org/Inbox.org"
	   "/Users/t.welch2/Library/CloudStorage/Dropbox/Org/Projects.org"
	   "/Users/t.welch2/Library/CloudStorage/Dropbox/Org/RPG.org"
	   "/Users/t.welch2/Library/CloudStorage/Dropbox/Org/Tasks.org"
	   "/Users/t.welch2/Library/CloudStorage/Dropbox/Org/Work.org")
	 (directory-files-recursively "/Users/t.welch2/Library/CloudStorage/Dropbox/Denote" "__journal\\.org$")))
;;  	 (Directory-Files-recursively "/Users/t.welch2/Library/CloudStorage/Dropbox/Denote" "\\.org$")))
  
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
                                                "") "* TODO %i%?")
                                ("t" "General Task" entry
                                 (file+headline "~/Dropbox/Org/Tasks.org"
                                                "To Do") "* TODO %i%?")
                                ("A" "Atheism Note" entry
                                 (file+headline "~/Dropbox/Org/Atheism.org"
                                                "INBOX") "** %i%?")
                                ("w" "Work Task" entry
                                 (file+headline "~/Dropbox/Org/Work.org"
                                                "INBOX") "* TODO %i%?")))
;;   (setq org-agenda-custom-commands
;;         '(
;;           ("g" "Agenda for week and all tasks"
;;            ((agenda "" ((org-agenda-overriding-header "Agenda")
;;                         (setq org-agenda-span 'day)
;;                         (setq org-agenda-start-day "0")
;;                         (setq org-agenda-start-on-weekday t)
;; 			(org-agenda-entry-types '(:deadline :scheduled :timestamp))
;; 			(org-agenda-skip-function
;; 			 '(org-agenda-skip-entry-if 'nottodo 'any))
;; ;			(setq org-agenda-restriction-lock-highlight-subtree nil)
;; ;			(setq org-agenda-todo-ignore-deadlines 'past)
;; ;			(setq org-agenda-todo-ignore-scheduled 'past)
;; ;			(setq org-agenda-todo-ignore-timestamp 'past)
;;                         (org-agenda-block-separator ?\n)))
            
;; ;            (tags-todo "+PRIORITY=\"A\"" (org-agenda-overriding-header "Priority Tasks"))
;;             (tags-todo "@Work"  (org-agenda-overriding-header "Work Tasks"))
;; ;	    (tags-todo "@Emacs" ((org-agenda-overriding-header "Emacs Tasks")))
;; ;	    (tags-todo "@Projects" ((org-agenda-overriding-header "Projects Tasks")))
;; ;            (tags-todo "@Personal" ((org-agenda-overriding-header "Personal Tasks")))
;; ;            (tags-todo "@INBOX" ((org-agenda-overriding-header "INBOX")))

;; 	   ("w" "Work Tasks"
;;             ((tags-todo "@Work" ((org-agenda-overriding-header "Work Tasks")))
;;              (agenda "")))
           
;;           ("p" "Personal Tasks"
;;            ((tags-todo "@Personal" ((org-agenda-overriding-header "Personal Tasks")))
;;             (agenda "")))
          
;;           ("e" "Emacs"
;;            ((tags-todo "@Emacs" ((org-agenda-overriding-header "Emacs Tasks")))
;;             (agenda "")))
          
;;            ("i" "INBOX"
;;             ((tags-todo "@INBOX" ((org-agenda-overriding-header "INBOX")))
;;              (agenda ""))))
;; 	   )


(setq org-agenda-custom-commands
        '(
          ("g" "Agenda for week and all tasks"
           ((agenda "" ((org-agenda-overriding-header "Agenda")
                        (setq org-agenda-span 'day)
                        (setq org-agenda-start-day "0")
                        (setq org-agenda-start-on-weekday t)
			(org-agenda-entry-types '(:deadline :scheduled :timestamp))
			(org-agenda-skip-function
			 '(org-agenda-skip-entry-if 'nottodo 'any))
;			(setq org-agenda-restriction-lock-highlight-subtree nil)
;			(setq org-agenda-todo-ignore-deadlines 'past)
;			(setq org-agenda-todo-ignore-scheduled 'past)
;			(setq org-agenda-todo-ignore-timestamp 'past)
                        (org-agenda-block-separator ?\n)))

	    (tags-todo "+PRIORITY=\"A\""
		       ((org-agenda-overriding-header "Priority Tasks")))
            
            (tags-todo "@Work"  ((org-agenda-overriding-header "Work Tasks")
				 (org-agenda-sorting-strategy nil)))

            (tags-todo "@Emacs" ((org-agenda-overriding-header "Emacs Tasks")))
            (tags-todo "@Projects" ((org-agenda-overriding-header "Projects Tasks")))
            (tags-todo "@Personal" ((org-agenda-overriding-header "Personal Tasks")))
            (tags-todo "@INBOX" ((org-agenda-overriding-header "INBOX")))))
           
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
             (agenda ""))))
	)





	  
  (setq org-agenda-block-separator "")
  (set-face-attribute 'org-agenda-structure nil :underline t) 
  
;  (custom-set-faces
;   '(org-agenda-date-today ((t (:weight bold :italic t :foreground "LightGoldenRod2"))))
;   '(org-agenda-overriding-header ((t (:weight bold :foreground "green")))))

  (setq org-insert-heading-respect-content t)
  (setq org-goto-interface 'outline-path-completion)
  (setq org-blank-before-new-entry '((heading) (plain-list-item)))
  (setq org-agenda-prefix-format
;;      '((agenda . " %i %-12:c%?-12t% s")
	'((agenda . " %i %?-12t% s")
;;	'((agenda . " %-12t %s")	  
;;        (todo   . " ")
	  (todo . "%-12:c%l   ")
;;          (tags   . " %-12:c")
	  (tags   . " %i %s")
          (search . " %-12:c")))

;;  (makunbound 'org-agenda-prefix-format)

  ;; (setq org-todo-keyword-faces
  ;;       '(("PROJECT" . "#4A90E2")
  ;; 	  ("DONE" . "green")
  ;;         ("org-headline-done" . "#ababab")))

;  (setq org-set-tag-faces '("@Personal" . (:foreground "yellow" :weight bold)))
;  (setq org-set-tag-faces '("@Work" . (:foreground "green" :weight bold)))
;  (setq org-set-tag-faces '(("@Emacs" . (:foreground "magenta" :weight light))))

  
;  (set-face-attribute 'org-headline-done nil :foreground "olive drab" :strike-through "indian red")
;  (setq org-fontify-done-headline t)
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
) ; End of org-mode config

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

;; (use-package org-journal
;;   :ensure t
;;   :defer t
;;   :init
;;   ;; Change default prefix key; needs to be set before loading org-journal
;;   (setq org-journal-prefix-key "C-c j ")
;;   :config
;;   (setq org-journal-dir "~/Dropbox/Journal"
;;         org-journal-date-format "%A, %d %B %Y"))

(use-package denote
  :ensure t
  :custom
  (denote-directory "~/Dropbox/Denote/")

  :config
  (add-hook 'dired-mode-hook #'denote-dired-mode)
;  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
  (denote-dired-mode t)
  (global-set-key (kbd "C-c d n") 'denote-create-note)
  (global-set-key (kbd "C-c d f") 'tw/denote-find-file)
  (global-set-key (kbd "C-c d j n") 'tw/denote-journal)
  (global-set-key (kbd "C-c d o") (lambda ()
				    (interactive)
				    (dired denote-directory)))
  (denote-rename-buffer-mode)
  (add-hook 'dired-mode-hook #'denote-dired-mode)

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
  
  (defun tw/denote-journal ()
    "Create an entry tagged 'journal' with the date as its title."
    (interactive)
    (denote
     (format-time-string "%A %e %B %Y")
     '("journal")
     nil
     "~/Dropbox/Journal/")
    (insert "* Today's Journal\n" "** Tasks\n\n" "** Notes\n\n" "** Daily Morning Routine
- Check yesterday's journal
- Check Orgzly for tasks
- Check changed files (C-c l c f)
- Sync Orgzly/Emacs
- Check Outlook calendar
- Check INBOX
- Check To Listen
")
    
(let ((heading "Tasks")
      (case-fold-search t)) ; Make search case-insensitive
  (goto-char (point-min)) ; Start from the beginning of the buffer
  (if (re-search-forward (format org-complex-heading-regexp-format (regexp-quote heading)) nil t)
      (progn
        (end-of-line)          ; Move to the end of the heading line
        (newline)              ; Insert a newline if needed
        (message "Moved to the line under heading: %s" heading))
    (message "Heading '%s' not found" heading))))

  (defun tw/denote-find-file ()
  "Use Ivy to find a Denote file."
  (interactive)
  (let ((default-directory denote-directory))
    (ivy-read "Find Denote file: "
              (directory-files denote-directory nil "^[^.].*\\.org$")
              :action (lambda (file)
                        (find-file (expand-file-name file denote-directory)))))))
 
