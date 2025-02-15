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
     ((t (:foreground "LightSkyBlue" :weight bold :underlined t :height 1.1))))
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

  (setq org-hide-emphasis-markers t)
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
  
  (setq org-agenda-files
	(append
	 '(
	   "~/Org/Emacs.org"
	   "~/Org/Inbox.org"
	   "~/Org/Projects.org"
	   "~/Org/RPG.org"
	   "~/Org/Tasks.org"
	   "~/Org/Work.org")
	 (directory-files-recursively "~/Denote/" "__journal\\.org$")))
  
  (setq org-refile-targets
        '(("~/Org/Projects.org" :maxlevel . 1)
          ("~/Org/Tasks.org" :maxlevel . 1)
          ("~/Org/Atheism.org" :maxlevel . 1)
          ("~/Org/Emacs.org" :maxlevel . 1)
          ("~/Org/RPG.org" :maxlevel . 1)
          ("~/Org/Work.org" :maxlevel . 3)
          ("~/Org/Inbox.org" :maxlevel . 1)))

  (setq org-capture-templates `(
                                ("i" "INBOX" entry
                                 (file+headline "~/Org/Inbox.org"
                                                "") "* TODO %i%?")
                                ("t" "General Task" entry
                                 (file+headline "~/Org/Tasks.org"
                                                "To Do") "* TODO %i%?")
                                ("A" "Atheism Note" entry
                                 (file+headline "~/Org/Atheism.org"
                                                "INBOX") "** %i%?")
                                ("w" "Work Task" entry
                                 (file+headline "~/Org/Work.org"
                                                "INBOX") "* TODO %i%?")))

  (setq org-agenda-custom-commands
        '(
          ("g" "Agenda for week and all tasks"
           ((agenda "" ((org-agenda-overriding-header "Agenda")
                        (setq org-agenda-span 'week)
			(setq org-agenda-sorting-strategy
			      '((agenda todo-state-up)
				(todo todo-state-up)
				(tags todo-state-up)
				(search todo-state-up)))
			(setq org-agenda-start-day "+0d")
			(setq org-agenda-start-on-weekday t)
;			(setq org-agenda-start-on-weekday nil)
			(setq org-deadline-warning-days 0)
(makunbound 'org-agenda-prefix-format)
(makunbound 'org-prefix-format)
			(setq org-agenda-prefix-format '((agenda  . " %-12T")))
			(setq org-prefix-format
			      '(
				;(agenda . "  %-12:c%?-12t% s")
				(timeline . "  %?-12t% s")
				(todo . "  %-12:c")
				(search . "  %-12:c")
				(tags . "  %-12:c")
				(tags-todo . "  %-12:c")))
			(setq org-agenda-remove-tags t)

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

          ("w" "Work Tasks"
           ((agenda "" ((org-agenda-overriding-header "Work Tasks")))
            (tags-todo "@Work")))
          ("p" "Personal Tasks"
           ((tags-todo "@Personal" ((org-agenda-overriding-header "Personal Tasks")))
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

  :config
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  ;(add-hook 'find-file-hook #'denote-link-buttonize-buffer)
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
    (denote (format-time-string "%A %e %B %Y") '("journal"))
    (insert "* Today's Journal\n" "** Tasks\n" "** Notes\n")
    
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

(load-file "~/.emacs.d/init-late.el")

