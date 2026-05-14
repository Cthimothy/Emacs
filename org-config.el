;;; Org-mode config

;; Set early at top level so both org and org-agenda blocks can use it
(setq org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Org/")

(require 'org)
(require 'org-capture)

(setq org-startup-folded nil)

(add-hook 'org-journal-mode-hook
          (lambda ()
            (org-overview)))

(setq org-todo-keywords
      '((sequence
         "TODO(t)"
         "PLANNED(p)"
         "|"
         "DONE(d)")))

(setq org-use-speed-commands t)
(setq diary-file "~/Org/diary")
(setq diary-show-holidays-flag nil)

(with-eval-after-load 'org
  ;; Reuse the built-in outline repeat map for Org navigation commands.
  (dolist (cmd '(org-next-visible-heading
                 org-previous-visible-heading
                 org-forward-heading-same-level
                 org-backward-heading-same-level
                 outline-up-heading))
    (put cmd 'repeat-map 'outline-navigation-repeat-map))

  (setq org-default-notes-file (expand-file-name "Inbox.org" org-directory))

  (setq org-tags-exclude-from-inheritance
        '("meeting" "project" "work" "personal" "rpg" "emacs"))

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
          (,(expand-file-name "Emacs.org"    org-directory) :maxlevel . 3)
          (,(expand-file-name "RPG.org"      org-directory) :maxlevel . 3)
          (,(expand-file-name "Journal.org"  org-directory) :maxlevel . 3)))

  (setq org-refile-use-outline-path            'file
        org-outline-path-complete-in-steps     nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-use-cache                   nil
        org-log-refile                         nil
        org-reverse-note-order                 nil))

(with-eval-after-load 'org-agenda
  (set-face-attribute 'org-agenda-diary nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'default)

  (setq org-agenda-include-diary t)

  (setq org-agenda-files
        (append
         (list
          (expand-file-name "Personal.org" org-directory)
          (expand-file-name "RPG.org"      org-directory)
          (expand-file-name "Work.org"     org-directory)
          (expand-file-name "Emacs.org"    org-directory)
          (expand-file-name "british-calendar.org" org-directory))
         (directory-files (expand-file-name "Journal/" org-directory) t "\\.org$")))

  (setq org-agenda-scheduled-leaders '("Schd:" "Schd %2dx: "))
  (setq org-agenda-deadline-leaders  '("Due: " "Due in %3d d: " "Over %3d d: "))

  (setq org-agenda-prefix-format
        '((agenda . " %-10:c%?-10t% s")
          (todo   . " %-10:c")
          (tags   . " %-10:c")
          (search . " %-10:c")))

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
        org-agenda-todo-ignore-scheduled nil
        org-agenda-sorting-strategy '((todo priority-down todo-state-up alpha-up))
        org-agenda-time-grid
        '((daily today require-timed)
          (800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800)
          "  "
          " ················"))

  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda ""
                    ((org-agenda-span 1)
                     (org-agenda-overriding-header
                      "DASHBOARD - Work + Personal + Emacs")))

            (tags "PRIORITY=\"A\""
                  ((org-agenda-overriding-header "HIGH PRIORITY")))

            (todo "TODO"
                  ((org-agenda-overriding-header "UNSCHEDULED TASKS (NON-PRIORITY)")
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if
                      'scheduled 'deadline
                      'regexp "\\[#A\\]"))))

            (alltodo ""
                     ((org-agenda-files
                       '("~/Library/Mobile Documents/com~apple~CloudDocs/Org/Inbox.org"))
                      (org-agenda-overriding-header "INBOX")))

            (alltodo ""
                     ((org-agenda-files
                       (directory-files-recursively
                        "~/Library/Mobile Documents/com~apple~CloudDocs/Org/Journal/"
                        "^Journal-[0-9]\\{4\\}-[0-9]\\{2\\}\\.org$"))
                      (org-agenda-overriding-header "JOURNAL")))

            (todo "PLANNED"
                  ((org-agenda-overriding-header "BACKLOG TASKS")))))

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
                   (org-agenda-overriding-header "Emacs Tasks"))))))))  ; ← end of org-agenda block


;;; Org Capture

(setq org-capture-templates
      `(("i" "Inbox" entry
         (file ,org-default-notes-file)
         "\n* TODO %?\n  Captured: %U\n")

        ("h" "New Habit" entry
         (file+headline
          (lambda () (expand-file-name "Personal.org" org-directory))
          "Habits")
         "* HABIT %?\n  SCHEDULED: <%<%Y-%m-%d> +1d>\n  :PROPERTIES:\n  :STYLE:    habit\n  :END:\n"
         :empty-lines 1)

        ("p" "Personal")
        ("pn" "New Note" entry
         (file+olp ,(expand-file-name "Personal.org" org-directory) "Commonplace Book")
         "* %?\n  Captured on: %U\n")

        ("pt" "New Task" entry
         (file+olp ,(expand-file-name "Personal.org" org-directory) "Tasks")
         "* %?\n  Captured on: %U\n")

        ("pa" "Daily Tarot" entry
         (file+headline "~/Org/Personal.org" "Occult")
         "**** %<%Y-%m-%d-%A>\n- RWS: \n- Thoth: \n- Reflection: "
         :empty-lines 1)

        ("pf" "Film log" entry
         (file+olp ,(expand-file-name "Personal.org" org-directory) "Media" "Films" "Watched")
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
  (add-hook 'org-capture-mode-hook #'tw/org-capture-force-single-window)
  (add-hook 'org-capture-after-finalize-hook #'tw/org-capture-delete-frame)
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
         (top  (/ (- screen-height frame-pixel-height) 2))
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
    (org-capture)))
(provide 'org-config.el)
