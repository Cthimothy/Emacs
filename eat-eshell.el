;; ─── Shell environment ────────────────────────────────────────────────────────
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("NVM_DIR")))

(setenv "VISUAL" "emacsclient")
(setenv "TERM" "xterm-256color")

;; ─── EAT + Eshell integration ─────────────────────────────────────────────────
(use-package eat
  :ensure t
  :hook (eshell-mode . eat-eshell-mode)
  :config
  (setq eat-kill-buffer-on-exit t))

;; ─── Eshell ───────────────────────────────────────────────────────────────────
(with-eval-after-load 'eshell

  ;; Modules
  (add-to-list 'eshell-modules-list 'eshell-rebind)

  ;; Visual commands
  (dolist (cmd '("htop" "ssh" "less" "bat" "man" "vim" "nvim"))
    (add-to-list 'eshell-visual-commands cmd))

  ;; Aliases
  (setq eshell-command-aliases-list
        '(("ll"   "ls -lhF $*")
          ("ls"   "ls -F $*")
          ("less" "bat $*")))

  ;; History
  (setq eshell-hist-ignoredups t)
  (setq eshell-history-size 10000)

  ;; Prompt
  (setq eshell-prompt-function
        (lambda ()
          (concat
           "\n"
           (propertize "(" 'face '(:foreground "#5C6B2D" :weight bold))  ; folio green
           (propertize (abbreviate-file-name (eshell/pwd))
                       'face '(:foreground "#274C77" :weight bold))       ; folio blue
           (propertize ")" 'face '(:foreground "#5C6B2D" :weight bold))  ; folio green
           "\n"
           (propertize "λ " 'face '(:foreground "#B68A14" :weight bold))))) ; folio yellow
  (setq eshell-prompt-regexp "^λ ")

  ;; Keybindings
  (define-key eshell-mode-map (kbd "C-l") 'eshell/clear)
  (define-key eshell-mode-map (kbd "C-l") 'eshell/clear)
  (define-key eshell-mode-map (kbd "M-p") 'previous-line)
  (define-key eshell-mode-map (kbd "M-n") 'next-line)

  ;; Company: Eshell-relevant completions only
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local company-backends '(company-capf))
              (setq-local company-idle-delay nil))))

(provide 'eat-eshell)
