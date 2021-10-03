; ╱╱╭┳━╮╭━┳━━━┳━━━┳━━━╮
; ╱╱┃┃┃╰╯┃┃╭━╮┃╭━╮┃╭━╮┃
; ╱╱┃┃╭╮╭╮┃┃╱┃┃┃╱╰┫╰━━╮
; ╭╮┃┃┃┃┃┃┃╰━╯┃┃╱╭╋━━╮┃
; ┃╰╯┃┃┃┃┃┃╭━╮┃╰━╯┃╰━╯┃
; ╰━━┻╯╰╯╰┻╯╱╰┻━━━┻━━━╯
;init.el

(setq inhibit-startup-message t)

(scroll-bar-mode -1)                ; Disable visible scrollbar
(tool-bar-mode -1)                  ; Disable the toolbar
(tooltip-mode -1)                   ; Disable tooltips
; (set-fringe-mode 10)                ; Give some breathing room

(menu-bar-mode 1)                  ; Remove the menu bar
(setq visible-bell nil)             ; Remove visible-bell
(setq ring-bell-function 'ignore)   ; Remove sound (notification)

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; Rebind global set
;; (global-set-key (kbd "C-i") 'universal-argument)
(defun swap-split-direction nil
  (interactive)
  (if (eq split-height-threshold 0)
      (progn
	(setq split-height-threshold nil)
	(setq split-width-threshold 0)
	)
    (progn
	(setq split-height-threshold 0)
	(setq split-width-threshold nil))))

(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Fix smooth scrolling
(setq redisplay-dont-pause t)

;; Switch option and command
(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper l)] 'goto-line)
(global-set-key [(hyper w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(hyper z)] 'undo)
(global-set-key [(hyper q)] 'save-buffers-kill-emacs)

;; Mac related stuff
;; Switch option and command key
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)
;; mac switch meta key
; see: https://gist.github.com/railwaycat/3498096
(defun mac-switch-meta nil 
  "switch meta between Option and Command"
  (interactive)
  (if (eq mac-option-modifier nil)
      (progn
	(setq mac-option-modifier 'meta)
	(setq mac-command-modifier 'hyper)
	)
    (progn 
      (setq mac-option-modifier nil)
      (setq mac-command-modifier 'meta)
      )
    )
  )

;; Open in finder
(defun show-in-finder nil
  (interactive)
  (shell-command (concat "open -R " buffer-file-name)))


;; Line number
(column-number-mode)
(global-display-line-numbers-mode 1)
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

; (set-face-attribute 'default nil :font "Fira Code" :height 110) ; Font
; (set-face-attribute 'default nil :font "Ubuntu Mono" :height 150) ; Font
(set-face-attribute 'default nil :font "Consolas" :height 150) ; Font
(set-face-attribute 'fixed-pitch nil :font "Consolas" :height 150) ; Font
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 170 :weight 'regular) ; Font
; (whitespace-mode) ; See whitespaces

(desktop-save-mode 1)

; Org setup
;(setq org-startup-indented t
;      ; org-hide-emphasis-markers t
;      org-startup-with-inline-images t
;      org-latex-create-formula-image-program 'dvisvgm
;      ; org-latex-create-formula-image-program 'dvipng
;      ; org-pretty-entities nil
;      org-image-actual-width '(300))
; (after! org (plist-put org-format-latex-options :scale 0.8))

;; Initialize Package Sources
(require 'package)
(setq use-package-always-ensure t)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Enable Evil
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump t))
(require 'evil)
(evil-mode 1)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-normal-state-map (kbd "C-b") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-b") 'evil-scroll-up)
(define-key evil-insert-state-map (kbd "C-b")
  (lambda ()
    (interactive)
    (evil-delete (point-at-bol) (point))))
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)

(use-package evil-collection
  :after (evil magit)
  :config
  (evil-collection-init))


;; Install pkgs
; (global-flycheck-mode)
; (use-package flycheck
;   :ensure t
;   :init
;   (global-flycheck-mode t))

(defun jmacs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))
;  (setq evil-auto-indent nil))

; (defun jmacs/org-font-setup ()
;   ;; Replace list hyphen with dot
;   (font-lock-add-keywords 'org-mode
;                           '(("^ *\\([-]\\) "
;                              (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package org
  :hook (org-mode . jmacs/org-mode-setup)
  :config
  (setq org-ellipsis " ▼"
	org-hide-emphasis-markers t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
	'("~/.emacs.d/Tasks.org"
	  "~/Personal/Birthdays.org"
	  "~/Personal/Habits.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
	'(("TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)"))))

(setq org-refile-targets
      '(("Archive.org" :maxlevel . 1)
	("Tasks.org" :maxlevel . 1)))

;; Save ORG buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

(setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/.emacs.d/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/Personal/journal/journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))


(define-key global-map (kbd "C-c j")
  (lambda () (interactive) (org-capture nil "jj")))


(setq org-tag-alist
      '((:startgroup)
	(:endgroup)
	("@errand" .?E)
	("@home" . ?H)
	("@work" . ?W)
	("agenda" . ?a)
	("planning" . ?p)
	("publish" . ?P)
	("batch" . ?b)
	("note" . ?n)
	("idea" . ?i)))

(use-package org-superstar
      :config
      ; (setq org-superstar-special-todo-items t)
      ; (setq org-hide-leading-stars nil)
      ; (setq org-superstar-leading-bullet ?\s)
      (setq org-superstar-cycle-headline-bullets nil)
      (setq org-superstar-headline-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
      ; (setq org-indent-mode-turns-on-hiding-stars nil)
      ; (setq org-superstar-todo-bullet-alist t)
      (add-hook 'org-mode-hook (lambda ()
                                 (org-superstar-mode 1))))

(dolist (face '((org-level-1 . 1.2)
		(org-level-2 . 1.1)
		(org-level-3 . 1.05)
		(org-level-4 . 1.0)
		(org-level-5 . 1.1)
		(org-level-6 . 1.1)
		(org-level-7 . 1.1)
		(org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

; (font-lock-add-keywords 'org-mode
; 			'(("^ *\\([-]\\) "
; 			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(use-package org-appear
    :hook (org-mode . org-appear-mode))

(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
; (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
; (set-face-attribute 'org-special-keyboard nil :inherit '(font-lock-comment-face fixed-pitch))

(defun jmacs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . jmacs/org-mode-visual-fill))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

; (use-package org-bullets
;   :after org
;   :hook (org-mode . org-bullets-mode)
;   :custom
;   (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package smooth-scrolling)
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

(use-package all-the-icons)
(use-package command-log-mode) ; For displaying commands
(use-package swiper) ; Fuzzy search in files
(use-package counsel ; Some nice stuff (helper for M-x etc)
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)) ; Git


(use-package rainbow-delimiters ; Rainbow paranteses
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable))

;; Themes
(use-package doom-themes
  :init (load-theme 'doom-one t))
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Install Ivy
(use-package ivy
  :init (ivy-mode 1)
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivt-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  )

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer jmacs/leader-keys
			  :keymaps '(normal insert visual emacs)
			  :prefix "SPC"
			  :global-prefix"C-SPC"))
(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(defhydra hydra-buffer-switch (:timeout 4)
  "switch buffer"
  ("j" previous-buffer)
  ("k" next-buffer)
  ("f" nil "finished" :exit t))

(jmacs/leader-keys
 "f" '(counsel-find-file :which-key "find file")
 "e" '(eval-last-sexp :which-key "eval last exp")
 "SPC" '(counsel-M-x :which-key "runs command")
 "g" '(magit :which-key "magit")
 "p" '(projectile-command-map :which-key "projectile command map")
 "s" '(counsel-projectile-rg :which-key "search current project")
 "d" '(dired :which-key "dired")
 "b" '(:ignore t :which-key "buffers")
 "bb" '(counsel-ibuffer :which-key "switch buffer")
 "bl" '(ibuffer :which-key "list buffers")
 "bk" '(kill-buffer :which-key "kill buffer")
 "bs" '(hydra-buffer-switch/body :which-key "switch buffer quickly")
 "bc" '(clean-buffer-list :which-key "clean unused buffers")
 "t" '(:ignore t :which-key "toggles")
 "tt" '(counsel-load-theme :which-key "choose theme")
 "ts" '(hydra-text-scale/body :which-key "scale text")
 "o" '(:ignore t :which-key "org")
 "oa" '(org-agenda :which-key "agenda")
 "os" '(org-agenda :which-key "schedule")
 "od" '(org-agenda :which-key "deadline")
 "ot" '(org-time-stamp :which-key "time-stamp"))

;; Shell fixes
; For help: https://github.com/pythonic-emacs/pyenv-mode/issues/32
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/programming")
    (setq projectile-project-search-path '("~/programming")))
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-enable-caching nil))

(use-package forge)

; (use-package counsel-projectile
;   :config (counsel-projectile-mode))

(use-package lsp-mode
  :commands lsp
  :hook ((
	  haskell-mode
	  typescript-mode
	  js2-mode
	  web-mode) . lsp)
  :bind (:map lsp-mode-map
         ("TAB" . completion-at-point))
  :custom (lsp-headerline-breadcrumb-enable nil))



;;; --- Custom set variables --- ;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1f2430" "#f28779" "#bae67e" "#ffd580" "#73d0ff" "#d4bfff" "#5ccfe6" "#cbccc6"])
 '(custom-safe-themes
   '("028c226411a386abc7f7a0fba1a2ebfae5fe69e2a816f54898df41a6a3412bb5" default))
 '(exwm-floating-border-color "#191b20")
 '(fci-rule-color "#5B6268")
 '(highlight-tail-colors
   ((("#333a38" "#99bb66" "green")
     . 0)
    (("#2b3d48" "#46D9FF" "brightcyan")
     . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(objed-cursor-color "#ff6c6b")
 '(package-selected-packages
   '(org-bullets hydra visual-fill-column visual-fill visual-fill-mode forge evil-collection evil-magit counsel-projectile projectile org-superstar org-appear smooth-scrolling exec-path-from-shell general helpful ivy-rich which-key rainbow-delimiters doom-modeline doom-themes counsel ayu-theme use-package swiper lsp-mode evil command-log-mode))
 '(pdf-view-midnight-colors (cons "#bbc2cf" "#282c34"))
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
