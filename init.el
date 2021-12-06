(setq inhibit-startup-message t)

    (scroll-bar-mode -1)                ; Disable visible scrollbar
    (tool-bar-mode -1)                  ; Disable the toolbar
    (tooltip-mode -1)                   ; Disable tooltips
    ; (set-fringe-mode 10)                ; Give some breathing room

    (menu-bar-mode 1)                  ; Remove the menu bar
    (setq visible-bell nil)             ; Remove visible-bell
    (setq ring-bell-function 'ignore)   ; Remove sound (notification)

  ;; Line number
  (column-number-mode)
  (global-display-line-numbers-mode 1)
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))
; (whitespace-mode) ; See whitespaces
;; Fix smooth scrolling
(setq redisplay-dont-pause t)

(set-face-attribute 'default nil :font "Consolas" :height 150) ; Font
(set-face-attribute 'fixed-pitch nil :font "Consolas" :height 150) ; Font
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 150 :weight 'regular) ; Font

;; set transparency
(set-frame-parameter (selected-frame) 'alpha '(99 99))
(add-to-list 'default-frame-alist '(alpha 99 99))

(visual-line-mode t)

(use-package neotree)

;; Switch option and command

;; Add extra/default MacOS commands to work with hyper key
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
  (setq mac-option-modifier 'meta) ; Set option as meta
  (setq mac-command-modifier 'hyper) ; Set cmd as hyper
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

;; Initialize Package Sources
(require 'package)
(setq use-package-always-ensure t)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Always refresh, otherwise it is a bit annoying
(package-refresh-contents)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package smooth-scrolling)
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

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

(desktop-save-mode 1)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump t))
(require 'evil)
(evil-mode 1)

;; Changes what state evil starts in
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-normal-state-map (kbd "C-b") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-b") 'evil-scroll-up)
(define-key evil-insert-state-map (kbd "C-b")
  (lambda ()
    (interactive)
    (evil-delete (point-at-bol) (point))))

(define-key evil-normal-state-map (kbd "gd") 'lsp-find-definition)

;; Changes such that it moves naturally up and down (not skip lines from visiual perspective)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;; Updates the keys in certain modes (e.g. magit can use hjkl etc. etc.)
  (use-package evil-collection
    :after (evil magit)
    :config
    (evil-collection-init))

(use-package general
  :init (general-auto-unbind-keys)
  :config
  (general-evil-setup t)
  (general-create-definer jmacs/leader-keys
                          :keymaps '(normal insert visual emacs dired-mode-map doc-view-mode-map)
                          :prefix "SPC"
                          :global-prefix "C-f"))

(jmacs/leader-keys
 "f" '(counsel-find-file :which-key "find file")
 "e" '(eval-last-sexp :which-key "eval last exp")
 "SPC" '(counsel-M-x :which-key "runs command")
 "g" '(magit :which-key "magit")
 "p" '(projectile-command-map :which-key "projectile command map")
 "s" '(counsel-projectile-rg :which-key "search current project")
 "ch" '(hippie-expand :which-key "Autocomplete file path")
 "d" '(dired :which-key "dired")
 "r" '(ranger :which-key "ranger")
 "b" '(:ignore t :which-key "buffers")
 "bb" '(counsel-ibuffer :which-key "switch buffer")
 "bl" '(ibuffer :which-key "list buffers")
 "bk" '(kill-buffer :which-key "kill buffer")
 "bs" '(hydra-buffer-switch/body :which-key "switch buffer quickly")
 "bc" '(clean-buffer-list :which-key "clean unused buffers")
 "t" '(:ignore t :which-key "toggles")
 "tt" '(counsel-load-theme :which-key "choose theme")
 "ts" '(hydra-text-scale/body :which-key "scale text")
 ; "tf" '(hydra-code-level/body :which-key "Different fold level")
 "o" '(:ignore t :which-key "org")
 "oa" '(org-agenda :which-key "agenda")
 "os" '(org-agenda :which-key "schedule")
 "od" '(org-agenda :which-key "deadline")
 "ot" '(org-time-stamp :which-key "time-stamp"))
 (global-set-key [(hyper p)] 'projectile-find-file)
 (global-set-key [(hyper b)] 'neotree-toggle)
 (global-set-key [(hyper .)] 'flyspell-correct-wrapper)
 (global-set-key [(hyper j)] 'shell-pop)
 (global-set-key [(hyper =)] 'text-scale-increase)
 (global-set-key [(hyper -)] 'text-scale-decrease)
 (global-set-key [(hyper t)] 'tab-bar-switch-to-tab)
 (global-set-key [(hyper T)] 'tab-new)
 (global-set-key [(hyper K)] 'tab-bar-close-tab)
 (global-set-key [(hyper k)] 'tab-bar-switch-to-recent-tab)

(use-package pdf-tools)
; (require 'doc-view-mode)
; (define-key doc-view-mode-map "l" 'doc-view-next-line-or-next-page)
; (define-key doc-view-mode-map "h" 'doc-view-previous-line-or-previous-page)
; (define-key doc-view-mode-map (kbd "C-s") 'isearch-forward-regexp)

(use-package ranger)

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
(use-package forge) ; Git interface (with example issues)

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



(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

; (use-package company
;   :ensure
;   :custom
;   (company-idle-delay 0.5) ;; how long to wait until popup
;   ;; (company-begin-commands nil) ;; uncomment to disable popup
;   :bind
;   (:map company-active-map
;               ("C-n". company-select-next)
;               ("C-p". company-select-previous)
;               ("M-<". company-select-first)
;               ("M->". company-select-last))
;   (:map company-mode-map
;   (("<tab>". tab-indent-or-complete)
;         ("TAB". tab-indent-or-complete))))

(use-package flycheck
  :config
  ;; the default value was '(save idle-change new-line mode-enabled)
  (setq flycheck-check-syntax-automatically '(save mode-enable)))

; Old things
; (use-package flycheck
;   :config
;   (global-flycheck-mode t)
;   (setq flycheck-highlighting-mode t))
; (use-package flycheck-aspell)
; (use-package flymake-aspell)
; (use-package flycheck-vale)
; (flycheck-vale-setup)
; (flycheck-vale-toggle-enabled)
; (flycheck-checkers 'vale)
; (setq flycheck-highlighting-mode t)

(use-package flyspell)

  ; Start flyspell-mode when text mode is started
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))

  ; For mac to make it detect two finger click on words
    (eval-after-load "flyspell"
      '(progn
         (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
         (define-key flyspell-mouse-map [mouse-3] #'undefined)))

(use-package flyspell-correct-ivy
    :bind ("C-." . flyspell-correct-wrapper)
    :init
    (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

;  (use-package typescript-mode
;    :mode "\\.ts\\'"
;    :hook (typescript-mode . lsp-deferred)
;    :config
;    (setq typescript-indent-level 2))

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  (setq lsp-eldoc-hook nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred

(use-package pyvenv
  :defer t
  :diminish
  :config
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  ; Show python venv name in modeline
      (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
      (pyvenv-mode t))

(use-package lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(use-package cmake-mode)

(use-package shell-pop
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/zsh")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(use-package diff-hl)

(use-package swiper) ; Fuzzy search in files

(use-package rainbow-delimiters ; Rainbow paranteses
  :hook (prog-mode . rainbow-delimiters-mode))

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

(use-package doom-themes
  :init (load-theme 'doom-moonlight t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config (setq doom-modeline-python-executable "python3"))

(use-package command-log-mode) ; For displaying commands

(use-package all-the-icons)

(use-package counsel ; Some nice stuff (helper for M-x etc)
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

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

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

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

(defun code-level-value-changer (key-code)
  (if "up"
      ()))

(defhydra hydra-code-level (:timeout 4)
  "switch buffer"
    ("j" (set-selective-display (- selective-display 1)))
    ("k" (set-selective-display (+ selective-display 1)))
    ("f" nil "finished" :exit t))

; (set-selective-display nil)
; (+ 1 selective-display)
; ()

;; Automatically tangle the Emacs.org config file when it is saved
 (defun jmacs/org-babel-tangle-config ()
   (when (string-equal (buffer-file-name)
                                  (expand-file-name "~/.emacs.d/Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
       (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'jmacs/org-babel-tangle-config)))

(defun jmacs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

; (defun jmacs/org-font-setup ()
;   ;; Replace list hyphen with dot
;   (font-lock-add-keywords 'org-mode
;                           '(("^ *\\([-]\\) "
;                              (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package org
  :hook (org-mode . jmacs/org-mode-setup)
  :config
  ; ▼
  (setq org-ellipsis " ▼"
        org-hide-emphasis-markers t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-src-tab-acts-natively t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (require 'org-faces)
  (setq org-image-actual-width nil)

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

(use-package org-appear
    :hook (org-mode . org-appear-mode))

(with-eval-after-load 'org-faces
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun jmacs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . jmacs/org-mode-visual-fill))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
