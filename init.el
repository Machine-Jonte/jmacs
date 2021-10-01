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

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; Rebind global set
;; (global-set-key (kbd "C-i") 'universal-argument)

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


;; Line number
(column-number-mode)
(global-display-line-numbers-mode 1)
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(desktop-save-mode 1)
; (set-face-attribute 'default nil :font "Fira Code" :height 110) ; Font
; (set-face-attribute 'default nil :font "Ubuntu Mono" :height 150) ; Font
(set-face-attribute 'default nil :font "Consolas" :height 150) ; Font
; (whitespace-mode) ; See whitespaces

; Org setup
(setq org-startup-indented t
      ; org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-latex-create-formula-image-program 'dvisvgm
      ; org-latex-create-formula-image-program 'dvipng
      ; org-pretty-entities nil
      org-image-actual-width '(300))
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
; (use-package evil
;   :init
;   (setq evil-want-integration t)
;   (setq evil-want-keybinding nil)
;   (setq evil-want-C-u-scroll t)
;   (setq evil-want-C-i-jump t)
;   :hook (evil-mode . jmacs/evil-hook)
;   :config
;   (evil-mode 1))

(use-package evil)
(require 'evil)
(evil-mode 1)

(define-key evil-normal-state-map (kbd "C-b") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-b") 'evil-scroll-up)
(define-key evil-insert-state-map (kbd "C-b")
  (lambda ()
    (interactive)
    (evil-delete (point-at-bol) (point))))


;; Install pkgs
; (global-flycheck-mode)
; (use-package flycheck
;   :ensure t
;   :init
;   (global-flycheck-mode t))
(use-package org-superstar
      :config
      (setq org-superstar-special-todo-items t)
      (add-hook 'org-mode-hook (lambda ()
                                 (org-superstar-mode 1))))
(use-package org-appear
    :hook (org-mode . org-appear-mode))
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

(use-package evil-collection
  :after magit)
(evil-collection-init)

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
  :init (load-theme 'doom-ayu-mirage t))
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
			  :global-prefix "C-SPC"))
(jmacs/leader-keys
 "f" '(counsel-find-file :which-key "find file")
 "e" '(eval-last-sexp :which-key "eval last exp")
 "SPC" '(counsel-M-x :which-key "runs command")
 "p" '(projectile-command-map :which-key "projectile command map")
 "s" '(counsel-projectile-rg :which-key "search current project")
 "b" '(:ignore t :which-key "buffers")
 "bb" '(counsel-ibuffer :which-key "switch buffer")
 "bh" '(previous-buffer :which-key "previous buffer")
 "bl" '(next-buffer :which-key "next buffer")
 "bk" '(kill-buffer :which-key "kill buffer")
 "t" '(:ignore t :which-key "toggles")
 "tt" '(counsel-load-theme :which-key "choose theme"))


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
   '(evil-collection evil-magit counsel-projectile projectile org-superstar org-appear smooth-scrolling exec-path-from-shell general helpful ivy-rich which-key rainbow-delimiters doom-modeline doom-themes counsel ayu-theme use-package swiper lsp-mode evil command-log-mode))
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
