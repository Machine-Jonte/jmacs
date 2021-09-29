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
(set-fringe-mode 10)                ; Give some breathing room

(menu-bar-mode -1)                  ; Remove the menu bar
(setq visible-bell nil)             ; Remove visible-bell
(setq ring-bell-function 'ignore)   ; Remove sound (notification)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Line number
(column-number-mode)
(global-display-line-numbers-mode 1)
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(desktop-save-mode 1)
(set-face-attribute 'default nil :font "Fira Code" :height 110) ; Font
; (whitespace-mode) ; See whitespaces

; Org setup
; (setq org-image-actual-width nil)
(setq org-startup-indented t
      org-pretty-entities nil
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(300))

; (setq org-latex-create-formula-image-program 'dvisvgm)
(setq org-latex-create-formula-image-program 'dvipng)
;; Increase size of LaTeX fragment previews
; (plist-put org-format-latex-options :scale 2)
; (setq-default line-spacing 3)
; (setq preview-image-type 'dvipng)


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

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-insert-state-map (kbd "C-u")
  (lambda ()
    (interactive)
    (evil-delete (point-at-bol) (point))))


;; Install pkgs
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
(use-package magit) ; Git
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

;; Language Server (LSP)
;; (use-package ivy-xref
;;   :straight t
;;   :init (if (< emacs-major-version 27)
;;           (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
;;           (setq xref-show-definitions-function #'ivy-xref-show-defs)))


;; (use-package lsp-mode
;;   :straight t
;;   :commands lsp
;;   :hook ((typescript-mode js2-mode web-mode) . lsp)
;;   :bind (:map lsp-mode-map
;;          ("TAB" . completion-at-point))
;;   :custom (lsp-headerline-breadcrumb-enable nil))
;; 
;; (dw/leader-key-def
;;   "l"  '(:ignore t :which-key "lsp")
;;   "ld" 'xref-find-definitions
;;   "lr" 'xref-find-references
;;   "ln" 'lsp-ui-find-next-reference
;;   "lp" 'lsp-ui-find-prev-reference
;;   "ls" 'counsel-imenu
;;   "le" 'lsp-ui-flycheck-list
;;   "lS" 'lsp-ui-sideline-mode
;;   "lX" 'lsp-execute-code-action)
;; 
;; (use-package lsp-ui
;;   :straight t
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :config
;;   (setq lsp-ui-sideline-enable t)
;;   (setq lsp-ui-sideline-show-hover nil)
;;   (setq lsp-ui-doc-position 'bottom)
;;   (lsp-ui-doc-show))

;; (use-package lsp-ivy
;;   :hook (lsp-mode . lsp-ivy-mode))

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
   '(org-superstar org-appear smooth-scrolling exec-path-from-shell general helpful ivy-rich which-key rainbow-delimiters doom-modeline doom-themes counsel ayu-theme use-package swiper lsp-mode evil command-log-mode))
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
