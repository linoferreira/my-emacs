;;; A basic modern-looking Emacs config
;;; Lino Ferreira

;;
;; This is a simple Emacs init file which aims to be short and easily
;; understandable. It sets up Emacs to look like a modern text editor
;; (with a dark theme and new icons, mode line and font) and adds some
;; IDE-like functionality (code completion with a drop-down menu and a
;; tree file explorer). It also installs modes for coding in R and
;; Markdown and sets some additional options (ruler at 80 char, etc.).
;;
;; To use this config, place the init file in your '.emacs.d/' folder.
;;
;; When you first run Emacs, it will install the following packages
;; if they are not already available:
;;  - use-package
;;  - fill-column-indicator
;;  - treemacs
;;  - all-the-icons
;;  - doom-themes
;;  - doom-mode-line
;;  - ess
;;  - markdown-mode
;;  - company
;;  - ivy
;;  - magit
;;


;; package set up
;; MELPA repo to download packages from
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; use-package to simplify package loading
;; (install if not yet installed, then load)
(unless (package-installed-p `use-package)
  (package-refresh-contents)
  (package-install `use-package))
(eval-when-compile
  (require 'use-package))



;; aesthetics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disable start-up screen
(setq inhibit-startup-screen t)

;; disable toolbar, menu bar and scroll bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1) 

;; fonts
;; default mono font
(when (member "Fira Code" (font-family-list))
  (custom-set-faces
   '(default ((t (:weight normal :height 120 :family "Fira Code"))))))
;; default variable-width font (for org-mode, etc)
(when (member "Source Sans Pro" (font-family-list))
  (custom-set-faces
   '(variable-pitch ((t (:weight normal :height 140 :family "Source Sans Pro"))))))

;; 80 char ruler
(use-package fill-column-indicator
  :ensure t)
(add-hook 'after-change-major-mode-hook 'fci-mode)
;; (global-display-fill-column-indicator-mode)  -- for Emacs 27

(put 'dired-find-alternate-file 'disabled nil)

;; file tree (toggle with `M-x treemacs`)
;; (use-package treemacs
;;   :ensure t)

;; sidebar
;; (use-package dash
;;   :ensure t)
;; (use-package dash-functional
;;   :ensure t)
;; (use-package s
;;   :ensure t)
;; (use-package ov
;;   :ensure t)
;; (use-package projectile
;;   :ensure t)
;; (use-package frame-local
;;   :ensure t)

;; (add-to-list 'load-path "~/.emacs.d/font-lock-plus/")
;; (require 'font-lock+)

;; (add-to-list 'load-path "~/.local/share/icons-in-terminal/")
;; (add-to-list 'load-path "~/.emacs.d/sidebar/")
;; (require 'sidebar)
;; (global-set-key (kbd "C-x C-f") 'sidebar-open)
;; (global-set-key (kbd "C-x C-a") 'sidebar-buffers-open)

;; (use-package dired-sidebar
;;   :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
;;   :ensure t
;;   :commands (dired-sidebar-toggle-sidebar)
;;   :init
;;   (add-hook 'dired-sidebar-mode-hook
;;             (lambda ()
;;               (unless (file-remote-p default-directory)
;;                 (auto-revert-mode))))
;;   :config
;;   (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
;;   (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

;;   (setq dired-sidebar-subtree-line-prefix "__")
;;   (setq dired-sidebar-theme 'vscode)
;;   (setq dired-sidebar-use-term-integration t)
;;   (setq dired-sidebar-use-custom-font t))

;; icons
(use-package all-the-icons
  :ensure t)
(unless (member "all-the-icons" (font-family-list))
  (all-the-icons-install-fonts t))

;; doom theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
    (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; mode line
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))



;; support for languages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; R
(use-package ess
  :ensure t
  :defer t)

;; LaTeX
(use-package auctex
  :ensure t
  :defer t)

;; markdown
(use-package markdown-mode
  :ensure t
  :defer t)



;; auto-completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company-mode (in-buffer code completion)
(unless (package-installed-p `company)
  (package-install `company))
(add-hook 'after-init-hook 'global-company-mode)

;; ivy (general completion of Emacs commands)
(unless (package-installed-p `ivy)
  (package-install `ivy))
(ivy-mode 1)



;; misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git
(unless (package-installed-p 'magit)
  (package-install 'magit))
(global-set-key (kbd "C-x g") 'magit-status)  ; `C-x g` - check status

;; org-mode
(setq org-hide-emphasis-markers t)  ; hide emphasis markers
(use-package org-bullets  ; UTF-8 bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(add-hook 'org-mode-hook 'variable-pitch-mode)  ; set default font
;; code blocks, etc in fixed-width font
(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
;; enable indenting by default
(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode)))
;; LaTeX preview
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

;; set file to store customise options (create it if it doesn't exist)
(unless (file-exists-p "~/.emacs.d/custom.el")
  (write-region "" nil "~/.emacs.d/custom.el"))
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; set folder to store backups (create it if it doesn't exist)
(unless (file-exists-p "~/.emacs.d/backups/")
  (make-directory "~/.emacs.d/backups/" t))
(setq backup-directory-alist `(("." . "~/.emacs.d/backups/")))

;; vim-style keybindings
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (setq-default evil-cross-lines t)
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line))


;; which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-allow-evil-operators t)
)

;; highlight matching parentheses
(show-paren-mode 1)
