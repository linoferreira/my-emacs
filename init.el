;;; A simple, modern-looking Emacs config
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
;; disable task bar and menu
(tool-bar-mode -1)
(menu-bar-mode -1)

;; font (set font to Fira Code if it is available)
(when (member "Fira Code" (font-family-list))
  (set-frame-font (font-spec :family "Fira Code" :size 16)))

;; 80 char ruler
(use-package fill-column-indicator
  :ensure t)
(add-hook 'after-change-major-mode-hook 'fci-mode)
;; (global-display-fill-column-indicator-mode)  -- for Emacs 27

;; file tree (toggle with `M-x treemacs`)
(use-package treemacs
  :ensure t)

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
(unless (package-installed-p `magit)
  (package-install `magit))
(global-set-key (kbd "C-x g") 'magit-status)  ; `C-x g` - check status

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
;; (if you wish to use evil-mode, uncomment the lines below)
;; (require 'evil)
;; (evil-mode 1)
