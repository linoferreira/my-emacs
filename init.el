;; package set up
;; MELPA repo
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; use-package
(eval-when-compile
  (require 'use-package))


;; support for languages
;; R
(use-package ess
  :defer t)


;; auto-completion
;; company-mode (in-buffer completion)
(add-hook 'after-init-hook 'global-company-mode)

;; ivy (mini-buffer completion)
(ivy-mode 1)


;; aesthetics
;; disable task bar and menu
(tool-bar-mode -1)
(menu-bar-mode -1)

;; 80 char ruler
(require 'fill-column-indicator)
(add-hook 'after-change-major-mode-hook 'fci-mode)
;; (global-display-fill-column-indicator-mode)  -- for Emacs 27

;; file tree
(use-package treemacs)

;; font
(set-frame-font (font-spec :family "Fira Code" :size 16))

;; icons (requires fonts to be installed manually, see GitHub)
(use-package all-the-icons)

;; doom theme
(use-package doom-themes
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
  :init (doom-modeline-mode 1))

;; key bindings
(global-set-key (kbd "C-x g") 'magit-status)  ; magit


;; set file to store customise options
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;; key bindings
;; evil-mode
;;(require 'evil)
;;(evil-mode 1)
;;(with-eval-after-load 'evil-maps
;;  (define-key evil-motion-state-map (kbd ":") 'evil-repeat-find-char)
;;  (define-key evil-motion-state-map (kbd ";") 'evil-ex))
;;(require 'evil-magit)
;;(evil-ex-define-cmd "gg" 'magit-status)
