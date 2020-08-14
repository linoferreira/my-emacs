;; package set up ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MELPA repo to download packages from
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use-package to simplify package loading
(unless (package-installed-p `use-package)
  (package-refresh-contents)
  (package-install `use-package))
(eval-when-compile
  (require 'use-package))
;; download & install packages if not available
(require 'use-package-ensure)
(setq use-package-always-ensure t)



;; aesthetics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disable start-up screen
(setq inhibit-startup-screen t)
;; disable toolbar, menu bar and scroll bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1) 

;; fonts
;; set default font
(set-frame-font "Source Code Pro-10")
;; set variable-width font
(set-face-font 'variable-pitch "Source Sans Pro-12")
;; choose when to use variable-width font smartly
(use-package mixed-pitch
  :hook (text-mode . mixed-pitch-mode))

;; icons
(use-package all-the-icons)
(unless (member "all-the-icons" (font-family-list))
  (all-the-icons-install-fonts t))

;; theme
(use-package doom-themes
  :config
  (load-theme 'doom-one-light t)  ; theme
  (doom-themes-visual-bell-config)  ; enable flashing mode line on errors
  (setq doom-themes-treemacs-theme "doom-colors")  ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  (doom-themes-org-config))  ; corrects org-mode's native fontification
 
;; mode line
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config (setq column-number-mode t))



;; support for languages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; R
(use-package ess
  :defer t)

;; LaTeX (TODO add spell checking by default!)
(use-package auctex
  :defer t
  :hook (LaTeX-mode . visual-line-mode))

;; Markdown
(use-package markdown-mode
  :defer t)



;; auto-completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; company-mode (in-buffer code completion)
(use-package company
  :init (global-company-mode t))
(use-package company-auctex)
(use-package company-bibtex)

;; ivy (general completion of Emacs commands)
(use-package ivy
  :init (ivy-mode 1))

;; avy (jump to any location on page)
(use-package avy
  :bind ("C-;" . avy-goto-char))



;; misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil mode (vim keybindings)
(use-package evil
  :config (evil-mode 1)
  (setq-default evil-cross-lines t)  ;; cross to previous/next line when at start/end
  ;; behave like normally with visual-line-mode lines
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line))

;; file tree (toggle with `M-x treemacs`)
(use-package treemacs
  :bind ("C-c t" . treemacs))
(use-package treemacs-evil)

;; git integration
(use-package magit
  :bind ("C-x g" . magit-status))
(use-package evil-magit)

;; which-key (display available shortcuts)
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-allow-evil-operators t))

;; highlight matching parentheses (TODO simplify this config!)
;; see https://notabug.org/stefano-m/.emacs.d/src/84a0a380d943ebe1627b5f63fb4d5aec681ae81d/init.d/parens.cfg.el
;; for smartparens-config
(use-package smartparens-config
  :ensure smartparens
  :init
  (add-hook 'minibuffer-setup-hook #'turn-on-smartparens-strict-mode)
  :config
  (show-smartparens-global-mode t)
  (smartparens-global-mode t)
  (sp-use-smartparens-bindings)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  :diminish smartparens-mode)

;; set file to store customise options (create it if it doesn't exist)
(unless (file-exists-p "~/.emacs.d/custom.el")
  (write-region "" nil "~/.emacs.d/custom.el"))
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;; set folder to store backups (create it if it doesn't exist)
(unless (file-exists-p "~/.emacs.d/backups/")
  (make-directory "~/.emacs.d/backups/" t))
(setq backup-directory-alist `(("." . "~/.emacs.d/backups/")))



;; notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
(use-package org
  :hook
  ;; (org-mode . org-indent-mode)
  (org-mode . visual-line-mode)
  ;; (org-mode . flyspell-mode)
  (org-mode . variable-pitch-mode)
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  :config
  (setq org-agenda-files (quote ("~/MEGA/notes"))  ; org-agenda dirs 
  org-hide-emphasis-markers t  ; hide emphasis markers
  org-format-latex-options (plist-put org-format-latex-options :scale 1.5)  ; LaTeX preview: increase font size
  ;; org-capture
  org-capture-templates
  '(("r" "Research" entry (file+headline "~/MEGA/notes/research.org" "Capture")
     "** %U %?\n")
    ("p" "Personal" entry (file+headline "~/MEGA/notes/personal.org" "Capture")
     "** %U %?\n"))))

;; UTF-8 bullet points
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; no distractions mode
(use-package olivetti
  :bind ("C-c o" . olivetti-mode))



;; reference management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy-bibtex (use ivy completion framework for searching references)
(use-package ivy-bibtex
  :config
  (setq bibtex-completion-bibliography '("~/MEGA/gms/library/zotero.bib")
        bibtex-completion-pdf-field "file"
        bibtex-completion-notes-path "~/MEGA/notes/papers/"
        bibtex-completion-pdf-open-function  ;; open PDFs with system viewer
	(lambda (fpath)
	  (call-process "evince" nil 0 nil fpath))))
  
;; org-ref (easily add references to org files)
(use-package org-ref
  :config
  (setq reftex-default-bibliography '("~/MEGA/gms/library/zotero.bib")
	org-ref-default-bibliography '("~/MEGA/gms/library/zotero.bib")
	org-ref-bibliography-notes "~/MEGA/notes/papers/"
	org-ref-completion-library 'org-ref-ivy-bibtex))
