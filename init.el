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

;; remove initial scratch message
(setq initial-scratch-message nil)

;; left margin
(set-window-margins nil 2)

;; borrowed from Nicolas Rougier's EMACS / NANO
;; No line breat space points
(setq auto-fill-mode nil)
;; Fill column at 80
(setq fill-column 80)
;; Pixel scroll (as opposed to char scrool)
;; (pixel-scroll-mode t)
;; No tabs
(setq-default indent-tabs-mode nil)
;; Tab.space equivalence
(setq tab-width 4)
;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; fonts
;; set default font
(set-frame-font "Source Code Pro-10")
;; set variable-width font
(set-face-font 'variable-pitch "Source Sans Pro-12")

;; increase line spacing
(setq-default line-spacing 0.1)

;; icons
(use-package all-the-icons)
(unless (member "all-the-icons" (font-family-list))
  (all-the-icons-install-fonts t))

;; theme
(use-package doom-themes
  :config
  (load-theme 'doom-solarized-light t)  ; theme
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



;; auto-completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company-mode (in-buffer code completion)
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



;; misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  ;; (org-mode . variable-pitch-mode)
  (org-mode . org-indent-mode)
  (org-mode . visual-line-mode)
  (org-mode . flyspell-mode)
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  :config
  (setq org-agenda-files (quote ("~/Tresorit/notes"))  ; org-agenda dirs 
  org-hide-emphasis-markers t  ; hide emphasis markers
  org-format-latex-options (plist-put org-format-latex-options :scale 1.5)  ; LaTeX preview: increase font size
  org-archive-location "~/Tresorit/notes/archive.org::* From %s"  ; archive file
  ;; org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE"))
  ;; org-todo-state-tags-triggers '(("NEXT" ("ARCHIVE" . t)))
  ;; org-capture
  org-capture-templates
  '(("r" "Research" entry (file+headline "~/Tresorit/notes/research.org" "Capture")
     "** %U %?\n")
    ("p" "Personal" entry (file+headline "~/Tresorit/notes/personal.org" "Capture")
     "** %U %?\n"))))

;; UTF-8 bullet points for org-mode
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; centred text mode
(use-package olivetti
  :bind ("C-c o" . olivetti-mode))
(setq-default olivetti-body-width 100)

;; reference management
;; ivy-bibtex (use ivy completion framework for searching references)
(use-package ivy-bibtex
  :config
  (setq bibtex-completion-bibliography '("~/Tresorit/gms/library/zotero.bib")
        bibtex-completion-pdf-field "file"
        bibtex-completion-notes-path "~/Tresorit/notes/papers/"
        bibtex-completion-pdf-open-function  ;; open PDFs with system viewer
	(lambda (fpath)
	  (call-process "evince" nil 0 nil fpath))))
  
;; org-ref (easily add references to org files)
(use-package org-ref
  :config
  (setq reftex-default-bibliography '("~/Tresorit/gms/library/zotero.bib")
	org-ref-default-bibliography '("~/Tresorit/gms/library/zotero.bib")
	org-ref-bibliography-notes "~/Tresorit/notes/papers/"
	org-ref-completion-library 'org-ref-ivy-bibtex))

;; tramp
(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
;; (add-to-list 'tramp-remote-path "/apps/eb/skylake/software/git/2.23.0-GCCcore-8.3.0-nodocs/bin")
(put 'dired-find-alternate-file 'disabled nil)

;; flycheck
(use-package flycheck
  :ensure t)
  ;; :init (global-flycheck-mode)




;; archive ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; variable width font config
;; (let* ((variable-tuple
;;         (cond ((x-list-fonts "Noto Serif") '(:font "Noto Serif"))))
;;        (base-font-color     (face-foreground 'default nil 'default))
;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
;;   (custom-theme-set-faces
;;    'user
;;    '(variable-pitch ((t (:family "Noto Serif" :height 110))))
;;    '(fixed-pitch ((t ( :family "Source Code Pro" :height 100))))
;;    '(org-block ((t (:inherit fixed-pitch))))
;;    '(org-block-begin-line ((t (:inherit fixed-pitch))))
;;    '(org-block-end-line ((t (:inherit fixed-pitch))))
;;    '(org-code ((t (:inherit (shadow fixed-pitch)))))
;;    ;; '(org-document-info ((t (:foreground "dark orange"))))
;;    ;; '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;;    '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
;;    ;; '(org-link ((t (:foreground "royal blue" :underline t))))
;;    ;; '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;    ;; '(org-property-value ((t (:inherit fixed-pitch))) t)
;;    ;; '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;    ;; '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
;;    '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
;;    '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
;;    ))

;; choose when to use variable-width font smartly
;; (use-package mixed-pitch
;;   :hook (text-mode . mixed-pitch-mode))


;; highlight matching parentheses
;; see https://notabug.org/stefano-m/.emacs.d/src/84a0a380d943ebe1627b5f63fb4d5aec681ae81d/init.d/parens.cfg.el
;; for smartparens-config
;; (use-package smartparens-config
;;   :ensure smartparens
;;   :init
;;   (add-hook 'minibuffer-setup-hook #'turn-on-smartparens-strict-mode)
;;   :config
;;   (show-smartparens-global-mode t)
;;   (smartparens-global-mode t)
;;   (sp-use-smartparens-bindings)
;;   (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
;;   :diminish smartparens-mode)
 

;; super-agenda
;; (use-package org-super-agenda
;;   :config
;;   (setq org-super-agenda-groups
;;        '(;; Each group has an implicit boolean OR operator between its selectors.
;;          (:name "Today"  ; Optionally specify section name
;;                 :time-grid t)  ; Items that appear on the time grid 
;;          (:name "PRS portability project"
;;                 :tag "PRS")
;;          (:name "IBD recombination project"
;;                 :tag "IBD")
;;          (:name "Admin"
;;                 :tag "admin")))
;;   (org-agenda nil "a"))


;; lsp
;; (use-package lsp-mode
;;     :hook ( (ess-r-mode-hook . lsp)
;;             (lsp-mode . lsp-enable-which-key-integration))
;;     :config
;;     (setq lsp-clients-r-server-command '("R" "--slave" "-e" "languageserver::run()")) )
