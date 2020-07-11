;; package set up ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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



;; aesthetics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disable start-up screen
(setq inhibit-startup-screen t)
;; disable toolbar, menu bar and scroll bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1) 

;; fonts
;; default mono font
(when (member "Source Code Pro" (font-family-list))
  (setq default-frame-alist '((font . "Source Code Pro-10"))))
;; default variable-width font (for org-mode, etc)
(when (member "Source Sans Pro" (font-family-list))
  (custom-set-faces
   '(variable-pitch ((t (:weight normal :height 120 :family "Source Sans Pro"))))))

;; icons
(use-package all-the-icons)
(unless (member "all-the-icons" (font-family-list))
  (all-the-icons-install-fonts t))

;; theme
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t  ; if nil, italics is universally disabled
	doom-themes-treemacs-theme "doom-colors")  ; use the colorful treemacs theme
  (load-theme 'doom-solarized-light t)  ; theme
  (doom-themes-visual-bell-config)      ; enable flashing mode-line on errors
  (doom-themes-treemacs-config)
  (doom-themes-org-config))  ; corrects (and improves) org-mode's native fontification
;; mode line
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config (setq column-number-mode t))

;; minimap
(use-package minimap)



;; support for languages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; R
(use-package ess
  :defer t)

;; LaTeX
(use-package auctex
  :defer t
  :hook (LaTeX-mode . visual-line-mode))

;; Markdown
(use-package markdown-mode
  :defer t)



;; auto-completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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



;; misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(use-package treemacs)
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

;; highlight matching parentheses
;; (show-paren-mode 1)  ; this is an Emacs function that highlights matching parentheses
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

;; terminal emulator
;; see https://github.com/akermu/emacs-libvterm for OS dependencies
(use-package vterm)



;; notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
(use-package org
  :hook
  (org-mode . org-indent-mode)
  (org-mode . variable-pitch-mode)
  (org-mode . visual-line-mode)
  (org-mode . flyspell-mode)
  :bind ("C-c a" . org-agenda)
  :config
  ;; set task keywords
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT" "WAIT(w@/!)" "SOMEDAY" "|" "DONE(d!)" "CANCELLED(c@)"))
  org-todo-state-tags-triggers '(("SOMEDAY" ("ARCHIVE" . t))) ; archive items with SOMEDAY keyword
  org-agenda-files (quote ("~/MEGA/gms/rotation-2/notes" "~/MEGA/gms/rotation-3/notes"))  ; org-agenda dirs 
  org-hide-emphasis-markers t  ; hide emphasis markers
  org-format-latex-options (plist-put org-format-latex-options :scale 1.5))  ; LaTeX preview: increase font size
  ;; code blocks, etc in fixed-width font
  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))  
;; UTF-8 bullet points
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; journal (separate from org-roam)
(use-package org-journal
  :defer t
  :config
  (setq org-journal-dir "~/MEGA/notes/journal/"
        org-journal-date-format "%A, %d %B %Y")
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org"))

;; org-roam (zettelkasten package inspired by Roam Research)
(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :config
  (setq org-roam-capture-templates
	'(("d" "default" plain (function org-roam--capture-get-point)
     "%?"
     :file-name "${slug}"
     :head "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>"
     :unnarrowed t)))
  (defun org-roam--title-to-slug (title)
    "Convert TITLE to a filename-suitable slug. Uses hyphens rather than underscores."
    (cl-flet* ((nonspacing-mark-p (char)
                                  (eq 'Mn (get-char-code-property char 'general-category)))
               (strip-nonspacing-marks (s)
                                       (apply #'string (seq-remove #'nonspacing-mark-p
                                                                   (ucs-normalize-NFD-string s))))
               (cl-replace (title pair)
                           (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-")  ;; convert anything not alphanumeric
                      ("--*" . "-")  ;; remove sequential underscores
                      ("^-" . "")  ;; remove starting underscore
                      ("-$" . "")))  ;; remove ending underscore
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
        (s-downcase slug))))
  :custom
  (org-roam-directory "~/MEGA/notes/org-roam")
  ;; (org-roam-index-file "~/MEGA/notes/org-roam/index.org")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n j" . org-roam-jump-to-index)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))
;; org-roam-server (for visualising graph as webpage)
(use-package org-roam-server
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))
;; org-roam-bibtex
(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-templates '(("r" "ref" plain (function org-roam-capture--get-point) ""
		       :file-name "${citekey}"
		       :head "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS: paper\n"
		       :unnarrowed t))))

;; Deft (for quick searching of notes)
(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/MEGA/notes/org-roam/"))



;; reference management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy-bibtex (use ivy completion framework for searching references)
(use-package ivy-bibtex
  :config
  (setq bibtex-completion-bibliography '("~/MEGA/gms/library/zotero.bib")
        bibtex-completion-pdf-field "file"
        bibtex-completion-notes-path "~/MEGA/notes/org-roam"
        bibtex-completion-pdf-open-function  ;; open PDFs with system viewer
	(lambda (fpath)
	  (call-process "evince" nil 0 nil fpath))))
  
;; org-ref (easily add references to org files)
(use-package org-ref
  :config
  (setq reftex-default-bibliography '("~/MEGA/gms/library/zotero.bib")
	org-ref-default-bibliography '("~/MEGA/gms/library/zotero.bib")
	org-ref-bibliography-notes "~/MEGA/notes/org-roam/"
	org-ref-completion-library 'org-ref-ivy-bibtex
        org-ref-notes-function 'orb-edit-notes))  ; use org-roam-bibtex
