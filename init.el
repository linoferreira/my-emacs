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
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
    (load-theme 'doom-solarized-light t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

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
  :defer t)
;; markdown
(use-package markdown-mode
  :defer t)



;; auto-completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company-mode (in-buffer code completion)
(unless (package-installed-p `company)
  (package-install `company))
(add-hook 'after-init-hook 'global-company-mode)
(use-package company-auctex)
(use-package company-bibtex)

;; ivy (general completion of Emacs commands)
(use-package ivy)
(ivy-mode 1)

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

;; git
(use-package magit
  :bind ("C-x g" . magit-status))
(use-package evil-magit)

;; which-key
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-allow-evil-operators t))

;; highlight matching parentheses
;; (show-paren-mode 1)  ;; this is an Emacs function that highlights matching parentheses
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

;; terminal
;; see https://github.com/akermu/emacs-libvterm for OS dependencies
(use-package vterm)



;; reference management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ivy-bibtex
  :config
  (setq bibtex-completion-bibliography '("~/MEGA/gms/library/zotero.bib")
	;; bibtex-completion-library-path '("~/MEGA/gms/library/pdfs/")
	bibtex-completion-pdf-field "file"
	bibtex-completion-notes-path "~/MEGA/gms/library/notes/"
        bibtex-completion-pdf-open-function  ;; open PDFs with system viewer
	(lambda (fpath)
	    (call-process "evince" nil 0 nil fpath))))

(use-package org-ref
  :config
  (setq org-ref-completion-library 'org-ref-ivy-cite
	org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-ivy-bibtex
	reftex-default-bibliography '("~/MEGA/gms/library/zotero.bib")
	;; org-ref-bibliography-notes "~/MEGA/gms/library/notes/"
	org-ref-default-bibliography '("~/MEGA/gms/library/zotero.bib")
	org-ref-notes-directory "~/MEGA/org-roam/"
        org-ref-notes-function 'orb-edit-notes))
	;; org-ref-pdf-directory "~/MEGA/gms/library/pdfs/"))



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
  (setq org-todo-keywords  
	'((sequence "TODO(t)" "NEXT" "WAIT(w@/!)" "SOMEDAY" "|" "DONE(d!)" "CANCELLED(c@)")))
  (setq org-todo-state-tags-triggers  ; archive items with SOMEDAY keyword
	'(("SOMEDAY" ("ARCHIVE" . t)))) 
  ;; org-agenda dirs 
  (setq org-agenda-files (quote ("~/MEGA/gms/rotation-2/notes"
				 "~/MEGA/gms/rotation-3/notes")))
  ;; hide emphasis markers
  (setq org-hide-emphasis-markers t)  
  ;; LaTeX preview: increase font size
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
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

;; journal
(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/MEGA/org-roam/")
  (org-journal-date-format "%A, %d %B %Y"))

;; Roam
(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :config
  (setq org-roam-capture-templates
	'(("d" "default" plain (function org-roam--capture-get-point)
     "%?"
     :file-name "${slug}"
     :head "#+title: ${title}\n#+date: %<%Y-%m-%d>"
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
  (org-roam-directory "~/MEGA/org-roam")
  ;; (org-roam-index-file "~/MEGA/org-roam/index.org")
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
  :bind (:map org-mode-map
              (("C-c n a" . orb-note-actions))))

;; Deft
(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/MEGA/org-roam/"))




;; ;; tabs
;; (use-package centaur-tabs
;;   :demand
;;   :config
;;   (centaur-tabs-mode t)
;;   :bind
;;   ("C-<prior>" . centaur-tabs-backward)
;;   ("C-<next>" . centaur-tabs-forward))

;; ;; ido flexible searching
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)

;; ;; this was meant to allow clicking on the nodes to open the file in emacs
;; (require 'org-roam-protocol)
;; (defvar op-file "~/.local/share/applications/org-protocol.desktop")
;; (if (not (file-exists-p op-file))
;;     (write-region
;; "[Desktop Entry]
;; Name=Org-Protocol
;; Exec=emacsclient %u
;; Icon=emacs-icon
;; Type=Application
;; Terminal=false
;; MimeType=x-scheme-handler/org-protocol"
;;      "" op-file))
