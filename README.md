This repo contains my basic Emacs init file configuration.

### Packages

It makes use of the following packages, listed here by category:

  - **Code completion:**
    - [ivy](https://github.com/abo-abo/swiper) (general completion)
    - [company-mode](https://company-mode.github.io/) (standard in-buffer drop-down completion)


  - **Language support:**
    - [ess](https://ess.r-project.org/) (for R)
    - [auctex](https://www.gnu.org/software/auctex/) (for LaTeX)
    - [markdown](https://jblevins.org/projects/markdown-mode/) (for Markdown)


  - **Tools:**
    - [magit](https://magit.vc/) (for Git)


  - **Aesthetics:**
    - [fill-column-indicator](https://www.emacswiki.org/emacs/FillColumnIndicator) (will no longer be needed with Emacs 27) (vertical ruler at 80 char)
    - [all-the-icons](https://github.com/domtronn/all-the-icons.el) (icons)
    - [doom-themes](https://github.com/hlissner/emacs-doom-themes) (main theme)
    - [doom-modeline](https://github.com/seagle0128/doom-modeline) (mode line)
    - [treemacs](https://github.com/Alexander-Miller/treemacs) (tree file browser)


  - **Misc:**
    - [use-package](https://github.com/jwiegley/use-package) (improves the init file package configuration)
	

### Installation

Begin by installing all the packages above. A simple way to do that is to use the MELPA package repository, adding the following lines to your Emacs init file:
```
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
```

Having done this, restart Emacs and install the packages using ``M-x package install RET 'package name`` . 

Note that after installing the ``all-the-icons`` pack you should also install the fonts included in the package by running ``M-x all-the-icons-install-fonts
``. Finally, you must also install [Fira Code font](https://github.com/tonsky/FiraCode), a monospaced font with ligatures, on your system.


### Screenshot
![screenshot](screenshot.png)
