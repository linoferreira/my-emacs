This is my basic Emacs init file configuration. 

### Packages

This config makes use of the following packages, listed here by category:

**Package management:**
- use-package
  
**Code completion:**
- ivy
- company-mode

**Language support:**
- ess (for R)
- auctex
- markdown

**Tools:**
- magit

**Aesthetics:**
- fill-column-indicator
- all-the-icons
- doom-themes
- doom-modeline
- treemacs

### Installation

Begin by installing all the packages above. The easiest way to do that is probably to use the MELPA package repository, adding the following lines to your Emacs init file:
```
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
```

Having done this, restart Emacs and install the packages using ``M-x package install RET 'package name`` . 

Note that, after installing the ``all-the-icons`` pack you should also install the fonts included in the package by running ``M-x all-the-icons-install-fonts
``. Finally, I use the [Fira Code font](https://github.com/tonsky/FiraCode), a monospaced font with ligatures, which must also be installed in your system.

### Screenshot
![screenshot](screenshot.png)
