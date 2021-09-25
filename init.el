;; Melpa
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))

(use-package which-key
:ensure t
:config
(which-key-mode))

;; Load configuration
(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
