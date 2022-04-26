;;===========================================================================;;
;; INIT.EL:
;;===========================================================================;;

;;-----------------------------------------------------------------------------
;; STARTUP:
;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
;; Lower threshold to speed up garbage collection
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))

;; load some basic settings
(load "~/.emacs.d/config/settings.el")
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; PACKAGES SETUP:
;;-----------------------------------------------------------------------------
(setq package-archives
    '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;;; bootstrap use-package
(package-initialize)

(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-verbose t)
(setq package-native-compile t)
(setq comp-deferred-compilation t)

;; loading org file
;; (org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;;-----------------------------------------------------------------------------
;; LOAD CONFIG:
;;-----------------------------------------------------------------------------
(load "~/.emacs.d/config/utils.el") ;; some functions and pluggins
(load "~/.emacs.d/config/completion.el") ;; corfu & more
(load "~/.emacs.d/config/evil-mappings.el") ;; evil and mappings
(load "~/.emacs.d/config/orgmode.el") ;; org stuff
(load "~/.emacs.d/config/color.el") ;; theming and modeline format
(load "~/.emacs.d/config/conf-eshell.el") ;; eshell
(load "~/.emacs.d/config/lsp.el") ;; lsp

;;-----------------------------------------------------------------------------
;; custom variables (emacs stuff)
;;-----------------------------------------------------------------------------
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(hindent org-capture yasnippet-snippets which-key visual-fill-column vertico use-package undo-fu org-tree-slide org-bullets orderless modus-themes markdown-mode key-chord haskell-mode evil-vimish-fold evil-surround evil-smartparens evil-lion evil-commentary eglot corfu-doc cape)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.05))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.1)))))
