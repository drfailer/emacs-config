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
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; PACKAGES:

;; dired:
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")))

;; WICHKEY
(use-package which-key
:ensure t
:config
(which-key-mode))

;; SINIPPETS
(use-package yasnippet
  :ensure t
  :init
    (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

;;; FOLDING
;; (use-package hideshow
;;   :hook (prog-mode . hs-minor-mode)
;;   :bind (:map hs-minor-mode-map
;;               ("C-c  C" . hs-toggle-hiding)))

;; haskell mode
(use-package haskell-mode
  :ensure t)

;; adding .hs extension for haskell mode
(push '("\\.hs\\'" . haskell-mode) auto-mode-alist)


;; load some files
(load "~/.emacs.d/config/completion.el") ;; corfu & more
(load "~/.emacs.d/config/evil.el") ;; evil stuff
(load "~/.emacs.d/config/orgmode.el") ;; org stuff
(load "~/.emacs.d/config/color.el") ;; theming
(load "~/.emacs.d/config/lsp.el") ;; lsp
(load "~/.emacs.d/config/conf-eshell.el") ;; eshell

;;-----------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(visual-fill-column org-tree-slide yasnippet-snippets which-key vertico use-package undo-fu spacegray-theme org-bullets orderless modus-themes key-chord haskell-mode general evil-vimish-fold evil-surround evil-smartparens evil-lion evil-commentary eglot corfu-doc cape annalist)))
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
(put 'dired-find-alternate-file 'disabled nil)
