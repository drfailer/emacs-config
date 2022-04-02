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
(push '("\\.hs\\'" . vhdl-mode) auto-mode-alist)


;; load some files
(load "~/.emacs.d/config/completion.el") ;; corfu & more
(load "~/.emacs.d/config/evil.el") ;; evil stuff
(load "~/.emacs.d/config/orgmode.el") ;; org stuff
(load "~/.emacs.d/config/color.el") ;; theming
(load "~/.emacs.d/config/lsp.el") ;; lsp

;;-----------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-vimish-fold vimish-fold evil-lion org-bullets yasnippet-snippets which-key vertico use-package undo-fu orderless key-chord haskell-mode general evil-surround evil-smartparens evil-commentary evil-collection corfu-doc)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
