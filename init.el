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
(load "~/.emacs.d/config/modeline.el") ;; modeline settings
(load "~/.emacs.d/config/theme.el") ;; theme settings
(load "~/.emacs.d/config/conf-eshell.el") ;; eshell
(load "~/.emacs.d/config/treemacs.el") ;; treemacs
(load "~/.emacs.d/config/lsp.el") ;; lsp

;;-----------------------------------------------------------------------------
;; custom variables (emacs stuff)
;;-----------------------------------------------------------------------------
