;;; Startup
;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold to speed up garbage collection
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))
;;; PACKAGE LIST
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;;; BOOTSTRAP USE-PACKAGE
(package-initialize)

(load "~/.emacs.d/config/config.el")
(load "~/.emacs.d/config/orgmode.el")
(load "~/.emacs.d/config/pluggins.el")
(load "~/.emacs.d/config/evil.el")
(load "~/.emacs.d/config/mappings.el")

(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-verbose t)
(setq package-native-compile t)
(setq comp-deferred-compilation t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(ivy-rich counsel undo-fu use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
