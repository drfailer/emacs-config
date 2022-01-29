;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 EVIL MODE:                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; UNDO
;; Vim style undo not needed for emacs 28
(use-package undo-fu)

;;; Vim Bindings
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  ;; allows for using cgn
  ;; (setq evil-search-module 'evil-search)
  (setq evil-want-keybinding nil)
  ;; no vim insert bindings
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

;;; Vim Bindings Everywhere else
(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))


(use-package general
  :config
  (general-evil-setup t))

(use-package key-chord
  :ensure t
  )
