;;===========================================================================;;
;; EVIL SETTINGS:
;;===========================================================================;;

;;-----------------------------------------------------------------------------
;; load evil packages:

;; UNDO
;; vim style undo
(use-package undo-fu
  :ensure t)

;; VIM BINDINGS
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
         ;; ("C-n" . evil-next-line)
         ;; ("C-p" . evil-previous-line))
  :init
  ;; allows for using cgn
  ;; (setq evil-search-module 'evil-search)
  (setq evil-want-keybinding nil)
  ;; no vim insert bindings
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-split-window-below t)
  (setq evil-split-window-right t)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-set-leader 'normal " "))

;; vim surround
(use-package evil-smartparens
  :hook (smartparens-mode . evil-smartparens-mode))

;; Commentary
(use-package evil-commentary
  :ensure t
  :after evil
  :bind (:map evil-normal-state-map
              ("gc" . evil-commentary)))

;; Surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; Vim Bindings Everywhere else
(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

;; easier way to map stuff
(use-package general
  :config
  (general-evil-setup t))

;; enable to map keysequence without modifier key (ex: "kj" -> ESC)
(use-package key-chord
  :ensure t
  )
;;-----------------------------------------------------------------------------

;; Enable evil mode
(evil-mode 1)

;; load mappings:
(load "~/.emacs.d/config/mappings.el")
