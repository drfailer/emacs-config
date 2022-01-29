;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            PLUGGINS:                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package which-key
:ensure t
:config
(which-key-mode))

;; sinippets:
(use-package yasnippet
  :ensure t
  :init
    (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

; deletes all the whitespace when you hit backspace or delete
(use-package hungry-delete
:ensure t
:config
(global-hungry-delete-mode))

(use-package ivy
  :defer 0.1
  :diminish
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  (ivy-rich-mode 1))

(use-package counsel
  :after ivy
  :config (counsel-mode))

;; Better search
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;; Autocompletion
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))

