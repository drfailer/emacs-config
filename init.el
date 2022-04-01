;; init.el

;; STARTUP
;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
;; Lower threshold to speed up garbage collection
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))

;; BASIC SETTINGS
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)
(global-hl-line-mode t)
(blink-cursor-mode 0)
(setq make-backup-files nil) ;; no backup files
(setq-default truncate-lines t)
;; fix transparancy bug:
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))
(global-display-line-numbers-mode 1) ;; line number

;;; PACKAGE LIST
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;;; BOOTSTRAP USE-PACKAGE
(package-initialize)

(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-verbose t)
(setq package-native-compile t)
(setq comp-deferred-compilation t)



;; PACKAGES

;; UNDO
;; vim style undo
(use-package undo-fu
  :ensure t)

;; COMPLETION UI
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-echo-documentation 0.25)
  (corfu-quit-at-boundary t)
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :init
  (corfu-global-mode))

(use-package corfu-doc)
(add-hook 'corfu-mode-hook #'corfu-doc-mode)
(define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down) ;; corfu-next
(define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)  ;; corfu-previous
(define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)

;; Optionally use the `orderless' completion style. See `+orderless-dispatch'
;; in the Consult wiki for an advanced Orderless style dispatcher.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))


;; Use dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

;; COMMAND COMPLETION
;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-scroll-margin 0)
  (setq vertico-count 20)
  (setq vertico-resize nil)
  ;; enable C-n and C-p
  (setq vertico-cycle t))



;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

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

;; VIM BINDINGS
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit)
         ("C-n" . evil-next-line)
         ("C-p" . evil-previous-line))
  :init
  ;; allows for using cgn
  (setq evil-search-module 'evil-search)
  (setq evil-want-keybinding nil)
  ;; no vim insert bindings
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-Y-yank-to-eol t)
  ;; (setq evil-split-window-below t)
  ;; (setq evil-split-window-right t)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-set-leader 'normal " "))

;; Moving threw panes:
(define-key evil-normal-state-map  (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map  (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map  (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map  (kbd "C-l") 'evil-window-right)
(define-key evil-insert-state-map  (kbd "C-i") 'evil-normal-state)

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

;; Enable evil mode
(evil-mode 1)

;; auto-pairing
(electric-pair-mode 1)
(setq electric-pair-pairs
    '(
        (?\" . ?\")
        (?\( . ?\))
        (?\[ . ?\])
        (?\{ . ?\})))

;;; FOLDING
(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
              ("C-c  C" . hs-toggle-hiding)))

;; THEME
(use-package spacegray-theme
  :ensure t
  )
(load-theme 'spacegray t)
;; black baground
;; (add-to-list 'default-frame-alist '(background-color . "#000000"))
(set-background-color "#000000")
(set-face-background 'fringe "#000000")

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-h ." . display-local-help)
              ("C-h d" . eldoc-doc-buffer)
              ("M-RET" . eglot-code-actions)
	      ("C-c r" . 'eglot-rename))
  :ensure t
  :hook (;; Whatever works
         (c-mode          . eglot-ensure)
         (bash-mode       . eglot-ensure)
         ;; require: eglot-java
         ;; (java-mode       . eglot-ensure)
         ;; pip install --user 'python-language-server[all]' -U
         (python-mode . eglot-ensure)))

;; (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
;; (define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)
;; (define-key eglot-mode-map (kbd "C-c h") 'eldoc)
;; (define-key eglot-mode-map (kbd "<f6>") 'xref-find-definitions)


;; FONTS SETTINGS
(set-face-attribute 'default nil
                    :font "hack"
                    :height 100
                    :weight 'medium)
(set-face-attribute 'variable-pitch nil
                    :font "hack"
                    :height 110
                    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
                    :font "hack"
                    :height 100
                    :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'italic)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet-snippets which-key vertico use-package undo-fu spacegray-theme org-bullets orderless key-chord hungry-delete gruvbox-theme general evil-surround evil-smartparens evil-commentary evil-collection eglot cyberpunk-theme corfu-doc auto-complete)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
