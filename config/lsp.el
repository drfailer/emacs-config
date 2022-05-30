;;===========================================================================;;
;; LSP
;;===========================================================================;;

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-h ." . display-local-help)
              ("C-h d" . eldoc-doc-buffer)
              ("M-RET" . eglot-code-actions)
	      ("C-c r" . 'eglot-rename))
  :ensure t
  :hook (;; Whatever works
         (c-mode          . eglot-ensure)
         (sh-mode         . eglot-ensure)
         ;; require: eglot-java
         ;; (java-mode       . eglot-ensure)
         ;; pip install --user 'python-language-server[all]' -U
         (python-mode . eglot-ensure)))

;; some mappings
(evil-define-key 'normal 'global (kbd "<leader>l r")   'eglot-rename)
(evil-define-key 'normal 'global (kbd "<leader>l o i") 'eglot-code-action-organize-imports)
(evil-define-key 'normal 'global (kbd "<leader>l a")   'eglot-code-actions)
(evil-define-key 'normal 'global (kbd "<leader>l d")   'eldoc)
(evil-define-key 'normal 'global (kbd "<leader>l v d") 'xref-find-definitions)
(evil-define-key 'normal 'global (kbd "<leader>l f")   'eglot-format)

;; lsp mode
;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          (c-mode . lsp)
;;          (sh-mode . lsp)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)

;;-----------------------------------------------------------------------------
;; fork of slime (lisp programming)
;; (use-package sly
;;   :commands (sly sly-connect)
;;   :init
;;   (setq sly-default-lisp 'sbcl)
;;   :config
;;   (define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))
