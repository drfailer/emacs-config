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

(require 'general)

;; some mappings
(evil-define-key 'normal 'global (kbd "<leader>l r")   'eglot-rename)
(evil-define-key 'normal 'global (kbd "<leader>l o i") 'eglot-code-action-organize-imports)
(evil-define-key 'normal 'global (kbd "<leader>l a")   'eglot-code-actions)
(evil-define-key 'normal 'global (kbd "<leader>l d")   'eldoc)
(evil-define-key 'normal 'global (kbd "<leader>l v d") 'xref-find-definitions)
(evil-define-key 'normal 'global (kbd "<leader>l f")   'eglot-format)

;;-----------------------------------------------------------------------------
;; fork of slime (lisp programming)
;; (use-package sly
;;   :commands (sly sly-connect)
;;   :init
;;   (setq sly-default-lisp 'sbcl)
;;   :config
;;   (define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))
