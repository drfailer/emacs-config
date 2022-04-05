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

;; (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
;; (define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)
;; (define-key eglot-mode-map (kbd "C-c h") 'eldoc)
;; (define-key eglot-mode-map (kbd "<f6>") 'xref-find-definitions)

(require 'general)

(nvmap :keymaps 'override :prefix "SPC"
  "l r"   '(eglot-rename :which-key "lsp rename")
  "l o i" '(eglot-code-action-organize-imports :which-key "lsp organize imports")
  "l d"   '(eldoc :which-key "lsp doc")
  "l v d" '(xref-find-definitions :which-key "lsp rename")
  "l f"   '(eglot-format :which-key "lsp format"))

;;-----------------------------------------------------------------------------
;; fork of slime (lisp programming)
;; (use-package sly
;;   :commands (sly sly-connect)
;;   :init
;;   (setq sly-default-lisp 'sbcl)
;;   :config
;;   (define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))
