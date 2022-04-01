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
         (bash-mode       . eglot-ensure)
         ;; require: eglot-java
         ;; (java-mode       . eglot-ensure)
         ;; pip install --user 'python-language-server[all]' -U
         (python-mode . eglot-ensure)))

;; (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
;; (define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)
;; (define-key eglot-mode-map (kbd "C-c h") 'eldoc)
;; (define-key eglot-mode-map (kbd "<f6>") 'xref-find-definitions)
