;===========================================================================;;
;; LSP
;;===========================================================================;;

;;-----------------------------------------------------------------------------
;; eglot
;; (use-package eglot
;;   :bind (:map eglot-mode-map
;;               ("C-h ." . display-local-help)
;;               ("C-h d" . eldoc-doc-buffer)
;;               ("M-RET" . eglot-code-actions)
;; 	      ("C-c r" . 'eglot-rename))
;;   :ensure t
;;   :hook (;; Whatever works
;;          (c-mode          . eglot-ensure)
;;          (sh-mode         . eglot-ensure)
;;          ;; require: eglot-java
;;          ;; (java-mode       . eglot-ensure)
;;          ;; pip install --user 'python-language-server[all]' -U
;;          (python-mode . eglot-ensure)))

;; some mappings
;; (evil-define-key 'normal 'global (kbd "<leader>l r")   'eglot-rename)
;; (evil-define-key 'normal 'global (kbd "<leader>l o i") 'eglot-code-action-organize-imports)
;; (evil-define-key 'normal 'global (kbd "<leader>l a")   'eglot-code-actions)
;; (evil-define-key 'normal 'global (kbd "<leader>l d")   'eldoc)
;; (evil-define-key 'normal 'global (kbd "<leader>l v d") 'xref-find-definitions)
;; (evil-define-key 'normal 'global (kbd "<leader>l f")   'eglot-format)

;;-----------------------------------------------------------------------------
;; lsp mode
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c-mode . lsp)
         (sh-mode . lsp))
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t))

;; config lsp
(setq lsp-headerline-breadcrumb-enable nil)

;; lsp ui
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-update-mode 'line))

;; lsp-mode mappings
(evil-define-key 'normal 'global (kbd "<leader>l r")   'lsp-rename)
(evil-define-key 'normal 'global (kbd "<leader>l o i") 'lsp-organize-imports)
(evil-define-key 'normal 'global (kbd "<leader>l a")   'lsp-execute-code-action)
(evil-define-key 'normal 'global (kbd "<leader>l d g")   'lsp-ui-doc-glance)
(evil-define-key 'normal 'global (kbd "<leader>l d s")   'lsp-ui-doc-show)
(evil-define-key 'normal 'global (kbd "<leader>l d f")   'lsp-ui-doc-focus-frame)
(evil-define-key 'normal 'global (kbd "<leader>l v d") 'lsp-find-definition)
(evil-define-key 'normal 'global (kbd "<leader>l v i") 'lsp-find-implementation)
(evil-define-key 'normal 'global (kbd "<leader>l v r") 'lsp-find-references)
(evil-define-key 'normal 'global (kbd "<leader>l f")   'lsp-format-buffer)
(evil-define-key 'visual 'global (kbd "<leader>l f")   'lsp-format-region)
(evil-define-key 'normal 'global (kbd "<leader>l e")   'flymake-show-buffer-diagnostics)

;; fix corfu completion with lsp
(add-hook 'lsp-completion-mode-hook
          (lambda ()
            (setf (alist-get 'lsp-capf completion-category-defaults) '((styles . (orderless))))))

;; Java
;; (use-package lsp-java
;;   :ensure t)
;; (add-hook 'java-mode-hook #'lsp)

;;-----------------------------------------------------------------------------
;; fork of slime (lisp programming)
;; (use-package sly
;;   :commands (sly sly-connect)
;;   :init
;;   (setq sly-default-lisp 'sbcl)
;;   :config
;;   (define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))
