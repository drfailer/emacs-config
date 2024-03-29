;;===========================================================================;;
;; COMPLETION:
;;===========================================================================;;

;;-----------------------------------------------------------------------------
;; AUTOCOMPLETE
;; completion ui
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-echo-documentation 0.25)
  (corfu-quit-at-boundary t)
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :bind
  (:map corfu-map ("RET" . nil) ("C-j" . corfu-insert))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

;; Optionally use the 'orderless' completion style. See '+orderless-dispatch'
;; in the Consult wiki for an advanced Orderless style dispatcher.
;; Enable 'partial-completion' for files to allow path expansion.
;; You may prefer to use 'initials' instead of 'partial-completion'.
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
  :bind (("M-n" . dabbrev-completion)
         ("C-M-j" . dabbrev-expand)))

;; (use-package corfu-doc)
;; (add-hook 'corfu-mode-hook #'corfu-doc-mode)
(define-key corfu-map (kbd "M-k") #'corfu-popupinfo-scroll-down) ;; corfu-next
(define-key corfu-map (kbd "M-j") #'corfu-popupinfo-scroll-up)  ;; corfu-previous
(define-key corfu-map (kbd "M-d") #'corfu-popupinfo-toggle)

(use-package cape
  :bind (("C-c C-f" . cape-file)
	 ;; ("C-c p w" . cape-ispell)
	 ("C-c p t" . cape-tex)) ;; bind for files
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  :config
  ;;---------------------------------------------------------
  ;; solve pcomplete bug (eshell)
  ;; Silence then pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  ;;---------------------------------------------------------
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                              corfu-quit-no-match t
                              corfu-auto t)
              (corfu-mode))))

;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
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

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  ;; Add prompt indicator to 'completing-read-multiple'.
  ;; Alternatively try 'consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  ;; Enable indentation+completion using the TAB key.
  ;; 'completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;;-----------------------------------------------------------------------------

;; Example configuration for Consult
(use-package consult
  ;; enable preview
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; register formatting
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Narrowing key.
  (setq consult-narrow-key "<")
)
