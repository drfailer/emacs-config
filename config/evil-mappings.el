;;===========================================================================;;
;; MAPPINGS:
;;===========================================================================;;


;;-----------------------------------------------------------------------------
;; EVIL:
;;-----------------------------------------------------------------------------
;; UNDO
;; vim style undo
(use-package undo-fu
  :ensure t)

;; VIM BINDINGS
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-keybinding nil)
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-split-window-below t)
  (setq evil-split-window-right t)
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-set-leader '(normal visual) " ") ;; <leader>
  (evil-set-initial-state 'dired-mode 'emacs)
  (setq evil-insert-state-cursor '(box "white")
	evil-normal-state-cursor '(box "white")))

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

;; use `gl' to align left or `gL' to align right
(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))

;; vim like folds
(use-package vimish-fold
  :ensure
  :after evil)

(use-package evil-vimish-fold
  :ensure
  :after vimish-fold
  :init
  (setq evil-vimish-fold-mode-lighter " ::")
  (setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
  :config
  (global-evil-vimish-fold-mode))

;; enable to map keysequence without modifier key (ex: "kj" -> ESC)
(use-package key-chord
  :ensure t)

;; Enable evil mode
(evil-mode 1)

;;-----------------------------------------------------------------------------
;; FUNCTIONS:
;;-----------------------------------------------------------------------------
;; onpen term and use zsh
(defun df/open-zsh-term ()
  (interactive)
  (term "zsh"))

(setq df/gdfcim nil)
(defun df/toggle-gdfcim ()
  (interactive)
  (setq fill-column 80)
  (if (not df/gdfcim)
      (setq df/gdfcim (global-display-fill-column-indicator-mode 1))
    (setq df/gdfcim (global-display-fill-column-indicator-mode 0))))

;; fill region with 80 columns
(defun df/fill-gegion ()
  (interactive)
  (setq fill-column 80)
  (fill-region (region-beginning) (region-end)))
;;-----------------------------------------------------------------------------
;; MAPPINGS:
;;-----------------------------------------------------------------------------

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map  "jk" 'evil-normal-state) ;; remap jk as escape
(key-chord-define evil-insert-state-map  "kl" 'forward-char) ;; easy skip parens

;; Moving threw panes:
(define-key evil-normal-state-map  (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map  (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map  (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map  (kbd "C-l") 'evil-window-right)

;; fix cyclig in org mode
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

;; launching app
(evil-define-key 'normal 'global (kbd "<leader>v e s") 'eshell)
(evil-define-key 'normal 'global (kbd "<leader>v t")   'df/open-zsh-term)
(evil-define-key 'normal 'global (kbd "<leader>x")     'dired-jump)

;; indent
(evil-define-key '(normal visual) 'global (kbd "<leader>TAB") 'indent-region)
(evil-define-key 'visual 'global (kbd "<leader>gq") 'df/fill-gegion)

;; buffer management
(evil-define-key 'normal 'global (kbd "<leader>f b") 'switch-to-buffer)
(evil-define-key 'normal 'global (kbd "<leader>b b") 'switch-to-buffer)
(evil-define-key 'normal 'global (kbd "<leader>b c") 'clone-indirect-buffer-other-window)
(evil-define-key 'normal 'global (kbd "<leader>b k") 'kill-current-buffer)
(evil-define-key 'normal 'global (kbd "<leader>n b") 'next-buffer)
(evil-define-key 'normal 'global (kbd "<leader>b p") 'previous-buffer)
(evil-define-key 'normal 'global (kbd "<leader>b B") 'ibuffer-list-buffers)
(evil-define-key 'normal 'global (kbd "<leader>b K") 'kill-buffer)

;; bookmarks
(evil-define-key 'normal 'global (kbd "<leader>f B") 'bookmark-jump)
(evil-define-key 'normal 'global (kbd "<leader>B j") 'bookmark-jump)
(evil-define-key 'normal 'global (kbd "<leader>B s") 'bookmark-set)

;; files mappings
(evil-define-key 'normal 'global (kbd "<leader>f f") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader>f C") 'copy-file)
(evil-define-key 'normal 'global (kbd "<leader>f D") 'delete-file)
(evil-define-key 'normal 'global (kbd "<leader>f R") 'rename-file)

;; toggle stuff
(evil-define-key 'normal 'global (kbd "<leader>t c") 'df/toggle-gdfcim)

;; (evil-define-key 'normal 'global (kbd "<leader>z") 'z)

;; run region of shell script using <leader>rr
(add-hook 'shell-mode-hook
	  (defun df/shellcmd ()
	    (interactive)
	    (shell-command-on-region (region-beginning) (region-end) "bash"))
	  (evil-define-key 'normal 'local (kbd "<leader>r r") 'df/shellcmd))

;; Simple convenient mappings:
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)
;; simpler mapping for azerty keyboard 
(global-set-key (kbd "C-x c") 'delete-window)
(global-set-key (kbd "C-x &") 'delete-other-windows)
(global-set-key (kbd "C-x é") 'split-window-below)
(global-set-key (kbd "C-x \"") 'split-window-right)

