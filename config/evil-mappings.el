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
  (setq evil-vsplit-window-right t)
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-set-leader '(normal visual) " ") ;; <leader>
  (evil-set-initial-state 'dired-mode 'emacs)
  (setq evil-insert-state-cursor '(box "white")
	evil-normal-state-cursor '(box "white")))

;; evil collection (evil integration everywhere)
(use-package evil-collection
  :ensure t
  :after evil
  :init
  (evil-collection-init))

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

;; multiple cursor
(use-package evil-multiedit
  :ensure t)

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
(key-chord-define evil-insert-state-map  "kj" 'evil-normal-state) ;; remap kj as escape

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
(evil-define-key 'normal 'global (kbd "<leader>e")     'dired-jump-other-window)
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
(evil-define-key 'normal 'global (kbd "<leader>f g") 'rgrep)

;; toggle stuff
(evil-define-key 'normal 'global (kbd "<leader>t c") 'df/toggle-gdfcim)
(evil-define-key 'normal 'global (kbd "<leader>t m") 'df/toggle-mode-line)
(evil-define-key 'normal 'global (kbd "<leader>t w") 'df/writing-mode)
(evil-define-key 'normal 'global (kbd "<leader>t p") 'df/var-pitch-mode)
(evil-define-key 'normal 'global (kbd "<leader>t a") 'df/toggle-transparancy)

;; multiedit (https://github.com/hlissner/evil-multiedit)
(evil-define-key 'visual 'global (kbd "R") 'evil-multiedit-match-all)
(evil-define-key '(normal visual) 'global (kbd "M-d") 'evil-multiedit-match-and-next)
(evil-define-key 'insert 'global (kbd "M-d") 'evil-multiedit-toggle-marker-here)
(evil-define-key '(normal visual) 'global (kbd "M-D") 'evil-multiedit-match-and-prev)
(evil-define-key 'visual 'global (kbd "C-M-D") 'evil-multiedit-restore)
(evil-define-key 'global evil-multiedit-mod-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)
(evil-define-key 'global evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)
(evil-define-key 'global evil-multiedit-mod-map (kbd "C-n") 'evil-multiedit-next)
(evil-define-key 'global evil-multiedit-mod-map (kbd "C-p") 'evil-multiedit-prev)
(evil-define-key 'insert evil-multiedit-insert-state-map (kbd "C-n") 'evil-multiedit-next)
(evil-define-key 'insert evil-multiedit-insert-state-map (kbd "c-p") 'evil-multiedit-prev)

(evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)


;; (evil-define-key 'normal 'global (kbd "<leader>z") 'z)

;; web search
(evil-define-key 'normal 'global (kbd "<leader>f s") 'df/web-search)

;; winner mode
(winner-mode)
(define-key evil-window-map (kbd ",") 'winner-undo)
(define-key evil-window-map (kbd ";") 'winner-undo)

(defun df/dired-alternate-up ()
  (interactive)
  (find-alternate-file ".."))

;; dired
(eval-after-load "dired"
	  '(progn
	    (define-key dired-mode-map (kbd "h") 'df/dired-alternate-up)
	    (define-key dired-mode-map (kbd "j") 'dired-next-line)
	    (define-key dired-mode-map (kbd "k") 'dired-previous-line)
	    (define-key dired-mode-map (kbd "l") 'dired-find-alternate-file)
	    (define-key dired-mode-map (kbd "J") 'dired-goto-file)
	    (define-key dired-mode-map (kbd "K") 'dired-kill-line)
	    (define-key dired-mode-map (kbd "^") 'describe-mode)
	    (define-key dired-mode-map (kbd "n") 'dired-do-redisplay)))

;; run region of shell script using <leader>rr
(add-hook 'shell-mode-hook
	  (defun df/shellcmd ()
	    (interactive)
	    (shell-command-on-region (region-beginning) (region-end) "bash"))
	  (evil-define-key 'normal 'local (kbd "<leader>r r") 'df/shellcmd))

(add-hook 'org-mode
          (evil-define-key 'normal 'global (kbd "<leader>t P") 'org-tree-slide-mode))

;; Simple convenient mappings:
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)
;; simpler mapping for azerty keyboard 
(global-set-key (kbd "C-x c") 'delete-window)
(global-set-key (kbd "C-x &") 'delete-other-windows)
(global-set-key (kbd "C-x é") 'split-window-below)
(global-set-key (kbd "C-x \"") 'split-window-right)

