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
         ;; ("C-n" . evil-next-line)
         ;; ("C-p" . evil-previous-line))
  :init
  ;; allows for using cgn
  ;; (setq evil-search-module 'evil-search)
  (setq evil-want-keybinding nil)
  ;; no vim insert bindings
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-split-window-below t)
  (setq evil-split-window-right t)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-set-leader 'normal " ")
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

;; easier way to map stuff
(use-package general
  :config
  (general-evil-setup t))

;; enable to map keysequence without modifier key (ex: "kj" -> ESC)
(use-package key-chord
  :ensure t)
;;-----------------------------------------------------------------------------

;; Enable evil mode
(evil-mode 1)

;;-----------------------------------------------------------------------------
;; FUNCTIONS:
;;-----------------------------------------------------------------------------
;; C-u to upcase the previous word
(defun df/caps-prev-word ()
  (interactive)
  (backward-word)
  (upcase-word 1))

;; onpen term and use zsh
(defun df/open-zsh-term ()
  (interactive)
  (term "zsh"))

;; open emacs config from anywhere
(defun df/fuzzy-find-file (path)
  (interactive)
  (cd path)
  (call-interactively 'find-file))

;;-----------------------------------------------------------------------------
;; MAPPINGS:
;;-----------------------------------------------------------------------------

;; Moving threw panes:
(define-key evil-normal-state-map  (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map  (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map  (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map  (kbd "C-l") 'evil-window-right)

;; leave insert mode
(define-key evil-insert-state-map  (kbd "C-i") 'evil-normal-state)

;; upercase previous word
(define-key evil-insert-state-map  (kbd "C-u") 'df/caps-prev-word)

(require 'key-chord)
(key-chord-mode 1)
;; remap jk as escape
(key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)
;; easy skip parens
(key-chord-define evil-insert-state-map  "kl" 'forward-char)

(require 'general)

(nvmap :keymaps 'override :prefix "SPC"
       "v e s" '(eshell :which-key "eshell")
       "v t"   '(df/open-zsh-term :which-key "term")
       ;; "v r c" '(df/configs :which-key "emacsrc")
       "x"     '(dired-jump :which-key "Ex"))

;; visal keymaps
(nvmap :state 'visual :keymaps 'override :prefix "SPC"
  "TAB"   '(indent-region :which-key "indent"))

;; buffers:
(nvmap :keymaps 'override :prefix "SPC"
  "f b"   '(switch-to-buffer :which-key "Switch to buffer")
  "b b"   '(switch-to-buffer :which-key "buffer")
  "b c"   '(clone-indirect-buffer-other-window :which-key "Clone indirect buffer other window")
  "b k"   '(kill-current-buffer :which-key "Kill current buffer")
  "n b"   '(next-buffer :which-key "Next buffer")
  "b p"   '(previous-buffer :which-key "Previous buffer")
  "b B"   '(ibuffer-list-buffers :which-key "Ibuffer list buffers")
  "b K"   '(kill-buffer :which-key "Kill buffer"))


;; find files
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
  "f B"   '(bookmark-jump :which-key "bookmark jump")
  "f f"   '(find-file :which-key "Find file")
  "f u"   '(sudo-edit-find-file :which-key "Sudo find file")
  "f C"   '(copy-file :which-key "Copy file")
  "f D"   '(delete-file :which-key "Delete file")
  "f R"   '(rename-file :which-key "Rename file")
  "f S"   '(write-file :which-key "Save file as...")
  "f U"   '(sudo-edit :which-key "Sudo edit file"))

;; bookmarks
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
  "B j"   '(bookmark-jump :which-key "bookmark jump")
  "B s"   '(bookmark-set :which-key "bookmark set"))

;; run region of shell script using <leader>rr
(add-hook 'shell-mode-hook
	  (defun df/shellcmd ()
	    (interactive)
	    (shell-command-on-region (region-beginning) (region-end) "bash"))

	  (nvmap :state 'visual :keymaps 'override :prefix "SPC"
	    "r r"   '(df/shellcmd :which-key "runshell")))

;; Simple convenient mappings:
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "C-x c") 'delete-window)
