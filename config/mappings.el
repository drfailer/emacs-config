;;===========================================================================;;
;; MAPPINGS:
;;===========================================================================;;

;; Moving threw panes:
(define-key evil-normal-state-map  (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map  (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map  (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map  (kbd "C-l") 'evil-window-right)

;; leave insert mode
(define-key evil-insert-state-map  (kbd "C-i") 'evil-normal-state)

;; C-u to upcase the previous word
(defun caps-prev-word ()
  (interactive)
  (backward-word)
  (upcase-word 1))

(define-key evil-insert-state-map  (kbd "C-u") 'caps-prev-word)

(require 'key-chord)
(key-chord-mode 1)
;; remap jk as escape
(key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map  "kl" 'forward-char)

(require 'general)

;; onpen term and use zsh
(defun open-zsh-term ()
  (interactive)
  (term "zsh"))

;; edit emacs config
(defun open-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/config/"))

(nvmap :keymaps 'override :prefix "SPC"
       "v e s" '(eshell :which-key "eshell")
       "v t"   '(open-zsh-term :which-key "term")
       "v r c" '(open-emacs-config :which-key "emacsrc")
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
       "f f"   '(find-file :which-key "Find file")
       "f u"   '(sudo-edit-find-file :which-key "Sudo find file")
       "f C"   '(copy-file :which-key "Copy file")
       "f D"   '(delete-file :which-key "Delete file")
       "f R"   '(rename-file :which-key "Rename file")
       "f S"   '(write-file :which-key "Save file as...")
       "f U"   '(sudo-edit :which-key "Sudo edit file"))

;; run region of shell script using <leader>rr
(add-hook 'shell-mode-hook
	  (defun shellcmd ()
	    (interactive)
	    (shell-command-on-region (region-beginning) (region-end) "bash"))

	  (nvmap :state 'visual :keymaps 'override :prefix "SPC"
	    "r r"   '(shellcmd :which-key "runshell")))

;; Simple convenient mappings:
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)
