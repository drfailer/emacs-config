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

(require 'key-chord)
(key-chord-mode 1)
;; remap jk as escape
(key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map  "kl" 'forward-char)

(require 'general)

(nvmap :keymaps 'override :prefix "SPC"
       "v t"   '(eshell :which-key "term")
       "v r c"   '((find-file "~/.emacs.d/") :which-key "term")
       "x"     '(dired-jump :which-key "Ex"))

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

;; Simple convenient mappings:
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)
