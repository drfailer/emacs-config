;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             MAPPING:                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)

(nvmap :keymaps 'override :prefix "SPC"
       "SPC"   '(counsel-M-x :which-key "M-x")
       "v t"   '(eshell :which-key "term")
       "e"     '(dired-jump :which-key "Ex"))

;; buffers:
(nvmap :prefix "SPC"
  "b b"   '(ibuffer :which-key "Ibuffer")
  "b c"   '(clone-indirect-buffer-other-window :which-key "Clone indirect buffer other window")
  "b k"   '(kill-current-buffer :which-key "Kill current buffer")
  "n b"   '(next-buffer :which-key "Next buffer")
  "b p"   '(previous-buffer :which-key "Previous buffer")
  "b B"   '(ibuffer-list-buffers :which-key "Ibuffer list buffers")
  "b K"   '(kill-buffer :which-key "Kill buffer"))


;; find files
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "."     '(find-file :which-key "Find file")
       "f f"   '(find-file :which-key "Find file")
       "f s"   '(save-buffer :which-key "Save file")
       "f u"   '(sudo-edit-find-file :which-key "Sudo find file")
       "f C"   '(copy-file :which-key "Copy file")
       "f D"   '(delete-file :which-key "Delete file")
       "f R"   '(rename-file :which-key "Rename file")
       "f S"   '(write-file :which-key "Save file as...")
       "f U"   '(sudo-edit :which-key "Sudo edit file"))


;; Moving threw panes:
(define-key evil-normal-state-map  (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map  (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map  (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map  (kbd "C-l") 'evil-window-right)

;; Simple convenient mappings:
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)
