;;===========================================================================;;
;; MAPPINGS:
;;===========================================================================;;


;;-----------------------------------------------------------------------------
;; FUNCTIONS:
;; find a key in a list of ((key val)..) and return val
(defun df/find-list (key lst)
  (cond
   ((null lst) nil)
   ((string= key (caar lst)) (cadar lst))
   (t (df/find-list key (cdr lst)))))

;; use completing-read function to make the user chose a bookmarded
;; file and return the file path
(defun df/select-bookmark (prompt files)
  (interactive)
  (let ((filename (completing-read prompt (mapcar (lambda (x) (car x))
						  files) nil t "")))
    (df/find-list filename files)))

;; open the selected file (filepath that has been returned by
;; `df/select-bookmark')
(defun df/open-bookmark (my-bookmarks)
  (interactive)
  (find-file (df/select-bookmark "Bookmarks: " my-bookmarks)))

;; open a bookmark
(defun df/bookmarks ()
  (interactive)
  (df/open-bookmark '(("init" "~/.emacs.d/init.el")
	   	      ("settings" "~/.emacs.d/config/settings.el")
		      ("mappings" "~/.emacs.d/config/mappings.el")
	   	      ("color" "~/.emacs.d/config/color.el")
	   	      ("evil" "~/.emacs.d/config/evil.el")
	   	      ("completion" "~/.emacs.d/config/completion.el")
	   	      ("conf-eshell" "~/.emacs.d/config/conf-eshell.el")
	   	      ("orgmode" "~/.emacs.d/config/orgmode.el")
	   	      ("lsp" "~/.emacs.d/config/lsp.el"))))

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
       "v r c" '(df/bookmarks :which-key "emacsrc")
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
	  (defun df/shellcmd ()
	    (interactive)
	    (shell-command-on-region (region-beginning) (region-end) "bash"))

	  (nvmap :state 'visual :keymaps 'override :prefix "SPC"
	    "r r"   '(df/shellcmd :which-key "runshell")))

;; Simple convenient mappings:
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "C-x c") 'delete-window)

;; NOTE: may be fun and useful
;; (completing-read
;;  "Complete a foo: "
;;  '(("foobar1" 1) ("barfoo" 2) ("foobaz" 3) ("foobar2" 4))
;;  nil t "")

