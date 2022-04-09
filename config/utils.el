;;===========================================================================;;
;; UTILS.EL:
;;===========================================================================;;

;;-----------------------------------------------------------------------------
;; PACKAGES:
;;-----------------------------------------------------------------------------

;; dired:
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")))

;; WICHKEY
(use-package which-key
:ensure t
:config
(which-key-mode))

;; SINIPPETS
(use-package yasnippet
  :ensure t
  :init
    (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

;;; FOLDING
;; (use-package hideshow
;;   :hook (prog-mode . hs-minor-mode)
;;   :bind (:map hs-minor-mode-map
;;               ("C-c  C" . hs-toggle-hiding)))

;; haskell mode
(use-package haskell-mode
  :ensure t)

;; adding .hs extension for haskell mode
(push '("\\.hs\\'" . haskell-mode) auto-mode-alist)

;;-----------------------------------------------------------------------------
;; FUNTIONS:
;;-----------------------------------------------------------------------------

;; use emacs builtin bookmark system instead
;; TODO: may be interessant to use this for doing smth else (brave script for instance)
;; TODO: extend this -> give a command to the function for instance
;; ;; use completing-read function to make the user chose a bookmarded
;; ;; file and return the file path
;; (defun df/select-config (prompt files)
;;   (interactive)
;;   (let ((filename (completing-read prompt (mapcar (lambda (x) (car x))
;; 						  files) nil t "")))
;;     (df/find-list filename files)))

;; ;; open the selected file (filepath that has been returned by
;; ;; `df/select-bookmark')
;; (defun df/open-config (my-configs-files)
;;   (interactive)
;;   (find-file (df/select-config "Bookmarks: " my-configs-files)))

;; ;; special bookmark system for config files
;; ;; similar to default emacs bookmark system
;; (defun df/configs ()
;;   (interactive)
;;   (df/open-config '(("init" "~/.emacs.d/init.el")
;; 	   	      ("settings" "~/.emacs.d/config/settings.el")
;; 		      ("mappings" "~/.emacs.d/config/mappings.el")
;; 	   	      ("color" "~/.emacs.d/config/color.el")
;; 	   	      ("evil" "~/.emacs.d/config/evil.el")
;; 	   	      ("completion" "~/.emacs.d/config/completion.el")
;; 	   	      ("conf-eshell" "~/.emacs.d/config/conf-eshell.el")
;; 	   	      ("orgmode" "~/.emacs.d/config/orgmode.el")
;; 	   	      ("lsp" "~/.emacs.d/config/lsp.el"))))

;; find a key in a list of ((key val)..) and return val
(defun df/repl-config (repls)
  (dolist (repl repls)
    (add-hook repl
	      (lambda ()
		(setq-local global-hl-line-mode nil)
		(display-line-numbers-mode 0)))))

;; rpls list
(df/repl-config '(term-mode-hook
		  inferior-python-mode-hook
		  inferior-lisp-mode-hook))


;; find item
(defun df/find-list (key lst)
  (cond
   ((null lst) nil)
   ((string= key (caar lst)) (cadar lst))
   (t (df/find-list key (cdr lst)))))


;; NOTE: may be fun and useful
;; (completing-read
;;  "Complete a foo: "
;;  '(("foobar1" 1) ("barfoo" 2) ("foobaz" 3) ("foobar2" 4))
;;  nil t "")
