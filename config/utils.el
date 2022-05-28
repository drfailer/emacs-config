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

;; markdown mode
(use-package markdown-mode
  :ensure t)

;;-----------------------------------------------------------------------------
;; FUNTIONS:
;;-----------------------------------------------------------------------------

;; find item
(defun df/find-in-list (key lst)
  (cond
   ((null lst) nil)
   ((string= key (caar lst)) (cadar lst))
   (t (df/find-list key (cdr lst)))))

;; select item in a dictionary
(defun df/select-item (prompt items)
  (interactive)
  (let ((names (completing-read prompt (mapcar (lambda (x) (car x))
						  items) nil t "")))
    (df/find-in-list names items)))

;; open web bookmark
(defun df/open-bmk (bmks)
  (interactive)
  (async-shell-command
   (concat "brave " (df/select-item "Bookmarks: " bmks))))

;; load bookmarks file
(load-file "~/.emacs.d/webmks.el")

(defun df/web-search ()
  (interactive)
  (df/open-bmk *df/bmks*))

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


;; open a directory using find-file (whithout selection)
(defun df/fuzzy-find-file (path)
  (interactive)
  (cd path)
  (call-interactively 'find-file))


;; NOTE: may be fun and useful
;; (completing-read
;;  "Complete a foo: "
;;  '(("foobar1" 1) ("barfoo" 2) ("foobaz" 3) ("foobar2" 4))
;;  nil t "")
