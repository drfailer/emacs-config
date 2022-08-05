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

(use-package hindent
  :ensure t)

;; adding .hs extension for haskell mode
(push '("\\.hs\\'" . haskell-mode) auto-mode-alist)

;; markdown mode
(use-package markdown-mode
  :ensure t)

;; rust
(use-package rust-mode
  :ensure t)

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-c <tab>"   . popper-toggle-latest)
         ("C-<tab>"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
	  "\\*Python\\*"
	  "\\*haskell\\*"
	  "\\*Flymake diagnostics for .*\\*"
	  "\\*Lisp\\*"
          "\\*Async Shell Command\\*"
	  ;; "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
          "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
          "^\\*term.*\\*$"   term-mode   ;term as a popup
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;; magit
(use-package magit
  :ensure t)
;; C-x g magit-status
;; TAB on unstage file git diff
;; s stage
;; ? actions

;; pdf tool
;; (use-package pdf-tools
;;    :defer t
;;    :config
;;        ;; (pdf-tools-install)
;;        (pdf-loader-install)
;;        (setq-default pdf-view-display-size 'fit-page)
;;    :bind (:map pdf-view-mode-map
;;          ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
;;          ("g"  . pdf-view-first-page)
;;          ("G"  . pdf-view-last-page)
;;          ("l"  . image-forward-hscroll)
;;          ("h"  . image-backward-hscroll)
;;          ("j"  . pdf-view-next-page)
;;          ("k"  . pdf-view-previous-page)
;;          ("e"  . pdf-view-goto-page)
;;          ("u"  . pdf-view-revert-buffer)
;;          ("al" . pdf-annot-list-annotations)
;;          ("ad" . pdf-annot-delete)
;;          ("aa" . pdf-annot-attachment-dired)
;;          ("am" . pdf-annot-add-markup-annotation)
;;          ("at" . pdf-annot-add-text-annotation)
;;          ("y"  . pdf-view-kill-ring-save)
;;          ("i"  . pdf-misc-display-metadata)
;;          ("s"  . pdf-occur)
;;          ("b"  . pdf-view-set-slice-from-bounding-box)
;;          ("r"  . pdf-view-reset-slice)))

;;-----------------------------------------------------------------------------
;; FUNTIONS:
;;-----------------------------------------------------------------------------

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
