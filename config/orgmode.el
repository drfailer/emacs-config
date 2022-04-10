;;===========================================================================;;
;; ORG CONFIG:
;;===========================================================================;;

;; add orgmode elpa to archives:
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;;-----------------------------------------------------------------------------
;; Org Agenda:
(setq org-directory "~/.emacs.d/Org/"
      org-agenda-files '("~/.emacs.d/Org/agenda.org"))

;; TODO keywords:
(setq org-todo-keywords '((sequence "TODO" "PROJ" "WAIT" "|" "DONE" "CANCELLED" )))
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; org settings

;; global orgmode settings
(use-package org
  :config
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t)
  (setq org-src-fontify-natively t)
  (setq org-fontify-whole-heading-line t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 2)
  (setq org-hide-block-startup nil)
  (setq org-src-preserve-indentation nil)
  (setq org-startup-folded 'content)
  (setq org-cycle-separator-lines 2))

;; title height
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.05))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.1)))))

;; babel load languages
(org-babel-do-load-languages
'org-babel-load-languages
'((haskell . t) (emacs-lisp . t) (shell . t)
    (C . t) (lua . t) (java . t)
    (lisp . t)))
(setq org-confirm-babel-evaluate nil)

;; nice bullets for headers;
(use-package org-bullets
  :ensure t
  :config
  (setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;-----------------------------------------------------------------------------
;; better display for writing
(use-package visual-fill-column
  :config
  (setq-default visual-fill-column-center-text t))

(setq df/writing-mode-active nil)

;; activate writing mode
(defun df/writing-mode-on ()
  (setq-local df/writing-mode-active t)
  (setq-local global-hl-line-mode nil)
  (display-line-numbers-mode 0)
  (visual-fill-column-mode 1)
  (text-scale-set 1))

;; deactivate writing mode
(defun df/writing-mode-off ()
  (setq-local df/writing-mode-active nil)
  (setq-local global-hl-line-mode t)
  (display-line-numbers-mode 1)
  (visual-fill-column-mode 0)
  (text-scale-mode 0))

;; toggle writing mode
(defun df/writing-mode ()
  (interactive)
  (if (not df/writing-mode-active)
      (df/writing-mode-on)
    (df/writing-mode-off)))

;;-----------------------------------------------------------------------------
;; org tree slide:

;; set some settings for the presetation
(defun df/prez-start ()
  (org-display-inline-images)
  (df/writing-mode-on)
  (text-scale-set 3))

(use-package org-tree-slide
  :hook ((org-tree-slide-play . df/prez-start)
	 (org-tree-slide-stop . df/writing-mode-off))
  :custom
  (org-tree-slide-slide-in-effect-toggle t))
