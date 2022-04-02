;;===========================================================================;;
;; ORG CONFIG:
;;===========================================================================;;

;; Org Agenda location:
(setq org-directory "~/.emacs.d/Org/"
      org-agenda-files '("~/.emacs.d/Org/agenda.org"))
(setq org-src-preserve-indentation nil
    org-src-tab-acts-natively t
    org-edit-src-content-indentation 0)

;; some basic settings
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
(setq org-cycle-separator-lines 2)

;; babel load languages
(org-babel-do-load-languages
'org-babel-load-languages
'((haskell . t) (emacs-lisp . t) (shell . t)
    (C . t) (lua . t) (java . t)
    (lisp . t)))
(setq org-confirm-babel-evaluate nil)

;; title height
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

;; TODO keywords:
(setq org-todo-keywords '((sequence "TODO" "PROJ" "WAIT" "|" "DONE" "CANCELLED" )))

;; add orgmode elpa to archives:
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; nice bullets for headers;
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
