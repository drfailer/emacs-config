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

;; TODO keywords:
(setq org-todo-keywords '((sequence "TODO" "PROJ" "WAIT" "|" "DONE" "CANCELLED" )))

;; add orgmode elpa to archives:
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; nice bullets for headers;
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
