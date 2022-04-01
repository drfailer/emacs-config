;;===========================================================================;;
;; ORG CONFIG:
;;===========================================================================;;

;; Org Agenda location:
(setq org-directory "~/.emacs.d/Org/"
      org-agenda-files '("~/.emacs.d/Org/agenda.org"))
(setq org-src-preserve-indentation nil
    org-src-tab-acts-natively t
    org-edit-src-content-indentation 0)

;; TODO keywords:
(setq org-todo-keywords '((sequence "TODO" "PROJ" "WAIT" "|" "DONE" "CANCELLED" )))

;; add orgmode elpa to archives:
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; nice bullets for headers;
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
