;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            ORG MODE:                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;; Some mappings:
; (nvmap :keymaps 'override :prefix "SPC"
;        "m *"   '(org-ctrl-c-star :which-key "Org-ctrl-c-star")
;        "m +"   '(org-ctrl-c-minus :which-key "Org-ctrl-c-minus")
;        "m ."   '(counsel-org-goto :which-key "Counsel org goto")
;        "m e"   '(org-export-dispatch :which-key "Org export dispatch")
;        "m f"   '(org-footnote-new :which-key "Org footnote new")
;        "m h"   '(org-toggle-heading :which-key "Org toggle heading")
;        "m i"   '(org-toggle-item :which-key "Org toggle item")
;        "m n"   '(org-store-link :which-key "Org store link")
;        "m o"   '(org-set-property :which-key "Org set property")
;        "m t"   '(org-todo :which-key "Org todo")
;        "m x"   '(org-toggle-checkbox :which-key "Org toggle checkbox")
;        "m B"   '(org-babel-tangle :which-key "Org babel tangle")
;        "m I"   '(org-toggle-inline-images :which-key "Org toggle inline imager")
;        "m T"   '(org-todo-list :which-key "Org todo list")
;        "o a"   '(org-agenda :which-key "Org agenda")
;        )
