;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        EMACS CONFIGURATION:                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)
;; fix transparancy bug:
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))
(setq-default indent-tabs-mode nil)
;; (add-hook 'prog-mode-hook 'linum-mode)
(global-display-line-numbers-mode 1)
(setq-default truncate-lines t)
(setq make-backup-files nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package gruvbox-theme
  :ensure t
  )
(load-theme 'gruvbox-dark-hard t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; electric pairing:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(electric-pair-mode 1)
(setq electric-pair-pairs
    '(
        (?\" . ?\")
        (?\( . ?\))
        (?\[ . ?\])
        (?\{ . ?\})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-attribute 'default nil
                    :font "Source Code Pro"
                    :height 100
                    :weight 'medium)
(set-face-attribute 'variable-pitch nil
                    :font "Source Code Pro"
                    :height 110
                    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
                    :font "Source Code Pro"
                    :height 100
                    :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'italic)
