;;===========================================================================;;
;; SETTINGS:
;;===========================================================================;;

;;-----------------------------------------------------------------------------
;; INTERFACE
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)
(global-hl-line-mode t)
(blink-cursor-mode 0)
(setq make-backup-files nil) ;; no backup files
(setq-default truncate-lines t)
;; fix transparancy bug:
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))
(global-display-line-numbers-mode 1) ;; line number
;; (global-display-fill-column-indicator-mode 1)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; auto-pairing
(electric-pair-mode 1)
(setq electric-pair-pairs
    '(
        (?\" . ?\")
        (?\( . ?\))
        (?\[ . ?\])
        (?\{ . ?\})))
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; FONTS SETTINGS
(set-face-attribute 'default nil
                    :font "hack"
                    :height 100
                    :weight 'medium)
(set-face-attribute 'variable-pitch nil
                    :font "hack"
                    :height 110
                    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
                    :font "hack"
                    :height 100
                    :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'italic)
;;-----------------------------------------------------------------------------
