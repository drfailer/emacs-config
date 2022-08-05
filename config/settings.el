;;===========================================================================;;
;; SETTINGS:
;;===========================================================================;;

;;-----------------------------------------------------------------------------
;; GENERAL SETTINGS
;;-----------------------------------------------------------------------------
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)
(global-hl-line-mode 1)
(blink-cursor-mode 0)
(show-paren-mode t)
(setq make-backup-files nil) ;; no backup files
(setq-default truncate-lines t)
(global-display-line-numbers-mode 1) ;; line number
(menu-bar--display-line-numbers-mode-relative)
;; turn off ring bell
(setq ring-bell-function 'ignore)
(delete-selection-mode 1)

;; fix scrolling
(setq scroll-step            1
    scroll-conservatively  10000)

;; use sbcl when run lisp
(setq inferior-lisp-program "sbcl")

;; use space rather than tabs
(indent-tabs-mode 0)

;;-----------------------------------------------------------------------------
;; UTF8
;;-----------------------------------------------------------------------------
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;;-----------------------------------------------------------------------------
;; AUTO-PAIRING
;;-----------------------------------------------------------------------------
(electric-pair-mode 1)
(setq electric-pair-pairs
    '(
        (?\" . ?\")
        (?\( . ?\))
        (?\[ . ?\])
        (?\{ . ?\})))

;;-----------------------------------------------------------------------------
;; FONTS SETTINGS
;;-----------------------------------------------------------------------------
(defun df/set-fonts ()
  (set-face-attribute 'default nil :font "Hack-9")
  (set-face-attribute 'fixed-pitch nil :font "Hack-9")
  ;;(set-face-attribute 'variable-pitch nil :font "FiraGO-9.5:weight=Light")
  (set-face-attribute 'variable-pitch nil :font "DejaVu Sans-9.5"))
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (df/set-fonts))))
    (df/set-fonts))
