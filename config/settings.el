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
(global-hl-line-mode 1)
(blink-cursor-mode 0)
(show-paren-mode t)
(setq make-backup-files nil) ;; no backup files
(setq-default truncate-lines t)
;; fix transparancy bug:
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))
(global-display-line-numbers-mode 1) ;; line number
(menu-bar--display-line-numbers-mode-relative)

;; (global-display-fill-column-indicator-mode 1)

;; fix scrolling
(setq scroll-step            1
    scroll-conservatively  10000)

;; use sbcl when run lisp
(setq inferior-lisp-program "sbcl")
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; UTF8
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
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
;; remove line number repl
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
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; FONTS SETTINGS
(defun df/set-fonts ()
  (set-face-attribute 'default nil :font "hack" :height 88 :weight 'medium)
  (set-face-attribute 'variable-pitch nil :font "hack" :height 90 :weight 'medium)
  (set-face-attribute 'fixed-pitch nil :font "hack" :height 88 :weight 'medium))
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (df/set-fonts))))
    (df/set-fonts))
;;-----------------------------------------------------------------------------
