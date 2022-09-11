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
(global-display-line-numbers-mode 1) ;; line number
(menu-bar--display-line-numbers-mode-relative)
(delete-selection-mode 1)
(indent-tabs-mode 0) ;; use space rather than tabs
(electric-pair-mode 1)
(setq make-backup-files nil) ;; no backup files
(setq ring-bell-function 'ignore) ;; turn off ring bell
(setq inferior-lisp-program "sbcl") ;; use sbcl when run lisp
(setq-default truncate-lines t)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq electric-pair-pairs '((?\" . ?\") (?\( . ?\)) (?\[ . ?\]) (?\{ . ?\})))
;; (setq outline-minor-mode-highlight 'override)
(setq outline-minor-mode-cycle t)

;; personal settings
(setq-default df/fp-font "JetBrains Mono-9")
(setq-default df/vp-font "FiraGO-9.5:weight=Light")
(setq-default df/use-spaceline t)
(setq-default df/mode-line-hidden nil) ;; mode line active by default
(setq-default df/current-theme "vivendi")
(setq-default df/transparancy-on nil)

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
;; FONTS SETTINGS
;;-----------------------------------------------------------------------------
(defun df/set-fonts ()
  "Apply font settings."
  (set-face-attribute 'default nil :font df/fp-font)
  (set-face-attribute 'fixed-pitch nil :font df/fp-font)
  (set-face-attribute 'variable-pitch nil :font df/vp-font))

(defun df/switch-fp-font ()
  "Interactive function that allows the user to switch between some fixed pitch
fonts. Change the global variable `df/fp-font'."
  (interactive)
  (progn
    (setq df/fp-font
	(completing-read
	 "Fixed pitch font: "
	 '("Hack-9" "JetBrains Mono-9" "Fira Code-9")))
    (df/set-fonts)))

(defun df/switch-vp-font ()
  "Interactive function that allows the user to switch between some variable
pitch fonts. Change the global variable `df/vp-font'."
  (interactive)
  (progn
    (setq df/vp-font
	(completing-read
	 "Variable pitch font: "
	 '("DejaVu Sans-9.5" "FiraGO-9.5:weight=Light")))
    (df/set-fonts)))

;;-----------------------------------------------------------------------------
;; SETTINGS FOR NEW DAEMON FRAMES
;;-----------------------------------------------------------------------------
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (df/set-fonts))))
    (df/set-fonts))
