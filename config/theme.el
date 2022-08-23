;;===========================================================================;;
;; COLOR:
;;===========================================================================;;

;;-----------------------------------------------------------------------------
;; THEME
(setq df/default-theme 'gruvbox-black)
(use-package modus-themes
  :ensure t)
(use-package doom-themes
  :ensure t)
(use-package gruvbox-theme
  :ensure t)
(use-package zenburn-theme
  :ensure t)

;; modus themes org headings
(setq modus-themes-headings
      '((1 . (rainbow nil))
	(2 . (rainbow nil))
	(3 . (rainbow nil))
	(4 . (rainbow nil))
	(t . (semilight nil))))

;; modus themes settings
;; (setq modus-themes-syntax '(faint))

;;-----------------------------------------------------------------------------
;; black baground
(defun df/black-bg ()
  (interactive)
  (progn
    (set-background-color "#000000")
    (set-face-background 'fringe "#000000")
    (set-face-background 'line-number "#000000")))

(defun df/org_src_color (color)
  (if (equal color 'black)
      (custom-set-faces ;; change Org blocks backgroud color
       '(org-block-begin-line ((t (:background "#111111" :foreground "#000000" :extend t))))
       '(org-block ((t (:background "#090909" :extend t))))
       '(org-block-end-line ((t (:background "#111111" :foreground "#000000" :extend t)))))
    (custom-set-faces ;; change Org blocks backgroud color
     '(org-block-begin-line ((t (:background "#EEEEEE" :foreground "#FFFFFF" :extend t))))
     '(org-block ((t (:background "#F0F0F0" :extend t))))
     '(org-block-end-line ((t (:background "#EEEEEE" :foreground "#FFFFFF" :extend t)))))))

;;-----------------------------------------------------------------------------
;; Transparancy:
(defun df/set-transparancy (an af)
  "set transparancy `an' for the current focused emacs frame and `af' for non
focused ones."
  (progn
    (set-frame-parameter (selected-frame) 'alpha (list an af))
    (add-to-list 'default-frame-alist (list 'alpha an af))))

;; set default transparancy
(df/set-transparancy 90 90)

;; toggle transparancy.
(setq df/transparancy-on 1)
(defun df/toggle-transparancy ()
  "Allow to toggle transparancy."
  (interactive)
  (if df/transparancy-on
      (progn
	(df/set-transparancy 100 100)
	(setq df/transparancy-on nil))
    (progn
	(df/set-transparancy 90 90)
	(setq df/transparancy-on 1))))

;;-----------------------------------------------------------------------------
;; theme settings
(defun df/theme-settings ()
  (cond
   ((equal df/default-theme 'white) ;;; MODUS-OPERANDI
    (progn
      (load-theme 'modus-operandi t)
      (df/org_src_color 'white)
      (set-face-attribute 'mode-line nil :background "#F1F1F1" :foreground "black" :box nil)
      (set-face-attribute 'mode-line-inactive nil :background "#FFFFFF" :foreground "grey" :box nil)))
   ((equal df/default-theme 'black) ;;; MODUS-VIVENDI
    (progn
      (load-theme 'modus-vivendi t)
      (df/org_src_color 'black)
      (set-face-attribute 'mode-line nil :background "#090909" :foreground "grey" :box nil)
      (set-face-attribute 'mode-line-inactive nil :background "#121212" :foreground "grey" :box nil)
      (set-face-attribute 'line-number nil :background "#010101")))
   ((equal df/default-theme 'one) (load-theme 'doom-one t)) ;;; ONE
   ((equal df/default-theme 'zenburn) (load-theme 'zenburn t)) ;;; ZENBURN
   ((equal df/default-theme 'gruvbox) ;;; GRUVBOX
    (progn
      (load-theme 'gruvbox-dark-hard t)
      (set-face-attribute 'mode-line nil :background "#282828" :foreground "grey" :box nil)
      (set-face-attribute 'mode-line-inactive nil :background "#252525" :foreground "grey" :box nil)
      (set-face-attribute 'line-number nil :background "#1d2021")))
   ((equal df/default-theme 'gruvbox-black) ;;; GRUVBOX-BLACK
    (progn
      (load-theme 'gruvbox-dark-hard t)
      (df/black-bg) ;; set backgroud to black
      (df/org_src_color 'black)
      (set-face-attribute 'mode-line nil :background "#090909" :foreground "grey" :box nil)
      (set-face-attribute 'mode-line-inactive nil :background "#121212" :foreground "grey" :box nil)
      (set-face-attribute 'line-number nil :background "#010101")))
   ((equal df/default-theme 'darkburn) ;;; DARKBURN
    (progn
      (load-theme 'zenburn t)
      (df/black-bg) ;; set backgroud to black
      (df/org_src_color 'black)))
   ))
(df/theme-settings)

;;-----------------------------------------------------------------------------
;; MODELINE SETTINGS:
(setq mode-line-position (list " [%l:%c]"))
(setq mode-line-format nil)
(setq display-time-string-forms
      '((propertize (concat "  " 24-hours ":" minutes " ")
 		    'face 'font-lock-keyword-face)))
(setq battery-mode-line-format " %b%p%%")
(display-battery-mode)
(display-time-mode)

;;-----------------------------------------------------------------------------
;; CUSTOM MODELINE:
;; (load "~/.emacs.d/config/modeline.el") ;; custom modeline

;;-----------------------------------------------------------------------------
;; SPACELINE:
(setq df/spaceline-active nil)
(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator (quote arrow))
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (setq df/spaceline-active t)
  (spaceline-spacemacs-theme))

;;-----------------------------------------------------------------------------
;; TOGGLE MODE LINE (doesn't work with spaceline):
(setq-default df/mode-line-hidden nil) ;; mode line active by default

(defun df/toggle-mode-line ()
  "function used to toggle the mode line, it sets up the modeline
format to nil or to `df/mode-line-format' depending of the status of
`df/mode-line-hidden'"
  (interactive)
  (if (not df/mode-line-hidden)
      (progn
	(setq df/mode-line-hidden t)
	(setq mode-line-format nil))
    (progn
      (setq df/mode-line-hidden nil)
      (setq mode-line-format df/mode-line-format))))

;;-----------------------------------------------------------------------------
;; SWITCH THEMES:
;; switch theme with a menu and apply my settings
(defun df/switch-theme ()
  (interactive)
  (let ((theme (completing-read "Themes: " '("one" "dark" "white" "gruvbox" "gruvbox-black" "zenburn" "darkburn"))))
    (progn
      (cond
       ((string= theme "one") (setq df/default-theme 'one))
       ((string= theme "white") (setq df/default-theme 'white))
       ((string= theme "dark") (setq df/default-theme 'black))
       ((string= theme "gruvbox") (setq df/default-theme 'gruvbox))
       ((string= theme "gruvbox-black") (setq df/default-theme 'gruvbox-black))
       ((string= theme "zenburn") (setq df/default-theme 'zenburn))
       ((string= theme "darkburn") (setq df/default-theme 'darkburn))
       (t (message "Unknown theme")))
      (df/theme-settings)
      (if df/spaceline-active ;; recompile spaceline if it is available
	(spaceline-compile)))))

(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;; disable previous theme before loading a new one
(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))
