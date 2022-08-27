;;===========================================================================;;
;; COLOR:
;;===========================================================================;;


;;-----------------------------------------------------------------------------
;; THEME
(setq df/default-theme 'black)
(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-italic-constructs nil
	modus-themes-bold-constructs t
	modus-themes-mixed-fonts t
	modus-themes-subtle-line-numbers t
	modus-themes-intense-mouseovers nil
	modus-themes-deuteranopia t
	modus-themes-variable-pitch-ui nil
	modus-themes-inhibit-reload t
	modus-themes-fringes nil ;; fringe in black
	modus-themes-mode-line '(accented borderless) ;; modeline
	modus-themes-syntax '(alt-syntax green-strings)
	modus-themes-hl-line '(accented)
	modus-themes-paren-match '(bold intense)
	modus-themes-links '(neutral-underline background)
	modus-themes-prompts '(intense bold) ;; repl and minibuffer prompts
	;; completion settings
	modus-themes-completions '((matches . (extrabold))
                                   (selection . (semibold accented))
                                   (popup . (accented intense)))
	modus-themes-region '(bg-only)
	modus-themes-diffs 'desaturated

	modus-themes-org-blocks nil ; {nil,'gray-background,'tinted-background}

	;; TODO: test this
	modus-themes-org-agenda ; this is an alist: read the manual or its doc string
	'((header-block . (1.3))
          (header-date . (grayscale workaholic bold-today 1.1))
          (event . (accented varied))
          (scheduled . uniform)
          (habit . traffic-light))
	modus-themes-headings ;; org headings
	'((t . (semibold)))))

(use-package doom-themes
  :ensure t)
(use-package gruvbox-theme
  :ensure t)
(use-package zenburn-theme
  :ensure t)

;;-----------------------------------------------------------------------------
;; black baground
(defun df/black-bg ()
  (interactive)
  (progn
    (set-background-color "#000000")
    (set-face-background 'fringe "#000000")
    (set-face-background 'line-number "#000000")))

;;-----------------------------------------------------------------------------
;; Transparancy:
(defun df/set-transparancy (an af)
  "set transparancy `an' for the current focused emacs frame and `af' for non
focused ones."
  (progn
    (set-frame-parameter (selected-frame) 'alpha (list an af))
    (add-to-list 'default-frame-alist (list 'alpha an af))))

;; set default transparancy
;; (df/set-transparancy 90 90)
(df/set-transparancy 100 100)

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
   ;;===========================================================================
   ;; MODUS-OPERANDI:
   ((equal df/default-theme 'white)
    (progn
      (load-theme 'modus-operandi t)
      (custom-set-faces
       '(powerline-active1 ((t (:foreground "#000000" :background "#FFFFFF")))))
      ;; (set-face-attribute 'mode-line nil :background "#F1F1F1" :foreground "black" :box nil)
      ;; (set-face-attribute 'mode-line-inactive nil :background "#FFFFFF" :foreground "grey" :box nil)
      ))
   ;;===========================================================================
   ;; MODUS-VIVENDI:
   ((equal df/default-theme 'black)
    (progn
      (load-theme 'modus-vivendi t)
      (custom-set-faces
       '(powerline-active1 ((t (:foreground "#FFFFFF" :background "#000000")))))
      ;; (set-face-attribute 'mode-line nil :background "#090909" :foreground "grey" :box nil)
      ;; (set-face-attribute 'mode-line-inactive nil :background "#121212" :foreground "grey" :box nil)
      ))
   ;;===========================================================================
   ;; ONE:
   ((equal df/default-theme 'one) (load-theme 'doom-one t))
   ;;===========================================================================
   ;; ZENBURN:
   ((equal df/default-theme 'zenburn) (load-theme 'zenburn t))
   ;;===========================================================================
   ;; GRUVBOX:
   ((equal df/default-theme 'gruvbox)
    (progn
      (load-theme 'gruvbox-dark-hard t)
      (set-face-attribute 'mode-line nil :background "#282828" :foreground "grey" :box nil)
      (set-face-attribute 'mode-line-inactive nil :background "#252525" :foreground "grey" :box nil)
      (set-face-attribute 'line-number nil :background "#1d2021")))
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
  (let ((theme (completing-read "Themes: " '("one" "dark" "white" "gruvbox" "zenburn"))))
    (progn
      (cond
       ((string= theme "one") (setq df/default-theme 'one))
       ((string= theme "white") (setq df/default-theme 'white))
       ((string= theme "dark") (setq df/default-theme 'black))
       ((string= theme "gruvbox") (setq df/default-theme 'gruvbox))
       ((string= theme "zenburn") (setq df/default-theme 'zenburn))
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
