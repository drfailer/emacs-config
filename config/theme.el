;;===========================================================================;;
;; MODELINE AND THEMES:
;;===========================================================================;;

;;-----------------------------------------------------------------------------
;; THEMES PACKAGES
(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-italic-constructs nil
	modus-themes-bold-constructs nil
	modus-themes-mixed-fonts t
	modus-themes-subtle-line-numbers t
	modus-themes-intense-mouseovers nil
	modus-themes-deuteranopia nil
	modus-themes-tabs-accented nil
	modus-themes-variable-pitch-ui nil
	modus-themes-inhibit-reload t
	modus-themes-fringes nil ;; fringe in black
	modus-themes-lang-checkers nil
	modus-themes-mode-line '(accented borderless) ;; modeline
	modus-themes-markup '(background intence)
	modus-themes-syntax '(alt-syntax green-strings)
	modus-themes-hl-line '(accented)
	modus-themes-paren-match '(bold intense)
	modus-themes-links '(neutral-underline)
	modus-themes-box-buttons nil
	modus-themes-prompts '(intense bold) ;; repl and minibuffer prompts

	;; completion settings
	modus-themes-completions '((matches . (extrabold))
                                   (selection . (semibold accented))
                                   (popup . (accented intense)))

	modus-themes-mail-citations nil
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
	'((t . (semibold))))

  ;; lighter background
  ;; (setq modus-themes-vivendi-color-overrides
  ;; 	'((bg-main . "#080808")))
  )

(use-package monokai-theme
  :ensure t)
(use-package gruvbox-theme
  :ensure t)
(use-package zenburn-theme
  :ensure t)

;;-----------------------------------------------------------------------------
;; BACKGROUND SETTINGS (color and alpha)
(defun df/black-bg ()
  "Set the main background, the fringe and the line-number's background to black
for the current frame."
  (interactive)
  (progn
    (set-background-color "#000000")
    (set-face-background 'fringe "#000000")
    (set-face-background 'line-number "#000000")))

(defun df/set-transparancy (an af)
  "set transparancy `an' for the current focused emacs frame and `af' for non
focused ones."
  (progn
    (set-frame-parameter (selected-frame) 'alpha (list an af))
    (add-to-list 'default-frame-alist (list 'alpha an af))))

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

;; set default transparancy
(if df/transparancy-on
    (df/set-transparancy 90 90)
  (df/set-transparancy 100 100))

;;-----------------------------------------------------------------------------
;; THEME SETTINGS
(defun df/theme-settings ()
  "Apply a theme and the corresponding settings depending on the value of
`df/current-theme'."
  (cond
   ;; MODUS-OPERANDI:
   ((string= df/current-theme "operandi")
    (progn
      (load-theme 'modus-operandi t)
      (set-face-attribute 'powerline-active1 nil :foreground "#000000" :background "#FFFFFF")))
   ;; MODUS-VIVENDI:
   ((string= df/current-theme "vivendi")
    (progn
      (load-theme 'modus-vivendi t)
      (set-face-attribute 'powerline-active1 nil :foreground "#FFFFFF" :background "#000000")))
   ;; MONOKAI:
   ((string= df/current-theme "monokai") (load-theme 'monokai t))
   ;; ZENBURN:
   ((string= df/current-theme "zenburn") (load-theme 'zenburn t))
   ;; GRUVBOX:
   ((string= df/current-theme "gruvbox")
    (progn
      (load-theme 'gruvbox-dark-hard t)
      (set-face-attribute 'mode-line nil :background "#282828" :foreground "grey" :box nil)
      (set-face-attribute 'mode-line-inactive nil :background "#252525" :foreground "grey" :box nil)
      (set-face-attribute 'line-number nil :background "#1d2021")))))
(df/theme-settings)


;;-----------------------------------------------------------------------------
;; SWITCH THEMES:
;; switch theme with a menu and apply my settings
(defconst df/themes-collection '("monokai" "vivendi" "operandi" "gruvbox" "zenburn"))
(defun df/switch-theme ()
  "Allow to switch between different themes from my collection. Change
`df/current-theme'."
  (interactive)
  (setq df/current-theme
	(completing-read "Themes: " df/themes-collection))
  (df/theme-settings)
  (if df/use-spaceline ;; recompile spaceline if needed
	(spaceline-compile)))

(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;; disable previous theme before loading a new one
(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))
