;;===========================================================================;;
;; COLOR:
;;===========================================================================;;

;;-----------------------------------------------------------------------------
;; THEME
(setq df/current-theme 'black)
(use-package modus-themes
  :ensure t
  )
(if (equal df/current-theme 'black)
    (load-theme 'modus-vivendi t)
  (load-theme 'modus-operandi t))
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; SOME FIX
;; black baground
;; (set-background-color "#000000")
;; (set-face-background 'fringe "#000000")
;;-----------------------------------------------------------------------------

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

(when (equal df/current-theme 'black)
  (set-face-attribute 'mode-line nil
		      :background "#121212"
		      :foreground "grey"
		      :box nil)
  (set-face-attribute 'mode-line-inactive nil
		      :background "#212121"
		      :foreground "grey"
		      :box nil))
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; MODELINE FUNCTIONS:
(defun get-vim-mode ()
  (cond
   (( eq evil-state 'visual) '(" VISUAL " 'font-lock-string-face))
   (( eq evil-state 'normal) '(" NORMAL " 'font-lock-comment-face))
   (( eq evil-state 'insert) '(" INSERT " 'font-lock-builtin-face))
   (t '("*" 'font-lock-comment-face))))

(defun display-read-only ()
  (cond
   (buffer-read-only '(" ﱣ " 'font-lock-warning-face))
   (t '("  " 'font-lock-constant-face))))
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; MODELINE FORMAT:
(setq df/mode-line-format
      (list
       ;;----------------------------------------------------------------------
       ;; read only buffer
       '(:eval
	 (let ((ro (display-read-only)))
	   (propertize (car ro) 'face (cadr ro))))

       ;;----------------------------------------------------------------------
       ;; vim mode
       '(:eval (let ((mode (get-vim-mode)))
		 (propertize (car mode) 'face (cadr mode))))


       ;;----------------------------------------------------------------------
       '(:eval (when-let (vc vc-mode)
                 (list " "
                       (propertize (substring vc 5)
                                   'face 'font-lock-comment-face)
                       " ")))

       ;;----------------------------------------------------------------------
       ;; file name
       '(:eval (propertize " %b " 'face 'font-lock-regexp-grouping-construct))

       ;;----------------------------------------------------------------------
       ;; buffer position (percentage)
       '(:eval (propertize " %p " 'face 'font-lock-type-face))
       
       ;;----------------------------------------------------------------------
       ;; position
       mode-line-position

       ;;----------------------------------------------------------------------
       ;; buffer modified
       '(:eval
	 (propertize
	  (cond ((buffer-modified-p) " [+]")
		(t ""))
	  'face 'font-lock-builtin-face))

       ;;----------------------------------------------------------------------
       ;; spaces to align right
       '(:eval (propertize
                " " 'display
                `((space :align-to (- (+ right right-fringe right-margin)
                                      ,(+ 5 (string-width mode-name)
					  (string-width eol-mnemonic-unix)
					  (string-width display-time-string)
					  (string-width battery-mode-line-string)))))))

       ;;----------------------------------------------------------------------
       ;; encoding and eol
       '(:eval (propertize " %Z " 'face 'font-lock-negation-char-face))

       ;;----------------------------------------------------------------------
       ;; the current major mode
       (propertize " %m " 'face 'font-lock-string-face)
       
       ;;----------------------------------------------------------------------
       ;; time and battery
       mode-line-misc-info))

;; default mode line format
(setq-default mode-line-format df/mode-line-format)
(setq-default df/mode-line-hidden nil) ;; mode line active by default

;; toggle mode line
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
