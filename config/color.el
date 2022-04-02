;;===========================================================================;;
;; COLOR:
;;===========================================================================;;

;;-----------------------------------------------------------------------------
;; THEME
(use-package spacegray-theme
  :ensure t
  )
(load-theme 'spacegray t)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; SOME FIX
;; black baground
;; (add-to-list 'default-frame-alist '(background-color . "#000000"))
(set-background-color "#000000")
(set-face-background 'fringe "#000000")
;;-----------------------------------------------------------------------------


;;-----------------------------------------------------------------------------
;; MODELINE SETTINGS:
(setq mode-line-position (list " [%l:%c]"))
(setq mode-line-format nil)

(set-face-attribute 'mode-line nil
		    :background "#121212"
		    :foreground "grey"
		    :box nil)
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
(setq-default mode-line-format
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
       mode-line-misc-info ; for eyebrowse

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
                                      ,(+ 6 (string-width mode-name) (string-width eol-mnemonic-unix)))))))

       ;;----------------------------------------------------------------------
       ;; encoding and eol
       '(:eval (propertize " %Z " 'face 'font-lock-negation-char-face))

       ;;----------------------------------------------------------------------
       ;; the current major mode
       (propertize " %m " 'face 'font-lock-string-face)))
