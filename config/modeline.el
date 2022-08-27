;;===========================================================================;;
;; CUSTOM MODELINE:
;;===========================================================================;;

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
                                      ,(+ 5
					  (string-width eol-mnemonic-unix)
					  (string-width display-time-string)
					  (string-width battery-mode-line-string)
					  (string-width (propertize " %m " 'face 'font-lock-string-face))))))))

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
