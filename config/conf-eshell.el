;;===========================================================================;;
;; ESHELL:
;;===========================================================================;;

;;-----------------------------------------------------------------------------
;; remve hl-line and numbers in eshell
;;-----------------------------------------------------------------------------
(add-hook 'eshell-mode-hook
	  (lambda ()
	     (setq-local global-hl-line-mode nil)
	     (display-line-numbers-mode 0)))

;;-----------------------------------------------------------------------------
;; set PATH
;;-----------------------------------------------------------------------------
(setenv "PATH"
        (concat
         "~/.config/zsh/scripts"
         (getenv "PATH")))

;;-----------------------------------------------------------------------------
;; SET PAGER
;;-----------------------------------------------------------------------------
(setenv "PAGER" "bat") ;; needs bat to be installed

;;-----------------------------------------------------------------------------
;; FUNCTION
;;-----------------------------------------------------------------------------
(defun df/pull-from-dir (dirname)
  "Uses `completing-read' to ask the user to select a file from the
directory `dirname' and poll the selected file in the current
directory."
  (interactive)
  (let ((my-fdn (lambda (x)
		  (if (= (aref dirname (- (length dirname) 1)) 47)
		      dirname
		    (concat dirname "/")))))
    (eshell/mv (concat (funcall my-fdn dirname)
		       (completing-read (concat dirname ": ") (directory-files dirname) nil t ""))
	       (eshell/pwd))))

;;-----------------------------------------------------------------------------
;; ALIASES
;;-----------------------------------------------------------------------------
(add-hook 'eshell-mode-hook (lambda ()
    ;; aps
    (eshell/alias "e" "find-file $1")
    (eshell/alias "emacs" "find-file $1")
    (eshell/alias "ee" "find-file-other-window $1")

    ;; find directories
    (eshell/alias "gc" "cd ~/Desktop/cours/s2/")
    (eshell/alias "gd" "cd ~/Downloads/")
    (eshell/alias "gs" "cd ~/.config/zsh/scripts")
    (eshell/alias "gC" "cd ~/.config/")
    (eshell/alias "d" "dired $1")

    ;; list
    (eshell/alias "ll" "ls -al")

    ;; move
    (eshell/alias "md" "df/pull-from-dir \"~/Downloads/\"")))

;;-----------------------------------------------------------------------------
;; PROMPT
;;-----------------------------------------------------------------------------
;; get the name of the current directory
(defun df/dir-base-name ()
  "give current directory base name (or ~ in /home/user/)"
  (let ((pwd (eshell/pwd))
	(myhome "/home/drfailer"))
    (if (equal pwd myhome)
	"~"
      (car (last (split-string pwd "/"))))))

;; prompt function
(setq-default eshell-prompt-function
      (lambda ()
	(concat
	 (propertize (df/dir-base-name) 'face 'font-lock-builtin-face)
	 (propertize " $ " 'face 'font-lock-keyword-face))))
(setq eshell-highlight-prompt nil)
