;;===========================================================================;;
;; ESHELL:
;;===========================================================================;;

;;-----------------------------------------------------------------------------
;; remve hl-line and numbers in eshell
(add-hook 'eshell-mode-hook
	  (lambda ()
	     (setq-local global-hl-line-mode nil)
	     (display-line-numbers-mode 0)))

;;-----------------------------------------------------------------------------
;; set PATH
(setenv "PATH"
        (concat
         "~/.config/zsh/scripts"
         (getenv "PATH")))

;;-----------------------------------------------------------------------------
;; set pager
(setenv "PAGER" "bat") ;; needs bat to be installed

;;-----------------------------------------------------------------------------
;; aliases
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
    (eshell/alias "d" "dired $1")))

;;-----------------------------------------------------------------------------
;; prompt
;; get the name of the current directory
(defun df/dir-base-name ()
  "give current directory base name (or ~ in /home/user/)"
  (let ((pwd (eshell/pwd))
	(myhome "/home/failer"))
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
