;;===========================================================================;;
;; ORG CONFIG:
;;===========================================================================;;

;; add orgmode elpa to archives:
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;;-----------------------------------------------------------------------------
;; font settings
(custom-set-faces ;; title height
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.05))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.1)))))

;; latex preview scale
(set-default 'preview-scale-function 1.2)
(defun update-org-latex-fragments ()
  (org-latex-preview '(64))
  (plist-put org-format-latex-options :scale text-scale-mode-amount)
  (org-latex-preview '(16)))
(add-hook 'text-scale-mode-hook 'update-org-latex-fragments)

(defun df/font-setup ()
 ;; Ensure that anything that should be fixed-pitch in Org files appears that way
 (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
 (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
 (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
 (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
 (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
 (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
 (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
 (set-face-attribute 'line-number nil :inherit 'fixed-pitch))

;; toggle variable-pitch-mode with proper fonts
(setq df/var-pitch-enable nil)
(defun df/var-pitch-mode ()
  (interactive)
  (if (not df/var-pitch-enable)
      (progn
	(setq df/var-pitch-enable t)
	(variable-pitch-mode 1)
	(df/font-setup))
    (progn
	(setq df/var-pitch-enable nil)
	(variable-pitch-mode 0))))

;;-----------------------------------------------------------------------------
;; org settings

;; global orgmode settings
(use-package org
  :config
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t)
  (setq org-src-fontify-natively t)
  (setq org-fontify-whole-heading-line t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 2)
  (setq org-hide-block-startup nil)
  (setq org-src-preserve-indentation nil)
  (setq org-startup-folded 'content)
  (setq org-cycle-separator-lines 2)
  (org-indent-mode 1)
  (df/font-setup))

;;-----------------------------------------------------------------------------
;; org babel
;; babel load languages
(org-babel-do-load-languages
'org-babel-load-languages
'((haskell . t) (emacs-lisp . t) (shell . t)
    (C . t) (lua . t) (java . t) (haskell . t)
    (lisp . t)))
(setq org-confirm-babel-evaluate nil)

;; nice bullets for headers;
(use-package org-bullets
  :ensure t
  :config
  (setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;-----------------------------------------------------------------------------
;; better display for writing
(use-package visual-fill-column
  :config
  (setq-default visual-fill-column-center-text t))

(setq df/writing-mode-active nil)

;; activate writing mode
(defun df/writing-mode-on ()
  (setq-local df/writing-mode-active t)
  (setq-local global-hl-line-mode nil)
  (org-indent-mode 1)
  (display-line-numbers-mode 0)
  ;; (text-scale-set 1)
  (visual-fill-column-mode 1))

;; deactivate writing mode
(defun df/writing-mode-off ()
  (setq-local df/writing-mode-active nil)
  (setq-local global-hl-line-mode t)
  (org-indent-mode 0)
  (display-line-numbers-mode 1)
  ;; (text-scale-mode 0)
  (visual-fill-column-mode 0))

;; toggle writing mode
(defun df/writing-mode ()
  (interactive)
  (if (not df/writing-mode-active)
      (df/writing-mode-on)
    (df/writing-mode-off)))

;;-----------------------------------------------------------------------------
;; org tree slide:

;; set some settings for the presetation
(defun df/prez-start ()
  (org-display-inline-images)
  (df/var-pitch-mode)
  (df/writing-mode-on)
  (text-scale-set 3))

(defun df/prez-end ()
  (df/writing-mode-off)
  (text-scale-set 0))

(use-package org-tree-slide
  :hook ((org-tree-slide-play . df/prez-start)
	 (org-tree-slide-stop . df/prez-end))
  :custom
  (org-tree-slide-slide-in-effect-toggle t))

;;-----------------------------------------------------------------------------
;; open notes and agenda
(evil-define-key 'normal 'global (kbd "<leader>O a") (lambda () (interactive) (find-file "~/.emacs.d/Org/agenda.org")))
(evil-define-key 'normal 'global (kbd "<leader>O j") (lambda () (interactive) (find-file "~/.emacs.d/Org/journal.org")))
(evil-define-key 'normal 'global (kbd "<leader>O n") (lambda () (interactive) (df/fuzzy-find-file "~/.emacs.d/Org/notes/")))

;;-----------------------------------------------------------------------------
;; Org Agenda:
(setq org-directory "~/.emacs.d/Org/"
      org-agenda-files '("~/.emacs.d/Org/agenda.org"))

;; TODO keywords and faces:
(setq org-todo-keywords '((sequence "TODO" "PROJ" "STRT" "WAIT" "|" "DONE" "CANCELLED" )))
(setq org-todo-keyword-faces
      '(("PROJ" . (:foreground "RoyalBlue1" :weight bold))
        ("STRT" . (:foreground "OrangeRed" :weight bold))
        ("WAIT" . (:foreground "SlateBlue3" :weight bold))))
(setq org-tag-faces
      '(("HOME" . (:foreground "GoldenRod" :weight bold))
        ("RESEARCH" . (:foreground "RoyalBlue" :weight bold))
        ("OS" . (:foreground "IndianRed1" :weight bold))
        ("DEV" . (:foreground "IndianRed1" :weight bold))
        ("IMPORTANT" . (:foreground "Red" :weight bold))
        ("noexport" . (:foreground "LimeGreen" :weight bold))))

;; Open agenda and schedule
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c C-a") (lambda () (interactive) (find-file "~/.emacs.d/Org/agenda.org")))
(global-set-key (kbd "C-c s") 'org-agenda-schedule)
(global-set-key (kbd "C-c c") 'org-capture)

;; custom command for agenda menu
(setq org-agenda-custom-commands
      '(("D" "Waiting tasks."
         ;; display WAIT items
         ((todo "WAIT" ;; focus on specific keyword
                ((org-agenda-overriding-header "Waiting tasks\n")))
	  ;; todos
          (todo "TODO" ;; focus on specific keyword
                ((org-agenda-block-separator nil)
		 (org-agenda-overriding-header "Pending tasks\n")))
          ;; display agenda elements
          (agenda ""
                  ((org-agenda-span 1) ;; number of days to display
                   (org-deadline-warning-days 0) ;; disable warnings for further deadlines
                   (org-agenda-day-face-function
                    (lambda (date) 'org-agenda-date)) ;; overwrite default function for displaying the date (remove under line)
                   (org-agenda-overriding-header "Daily agenda tasks\n")))))))

;;-----------------------------------------------------------------------------
;; TODO: Org Capture:
(use-package org-capture
  :ensure nil
  :config
  (setq org-capture-templates
        '(;; add a TODO to Work
	  ("w" "TODO Work" entry
           (file+headline "~/.emacs.d/Org/agenda.org" "Work")
           "* TODO %?\n")
	  ;; add a TODO to Home
	  ("h" "TODO Home" entry
           (file+headline "~/.emacs.d/Org/agenda.org" "Home")
           "* TODO %?\n")
	  ;; add a TODO to PC
	  ("p" "TODO PC" entry
           (file+headline "~/.emacs.d/Org/agenda.org" "PC")
           "* TODO %?\n")
	  ;; add a TODO to Personal
	  ("o" "TODO Perso" entry
           (file+headline "~/.emacs.d/Org/agenda.org" "Personal")
           "* TODO %?\n")

	  ;; Prompts for keywords
          ("k" "keywords")
	  ;; Work
          ("kw" "KEY Work" entry
           (file+headline "~/.emacs.d/Org/agenda.org" "Work")
           "* %^{keyword|TODO|PROJ|STRT|WAIT} %?\n")
	  ;; Home
          ("kh" "KEY Home" entry
           (file+headline "~/.emacs.d/Org/agenda.org" "Home")
           "* %^{keyword|TODO|PROJ|STRT|WAIT} %?\n")
	  ;; PC
          ("kp" "KEY PC" entry
           (file+headline "~/.emacs.d/Org/agenda.org" "PC")
           "* %^{keyword|TODO|PROJ|STRT|WAIT} %?\n")
	  ;; Personal
          ("ko" "KEY Perso" entry
           (file+headline "~/.emacs.d/Org/agenda.org" "Personal")
           "* %^{keyword|TODO|PROJ|STRT|WAIT} %?\n")

	  ;; Scheduling
          ("s" "Scheduling")
	  ;; Work
          ("sw" "Schedule Work" entry
           (file+headline "~/.emacs.d/Org/agenda.org" "Work")
           "* %^{keyword|TODO|PROJ|STRT|WAIT} %?\n SCHEDULE: %^t")
	  ;; Home
          ("sh" "Schedule Home" entry
           (file+headline "~/.emacs.d/Org/agenda.org" "Home")
           "* %^{keyword|TODO|PROJ|STRT|WAIT} %?\n SCHEDULE: %^t")
	  ;; Personal
          ("so" "Schedule Perso" entry
           (file+headline "~/.emacs.d/Org/agenda.org" "Personal")
           "* %^{keyword|TODO|PROJ|STRT|WAIT} %?\n SCHEDULE: %^t")

	  ;; journal
	  ("j" "journal" entry (file+datetree "~/.emacs.d/Org/journal.org")
	   "* %?\n %i\n %a")

	  ;; new note
          ("n" "NOTES")
	  ("nn" "NOTE" entry
           (file "~/.emacs.d/Org/notes/notes.org")
           "* %?\n")
	  ("nc" "cours" entry
           (file+headline "~/.emacs.d/Org/notes/notes.org" "Cours")
           "* %?\n")
	  ("np" "prog" entry
           (file+headline "~/.emacs.d/Org/notes/notes.org" "Programming")
           "* %?\n")
	  ("nP" "proj" entry
           (file+headline "~/.emacs.d/Org/notes/notes.org" "Projects")
           "* %?\n"))))
