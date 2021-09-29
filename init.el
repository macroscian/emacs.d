;; README

;; most global objects are prefixed 'gpk-' - they shouldn't link to my area of the file-system, but they might depend on following my working patterns.

;; Things that are most likely specific to my way of working are probably signalled by use of anything derived from any calls to "user-login-name" or "gpk-babshome".  These are probably worth searching for.

;; Localisation

(if (string-match "babs" (system-name))
    (setq gpk-babshome (getenv "my_lab")
	  gpk-oncamp t)
  (setq gpk-babshome "I:\\"
	gpk-oncamp nil)
  )
(setq inhibit-default-init t)
(setq use-package-always-ensure t)
(defun gpk-ssh-refresh ()
  "Reset the environment variable SSH_AUTH_SOCK"
  (interactive)
  (let (ssh-auth-sock-old (getenv "SSH_AUTH_SOCK"))
    (setenv "SSH_AUTH_SOCK"
            (car (split-string
                  (shell-command-to-string
                   "ls -t $(find /tmp/ssh-* -user $USER -name 'agent.*' 2> /dev/null)"))))
    (message
     (format "SSH_AUTH_SOCK %s --> %s"
             ssh-auth-sock-old (getenv "SSH_AUTH_SOCK")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(setq use-package-always-ensure t)
(require 'package)
(add-to-list 'package-archives
 	     '("MELPA" . "https://melpa.org/packages/"))
(package-initialize)
(require 'use-package)
(require 'dash)
(add-to-list 'load-path "~/.emacs.d/hideshow-org")
(require 'hideshow-org)
(global-set-key "\C-ch" 'hs-org/minor-mode)
(add-to-list 'hs-special-modes-alist
             '(ess-mode "{" "}" "/[*/]" nil
			hs-c-like-adjust-block-beginning))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Prefs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
					;(load-theme 'solarized t)
(setq visible-bell t)
(setq solarized-use-variable-pitch nil)
(setq solarized-scale-org-headlines nil)
(load-theme 'solarized-light t)
					;(set-face-attribute 'region nil :foreground "#eee8d5" :background "#839496") 
(let ((line "#cccec4"))
  (set-face-attribute 'mode-line          nil :overline   line)
  (set-face-attribute 'mode-line-inactive nil :overline   line)
  (set-face-attribute 'mode-line-inactive nil :underline  line)
  (set-face-attribute 'mode-line          nil :box        nil)
  (set-face-attribute 'mode-line-inactive nil :box        nil)
  (set-face-attribute 'mode-line          nil :background "#2aa198")
  (set-face-attribute 'mode-line          nil :foreground "#532f62")
  (set-face-attribute 'mode-line-inactive nil :background "#eee8d5")
  )
(set-cursor-color "#532f62") 
(setq cursor-type 'bar)
(setq compilation-scroll-output t)

(setq ibuffer-saved-filter-groups
      '(("home" 
	 ("R scripts" (or
		       (mode . ess-r-mode)
		       (filename . "rmd$")
		       (filename . "Rmd$")
		       ))
	 ("R" (mode . inferior-ess-r-mode))
	 ("Julia scripts" (or
			   (mode . ess-julia-mode)
			   (filename . "jl$")
			   ))
	 ("Julia" (mode . inferior-ess-julia-mode))
	 ("Org" (or
		 (mode . org-mode)
		 (filename . "OrgMode")
		 ))
	 ("dired" (mode . dired-mode))
	 ("emacs" (or
		   (name . "^\\*scratch\\*$")
		   (name . "^\\*Messages\\*$")
		   (filename . ".emacs.d")
		   (filename . ".emacs")
		   (filename . "emacs-config")
		   ))
	 ;; ("Web Dev" (or (mode . html-mode)
	 ;; 		(mode . css-mode)))
	 ("Magit" (name . "^magit"))
	 ("Build" (mode . makefile-gmake-mode))
	 ("Forge" (or (name . "^\\*forge") (name . "^\\*Forge") ))
	 ("Help" (or
		  (name . "Help")
		  (name . "Apropos")
		  (name . "info")
		  ))
	 )))
(setq ibuffer-show-empty-filter-groups nil)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-auto-mode 1)
	     (ibuffer-switch-to-saved-filter-groups "home")))

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))


(use-package minions
  :config (minions-mode 1))


(set-fontset-font t '(#xe100 . #xe16f) "Iosevka") ;; see link on gpk-ESS-pretty-hook

(if gpk-oncamp
    (add-to-list 'default-frame-alist '(font . "Iosevka-12" ))
  (add-to-list 'default-frame-alist '(font . "Consolas" ))
  )

(menu-bar-mode nil) 
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(tool-bar-mode -1); hide the toolbar
(prefer-coding-system 'utf-8)
(display-time-mode t)
(global-prettify-symbols-mode 1)

;;Editing
(global-font-lock-mode t); syntax highlighting
(delete-selection-mode t); entry deletes marked text
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; wrap long lines in text mode
;;Shell
(setq shell-file-name "bash")
(setq shell-command-switch "-c")
;; Scrolling
(setq scroll-preserve-screen-position "always"
      scroll-conservatively 5
      scroll-margin 2)
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))
(winner-mode 1)
;;Backup Prefs
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . ".~"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t       ; use versioned backups
 vc-make-backup-files t
 )
(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . ".~/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook  'force-backup-of-buffer)
					;Spelling
(setq ispell-really-hunspell t)
;;(setq ispell-program-name "/camp/stp/babs/working/kellyg/code/bin/hunspell.sh")
(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "en_GB")
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-strip-common-suffix nil)
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package purpose-mode
;; :ensure t
;; :defer nil
;; :init
;; )



(use-package ace-window
  :ensure t
  :defer t
  :init
  (progn
    (global-set-key (kbd "M-o") 'ace-window)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    ;;more info at https://github.com/abo-abo/ace-window
    )
  )


(use-package easy-hugo
  :init
  (setq easy-hugo-basedir "/camp/stp/babs/working/kellyg/projects/babs/gavin.kelly/blog/")
  (setq easy-hugo-url "https://www.guermantes.xyz")
  (setq easy-hugo-sshdomain "blogdomain")
  (setq easy-hugo-root "/home/blog/")
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-bloglist
	;; blog2 setting
	'(((easy-hugo-basedir . "/camp/stp/babs/working/kellyg/projects/github/FrancisCrickInstitute/BABS_lab_site")
	   (easy-hugo-url . "https://wiki-bioinformatics.thecrick.org/~kellyg/blog")
	   (easy-hugo-sshdomain . "localhost")
	   (easy-hugo-postdir . "content/post")
	   (easy-hugo-root . "/camp/stp/babs/www/kellyg/public_html/LIVE/blog"))
	  ;; blog3 setting for aws s3
	  ;; blog4 setting for google cloud strage
	  ;; blog5 for github pages
	  ;; blog6 for firebase hosting
	  ))
  (defun gpk-hugo-no-hidden (dirs)
    (seq-filter
     (lambda (x) (not (string-match-p (regexp-quote "/.") x)))
     dirs)
    )
      
  (advice-add 'easy-hugo--directory-files-recursively :filter-return #'gpk-hugo-no-hidden)
  
  :bind ("C-c C-e" . easy-hugo))

;; (use-package company
;;   :ensure t
;;   :config
;;   (global-company-mode)
;;   (define-key company-active-map [return] nil)
;;   (define-key company-active-map [tab] 'company-complete-common)
;;   (define-key company-active-map (kbd "TAB") 'company-complete-common)
;;   (define-key company-active-map (kbd "M-TAB") 'company-complete-selection)
;;   (setq company-selection-wrap-around t
;; 	company-tooltip-align-annotations t
;; 	company-idle-delay 1.5
;; 	company-minimum-prefix-length 2
;; 	company-tooltip-limit 10)
;;   )

(use-package outshine
  :init
  (setq outshine-use-speed-commands t)
  (add-hook 'ess-mode-hook 'outshine-mode)
  (add-hook 'R-mode-hook 'outshine-mode)
  (add-hook 'julia-mode-hook 'outshine-mode)
  )



;; (use-package projectile
;;   :ensure t
;;   :config
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;   (projectile-mode +1))


(use-package magit
  :commands magit-get-top-dir
  :bind ("C-x g" . magit-status))

(use-package forge
  :after magit)

(use-package s)

(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  )

(use-package web-mode
  :commands web-mode
  :mode (("\\.html\\'" . web-mode)
	 ("\\.php\\'" . web-mode)
	 )
  :config
  (setq web-mode-markup-indent-offset 2)
  )

(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)
  )

(use-package dired+
  :config
  (diredp-toggle-find-file-reuse-dir 1)
  )

(use-package pandoc-mode
  :hook markdown-mode)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  )

(use-package polymode 
  :ensure t
  :mode
  ("\\.Snw" . poly-noweb+r-mode)
  ("\\.Rnw" . poly-noweb+r-mode)
  ("\\.Rmd" . poly-markdown+r-mode)
  ("\\.rmd" . poly-markdown+r-mode)
  )

(use-package highlight-parentheses
  :config
  (show-paren-mode t)
  (setq hl-paren-colors '("#1b9e77" "#d95f02" "#7570b3" "#e7298a" "#a6761d" "#e6ab02"))
  (global-highlight-parentheses-mode t)
  )

(use-package bookmark
  :config
  (use-package bookmark+))


(use-package treemacs
  :bind ("C-c t" . gpk-treemacsify)
  :bind (:map treemacs-mode-map
         ("W" . treemacs-switch-workspace))
  :init
  (defun gpk-treemacsify ()
    (interactive)
    "Put org projects into treemacs workspaces"
    (treemacs)
    (with-temp-buffer 
      (insert "* active\n")
      (setq active-pos (point-marker))
      (insert "* scientist\n")
      (setq sci-pos (point-marker))
      (insert "* inactive\n")
      (setq inactive-pos (point-marker))
      (insert-file-contents (concat gpk-babshome "www/kellyg/public_html/LIVE/tickets/yml/" user-login-name ".yml"))
      (set-marker-insertion-type active-pos t)
      (set-marker-insertion-type inactive-pos t)
      (set-marker-insertion-type sci-pos t)
      (keep-lines "^  \\(Project\\|Path\\|Active\\|Scientist\\): " inactive-pos (point-max))
      (while (re-search-forward "  Project: " nil t)
	(replace-match "** ")
	(when (re-search-forward "  Scientist: \\(.*\\)" nil t) (setq gpk-sci (car (split-string (match-string 1) "@"))))
	(when (re-search-forward "  Path: " nil t) (replace-match " - path :: "))
	(when (re-search-forward "  Active: \\(True\\|False\\)")
	  (if (string= (match-string 1) "True")
	      (setq gpk-move-to active-pos)
	    (setq gpk-move-to inactive-pos)
	    )
	  (save-excursion
	    (setq end-of-reg (point))
	    (forward-line -3)
	    (kill-region (point) end-of-reg)
	    (goto-char gpk-move-to)
	    (yank)
	    (insert "\n")
	    (goto-char sci-pos)
	    (yank)
	    (insert "\n")
	    (when (re-search-backward "\\*\\*" nil t) (replace-match (concat "** " gpk-sci)))
	    )
	  )
	)
      (beginning-of-buffer)
      (flush-lines "^  Active")
      (beginning-of-buffer)
      (flush-lines "^  Scientist")
      (beginning-of-buffer)
      (while (re-search-forward "/\\.babs$" nil t)
	(replace-match "")
	)
      (insert "\n* templates\n"
       (mapconcat (lambda (path) (concat "** " (file-name-nondirectory (substring path 0 -1)) "\n - path :: " path ))
		  (split-string
		   (shell-command-to-string (concat "ls -d " gpk-babshome "working/" user-login-name "/templates/*/")))
		  "\n" )
       )
      (write-file treemacs-persist-file)
      (treemacs--restore)
      )
    )
  )




(use-package perspective
  :config
  (persp-mode))

(use-package yasnippet
  :commands
  (yas-minor-mode)
  :init
  (setq yas-indent-line 'fixed)
  (yas-global-mode 1)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  (add-hook 'ess-mode-hook #'yas-minor-mode)
  :config
  (yas-reload-all)
  )

(load-file (expand-file-name "gpk/ess.el" user-emacs-directory))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; work patterns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Abbreviations for standard directories
(defun gpk-abbrev (pth)
  (let ((gpk-abbrev-alist `(("//CAMP/working/USER/projects/" ."PROJ>")
			    ("//CAMP/working/USER/" . "GPK>")
			    ("//CAMP/working/" . "WORK>")
			    ("//CAMP" . "BABS>")
			    (,(getenv "HOME") . "~")
			    ("/home/camp/USER" . "~")
			    )))
    (seq-reduce (lambda (thispth sublist) (replace-regexp-in-string
					   (replace-regexp-in-string "USER" user-login-name
								     (replace-regexp-in-string "^//CAMP/" gpk-babshome (first sublist))
								     )
					   (rest sublist) thispth))
		gpk-abbrev-alist pth)
    )
  )

;; Use abbreviations in window title
(setq frame-title-format
      '((:eval (gpk-abbrev (if (buffer-file-name)
			       (buffer-file-name)
			     (comint-directory "."))
			   )))
      )

;; Get lab names from directory structure
(if gpk-oncamp
    (setq gpk-lab-names (append
			 '("external")
			 (directory-files "/camp/lab" nil "^[a-z]")
			 (directory-files "/camp/stp" nil "^[a-z]")))
  (setq gpk-lab-names (append
		       '("external")
		       (mapcar 
			(lambda (x) (replace-regexp-in-string "lab-" "" x))
			(directory-files "\\\\data.thecrick.org" nil "^[a-z]"))
		       )
	)
  )


;; My bookmarks
(dolist (r `((?e (file . ,(concat "~/.emacs.d/init.el")))
	     (?t (file . ,(concat gpk-babshome "working/" user-login-name "/templates")))
	     (?P (file . ,(concat gpk-babshome "working/" user-login-name "/projects")))
	     (?T "## * TODO ")
	     (?c "@crick.ac.uk")
	     ))
  (set-register (first r) (second r)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interacting with slurm
(require 'transient)
(load-file (expand-file-name "slurm/slurm.el" user-emacs-directory))

(define-transient-command slurmacs ()
  "Slurmacs"
  :value (lambda () (list (concat "--output=" (buffer-file-name) ".out.log")
			  (concat "--error=" (buffer-file-name) ".err.log")
			  (concat "--job-name=" (buffer-name) "")
			  (concat "output_file='" (file-name-base) ".html'")
			  (concat "output_dir='" default-directory
				  (and (eq major-mode `ess-r-mode)
				       (first (ess-get-words-from-vector "dirname(vDevice())\n")) "'"))
			  (concat "params=list(verion='" (magit-git-str "describe") "')" )
			  "--mem=30G"  "--partition=cpu" "--time=1:00:00" "--cpus-per-task=4"
			  ))
  ["Arguments"
   ("-t" "Time Limit" "--time=" :reader gpk-sreader)
   ("-p" "Partition" "--partition=" :reader gpk-sreader)
   ("-c" "CPUs" "--cpus-per-task=" :reader gpk-sreader)
   ("-m" "Memory" "--mem=" :reader gpk-sreader)
   ("-l" "Log" "--output=" :reader gpk-freader)
   ("-e" "Error file" "--error=" :reader gpk-freader)
   ("-j" "Job Name" "--job-name=" :reader gpk-sreader)
   ]
  ["Markdown"
   :if (lambda () (bound-and-true-p poly-markdown+r-mode))
   ("-o" "Output file" "output_file=")
   ("-d" "Output dir" "output_dir=")
   ("-v" "Params" "params=")
   ]
  ["Actions"
   ("s" "Submit to slurm" slurmacs-submit)
   ("b" "Build script" slurmacs-message)
   ("S" "Status" slurm)
   ]
  )
(global-set-key "\C-cs" 'slurmacs)
(defun slurmacs-submit (&optional args)
  "Submit script to CAMP"
  (interactive (list (transient-args 'slurmacs)))
  (let ((script-string (gpk-build-slurm args))
	(inhibit-message t))
    (message script-string)
    (shell-command-to-string script-string)
    )
  )
(defun slurmacs-message (&optional args)
  "Submit script to CAMP"
  (interactive (list (transient-args 'slurmacs)))
  (setq shell-script (gpk-build-slurm args))
  (switch-to-buffer "slurm.sh")
  (insert shell-script)
  )
(defun gpk-build-slurm (args)
  "Make the bash script that will submit to slurm"
  (interactive (list (transient-args 'slurmacs)))
  (let* ((this-file (buffer-file-name (current-buffer)))
	 (all-args (gpk-slurmacs-args 'slurmacs))
	 (slurm-args (gpk-slurmacs-sub-args args all-args "Arguments"))
	 (rmd-args (gpk-slurmacs-sub-args args all-args "Markdown"))
	 (cmd (cond ((bound-and-true-p poly-markdown+r-mode)
		     (concat "R -e \"rmarkdown::render('" this-file "', " (string-join rmd-args ",")"')\"")
		     )
		    ((eq major-mode `ess-r-mode)
		     (concat "Rscript \"" this-file "\"")
		     )
		    (t this-file)
		    )
	      )
	 )
    (concat "sbatch " (string-join slurm-args " ") " <<EOF
#!/bin/bash
source " inferior-ess-r-program "
" cmd  "
[[ -z \"${SLACK_URL}\" ]] || curl -X POST -H 'Content-type: application/json' --data '{\"text\":\"'$SLURM_JOB_NAME' has finished\"}' \"${SLACK_URL}\"
EOF")
    )
  )
(defun gpk-slurmacs-args (trans)
  "Give a grouped list of all arguments in a transient suffix"
  (mapcar (lambda (grp) (cons (plist-get (aref grp 2) :description)
			      (seq-map (lambda (cmd) (plist-get (third cmd) :argument)) (aref grp 3))))
	  (get trans 'transient--layout)
	  ))
(defun gpk-slurmacs-sub-args (args grouped-args group)
  "Find which args belong to a specific group"
  (let (
	(these-args (rest (seq-find (lambda (grp) (string= (first grp) group)) grouped-args)))
	)
    (seq-filter (lambda (argstr)
		  (member (replace-regexp-in-string "\\(=\\).*" "\\1" argstr) these-args)
		  )
		args)
    )
  )



(org-add-link-type "outlook" 'org-outlook-open)

(defun org-outlook-open (id)
  "Open the Outlook item identified by ID.  ID should be an Outlook GUID."
  ;;(w32-shell-execute "open" (concat "outlook:" id))
  )

(define-transient-command dcamp ()
  "Dcamp"
  :value (lambda () (list (concat "file=" (or buffer-file-name default-directory))  (concat "host=" (first (split-string (getenv "SSH_CLIENT"))))))
  ["Arguments"
   ("-f" "File" "file=" :reader gpk-freader)
   ("-h" "Host" "host=" :reader gpk-sreader)
   (dcamp-region)
   ]
  [
   ["Actions"
    ("o" "Open on PC" run-dcamp)]
   ["Clipboard "
    ("c" "Copy File" run-dcamp)
    ("p" "Copy Path" run-dcamp)]
   ["Email"
    ("e" "Create" run-dcamp)
    ("r" "Reply to selected" run-dcamp)]
   ]
  )
(setq dcamp-map '(("o" . "open") ("c" . "cfile") ("p" . "cpath") ("e" . "email")("r" . "reply")))
(global-set-key "\C-cd" 'dcamp)
(defun run-dcamp (args dcamp-prefix)
  (interactive (list (transient-args 'dcamp) (alist-get (this-command-keys) dcamp-map nil nil 'equal)))
  (let* (
	 (all-vars (mapcar 'gpk-kv-to-cons args))
	 (path (rest (assoc "file" all-vars)))
	 (host (first(split-string
		      (alist-get "SSH_CONNECTION" (mapcar 'gpk-kv-to-cons (frame-parameter nil 'environment)) nil nil 'equal)
		      " ")))
	 )
    (make-network-process :name "dcamp"
			  :host host
			  :service (getenv "DCAMP_PORT")
			  :nowait t
			  :sentinel (lambda (&rest msg) (message (format "Connection message [%s]" msg)))
			  )
    (sit-for 1)
    (process-send-string (get-process "dcamp")
			 (concat (secure-hash 'sha256 (getenv "DCAMP")) ":"
				 dcamp-prefix ":-:"
				 (replace-regexp-in-string "/camp/stp" "//data.thecrick.org" path))
			 )
    (delete-process (get-process "dcamp"))
    )
  )
(define-infix-argument dcamp-region ()
  :description "Region"
  :class 'transient-switches
  :shortarg "-r"
  :key "-r"
  :argument "--region="
  :argument-regexp "\\(Path\\|Body\\)"
  :argument-format "%s"
  :choices '("Path" "Body"))
(defun gpk-sreader (prompt init hist)
  (let* (
	 (prefix (second (split-string (symbol-name (oref obj command)) ":")))
	 (all-vars (mapcar 'gpk-kv-to-cons (transient-args (intern prefix))))
	 (default (rest (assoc (string-remove-suffix "=" prompt) all-vars)))
	 )
    (read-string prompt default)
    )
  )
(defun gpk-freader (prompt init hist)
  (let* (
	 (prefix (second (split-string (symbol-name (oref obj command)) ":"))) ; 'obj' is inherited from transient-infix-read
	 (all-vars (mapcar 'gpk-kv-to-cons (transient-args (intern prefix))))
	 (default (rest (assoc (string-remove-suffix "=" prompt) all-vars)))
	 )
    (read-file-name prompt default)
    )
  )
(defun gpk-kv-to-cons (kv)
  (let ((ss (split-string kv "=")))
    (cons (first ss) (second ss)))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(browse-url-browser-function 'eww-browse-url)
 '(custom-safe-themes
   '("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "412c25cf35856e191cc2d7394eed3d0ff0f3ee90bacd8db1da23227cdff74ca2" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))
 '(dropbox-access-token
   "NzTOg8k2OwoAAAAAAAAH-6vExX9OI3S3LD3sJ_ToWYsYF_VNM-KnnTVTKmddhKkQ")
 '(ess-swv-pdflatex-commands '("pdflatex" "texi2pdf" "make"))
 '(ess-swv-processor 'knitr)
 '(hl-sexp-background-color "#efebe9")
 '(markdown-command "pandoc")
 '(moody-mode-line-height 20)
 '(org-clock-rounding-minutes 60)
 '(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (g-frame)))
 '(org-speed-commands-user '(("h" . org-clock-in) ("d" . lambda)))
 '(org-use-speed-commands t)
 '(package-selected-packages
   '(dash org-trello pcre2el julia-mode julia-repl julia-shell ess epc simpleclip poly-R poly-markdown poly-org lsp-mode purpose-mode window-purpose solarize-theme gnu-elpa-keyring-update hyperbole exwme exwm matlab-mode easy-hugo font-lock-studio gist dropbox sqlite r-autoyas pretty-symbols org-fancy-priorities flucui-themes company flycheck zenburn image+ color-theme-solarized groovy-mode f bookmark+ dired+ highlight-parentheses undo-tree))
 '(safe-local-variable-values '((babshash . babs8aecf935))))


					;(custom-set-faces
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
					; '(default ((t (:family "Consolas" :foundry "microsoft" :slant normal :weight normal :height 105 :width normal)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-highlight-input ((t (:inherit nil :foreground "#0000FF" :weight normal))))
 '(outline-1 ((t (:foreground "#268bd2" :box (:line-width 2 :color "grey75" :style pressed-button) :overline nil :weight bold :height 1.6))))
 '(outline-2 ((t (:foreground "#2aa198" :box (:line-width 2 :color "grey75" :style pressed-button) :overline nil :weight bold :height 1.4))))
 '(outline-3 ((t (:foreground "#b58900" :box (:line-width 2 :color "grey75" :style pressed-button) :overline nil :weight bold :height 1.2)))))
(put 'narrow-to-region 'disabled nil)
