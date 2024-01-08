;;; Compiled snippets and support files for `inferior-ess-r-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'inferior-ess-r-mode
		     '(("rmd" "rmarkdown::render(\"${1:`(file-name-nondirectory (buffer-file-name (other-buffer (current-buffer) t)))`}\",\noutput_file = \"${1:$(concat (file-name-base yas-text) \"_\" (magit-git-string '(\"describe\" \"--always\" \"--tags\")) )}.html\", output_options = list(self_contained=FALSE))\nfs::file_move(c(\"${1:$(concat (file-name-base yas-text) \"_\" (magit-git-string '(\"describe\" \"--always\" \"--tags\")) )}.html\", \"${1:$(concat (file-name-base yas-text) \"_\" (magit-git-string '(\"describe\" \"--always\" \"--tags\")) )}_files\"),  \"results/`(magit-git-string \"tag\")`)\")\n" "rmd" nil nil nil "/camp/home/kellyg/.emacs.d/snippets/inferior-ess-r-mode/rmd" nil nil)
		       ("display" "Sys.setenv(`(replace-regexp-in-string \"\\n$\" \"\\\"\" (string-replace \"=\" \"=\\\"\" (shell-command-to-string \"tmux show-environment DISPLAY\")))`)" "Inherit DISPLAY from TMUX environment" nil nil nil "/camp/home/kellyg/.emacs.d/snippets/inferior-ess-r-mode/display" nil nil)))


;;; Do not edit! File generated at Fri Nov  3 15:55:50 2023
