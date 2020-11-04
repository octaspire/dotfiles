(load "~/.config/emacs/octaspire/submodules/dern/release/tool-support/emacs/syntax/octaspire-dern-mode.el")
(load "~/.config/emacs/octaspire/submodules/ox-base64-html/ox-base64-html.el")
(require 'ob-css)
(require 'ox-publish)

(setq org-publish-project-alist
      '(("org"
	 :base-directory "~/src/octaspire/org-www/"
	 :base-extension "org"
	 :htmlized-source t
	 :recursive t
	 :exclude "README.org\\|base64-style.org\\|file-style.org"
	 :publishing-directory "~/www/"
	 :publishing-function octaspire/ox-base64-html-publish)
	("assets"
	 :base-directory "~/.config/emacs/octaspire/submodules/ox-octaspire-css/"
	 :base-extension "css"
	 :publishing-directory "~/www/assets/"
	 :publishing-function org-publish-attachment)
	("octaspire" :components ("org" "assets"))))

(defun octaspire/publish ()
  (interactive)
  (org-publish "octaspire" t))

(global-set-key (kbd "C-c C-p") 'octaspire/publish)
