(load "~/.config/emacs/octaspire/elisp/octaspire-dern-mode.el")
(load "~/.config/emacs/octaspire/elisp/ox-base64-html.el")
(require 'ob-css)
(require 'ox-publish)

(setq org-publish-project-alist
      '(("org"
	 :base-directory "~/src/octaspire/org-www/"
	 :base-extension "org"
	 :htmlized-source t
	 :recursive t
	 :publishing-directory "~/www/"
	 :publishing-function octaspire/ox-base64-html-publish)
	("assets"
	 :base-directory "~/.config/emacs/octaspire/css/"
	 :base-extension "css"
	 :publishing-directory "~/www/assets/"
	 :publishing-function org-publish-attachment)
	("octaspire" :components ("org" "assets"))))

(defun octaspire/publish ()
  (interactive)
  (org-publish "octaspire" t))

(global-set-key (kbd "C-c C-p") 'octaspire/publish)
