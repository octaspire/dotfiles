;; Octaspire dotfiles - Various configuration files
;; Copyright 2017, 2018, 2020  www.octaspire.com
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;    http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
(require 'package)
(require 'server)
;; This must be set before org is loaded,
;; or it doesn't have any effect.
(setq org-replace-disputed-keys t)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Package 'exec-path-from-shell' should be installed as early as possible,
;; because without it all the tests using 'executable-find' might fail to find
;; existing executables in macOS.
(use-package exec-path-from-shell
  :ensure t
  :config (when (memq window-system '(mac ns x))
	    (exec-path-from-shell-initialize)))

(setq octaspire/root-dir (concat (file-name-as-directory user-emacs-directory)
				 (file-name-as-directory "octaspire")))

(setq octaspire/elisp-dir (concat octaspire/root-dir
				  (file-name-as-directory "elisp")))

(load (concat octaspire/elisp-dir
	      "octaspire-dern-mode.el"))

(setq octaspire/private-config-file (concat octaspire/elisp-dir
					    "octaspire-private.el"))

(setq octaspire/ox-base64-html-file (concat octaspire/elisp-dir
					    "ox-base64-html.el"))

(defun octaspire/init-file-open (arg)
  "Visit Emacs initialization file."
  (interactive "P")
  (if arg
      (find-file octaspire/private-config-file)
    (find-file user-init-file)))

(defun octaspire/notify-warning (type title body)
  "Notify used of a warning."
  (display-warning type body)
  (message "ERROR: %s" body)
  (if (eq system-type 'darwin)
      (let ((terminal-notifier (executable-find "terminal-notifier")))
	(when terminal-notifier
	  (start-process "terminal-notifier"
			 "*terminal-notifier*"
			 terminal-notifier
			 "-title" title
			 "-message" body
			 "-sender" "org.gnu.Emacs")))
    (notifications-notify
     :title title
     :body body
     :urgency 'critical)))

(defun octaspire/counsel-ag-in-project ()
  "Use `counsel-ag' to search in the current (projectile) project.
See also `counsel-git-grep'."
  (interactive)
  (counsel-ag nil (projectile-project-root)))

(defun octaspire/open-and-goto-line-below()
  "Create new indented line below current one and go there."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun octaspire/kill-path-of-buffer ()
  "Copy path of current buffer into kill ring"
  (interactive)
  (let ((path (buffer-file-name)))
    (when path
      (let ((truename (file-truename path)))
	(kill-new truename)
	(message truename)))))

(defun octaspire/ensure-at-beginning-of-defun ()
  "Make sure that point is at beginning of current list"
  (unless (eq ?\( (char-after))
    (beginning-of-defun)))

(defun octaspire/cl/form-to-1am-test ()
  "Create a Common Lisp 1am unit test for the form at point."
  (interactive)
  (octaspire/ensure-at-beginning-of-defun)
  (let* ((thing-as-string (thing-at-point 'list))
	 (normalized-string (s-replace-all '(("'" . "Q")
					     (")" . "]")
					     ("(" . "["))
					   (string-trim  thing-as-string "(" ")")))
	 (name (string-join (split-string normalized-string " ") "-"))
	 (result (concat "(test " name "-test\n(is (equal " thing-as-string " )))")))
    (insert result)
    (indent-for-tab-command)
    (kill-sexp)
    (left-char 3)))

(use-package dashboard
  :ensure t
  :config (progn
	    (setq dashboard-set-footer nil)
	    (dashboard-setup-startup-hook)))

(when (executable-find "git")
  (use-package magit
    :ensure t
    :bind (("C-c M-s" . magit-status))))

(use-package nhexl-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)

(use-package lorem-ipsum
  :ensure t)

(use-package projectile
  :ensure t
  :bind (("C-c p" . projectile-command-map))
  :config (progn
	    (when (or (eq system-type 'darwin)
		      (eq system-type 'gnu/linux)
		      (eq system-type 'berkeley-unix))
	      (when (octaspire/lisp-found-p)
		(setq projectile-project-search-path
		      '("~/quicklisp/local-projects/"))))
	    (projectile-mode +1)))

(let ((name (executable-find "aspell")))
  (when name
    (setq ispell-program-name name)))

(use-package smart-mode-line
  :ensure t)

 (use-package company
  :ensure t
  :config (progn
	    (setq company-idle-delay            0.5
		  company-minimum-prefix-length 2
		  company-dabbrev-other-buffers 'all
		  company-dabbrev-downcase      nil
		  company-backends              '((company-etags)
						  (company-capf)
						  (company-dabbrev-code)
						  (company-dabbrev)))
	    (global-company-mode)
	    (define-key company-active-map (kbd "M-n") nil)
	    (define-key company-active-map (kbd "M-p") nil)
	    (define-key company-active-map (kbd "C-n") #'company-select-next)
	    (define-key company-active-map (kbd "C-p") #'company-select-previous)
	    (define-key company-active-map (kbd "C-j") #'company-complete-selection))
  :bind (("C-c c" . company-complete-common)))

(use-package remember
  :ensure t
  :bind (("C-c r" . remember)
	 ("C-c R" . remember-region)))

(use-package expand-region
  :ensure t
  :bind (("C-c M-e" . er/expand-region)))

(use-package speed-type
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(when (executable-find "cscope")
  (use-package xcscope
    :ensure t
    :config (cscope-setup)))

(when (executable-find "go")
  (use-package go-mode
    :ensure t))

(when (executable-find "drracket")
  (use-package racket-mode
    :ensure t))

(when (executable-find "gforth")
  (use-package forth-mode
    :ensure t))

(when (executable-find "nasm")
  (use-package nasm-mode
    :ensure t
    :config (add-to-list 'auto-mode-alist '("\\.nasm\\'" . nasm-mode))))

(defun octaspire/lisp-found-p ()
  "Tell if Common Lisp is available in the system."
  (executable-find "sbcl"))

(use-package lispy
    :ensure t
    :config (progn
	      (setq-default lispy-no-space t)
	      (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))))

(when (octaspire/lisp-found-p)
  (let ((style "sbcl"))
    (setq inferior-lisp-program     style
	  common-lisp-style         style
	  common-lisp-style-default style))
  (let ((path (expand-file-name "~/common-lisp/HyperSpec-7-0/HyperSpec/")))
    (when (file-directory-p path)
      (setq common-lisp-hyperspec-root (concat "file://" path))))
  (use-package slime-company
    :ensure t
    :after (slime company)
    :config (setq slime-company-completion 'fuzzy))
  (use-package slime
    :ensure t
    :config (progn (slime-setup '(slime-fancy
				  slime-asdf
				  slime-quicklisp
				  slime-scratch
				  slime-fuzzy
				  slime-company
				  slime-banner))
		   (add-hook 'slime-mode-hook
			     (lambda ()
			       (lispy-mode 1)
			       (define-key slime-mode-map (kbd "C-c e") 'slime-eval-buffer)
			       (define-key slime-mode-map (kbd "C-c C-l") nil)
			       (define-key slime-mode-map (kbd "M-p")     nil)))
		   (add-hook 'slime-repl-mode-hook (lambda () (lispy-mode 1)))
		   (add-hook 'slime-editing-mode-hook
			     (lambda ()
			       (define-key slime-editing-map (kbd "C-c C-l") nil))))))

(use-package counsel
	     :ensure t)

(use-package swiper
	     :ensure t
	     :config (progn
		       (require 'ivy)
		       (require 'swiper)
		       (ivy-mode 1)
		       (counsel-mode)
		       (setq ivy-wrap t
			     ivy-use-virtual-buffers t
			     ivy-count-format "(%d/%d) "))
	     :bind (("C-s" . swiper-isearch)))

(use-package avy
  :ensure t
  :bind (("C-=" . avy-goto-char)
	 ("M-=" . avy-goto-line)))

(use-package operate-on-number
  :ensure t
  :bind (("C-c n" . operate-on-number-at-point)))

(use-package ace-window
  :ensure t
  :bind (("M-p" . ace-window)))

(use-package dumb-jump
  :ensure t
  :bind (("M-g j" . dumb-jump-go)
	 ("M-g o" . dumb-jump-go-other-window)
	 ("M-g b" . dumb-jump-back)
	 ("M-g i" . dumb-jump-go-prompt))
  :config (setq dumb-jump-selector 'ivy))

(use-package goto-last-change
	     :ensure t
	     :bind (("C-c C-/" . goto-last-change)))

(use-package buffer-move
  :ensure t
  :bind (("<C-S-up>"    . buf-move-up)
	 ("<C-S-down>"  . buf-move-down)
	 ("<C-S-left>"  . buf-move-left)
	 ("<C-S-right>" . buf-move-right)))

(when (and (executable-find "pkg-config")
           (string-prefix-p
            "-I"
            (shell-command-to-string "pkg-config --cflags-only-I poppler-glib")))
  (use-package pdf-tools
    :ensure t
    :config (progn (when (eq system-type 'darwin)
		     (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))
		   (pdf-tools-install)
		   (setq pdf-view-midnight-colors '("#000000" . "#ffffea"))
		   (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
		   (add-hook 'doc-view-mode-hook (lambda ()
						   (when (eq doc-view-doc-type 'pdf)
						     (pdf-view-mode)))))))

(use-package s
  :ensure t)

(use-package htmlize
  :ensure t)

(when (and module-file-suffix (executable-find "cmake"))
  (use-package vterm
    :ensure t
    :bind (:map vterm-mode-map ("C-]" . (lambda ()
					  (interactive)
					  (vterm-send-key "]" nil nil t))))))

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(which-function-mode 1)
(set-language-environment "UTF-8")
(save-place-mode 1)
(savehist-mode 1)
(show-paren-mode 1)
(global-auto-revert-mode 1)
(electric-pair-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-message        t
      browse-url-browser-function    'eww-browse-url
      ring-bell-function             'ignore
      display-time-24hr-format       t
      calendar-week-start-day        1
      large-file-warning-threshold   nil
      tags-add-tables                t
      backup-directory-alist         `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      org-src-fontify-natively       1
      org-export-with-smart-quotes   1
      org-html-htmlize-output-type   'inline-css
      org-html-head-include-default-style nil
      user-full-name                 ""
      user-mail-address              ""
      octaspire/elfeed/feeds         '()
      org-replace-disputed-keys      t
      org-src-preserve-indentation   t
      org-html-checkbox-type         'html
      global-hl-line-sticky-flag     t
      org-html-doctype               "html5"
      org-html-html5-fancy           t
      org-html-postamble             "Exported %T. &nbsp; | &nbsp; Modified %C.<br/>%c")

(global-hl-line-mode)

(defun octaspire/load-if-exists (name)
  "Load Emacs lisp file NAME, if it exists"
  (when (file-exists-p name)
    (load name)))

(octaspire/load-if-exists octaspire/private-config-file)
(octaspire/load-if-exists octaspire/ox-base64-html-file)

(use-package elfeed
  :ensure t
  :config (setq elfeed-feeds
		octaspire/elfeed/feeds))

(unless (or (< (length user-full-name) 2)
	    (< (length user-mail-address) 2))
  (setq org-html-postamble
	(concat org-html-postamble "<br/>%a %e")))

(defun octaspire/whitespace-mode ()
  "Enable whitespace mode."
  (set-face-background 'trailing-whitespace "yellow")
  (setq show-trailing-whitespace 1
	whitespace-style         '(trailing))
  (whitespace-mode))

(defun octaspire/terminal-launch ()
  "Launch vterm if installed, otherwise eshell."
  (interactive)
  (if (featurep 'vterm)
      (vterm)
    (eshell)))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'prog-mode-hook 'octaspire/whitespace-mode)
(add-hook 'org-mode-hook  'octaspire/whitespace-mode)
(add-hook 'nxml-mode-hook 'octaspire/whitespace-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(require 'org)
(define-key org-mode-map (kbd "C-c C-p") nil)
(define-key org-mode-map (kbd "M-p")     nil)

(global-set-key (kbd "C-c i")   'octaspire/init-file-open)
(global-set-key (kbd "C-c m")   'recompile) ; build with 'make -k'
(global-set-key (kbd "C-c j")   'octaspire/open-and-goto-line-below)
(global-set-key (kbd "C-c v")   'octaspire/terminal-launch)
(global-set-key (kbd "C-c e")   'eval-buffer)
(global-set-key (kbd "C-c M-.") 'swiper-isearch-thing-at-point)
(global-set-key (kbd "C-c t")   'octaspire/cl/form-to-1am-test)
(global-set-key (kbd "C-c M-p") 'octaspire/kill-path-of-buffer)
(global-set-key (kbd "C-c M-g") 'octaspire/counsel-ag-in-project)
(global-set-key (kbd "s-u")     'up-list)
(global-set-key (kbd "C-x M-f") 'counsel-file-jump) ; or find-name-dired, counsel-fzf
(global-set-key (kbd "C-c M-f") 'counsel-git-grep)

(put 'narrow-to-region 'disabled nil)

(use-package acme-theme
  :ensure t
  :config (load-theme 'acme t))

(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta  nil
	mac-command-key-is-meta t
	mac-command-modifier    'meta
	mac-option-modifier     nil))

(add-hook 'latex-mode-hook
	  (lambda ()
	    (define-key latex-mode-map (kbd "C-c C-l") nil)))

(add-hook 'eshell-mode-hook
	  (lambda ()
	    (define-key eshell-mode-map (kbd "C-c l") 'eshell-list-history)
	    (define-key eshell-mode-map (kbd "C-c C-l") nil)
	    (setq-local global-hl-line-mode nil)))

(add-hook 'vterm-mode-hook
	  (lambda ()
	    (setq-local global-hl-line-mode nil)))

(add-hook 'dashboard-mode-hook
	  (lambda ()
	    (setq-local global-hl-line-mode nil)))

;; C coding style
(defun octaspire/c-mode-hook ()
  (define-key c-mode-base-map (kbd "C-j") 'newline)
  (define-key c-mode-base-map (kbd "C-c C-l") nil)
  (let ((spaces 4))
    (setq c-default-style  "bsd"
	  indent-tabs-mode nil
	  tab-width        spaces
	  c-basic-offset   spaces)
    ;; Indent 'case' labels in switch statements.
    (c-set-offset 'case-label '+)
    ;; Don't indent '{' after 'if' (when on its own line).
    (c-set-offset 'substatement-open 0)
    ;; Continued lines should be indented by one depth.
    (c-set-offset 'statement-cont spaces)
    (c-set-offset 'arglist-cont-nonempty '+)
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'statement-block-intro '+)))

(add-hook 'c-mode-common-hook 'octaspire/c-mode-hook)

(defun octaspire/indenting-advice (orig-fun &rest args)
  (let ((transient-mark-mode nil))
    (apply orig-fun args)
    (unless (member major-mode '(latex-mode org-mode))
      (indent-region (region-beginning) (region-end)))))

(advice-add 'yank :around #'octaspire/indenting-advice)
(advice-add 'yank-pop :around #'octaspire/indenting-advice)

(unless (server-running-p)
  (server-start))

(when (or (file-exists-p "~/.fonts/IBMPlexMono-Regular.ttf")
	  (file-exists-p "~/Library/Fonts/IBMPlexMono-Regular.ttf"))
  (custom-set-faces
   '(default ((t (:family "IBM Plex Mono"))))
   '(bold ((t (:weight bold :family "IBM Plex Mono"))))
   '(bold-italic ((t (:slant italic :weight bold :family "IBM Plex Mono"))))
   '(italic ((t (:slant italic :family "IBM Plex Mono"))))
   '(variable-pitch ((t (:family "IBM Plex Sans"))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dashboard-items
   '((bookmarks . 10)
     (projects . 8)
     (agenda . 5)
     (recents . 10))))
