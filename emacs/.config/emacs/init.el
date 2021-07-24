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

(setq octaspire/dark-mode
      (getenv "OCTASPIRE_EMACS_DARK_MODE"))

(setq octaspire/projectile-main-project nil)

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

(setq octaspire/desktop-dir (concat octaspire/root-dir
				    (file-name-as-directory "desktop")))

(setq octaspire/submodule-dir (concat octaspire/root-dir
				      (file-name-as-directory "submodules")))

(load (concat octaspire/submodule-dir
	      (file-name-as-directory "dern")
	      (file-name-as-directory "release")
	      (file-name-as-directory "tool-support")
	      (file-name-as-directory "emacs")
	      (file-name-as-directory "syntax")
	      "octaspire-dern-mode.el"))

(setq octaspire/private-config-file (concat octaspire/elisp-dir
					    "octaspire-private.el"))

(setq octaspire/publish-config-file (concat octaspire/elisp-dir
					    "publish.el"))

(setq octaspire/ox-base64-html-file (concat octaspire/submodule-dir
					    (file-name-as-directory "ox-base64-html")
					    "ox-base64-html.el"))

(defun octaspire/stringify-region-or-symbol-at-point (&optional argument)
  "Convert region (if active) into string, otherwise convert symbol at point.
If invoked with optional prefix ARGUMENT, ask for the two delimiter strings
to be used."
  (interactive "P")
  (let* (
	 (delim1 (if argument (read-string "first delimiter: ") "\""))
	 (delim2 (if argument (read-string "second delimiter: ") "\""))
	 (bounds (if mark-active
		     (car (region-bounds))
		   (bounds-of-thing-at-point 'symbol)))
	 (pos1 (car bounds))
	 (pos2 (cdr bounds)))
    (save-excursion
      (goto-char pos2)
      (insert delim2)
      (goto-char pos1)
      (insert delim1))))

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

(defun octaspire/utf-8-region-byte-length (arg)
  "Echo and, if prefix ARG isn't true, copy the length of the active region
as UTF-8 bytes. If given a prefix argument ARG, only echo the result without
adding it to the kill ring as a new kill."
  (interactive "P")
  (let* ((str (encode-coding-region
	       (region-beginning)
	       (region-end)
	       'utf-8
	       t))
	 (len (length str))
	 (res (format "%d" len)))
    (message "%s" res)
    (unless arg
      (kill-new res))))

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

(when (executable-find "rg")
  (use-package rg
    :ensure t
    :config (rg-enable-default-bindings)))

(use-package nhexl-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package yaml-mode
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
	    (projectile-mode +1)
	    (advice-add 'projectile-project-root :before-until
			(lambda (&rest args)
			  octaspire/projectile-main-project))))

(let ((name (executable-find "aspell")))
  (when name
    (setq ispell-program-name name)))

(use-package telephone-line
  :ensure t
  :config (progn
	    (setq telephone-line-lhs '((nil . (telephone-line-buffer-segment))))
	    (telephone-line-mode 1)))

(use-package company
  :ensure t
  :config (progn
	    (setq company-idle-delay            1.0
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

(use-package which-key
  :ensure t
  :config (which-key-mode))

(when (executable-find "cscope")
  (use-package xcscope
    :ensure t
    :config (cscope-setup)))

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
	      (add-hook 'emacs-lisp-mode-hook (lambda ()
						(lispy-mode 1)
						(define-key lispy-mode-map (kbd "M-m")      nil)
						(define-key lispy-mode-map (kbd "<return>") 'lispy-alt-line)))))

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
  (use-package slime-repl-ansi-color
    :ensure t
    :after (slime))
  (use-package slime
    :ensure t
    :config (progn (slime-setup '(slime-fancy
				  slime-cl-indent
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
			       (define-key slime-mode-map (kbd "C-c C-l") nil)))
		   (add-hook 'slime-repl-mode-hook (lambda ()
						     (lispy-mode 1)
						     (slime-repl-ansi-color-mode)
						     (define-key slime-repl-mode-map (kbd "<return>") 'slime-repl-newline-and-indent)
						     (define-key slime-repl-mode-map (kbd "C-j")      'slime-repl-return)))
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
  :bind (("C-c a" . ace-window)))

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

(when (and (executable-find "pkg-config")
           (string-prefix-p
            "-I"
            (shell-command-to-string "pkg-config --cflags-only-I poppler-glib")))
  (use-package pdf-tools
    :ensure t
    :config (progn (when (eq system-type 'darwin)
		     (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
		     (setq pdf-view-use-scaling t
			   pdf-view-use-imagemagick nil))
		   (pdf-tools-install)
		   (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
		   (setq pdf-view-midnight-colors (if octaspire/dark-mode
						      '("#b0c09f" . "#303030")
						    '("#686b93" . "#fdfde7")))
		   (add-hook 'doc-view-mode-hook (lambda ()
						   (when (eq doc-view-doc-type 'pdf)
						     (pdf-view-mode)))))))
(use-package s
  :ensure t)

(use-package htmlize
  :ensure t)

(if (eq system-type 'berkeley-unix)
    (use-package exwm
      :ensure t
      :config (progn
		(unless (server-running-p)
		  (server-start))
		(setenv "EDITOR" "emacsclient")
		(require 'exwm)
		(require 'exwm-config)
		(exwm-config-default)))
  (unless (server-running-p)
    (server-start)))

(when (and module-file-suffix (executable-find "cmake"))
  (use-package vterm
    :ensure t
    :custom
    (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no" "Ensure build succeeds")
    (vterm-always-compile-module t "Compile module without asking")
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
      tags-revert-without-query      t
      backup-directory-alist         `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      org-src-fontify-natively       1
      org-export-with-smart-quotes   1
      org-html-htmlize-output-type   'inline-css
      org-html-head-include-default-style nil
      user-full-name                 ""
      user-mail-address              ""
      org-replace-disputed-keys      t
      org-src-preserve-indentation   t
      org-html-checkbox-type         'html
      global-hl-line-sticky-flag     t
      org-html-doctype               "html5"
      org-html-html5-fancy           t
      org-html-postamble             "Exported %T. &nbsp; | &nbsp; Modified %C.<br/>%c"
      octaspire-require-final-nl     nil ;; Change nil to 'ask to ask every time.
      require-final-newline          octaspire-require-final-nl)

(global-hl-line-mode)

(defun octaspire/load-if-exists (name)
  "Load Emacs lisp file NAME, if it exists"
  (when (file-exists-p name)
    (load name)))

(octaspire/load-if-exists octaspire/private-config-file)
(octaspire/load-if-exists octaspire/ox-base64-html-file)
(octaspire/load-if-exists octaspire/publish-config-file)

(unless (or (< (length user-full-name) 2)
	    (< (length user-mail-address) 2))
  (setq org-html-postamble
	(concat org-html-postamble "<br/>%a %e")))

(defun octaspire/whitespace-mode ()
  "Enable whitespace mode."
  (set-face-background 'trailing-whitespace "yellow")
  (setq show-trailing-whitespace 1
	whitespace-style         '(face trailing tabs))
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
(define-key org-mode-map (kbd "C-c w")   (lambda () (interactive) (org-table-insert-row)))
(define-key org-mode-map (kbd "C-c s")   (lambda () (interactive) (org-table-insert-row t)))
(require 'dashboard)
(define-key dashboard-mode-map (kbd "C-j") 'dashboard-return)

(global-set-key (kbd "C-c i")   'octaspire/init-file-open)
(global-set-key (kbd "C-c \"")  'octaspire/stringify-region-or-symbol-at-point)
(global-set-key (kbd "C-c m")   'recompile) ; build with 'make -k'
(global-set-key (kbd "C-c j")   'octaspire/open-and-goto-line-below)
(global-set-key (kbd "C-c v")   'octaspire/terminal-launch)
(global-set-key (kbd "C-c u")   'octaspire/utf-8-region-byte-length)
(global-set-key (kbd "C-c e")   'eval-buffer)
(global-set-key (kbd "C-c M-.") 'swiper-isearch-thing-at-point)
(global-set-key (kbd "C-c t")   'octaspire/cl/form-to-1am-test)
(global-set-key (kbd "C-c M-p") 'octaspire/kill-path-of-buffer)
(global-set-key (kbd "C-c M-g") 'octaspire/counsel-ag-in-project)
(global-set-key (kbd "s-u")     'up-list)
(global-set-key (kbd "C-x M-f") 'counsel-file-jump) ; or find-name-dired, counsel-fzf
(global-set-key (kbd "C-c M-f") 'counsel-git-grep)
(global-set-key (kbd "C-c d")   (lambda () (interactive) (desktop-save octaspire/desktop-dir)))
(global-set-key (kbd "C-c M-d") (lambda () (interactive) (desktop-read octaspire/desktop-dir)))

(put 'narrow-to-region 'disabled nil)

(use-package moe-theme
  :ensure t
  :config (load-theme (if octaspire/dark-mode 'moe-dark 'moe-light) t))

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

(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "C-j") 'dired-find-file)))

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

(when (or (file-exists-p "~/.fonts/IBMPlexMono-Regular.ttf")
	  (file-exists-p "~/Library/Fonts/IBMPlexMono-Regular.ttf"))
  (let ((h 140))
    (custom-set-faces
     `(default ((t (:family "IBM Plex Mono" :height ,h))))
     `(bold ((t (:weight bold :family "IBM Plex Mono" :height ,h))))
     `(bold-italic ((t (:slant italic :weight bold :family "IBM Plex Mono" :height ,h))))
     `(italic ((t (:slant italic :family "IBM Plex Mono" :height ,h))))
     `(variable-pitch ((t (:family "IBM Plex Sans" :height ,h)))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dashboard-items
   '((bookmarks . 10)
     (projects . 8)
     (agenda . 5)
     (recents . 10)))
 '(mode-require-final-newline octaspire-require-final-nl)
 '(truncate-lines t))

(put 'downcase-region 'disabled nil)

(provide 'octaspire-init-el)
