;; Octaspire dotfiles - Various configuration files
;; Copyright 2017, 2018, 2020, 2022  www.octaspire.com
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

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Package 'exec-path-from-shell' should be installed as early as possible.
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta  nil
        mac-command-key-is-meta t
        mac-command-modifier    'meta
        mac-option-modifier     nil))

(use-package magit
  :ensure t)

(use-package evil
  :ensure
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (require 'evil)
  (evil-mode)
  (add-hook 'evil-insert-state-entry-hook (lambda () (hl-line-mode -1)))
  (add-hook 'evil-insert-state-exit-hook  (lambda () (hl-line-mode +1)))
  (add-hook 'evil-normal-state-entry-hook (lambda () (hl-line-mode +1))))

(use-package evil-collection
  :after evil
  :ensure t
  :init
  (evil-collection-init))

(use-package lispy
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)
  (add-hook 'lisp-mode-hook       (lambda () (lispy-mode 1))))))

(let ((sbcl   (executable-find "sbcl"))
      (aspell (executable-find "aspell")))
  (when sbcl
    (use-package slime
      :ensure t
      :config
      (setq inferior-lisp-program sbcl)))
  (when aspell
    (setq ispell-program-name aspell)))

(use-package vterm
  :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq
    projectile-enable-caching                  t
    projectile-indexing-method                 'native
    projectile-auto-update-cache               nil
    projectile-globally-ignored-directories    '(".git")
    projectile-globally-ignored-file-suffixes  '("a" "so" "dylib" "lib" "plist" "bin"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;(projectile-add-known-project "~/add-path-here")
  ; ...
  )

(use-package selectrum
  :ensure t
  :config
  (selectrum-mode +1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox t))

(use-package htmlize
  :ensure t)

(use-package ox-base64-html
  :ensure nil
  :load-path "~/src/octaspire/ox-base64-html")

(setq
  vc-handled-backends                     nil
  column-number-mode                      t
  read-file-name-completion-ignore-case   t
  read-buffer-completion-ignore-case      t
  completion-styles                       '(orderless)
  orderless-skip-highlighting             (lambda () selectrum-is-active)
  selectrum-highlight-candidates-function #'orderless-highlight-matches
  display-line-numbers-type               'relative
  backup-directory-alist                  `((".*" . ,temporary-file-directory))
  auto-save-file-name-transforms          `((".*" ,temporary-file-directory t))
  org-src-fontify-natively                1
  org-export-with-smart-quotes            1
  org-html-htmlize-output-type            'inline-css
  org-html-head-include-default-style     nil
  user-full-name                          ""
  user-mail-address                       ""
  org-replace-disputed-keys               t
  org-src-preserve-indentation            t
  org-html-checkbox-type                  'html
  org-html-doctype                        "html5"
  org-html-html5-fancy                    t
  org-html-postamble                      "Exported %T. &nbsp; | &nbsp; Modified %C.<br/>%c"
  indent-tabs-mode                        nil)

(unless
  (or (< (length user-full-name) 2)
	    (< (length user-mail-address) 2))
  (setq org-html-postamble (concat org-html-postamble "<br/>%a %e")))

(setq-default
   inhibit-startup-message               t
   ring-bell-function                    'ignore
   display-time-24hr-format              t
   calendar-week-start-day               1
   large-file-warning-treshold           nil
   tags-add-tables                       t
   tags-revert-without-query             t
   display-line-numbers-current-absolute t
   show-trailing-whitespace              1
   whitespace-style                      '(face trailing tabs))

(global-display-line-numbers-mode)
(menu-bar-mode -1)
(which-function-mode 1)
(set-language-environment "UTF-8")
(save-place-mode 1)
(savehist-mode 1)
(show-paren-mode 1)
(global-auto-revert-mode 1)
(electric-pair-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(whitespace-mode +1)

(defun octaspire/init-file-open ()
  "Visit Emacs initialization file."
  (interactive)
  (find-file user-init-file))

(defun octaspire/terminal-launch ()
  "Launch vterm if installed, otherwise eshell."
  (interactive)
  (if (featurep 'vterm)
      (vterm)
    (eshell)))

(defun octaspire/c-mode-hook ()
  (let ((spaces 2))
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

(global-set-key (kbd "C-c i") 'octaspire/init-file-open)
(global-set-key (kbd "C-c t") 'octaspire/terminal-launch)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'subword-mode)

(provide 'octaspire-init-el)

(unless (server-running-p)
  (server-start))
