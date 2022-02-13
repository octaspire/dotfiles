;; Octaspire dotfiles - Various configuration files
;; Copyright 2017, 2018, 2020, 2021, 2022  www.octaspire.com
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
(require 'recentf)
(require 'server)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(when (eq system-type 'darwin)
  ;; Package 'exec-path-from-shell' should be installed as early as possible,
  ;; because without it all the tests using 'executable-find' might fail to
  ;; find existing executables in macOS.
  (use-package exec-path-from-shell
    :ensure t
    :config (when (memq window-system '(mac ns x))
              (exec-path-from-shell-initialize)))
  (setq mac-option-key-is-meta  nil
        mac-command-key-is-meta t
        mac-command-modifier    'meta
        mac-option-modifier     nil))

(use-package goto-last-change
  :ensure t
  :bind (("C-c C-/" . goto-last-change)))

(when (executable-find "git")
  (use-package magit
    :ensure t
    :bind (("C-c M-s" . magit-status))))

(use-package paredit
  :ensure t
  :config (require 'paredit))

(let ((path "~/quicklisp/slime-helper.el"))
  (if (file-exists-p path)
      (load (expand-file-name path))
    (warn "No '%s'. Not loading SLIME" path)))

(put 'narrow-to-region 'disabled nil)

(set-language-environment "UTF-8")
(save-place-mode 1)
(global-auto-revert-mode 1)
(recentf-mode t)
(ido-mode 1)
(menu-bar-mode 0)
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))
(show-paren-mode)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)

(defvar inferior-lisp-ecl   "ecl")
(defvar inferior-lisp-ccl64 "ccl64")
(defvar inferior-lisp-clisp "clisp")
(defvar inferior-lisp-sbcl  "sbcl")

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist         `((".*" . ,temporary-file-directory))
      browse-url-browser-function    'eww-browse-url
      calendar-week-start-day        1
      column-number-mode             t
      default-directory              (getenv "HOME")
      display-time-24hr-format       t
      ido-create-new-buffer          'always
      ido-enable-flex-matching       t
      ido-everywhere                 t
      ido-file-extensions-order      '(".lisp" ".asd" ".el" ".org")
      ido-ignore-extensions          t
      ido-use-filename-at-point      'guess
      ido-use-virtual-buffers        t
      inferior-lisp-program          inferior-lisp-sbcl
      inhibit-startup-message        t
      org-src-fontify-natively       1
      ring-bell-function             'ignore
      show-paren-delay               0
      tags-add-tables                t
      tags-revert-without-query      t)

(defun octaspire/init-file-open ()
  "Visit Emacs initialization file."
  (interactive)
  (find-file user-init-file))

(defun octaspire/whitespace-mode ()
  "Enable whitespace mode."
  (set-face-background 'trailing-whitespace "yellow")
  (setq show-trailing-whitespace 1
        whitespace-style         '(face trailing tabs))
  (whitespace-mode))

(defun octaspire/programming-mode-hook ()
  "Setup programming modes"
  (paredit-mode +1)
  (flyspell-prog-mode)
  (unless (string= major-mode "slime-repl-mode")
    (octaspire/whitespace-mode)))

(global-set-key (kbd "C-S-s") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-c i") 'octaspire/init-file-open)
(global-set-key (kbd "C-C m") 'recompile)
(global-set-key (kbd "C-C s") 'slime)
(global-set-key (kbd "C-C t") 'eshell)

(add-hook 'prog-mode-hook       'octaspire/programming-mode-hook)
(add-hook 'slime-repl-mode-hook 'octaspire/programming-mode-hook)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config (load-theme 'sanityinc-tomorrow-night))

(set-face-attribute 'default nil :height 120)

(unless (server-running-p)
  (server-start))
