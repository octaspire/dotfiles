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

(setq inhibit-startup-message        t
      org-replace-disputed-keys      t
      octaspire-inferior-lisp        "sbcl"
      show-paren-delay               0
      recentf-max-saved-items        50
      default-directory              (getenv "HOME")
      ido-enable-flex-matching       t
      ido-everywhere                 t
      ido-use-filename-at-point      'guess
      ido-create-new-buffer          'always
      ido-ignore-extensions          t
      ido-file-extensions-order      '(".lisp" ".asd" ".el" ".org")
      inferior-lisp-program          octaspire-inferior-lisp
      common-lisp-style              octaspire-inferior-lisp
      common-lisp-style-default      octaspire-inferior-lisp
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
      require-final-newline          nil)

(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta  nil
        mac-command-key-is-meta t
        mac-command-modifier    'meta
        mac-option-modifier     nil))

(set-face-attribute 'default nil :height 140)

(put 'narrow-to-region 'disabled nil)

(setq-default indent-tabs-mode        nil
              electric-indent-inhibit t
              frame-title-format      '(:eval (format "GNU Emacs - %s"
                                                      (if buffer-file-name
                                                          buffer-file-name
                                                        "%b"))))

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

(use-package sly
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package lispy
  :ensure t)

(use-package operate-on-number
  :ensure t
  :bind (("C-c n" . operate-on-number-at-point)))

(use-package avy
  :ensure t
  :bind (("C-=" . avy-goto-char-timer)
         ("M-=" . avy-goto-line)))

(use-package goto-last-change
  :ensure t
  :bind (("C-c C-/" . goto-last-change)))

(when (and module-file-suffix (executable-find "cmake"))
  (use-package vterm
    :ensure t
    :custom
    (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no" "Ensure build succeeds")
    (vterm-always-compile-module t "Compile module without asking")
    :bind (:map vterm-mode-map ("C-]" . (lambda ()
                                          (interactive)
                                          (vterm-send-key "]" nil nil t))))))

(when (executable-find "git")
  (use-package magit
    :ensure t
    :bind (("C-c M-s" . magit-status))))

(recentf-mode t)
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
(electric-indent-mode -1)
(ido-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode)

(defun octaspire/lisp-mode-hook ()
  (lispy-mode 1)
  (rainbow-delimiters-mode))

(defun octaspire/whitespace-mode ()
  "Enable whitespace mode."
  (set-face-background 'trailing-whitespace "yellow")
  (setq show-trailing-whitespace 1
        whitespace-style         '(face trailing tabs))
  (whitespace-mode))

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

(defun octaspire/kill-path-of-buffer ()
  "Copy path of current buffer into kill ring"
  (interactive)
  (let ((path (buffer-file-name)))
    (when path
      (let ((truename (file-truename path)))
        (kill-new truename)
        (message truename)))))

(add-hook 'emacs-lisp-mode-hook #'octaspire/lisp-mode-hook)
(add-hook 'lisp-mode-hook       #'octaspire/lisp-mode-hook)
(add-hook 'sly-mrepl-mode-hook  #'octaspire/lisp-mode-hook)
(add-hook 'text-mode-hook       'flyspell-mode)
(add-hook 'prog-mode-hook       'flyspell-prog-mode)
(add-hook 'prog-mode-hook       'subword-mode)
(add-hook 'prog-mode-hook       'octaspire/whitespace-mode)
(add-hook 'org-mode-hook        'octaspire/whitespace-mode)
(add-hook 'nxml-mode-hook       'octaspire/whitespace-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))

(global-set-key (kbd "C-S-s")   'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-C m")   'recompile)
(global-set-key (kbd "C-c i")   'octaspire/init-file-open)
(global-set-key (kbd "C-c v")   'octaspire/terminal-launch)
(global-set-key (kbd "C-c M-p") 'octaspire/kill-path-of-buffer)

(unless (server-running-p)
  (server-start))

(provide 'octaspire-init-el)
