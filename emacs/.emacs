;-----------------------------------------------------------------------------
;  Octaspire dotfiles - Various configuration files
;  Copyright 2017 www.octaspire.com
;
;  Licensed under the Apache License, Version 2.0 (the "License");
;  you may not use this file except in compliance with the License.
;  You may obtain a copy of the License at
;
;     http://www.apache.org/licenses/LICENSE-2.0
;
;  Unless required by applicable law or agreed to in writing, software
;  distributed under the License is distributed on an "AS IS" BASIS,
;  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;  See the License for the specific language governing permissions and
;  limitations under the License.
;-----------------------------------------------------------------------------
;
; To use this configuration:
;
; 1. Install ispell using the OS package manager.
;
; 2. Make sure the ~/.emacs.d/elisp directory has the following files/directories:
;
;       highlight.el
;       linum-relative.el
;       octaspire-dern-mode.el
;       https://github.com/sellout/emacs-color-theme-solarized in directory ~/.emacs.d/themes/
;
; 2. Give the following commands inside Emacs:
;
;       M-x package-refresh-contents
;       M-x package-install RET ox-twbs
;       M-x package-install RET htmlize
;
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

(require 'octaspire-dern-mode)
(require 'highlight)
(require 'goto-chg)
(require 'linum-relative)
(require 'whitespace)
(require 'flyspell)
(require 'org)
(require 'ox-twbs)
(require 'htmlize)

(linum-mode)
(linum-relative-global-mode)
(global-hl-line-mode                        1)
(save-place-mode                            1)
(savehist-mode                              1)
(which-function-mode                        1)
(global-whitespace-mode                     1)
(modify-face                                whitespace-tab nil "#F92672")
(set-face-background                        'trailing-whitespace "yellow")
(defface extra-whitespace-face              '((t (:background "pale green"))) "Color for tabs and such.")
(defvar  bad-whitespace                     '(("\t" . 'extra-whitespace-face)))
(fset    'yes-or-no-p                       'y-or-n-p)
(tool-bar-mode                              -1)
(menu-bar-mode                              -1)
(toggle-scroll-bar                          -1)
(show-paren-mode                             1)
(flyspell-mode                               1)
(flyspell-prog-mode)

(setq-default show-trailing-whitespace      1)
(setq-default savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq-default key-chord-two-keys-delay      0.5)
(setq-default make-backup-files             nil)
(setq-default compile-command               "make")
(setq-default make-backup-files             nil)
(setq-default inhibit-startup-screen        1)
(setq-default abbrev-mode                   1)
(setq-default save-abbrevs                  'silently)
(setq-default indent-tabs-mode              nil)
(setq-default goto-chg                      1)
(setq-default linum-relative-current-symbol "")
(setq-default c-default-style               "bsd" c-basic-offset 4 indent-tabs-mode nil)
(setq-default whitespace-line-column        110)
(setq-default whitespace-style              '(face tabs trailing lines lines-tail tab-mark))
(setq-default column-number-mode            1)
(setq-default org-src-fontify-natively      1) ;syntax highlight code blocks in org mode
(setq-default org-export-with-smart-quotes  1)

(add-hook 'c-mode-hook    #'(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'c-mode-hook    #'(lambda () (flyspell-prog-mode)))
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode 1)

(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)
(load-theme 'solarized t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (ox-twbs org-edna avy syndicate htmlize))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(org-babel-do-load-languages
  'org-babel-load-languages
  '(
    (sh . t)
    (python  . t)
    ))


;(setq org-html-htmlize-output-type 'css)
(setq org-html-htmlize-output-type 'inline-css)

