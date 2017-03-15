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
(package-initialize)
(setq inhibit-startup-screen t)

(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/evil")

(require 'highlight)
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)

(require 'octaspire-dern-mode)

(require 'goto-chg)

;(setq evil-search-module 'evil-search)
(require 'evil)
(evil-mode 1)
;(evil-select-search-module 'evil-search-module 'evil-search)
(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol))

(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

(eval-after-load 'evil-maps
    '(progn
        (define-key evil-motion-state-map (kbd "ö") 'evil-ex)))

(require 'linum-relative)
(linum-mode)
(linum-relative-global-mode)
(setq linum-relative-current-symbol "")

(setq c-default-style "bsd"
      c-basic-offset 4
      indent-tabs-mode nil)

(setq-default indent-tabs-mode nil)

;(with-eval-after-load 'evil
;    (defalias #'forward-evil-word #'forward-evil-symbol))

(which-function-mode 1)

;(global-hl-line-mode 1)
;(set-face-background 'hl-line "#330")

(setq make-backup-files nil)

(save-place-mode 1)
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)
;(load-theme 'adwaita)
;(load-theme 'tango)

(setq column-number-mode t)

(setq org-log-done 'time)

(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1373e3623ed5d758ef06dd19f2c8a736a69a15496c745a113d42230ab71d6b58" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
