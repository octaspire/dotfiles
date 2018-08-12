(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org"   . "https://orgmode.org/elpa/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package org
  :ensure t
  :mode (("\\.org$" . org-mode)))

(org-babel-load-file (locate-user-emacs-file "myinit.org"))

(custom-set-faces
;;Rainbow delimiters font modification is based on
;;https://ericscrivner.me/2015/06/better-emacs-rainbow-delimiters-color-scheme/
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark red"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "dark green"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "dark blue"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "hot pink"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "turquoise1"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta4"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "tomato4"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "LightSteelBlue4")))))

