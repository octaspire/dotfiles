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
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "dark gray"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "tomato"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "dark olive green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "dark cyan"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "red4"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "magenta1")))))
