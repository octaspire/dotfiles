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

;(unless (package-installed-p 'auto-package-update)
;  (package-refresh-contents)
;  (package-install 'auto-package-update))

;(package-install 'org)
;(package-initialize)
; Org mode cannot probably be updated from myinit.org
; because org mode would be active during the update.
;(auto-package-update-now)

(org-babel-load-file (locate-user-emacs-file "myinit.org"))
