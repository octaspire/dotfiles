; Based on tutorial on emacswiki.org/emacs/ModeTutorial
(require 'highlight)

(defvar octaspire-dern-mode-hook
  (lambda ()
    (hlt-highlight-regexp-region nil nil "|newline|"       'highlight)
    (hlt-highlight-regexp-region nil nil "|tab|"           'highlight)
    (hlt-highlight-regexp-region nil nil "|bar|"           'highlight)
    (hlt-highlight-regexp-region nil nil "|string\-start|" 'highlight)
    (hlt-highlight-regexp-region nil nil "|string\-end|"   'highlight)
    (hlt-highlight-regexp-region nil nil "|[0-9a-fA-F]+|"  'highlight)))

(defvar octaspire-dern-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for octaspire-dern major mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dern\\'" . octaspire-dern-mode))

(defconst octaspire-dern-lock-keywords-1
  (list
   (cons (concat "\\_<" (regexp-opt '("!=" "*" "+" "++" "+=" "-" "--" "-=" "-==" "/" "<" "<=" "="
                       "==" "===" ">" ">=" "abort" "and" "define" "do" "doc" "env-current"
                       "env-global" "env-new" "eval" "exit" "find" "fn" "for" "hash-map" "if"
                       "input-file-open" "io-file-open" "len" "mod" "not" "ln@" "cp@" "or" "pop-front"
                       "port-close" "port-dist" "port-flush" "port-length" "port-read" "port-seek"
                       "port-write" "print" "println" "quote" "read-and-eval-path"
                       "read-and-eval-string" "return" "select" "starts-with?"
                       "string-format" "to-integer" "to-string" "uid" "vector" "while"
                       "output-file-open" "port-supports-output?" "port-supports-input?" "require"
                       "queue" "queue-with-max-length" "list" "howto" "howto-ok" "howto-no" "as"
                       "in" "pop-back"
                       ) t) "\\_>") font-lock-builtin-face)
   (cons (regexp-opt '("nil" "false" "true") t) font-lock-constant-face)
   (cons "\\('\\w*'\\)" font-lock-variable-name-face)
   (cons "\\('\\d*'\\)" font-lock-constant-face)
   )
   "Minimal highlighting expressions for octaspire-dern mode")

(defvar octaspire-dern-font-lock-keywords octaspire-dern-lock-keywords-1
  "Default highlighting expressions for octaspire-dern mode")

(defvar octaspire-dern-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\; "<"       st)
    (modify-syntax-entry ?\n ">"       st)
    (modify-syntax-entry ?'  "'"       st)
    (modify-syntax-entry ?(  "()"      st)
    (modify-syntax-entry ?)  ")("      st)
    (modify-syntax-entry ?[  "|"       st)
    (modify-syntax-entry ?]  "|"       st)
    (modify-syntax-entry ?|  "\""      st)
    (modify-syntax-entry ?#  ". 14b"   st)
    (modify-syntax-entry ?!  ". 23b"   st)
    (modify-syntax-entry ??  "w"       st)
    st)
  "Syntax table for octaspire-dern-mode")

(defun octaspire-dern-mode ()
 "Major mode for editing octaspire dern language files"
 (interactive)
 (kill-all-local-variables)
 (set-syntax-table octaspire-dern-mode-syntax-table)
 (use-local-map octaspire-dern-mode-map)
 (set (make-local-variable 'font-lock-defaults) '(octaspire-dern-font-lock-keywords))
 (setq major-mode 'octaspire-dern-mode)
 (setq mode-name "OCTASPIRE DERN")
 (run-hooks 'octaspire-dern-mode-hook))

(show-paren-mode 1)

(provide 'octaspire-dern-mode)
