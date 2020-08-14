(require 'org)
(require 'ox-html)

(defun octaspire/get-mime-type (name)
  "Get the mime type for the given file NAME."
  (let ((ext (or (file-name-extension name) "")))
    (if (string= ext "svg")
	"svg+xml"
      ext)))

(defun octaspire/get-data-url-prefix (type name)
  "Get the first three components of data URL for the given file NAME."
  (let ((mime (octaspire/get-mime-type name)))
    (format "data:%s/%s;base64,"
	    type
	    mime)))

(defun octaspire/get-file-as-base64-string (name)
  "Get the file NAME as base64 encoded string."
  (base64-encode-string
   (with-temp-buffer
     (insert-file-contents-literally name)
     (buffer-string))))

(defun octaspire/get-data-url (type name)
  "Get the full data URL for the given file NAME."
  (let ((prefix (octaspire/get-data-url-prefix type name)))
    (format "data:%s/%s;base64,%s"
	    type
	    (octaspire/get-mime-type name)
	    (octaspire/get-file-as-base64-string name))))

(defun octaspire/org-html--format-image (source attributes info)
  (format "<img src=\"%s\" %s />"
	  (octaspire/get-data-url "image" source)
	  (file-name-nondirectory source)))

(defun octaspire/org-html-special-block (special-block contents info)
  (save-match-data
    (let* ((block-type (org-element-property :type special-block))
	   (attributes (org-export-read-attribute :attr_html special-block))
	   (str (concat " " (org-html--make-attribute-string attributes)))
	   (regexp "src=\".*\" ")
	   (start (string-match regexp contents))
	   (end (match-end 0)))
      (if (and start end)
	  (format "<%s%s>\n<source src=\"%s\" type=\"video/mp4\">\n\
Your browser does not support the video tag.\n<%s>"
		  block-type
		  str
		  (octaspire/get-data-url
		   "video"
		   (substring contents (+ start 5) (- end 2)))
		  block-type)
	""))))

(defun octaspire/org-html-publish-to-base64-html (plist filename pub-dir)
  (cl-letf (((symbol-function 'org-html--format-image) 'octaspire/org-html--format-image)
	    ((symbol-function 'org-html-special-block) 'octaspire/org-html-special-block))
    (org-html-publish-to-html plist filename pub-dir)))

(defun octaspire/org-html-export-to-base64-html (async subtree visible body)
  (cl-letf (((symbol-function 'org-html--format-image) 'octaspire/org-html--format-image)
	    ((symbol-function 'org-html-special-block) 'octaspire/org-html-special-block))
    (org-html-export-to-html nil subtree visible body)))

(org-export-define-derived-backend 'octaspire/html-base64 'html
  :menu-entry '(?h "Export to HTML"
		   ((?b "As base64 data URL (images, mp4) HTML file"
			octaspire/org-html-export-to-base64-html))))
