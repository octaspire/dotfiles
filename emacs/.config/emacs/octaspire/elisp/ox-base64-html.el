(require 'org)
(require 'ox-html)

(defun octaspire/get-mime-type (name)
  "Get the mime type for the given file NAME."
  (let ((ext (or (downcase (file-name-extension name)) "")))
    (cond ((string= ext "svg") "image/svg+xml")
	  ((string= ext "gif") "image/gif")
	  ((string= ext "png") "image/png")
	  ((or (string= ext "jpg")
	       (string= ext "jpeg")) "image/jpeg")
	  ((string= ext "zip") "application/zip")
	  ((string= ext "tar") "application/x-tar")
	  ((string= ext "gz") "application/gzip")
	  ((or (string= ext "txt")
	       (string= ext "patch")) "text/plain")
	  (t "application/octet-stream"))))

(defun octaspire/get-data-url-prefix (name)
  "Get the first three components of data URL for the given file NAME."
  (let ((mime (octaspire/get-mime-type name)))
    (format "data:%s;base64,"
	    mime)))

(defun octaspire/get-file-as-base64-string (name)
  "Get the file NAME as base64 encoded string."
  (base64-encode-string
   (with-temp-buffer
     (insert-file-contents-literally name)
     (buffer-string))))

(defun octaspire/get-data-url (name)
  "Get the full data URL for the given file NAME."
  (format "%s%s"
	    (octaspire/get-data-url-prefix name)
	    (octaspire/get-file-as-base64-string name)))

(defun octaspire/org-html--format-image (source attributes info)
  (format "<img src=\"%s\" %s />"
	  (octaspire/get-data-url source)
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
Your browser does not support the video tag.\n</%s>"
		  block-type
		  str
		  (octaspire/get-data-url
		   (substring contents (+ start 5) (- end 2)))
		  block-type)
	""))))

(defun octaspire/ox-base64-html-link (link desc info)
  (let ((type (org-element-property :type link))
	(path (org-element-property :path link)))
    (if (and (string= type "file") (not (org-export-inline-image-p link)))
	(format "<a download=\"%s\" href=\"%s\">%S</a>"
		path
		(octaspire/get-data-url path)
		path)
      (cl-letf (((symbol-function 'org-html--format-image) 'octaspire/org-html--format-image))
	(org-export-with-backend 'html link desc info)))))

(defun octaspire/org-html-export-to-base64-html (&optional async subtree visible body)
  (interactive)
  (let ((org-html-doctype "html5")
	(org-html-html5-fancy t)
	(html-inline-images t)
	(org-html-inline-images t))
    (cl-letf (((symbol-function 'org-html--format-image) 'octaspire/org-html--format-image))
      (org-export-to-buffer 'octaspire/html-base64 "*Org OCTASPIRE/HTML-BASE64 Export*"))))

(org-export-define-derived-backend 'octaspire/html-base64 'html
  :translate-alist '((link                   . octaspire/ox-base64-html-link)
                     (org-html-special-block . octaspire/org-html-special-block))
  :menu-entry
  '(?h "Export to HTML"
       ((?b "As base64 data URL (images, mp4) HTML file"
	    octaspire/org-html-export-to-base64-html))))
