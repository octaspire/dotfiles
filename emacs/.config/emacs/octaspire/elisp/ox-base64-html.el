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
     (buffer-string))
   t))

(defun octaspire/get-data-url (name)
  "Get the full data URL for the given file NAME."
  (format "%s%s"
	    (octaspire/get-data-url-prefix name)
	    (octaspire/get-file-as-base64-string name)))

(defun octaspire/org-html--format-image (source attributes info)
  (format "<img src='%s' alt='%s'/>"
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

(defun octaspire/html-copy-button (id text)
  (concat "<button title='copy to clipboard' class='src-copy-button' id='button_"
	  id
	  "'>copy to clipboard</button>\n"
	  "<textarea readonly id='area_" id "' style='opacity:.01;height:0;position:absolute;z-index:-1;overflow:hidden;'>"
	  (s-replace "'" "&apos;"
		     (s-replace "\"" "&quot;"
				(s-replace ">" "&gt;"
					   (s-replace "<" "&lt;"
						      (s-replace "&" "&amp"
								 (s-chop-prefix "\"" (s-chop-suffix "\"" text)))))))
	  "</textarea>\n"))

(defun octaspire/html-copy-script (id)
  (concat "<script>\n"
	  "  var button = document.querySelector('#button_" id "');\n"
	  "  button.addEventListener('click', function(event) {\n"
	  "    var area = document.querySelector('#area_" id "');\n"
	  "    area.select();\n"
	  "    document.execCommand('copy');"
          "  });\n"
          "</script>"))

(defun octaspire/org-src-block-to-string (src-block info)
  (let ((print-escape-newlines nil))
    (prin1-to-string (org-export-format-code-default src-block info) t)))

(defun octaspire/org-src-block (src-block content info)
  (let* ((id (symbol-name (cl-gensym)))
	 (text (octaspire/org-src-block-to-string src-block info))
	 (button (octaspire/html-copy-button id text))
	 (script (octaspire/html-copy-script id)))
    (concat
     "\n\n"
     button
     "\n"
     script
     "\n"
     (org-export-with-backend 'html src-block content info)
     "\n")))

(defun octaspire/ox-base64-html-link (link desc info)
  (let ((type (org-element-property :type link))
	(path (org-element-property :path link)))
    (if (and (string= type "file") (not (org-export-inline-image-p link)))
	(format "<a download=\"%s\" href=\"%s\">%s</a>"
		path
		(octaspire/get-data-url path)
		path)
      (cl-letf (((symbol-function 'org-html--format-image) 'octaspire/org-html--format-image))
	(org-export-with-backend 'html link desc info)))))

(defun octaspire/org-html-export-to-base64-html-buffer (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let ((org-html-doctype "html5")
	(org-html-html5-fancy t)
	(html-inline-images t)
	(org-html-inline-images t))
    (cl-letf (((symbol-function 'org-html--format-image) 'octaspire/org-html--format-image))
      (org-export-to-buffer
	  'octaspire/html-base64
	  "*Org OCTASPIRE/HTML-BASE64 Export*"
	async
	subtreep
	visible-only
	body-only
	ext-plist
	(lambda () (set-auto-mode t))))))

(defun octaspire/org-html-export-to-base64-html-file (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let ((org-html-doctype "html5")
	(org-html-html5-fancy t)
	(html-inline-images t)
	(org-html-inline-images t)
	(file (org-export-output-file-name ".html" subtreep))
	(org-export-coding-system org-html-coding-system))
    (cl-letf (((symbol-function 'org-html--format-image) 'octaspire/org-html--format-image))
      (org-export-to-file 'octaspire/html-base64 file async subtreep visible-only body-only ext-plist))))

(defun octaspire/org-html-toc-advice (orig-fun &rest args)
  (concat "<button title='toggle table of contents' class='toc-toggle-button' id='button_toc_toggle'>toggle table of contents</button>"
	  "<script>\n"
	  "  var button = document.querySelector('#button_toc_toggle');\n"
	  "  button.addEventListener('click', function(event) {\n"
	  "    var s= document.querySelector('#table-of-contents').style;\n"
	  "    if (s.display == 'none' || s.display == '') { s.display = 'block'; } else { s.display = 'none'; }"
          "  });\n"
          "</script>"
	  (apply orig-fun args)))

(advice-add 'org-html-toc :around #'octaspire/org-html-toc-advice)

(org-export-define-derived-backend 'octaspire/html-base64 'html
  :translate-alist '((link          . octaspire/ox-base64-html-link)
                     (special-block . octaspire/org-html-special-block)
		     (src-block     . octaspire/org-src-block))
  :menu-entry
  '(?b "As base64 data URL HTML"
       ((?H "As HTML buffer"
	    octaspire/org-html-export-to-base64-html-buffer)
	(?h "As HTML file"
	    octaspire/org-html-export-to-base64-html-file)
	(?o "As HTML file and open"
	    (lambda (a s v b)
	      (if a
		  (octaspire/org-html-export-to-base64-html-file t s v b)
		(org-open-file
		 (octaspire/org-html-export-to-base64-html-file nil s v b))))
	    octaspire/org-html-export-to-base64-html-file))))

(defun octaspire/ox-base64-html-publish (plist filename pub-dir)
  (org-publish-org-to 'octaspire/html-base64 filename
		      (concat "." (or (plist-get plist :html-extension)
				      org-html-extension
				      "html"))
		      plist pub-dir))
