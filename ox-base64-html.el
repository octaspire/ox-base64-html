;;; ox-base64-html.el --- Export Org document to data URI HTML

;; Copyright (C) 2020 octaspire.com
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

;; Author: octaspire.com
;; Version: 0.1.0
;; Package-Requires: ((noflet "0.0.15") (s "1.12.0"))
;; Keywords: Org, export, HTML5, base64

;;; Commentary:

;; This package provides Org HTML export backend that embeds
;; assets like images, videos and compressed archives as base64
;; encoded data URIs into the generated HTML5 file.

(use-package noflet
  :ensure t)

(use-package s
  :ensure t)

(require 'org)
(require 'ox-html)
(require 'noflet)

;;; Code:

(defun octaspire/get-mime-type (name)
  "Get the mime type for the given file NAME."
  (let ((ext (downcase (or (file-name-extension name) ""))))
    (cond ((string= ext "svg") "image/svg+xml")
	  ((string= ext "gif") "image/gif")
	  ((string= ext "png") "image/png")
	  ((or (string= ext "jpg")
	       (string= ext "jpeg")) "image/jpeg")
	  ((string= ext "zip") "application/zip")
	  ((string= ext "tar") "application/x-tar")
	  ((string= ext "gz") "application/gzip")
	  ((or (string= ext "txt")
	       (string= ext "patch")
	       (string= ext "asc")) "text/plain")
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
  "Format data URI \"img\" tag with given SOURCE and ATTRIBUTES.
SOURCE is a string specifying the location of the image file.
Binary data inside SOURCE is base64 encoded and embedded in
the image element using data URI scheme.  ATTRIBUTES is a plist,
similar to one returned by 'org-export-read-attribute'.  INFO is
a plist used as a communication channel."
  (format "<img src='%s' alt='%s'/>"
	  (octaspire/get-data-url source)
	  (file-name-nondirectory source)))

(defun octaspire/org-html-special-block (special-block contents info)
  "Handle SPECIAL-BLOCK.
Either format a \"video\" tag with
binary video data from CONTENTS embedded as a base64 encoded
data URI, or dispatch other types to default handler.
INFO is a plist holding extra information."
  (save-match-data
    (let* ((block-type (org-element-property :type special-block))
	   (attributes (org-export-read-attribute :attr_html special-block))
	   (poster (plist-get attributes :poster))
	   (str (concat " " (org-html--make-attribute-string attributes)))
	   (regexp "src=\".*\" ")
	   (start (string-match regexp contents))
	   (end (match-end 0)))
      (if (and (string= block-type "video") (and start end))
	  (progn
	    (when poster
	      (setq attributes (plist-put
				attributes
				:poster
				(octaspire/get-data-url poster)))
	      (setq str (concat " " (org-html--make-attribute-string attributes))))
	    (format "<%s%s>\n<source src=\"%s\" type=\"video/mp4\">\n\
Your browser does not support the video tag.\n</%s>"
		    block-type
		    str
		    (octaspire/get-data-url
		     (substring contents (+ start 5) (- end 2)))
		    block-type))
	(org-html-special-block special-block contents info)))))

(defun octaspire/html-copy-button (id text)
  "Return string defining HTML button and hidden text area containing TEXT.
ID is used to construct identifiers for both."
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
  "Return a string containing JavaScript for implementing copying.
Text from correct text area is copied into the system clipboard when
the button is clicked.  Identifiers for the button and  text area are
constructed based on the given ID."
  (concat "<script>\n"
	  "  var button = document.querySelector('#button_" id "');\n"
	  "  button.addEventListener('click', function(event) {\n"
	  "    var area = document.querySelector('#area_" id "');\n"
	  "    area.select();\n"
	  "    document.execCommand('copy');"
          "  });\n"
          "</script>"))

(defun octaspire/org-src-block-to-string (src-block info)
  "Convert SRC-BLOCK to text.
INFO is a plist holding extra information."
  (let ((print-escape-newlines nil))
    (prin1-to-string (org-export-format-code-default src-block info) t)))

(defun octaspire/org-src-block (src-block content info)
  "Convert SRC-BLOCK to HTML.
CONTENT contains the contents of the item.
INFO is a plist holding extra information."
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
  "Convert LINK to HTML.
DESC is the description for the link, or the empty string.
INFO is a plist holding extra information.
Links representing files are embedded as base64 embedded data URIs
into the HTML for downloading.  Images are embedded also as base64
encoded data URIs."
  (let ((type (org-element-property :type link))
	(path (org-element-property :path link)))
    (if (and (string= type "file") (not (org-export-inline-image-p link)))
	(format "<a download=\"%s\" href=\"%s\">%s</a>"
		path
		(octaspire/get-data-url path)
		path)
      (if (string= type "fuzzy")
	  (format "<a href=\"%s\">%s</a>"
		  path
		  desc)
	(cl-letf (((symbol-function 'org-html--format-image) 'octaspire/org-html--format-image))
	  (org-export-with-backend 'html link desc info))))))

(defun octaspire/org-html-export-to-base64-html-buffer
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to HTML buffer, assets embedded as base64 data URIs.
Non-nil optional ASYNC means the process should be asynchronous.
Non-nil optional SUBTREEP means that subtree at point is exported.
Non-nil optional VISIBLE-ONLY means that hidden elements are not exported.
Non-nil optional BODY-ONLY means that writing happens only inside a body
element.  EXT-PLIST, when given, is a property list that overrides default
Org settings, but not any file-local settings."
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

(defun octaspire/org-html-export-to-base64-html-file
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to HTML file, assets embedded as base64 data URIs.
Non-nil optional ASYNC means the process should be asynchronous.
Non-nil optional SUBTREEP means that subtree at point is exported.
Non-nil optional VISIBLE-ONLY means that hidden elements are not exported.
Non-nil optional BODY-ONLY means that writing happens only inside a body
element.  EXT-PLIST, when given, is a property list that overrides default
Org settings, but not any file-local settings."
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
  "Advice regular table of contents creation by adding a toggle button.
ORIG-FUN is the function that is adviced.
ARGS holds all the arguments."
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
  "Publish an Org file to HTML with assets embedded as base64 data URIs.
PLIST is the property list for the project.  FILENAME is the name
of the Org file being published.  PUB-DIR is the target directory
for the publishing."
  (org-publish-org-to 'octaspire/html-base64 filename
		      (concat "." (or (plist-get plist :html-extension)
				      org-html-extension
				      "html"))
		      plist pub-dir))


;; Unit tests

(ert-deftest octaspire/get-mime-type-test ()
  (should (string= "image/svg+xml"            (octaspire/get-mime-type "test.svg")))
  (should (string= "image/gif"                (octaspire/get-mime-type "test.gif")))
  (should (string= "image/png"                (octaspire/get-mime-type "test.png")))
  (should (string= "image/png"                (octaspire/get-mime-type "test.png")))
  (should (string= "image/jpeg"               (octaspire/get-mime-type "test.jpg")))
  (should (string= "image/jpeg"               (octaspire/get-mime-type "test.jpeg")))
  (should (string= "application/zip"          (octaspire/get-mime-type "test.zip")))
  (should (string= "application/x-tar"        (octaspire/get-mime-type "test.tar")))
  (should (string= "application/gzip"         (octaspire/get-mime-type "test.gz")))
  (should (string= "text/plain"               (octaspire/get-mime-type "test.txt")))
  (should (string= "text/plain"               (octaspire/get-mime-type "test.patch")))
  (should (string= "text/plain"               (octaspire/get-mime-type "test.asc")))
  (should (string= "application/octet-stream" (octaspire/get-mime-type "test.nosuch"))))

(ert-deftest octaspire/get-data-url-prefix ()
  (should (string= "data:image/svg+xml;base64,"
		   (octaspire/get-data-url-prefix "test.svg")))
  (should (string= "data:application/octet-stream;base64,"
		   (octaspire/get-data-url-prefix "test.nosuch"))))

(ert-deftest octaspire/get-file-as-base64-string ()
  (noflet ((insert-file-contents-literally (name) (insert "abc123")))
    (should (string= "YWJjMTIz" (octaspire/get-file-as-base64-string "nosuchfile")))))

(ert-deftest octaspire/get-data-url ()
  (noflet ((insert-file-contents-literally (name) (insert "abc123")))
    (should (string= "data:application/octet-stream;base64,YWJjMTIz"
		     (octaspire/get-data-url "nosuchfile")))
    (should (string= "data:image/png;base64,YWJjMTIz"
		     (octaspire/get-data-url "nosuchfile.png")))))

(ert-deftest octaspire/org-html--format-image ()
  (noflet ((insert-file-contents-literally (name) (insert "abc123")))
    (should (string=
	     "<img src='data:image/png;base64,YWJjMTIz' alt='nosuchfile.png'/>"
	     (octaspire/org-html--format-image "nosuchfile.png" nil nil)))))

;; octaspire/org-html-special-block not tested at the moment

(ert-deftest octaspire/html-copy-button ()
  (should (string=
	   "<button title='copy to clipboard' class='src-copy-button' id='button_myId'>copy to clipboard</button>\n<textarea readonly id='area_myId' style='opacity:.01;height:0;position:absolute;z-index:-1;overflow:hidden;'>&apos;&quot;&gt;&lt;&amp myText</textarea>\n"
	   (octaspire/html-copy-button "myId" "'\"><& myText"))))


(provide 'ox-base64-html)

;;; ox-base64-html.el ends here
