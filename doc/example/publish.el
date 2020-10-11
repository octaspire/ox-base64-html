(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(load "~/ox-base64-html/ox-base64-html.el")
(require 'ob-css)
(require 'ox-publish)

(setq org-publish-project-alist
      '(("org"
	 :base-directory "~/ox-base64-html/doc/example/"
	 :base-extension "org"
	 :htmlized-source t
	 :recursive t
	 :publishing-directory "~/www/"
	 :publishing-function octaspire/ox-base64-html-publish)
	("octaspire" :components ("org"))))

(defun octaspire/publish ()
  (interactive)
  (org-publish "octaspire" t))
