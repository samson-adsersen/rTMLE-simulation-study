
(require 'org)
(require 'ox-html)
(require 'ox-publish)

;; Locate repository from this script
(defvar my/site-root
  (file-name-directory
   (or load-file-name buffer-file-name)))

(defvar my/site-source
  (expand-file-name "website/" my/site-root))

(defvar my/site-output
  (expand-file-name "docs/" my/site-root))

(setq org-publish-project-alist
      `(("simulation-pages"
         :base-directory ,my/site-source
         :base-extension "org"
         :publishing-directory ,my/site-output
         :recursive t
         :publishing-function org-html-publish-to-html
         :with-toc t
         :section-numbers t
         :html-head-include-default-style nil)

        ("simulation-assets"
         :base-directory ,my/site-source
         :base-extension "css\\|js\\|png\\|jpg\\|svg\\|html\\|csv"
         :publishing-directory ,my/site-output
         :recursive t
         :publishing-function org-publish-attachment)

        ("simulation-website"
         :components
         ("simulation-pages" "simulation-assets"))))

;; Publish everything
;;#(org-publish "simulation-website" t)

;; Publish quietly
(let ((inhibit-message t)
      (message-log-max nil)
      (org-publish-list-skipped-files nil))
  (org-publish "simulation-website" nil))

