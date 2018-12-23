;;; Some custom code for my emacs setting

;; (defun hrs/mark-done-and-archive ()
;;   "Mark the state of an org-mode item as DONE and archive it."
;;   (interactive)
;;   (org-todo 'done)
;;   (org-archive-subtree))

;; (define-key org-mode-map (kbd "C-c C-x C-s") 'hrs/mark-done-and-archive)



;; reveal.js for presentation
(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-mathjax t)

;; ------------------------------------------------------------------------
;;  iedit mode
;; ------------------------------------------------------------------------
;;(require-package 'iedit)

;; ------------------------------------------------------------------------
;;  prodigy
;; ------------------------------------------------------------------------
;; (require 'prodigy)
;; (prodigy-define-service
;;   :name "nikola"
;;   :command "nikola"
;;   :args "auto"
;;   :cwd "~/Dropbox/MyProject/Blog/KvinLiu.github.io"
;;   :tags '(blog nikola)
;;   :stop-signal 'sigint
;;   :kill-process-buffer-on-stop t)

;; (prodigy-define-service
;;   :name "hugo"
;;   :command "hugo"
;;   :args '("server" "-D" "--navigateToChanged")
;;   :cwd "~/Dropbox/MyProject/Blog/quickstart"
;;   :url "http://localhost:1313"
;;   :tags '(blog hugo)
;;   :stop-signal 'sigkill
;;   :kill-process-buffer-on-stop t)

;; ------------------------------------------------------------------------
;;  Hugo Blog Setting
;; ------------------------------------------------------------------------
;; Populates only the EXPORT_FILE_NAME property in the inserted headline.
;; (with-eval-after-load 'org-capture
;;   (defun org-hugo-new-subtree-post-capture-template ()
;;     "Returns `org-capture' template string for new Hugo post."
;;     (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
;;            (fname (org-hugo-slug title)))
;;       (mapconcat #'identity
;;                  `(
;;                    ,(concat "* TODO " title)
;;                    ":PROPERTIES:"
;;                    ,(concat ":EXPORT_HUGO_BUNDLE: " fname)
;;                    ":EXPORT_FILE_NAME: index"
;;                    ":END:"
;;                    "\	%?\n")              ;Place the cursor here finally
;;                  "\n")))
;;   (add-to-list 'org-capture-templates
;;                '("ph"                    ;`org-capture' binding + h
;;                  "Hugo post"
;;                  entry
;;                  ;; It is assumed that below file is present in `org-directory'
;;                  ;; and that it has a "Blog Ideas" heading. It can even be a
;;                  ;; symlink pointing to the actual location of all-posts.org!
;;                  (file+olp "~/Dropbox/Org_Files/Captures/Hugo_Blog/all_posts.org" "Blog Ideas")
;;                  (function org-hugo-new-subtree-post-capture-template))))

;; (with-eval-after-load 'ox
;;   (require 'ox-hugo))
;; ;; This is for live-preview of hugo-rendered posts
;; (require 'ox-hugo-auto-export)

;; ------------------------------------------------------------------------
;;  Easy hugo Settings
;; ------------------------------------------------------------------------
;; (setq easy-hugo-basedir "~/Dropbox/MyProject/Blog/quickstart/")
;; (setq easy-hugo-root "~/Dropbox/MyProject/Blog/quickstart/")

;; ------------------------------------------------------------------------
;;  Vue Settings
;; ------------------------------------------------------------------------
;; (require-package 'lsp-ui)
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; (require-package 'lsp-mode)
;; (require-package 'lsp-vue)
;; (add-hook 'vue-mode-hook #'lsp-vue-mmm-enable)
;; (add-hook 'vue-mode-hook 'flycheck-mode)
;; (add-hook 'major-mode-hook #'lsp-vue-enable)
;; (setq vetur.validation.template t)


(provide 'init-local)
