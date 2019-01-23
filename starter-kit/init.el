;;; init.el --- Where all the magic begins
;;
;; Part of the LigonLab Starter Kit, derived from
;; http://eschulte.github.io/emacs24-starter-kit/
;;
;; This is the first thing to get loaded.
;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(setq package-enable-at-startup nil)

(server-start)

;; 24.1 needs this for compatibility?
;(require 'org-compat)

;; Install (if necessary) & initialize el-get
(add-to-list 'load-path (expand-file-name "~/.emacs.d/el-get/el-get"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/el-get/org-plus-contrib"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; Directory may not exist, in which case create it.
(let ((dir (file-name-directory (expand-file-name "~/.emacs.d/el-get/el-get-user/"))))
       (unless (file-directory-p dir)
         (make-directory dir)))

(let ((dir (file-name-directory (expand-file-name "~/.emacs.d/el-get/el-get-user/recipes/"))))
       (unless (file-directory-p dir)
         (make-directory dir))
       (add-to-list 'el-get-recipe-path dir))

(require 'el-get-elpa)
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

(require 'org)

(load-file "~/.emacs.d/el-get/org-plus-contrib/org-element.el")
(load-file "~/.emacs.d/el-get/org-plus-contrib/org.el")

;(el-get 'sync) ; Premature?


;; load Org-mode from source when the ORG_HOME environment variable is set
;(when (getenv "ORG_HOME")
;  (let ((org-lisp-dir (getenv "ORG_HOME")))
;    (when (file-directory-p org-lisp-dir)
;      (add-to-list 'load-path org-lisp-dir)
;      (require 'org))))

(org-reload)

;;load the starter kit from the `after-init-hook' so all packages are loaded
(add-hook 'after-init-hook
 `(lambda ()
    ;; remember this directory
    (setq starter-kit-dir (file-name-directory (or load-file-name (buffer-file-name))))
    ;; only load org-mode later if we didn't load it just now
    ,(unless (and (getenv "ORG_HOME")
                  (file-directory-p (getenv "ORG_HOME")))
       '(require 'org))
    ;; load up the starter kit
    (org-babel-load-file (expand-file-name "starter-kit.org" starter-kit-dir))))

;;; init.el ends here
