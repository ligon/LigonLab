;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(let ((file-name-handler-alist nil))  ; this is open, and closed only in epilogue

(setq gc-cons-threshold most-positive-fixnum)

(let ((custom-file "~/.emacs.d/custom.el"))
  (when (file-exists-p custom-file)
    (load-file custom-file))
)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(require 'use-package)
;; Configure use-package to use straight.el by default
(use-package straight
           :custom (straight-use-package-by-default t))

(customize-set-variable 'use-package-verbose t)

(customize-set-variable 'use-package-always-defer t)

(customize-set-variable 'load-prefer-newer t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

;(add-to-list 'load-path "~/.emacs.d/src")

(use-package org
             :demand t
             :load-path "~/.emacs.d/straight/build/org/"
             :mode ("\\*.org$" . org-mode)
             :requires ox-extra)
(straight-use-package 'f)

(setq starter-kit-dir (expand-file-name "~/.emacs.d/"))
(org-babel-load-file (expand-file-name "starter-kit.org" starter-kit-dir))

)

(setq gc-cons-threshold (* 2 1000 1000))
