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

(customize-set-variable 'package-archives
                        '(("gnu"       . "https://elpa.gnu.org/packages/")
                          ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                          ;;("marmalade" . "https://marmalade-repo.org/packages/")
                          ("melpa"     . "https://melpa.org/packages/")
                          ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(require 'use-package)

(customize-set-variable 'use-package-always-ensure t)

(customize-set-variable 'use-package-always-defer t)

(customize-set-variable 'use-package-verbose t)

(customize-set-variable 'load-prefer-newer t)
(use-package auto-compile
  :defer nil
  :config (auto-compile-on-load-mode))

(add-to-list 'load-path "~/.emacs.d/src")

(use-package org
  :mode ("\\*.org$" . org-mode)
  :ensure org
  :defer t)

(setq starter-kit-dir (expand-file-name "~/.emacs.d/"))
(org-babel-load-file (expand-file-name "starter-kit.org" starter-kit-dir))

)
