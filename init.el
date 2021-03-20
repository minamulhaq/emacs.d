(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))



(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 2048 2048 2048))


(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold gc-cons-threshold-original)
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'gc-cons-threshold-original)
   (makunbound 'file-name-handler-alist-original)
      (message "gc-cons-threshold and file-name-handler-alist restored")))







(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
						 ("melpa-stable" . "https://stable.melpa.org/packages/")
						 ("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize) ;Not required in emacs 27

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
    (auto-package-update-maybe))

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/themes/")
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))


(use-package org                        
       :ensure t)
(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
