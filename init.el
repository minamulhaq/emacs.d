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





  




(setq-default tab-width 4)
(setq-default scroll-step 1)
(setq-default indent-tabs-mode nil)
(setq-default whitespace-style '(face lines-tail empty trailing))
(global-whitespace-mode 1)
(xterm-mouse-mode 1)
(setq make-backup-files nil)
(setq auto-save-default nil)






;; Ivy Counsel Swiper
  
;; Swiper gives us a really efficient incremental search with regular expressions and Ivy / Counsel 
;; replace a lot of ido or helms completion functionality

;; [[https://oremacs.com/swiper][reference documentation]]
;; C-M-j (ivy-immediate-done) Exits with the current input instead of the current candidate 
;; (like other commands). This is useful e.g. when you call find-file to create a new file, but 
;; the desired name matches an existing file. In that case, using C-j would select that existing 
;; file, which isn’t what you want - use this command instead.

(use-package flx
  :ensure t)

(use-package counsel
  :ensure t
  :pin melpa
  :diminish
  :hook (ivy-mode . counsel-mode)
  :config
  (global-set-key (kbd "s-P") #'counsel-M-x)
  (global-set-key (kbd "s-f") #'counsel-grep-or-swiper)
  (setq counsel-rg-base-command "rg --vimgrep %s"))

(use-package counsel-projectile
  :ensure t
  :pin melpa
  :config (counsel-projectile-mode +1)
  :bind (("C-c p SPC" . counsel-projectile))
  )

(use-package ivy
  :ensure t
  :pin melpa
  :diminish
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-display-style nil)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<escape>") #'minibuffer-keyboard-quit)
  (setq ivy-use-selectable-prompt t)   ;; make prompt line selectagle
  (setq ivy-re-builders-alist
        '((counsel-rg . ivy--regex-plus)
          (counsel-projectile-rg . ivy--regex-plus)
          (counsel-ag . ivy--regex-plus)
          (counsel-projectile-ag . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil))

(use-package swiper
  :ensure t
  :after ivy
  :diminish
;;  :custom-face (swiper-line-face ((t (:foreground "#ffffff" :background "#60648E"))))
  :config
  (setq swiper-action-recenter t)
  (setq swiper-goto-start-of-match t))





  (use-package ivy-posframe
  :ensure t
  :pin melpa
  :after ivy
  :config
  (setq ivy-posframe-display-functions-alist
      '((swiper          . ivy-posframe-display-at-point)
        (complete-symbol . ivy-posframe-display-at-point)
        (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
        (t               . ivy-posframe-display))
        ivy-posframe-height-alist '((t . 20))
        ivy-posframe-parameters '((internal-border-width . 5)))
  (setq ivy-posframe-width 120)
  (ivy-posframe-mode +1))


(use-package ivy-rich
  :ensure t
  :after (ivy ivy-postframe)
  :pin melpa
  :preface
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
       (all-the-icons-icon-for-mode major-mode)))
   :init
  (setq ivy-rich-display-transformers-list ; max column width sum = (ivy-poframe-width - 1)
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-switch-buffer-icon (:width 2))
            (ivy-rich-candidate (:width 35))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-major-mode (:width 13 :face warning)))
           :predicate
           #'(lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 35))
            (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 35))
            (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 35))
            (ivy-rich-counsel-variable-docstring (:width 34 :face font-lock-doc-face))))
          package-install
          (:columns
           ((ivy-rich-candidate (:width 25))
            (ivy-rich-package-version (:width 12 :face font-lock-comment-face))
            (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
            (ivy-rich-package-install-summary (:width 23 :face font-lock-doc-face))))))
  :config
  (ivy-rich-mode +1)
  ;(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  )

  





(use-package wgrep
  :ensure t
  :config
  (setq wgrep-enable-key (kbd "C-c C-w")) ; change to wgrep mode
  (setq wgrep-auto-save-buffer t))

(use-package prescient
  :ensure t
  :config
  (setq prescient-filter-method '(literal regexp initialism fuzzy))
  (prescient-persist-mode +1))

(use-package ivy-prescient
  :ensure t
  :after (prescient ivy)
  :config
  (setq ivy-prescient-sort-commands
        '(:not swiper counsel-grep ivy-switch-buffer))
  (setq ivy-prescient-retain-classic-highlighting t)
  (ivy-prescient-mode +1))

(use-package company-prescient
  :ensure t
  :after (prescient company)
  :config (company-prescient-mode +1))




; Configs from Manuel 

(use-package counsel
:ensure t
)

(use-package ivy
:ensure t
:diminish (ivy-mode)
:bind (("C-x b" . ivy-switch-buffer))
:config
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-display-style 'fancy))
(define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word)

(use-package swiper
:ensure try
:bind (("C-s" . swiper)
	 ("C-r" . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file))
:config
(progn
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  ))




; Avy - navigate by searching for a letter on the screen and jumping to it
; See https://github.com/abo-abo/avy for more info


(use-package avy
:ensure t
:bind ("M-s" . avy-goto-word-1)) ;; changed from char as per jcs




;PDF tools
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(use-package org-pdftools
  :ensure t)



;Magit
(use-package transient
  :ensure t)


(use-package magit
  :ensure t
  :commands magit-status
  :bind (("C-x g" . magit-status))
  :config
  (use-package git-commit
    :ensure t)
)



; Basic projectile Settings


(projectile-global-mode)

;; helm autocompletion mode and integration with projectile
(use-package helm-projectile
  :ensure t
  :after helm
  :defer t
  :config
  (progn
     (setq projectile-completion-system 'helm)
     (helm-projectile-on)
     (setq projectile-switch-project-action 'helm-projectile)
     (setq projectile-enable-idle-timer t)
     (setq projectile-globally-unignored-files (quote ("*.o" "*.pyc" "*~")))
     (setq projectile-tags-backend (quote find-tag))
     (setq projectile-enable-caching t)))



; Treemacs

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-follow-after-init          t
          treemacs-width                      35
          treemacs-indentation                2
          treemacs-git-integration            t
          treemacs-collapse-dirs              3
          treemacs-silent-refresh             nil
          treemacs-change-root-without-asking nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-show-hidden-files          t
          treemacs-never-persist              nil
          treemacs-is-never-other-window      nil
          treemacs-goto-tag-strategy          'refetch-index)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t))
  :bind
  (:map global-map
        ("C-c 0"        . treemacs-toggle)
        ;([f8]       . treemacs-toggle)
        ("M-0"       . treemacs-select-window)
        ("C-c 1"     . treemacs-delete-other-windows)
        ("M-n ft"    . treemacs-toggle)
        ("M-n fT"    . treemacs)
        ("M-n f C-t" . treemacs-find-file)))
(use-package treemacs-projectile
  :after treemacs
  :defer t
  :ensure t
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header)
  :bind (:map global-map
              ("M-n fP" . treemacs-projectile)
              ("M-n fp" . treemacs-projectile-toggle)))

(use-package treemacs-magit
  :after treemacs magit
  :defer t
  :ensure t)









