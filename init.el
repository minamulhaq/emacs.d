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
(org-babel-load-file (expand-file-name "~/.emacs.d/configurations/myinit.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/configurations/myEvil.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/configurations/myProgModes.org"))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist
                          '("el" . "src emacs-lisp"))
                          ;; '("el" "#+BEGIN_SRC emacs-lisp \n\n#+END_SRC"))















(setq-default tab-width 4)
(setq-default scroll-step 1)
(setq-default indent-tabs-mode nil)
(setq-default whitespace-style '(face lines-tail empty trailing))
(global-whitespace-mode 1)
(xterm-mouse-mode 1)
(setq make-backup-files nil)
(setq auto-save-default nil)







;; ;; Enable Pretty Mode. Converts lambda to actual symbols (Package CL is deprecated)
;; (use-package pretty-mode
;; 			 :ensure t
;; 			 :config
;; 			 (global-pretty-mode t))






(require 'helm-xref)
(setq xref-show-xrefs-function 'helm-xref-show-xrefs)

(require 'helm)
(require 'helm-config)
(require 'helm-grep)
(helm-projectile-on)

(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)













(use-package undo-tree
  :init
  :after evil
  
  (global-undo-tree-mode)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SET HYDRA FOR ZOOM IN ZOOM OUT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hydra)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TO DO ;; Configure projectile with Evil

(use-package projectile
  :ensure t
  ;;:delight '(:eval (concat " " (projectile-project-name)))
  :diminish
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)
  (define-key projectile-mode-map (kbd "s-p") #'projectile-find-file) ; counsel
  (define-key projectile-mode-map (kbd "s-F") #'projectile-ripgrep) ; counsel
  (setq projectile-sort-order 'recentf
        projectile-indexing-method 'hybrid
		;projectile-completion-system 'helm)
        projectile-completion-system 'ivy))




; (use-package company
;   :ensure t
;   :init
;   (global-company-mode)
;   :bind (("<backtab>" . company-complete-common-or-cycle))
;   :config
;   (setq company-dabbrev-other-buffers t
;         company-dabbrev-code-other-buffers t)
;   :hook ((text-mode . company-mode)
;          (prog-mode . company-mode)))







;; Recent Files

(use-package recentf
  :ensure t
  :config
    (progn
       (recentf-mode 1)
       (setq recentf-max-menu-items 25)
       (global-set-key "\C-x\ \C-r" 'recentf-open-files)))



;; Which Key

(use-package which-key
      :ensure t
      :config
      (which-key-mode))



(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))




(use-package org-bullets
  :ensure t
  :hook ((org-mode . org-bullets-mode)
         (org-mode . flyspell-mode)
         (org-mode . linum-mode)
         (org-mode . show-paren-mode))
  :config
  (progn

;;; add autocompletion
(defun org-easy-template--completion-table (str pred action)
  (pcase action
		 (`nil (try-completion  str org-structure-template-alist pred))
		 (`t   (all-completions str org-structure-template-alist pred))))

(defun org-easy-template--annotation-function (s)
  (format " -> %s" (cadr (assoc s org-structure-template-alist))))

(defun org-easy-template-completion-function ()
  (when (looking-back "^[ \t]*<\\([^ \t]*\\)" (point-at-bol))
	(list
	  (match-beginning 1) (point)
      'org-easy-template--completion-table
      :annotation-function 'org-easy-template--annotation-function
      :exclusive 'no)))

(defun add-easy-templates-to-capf ()
  (add-hook 'completion-at-point-functions
			'org-easy-template-completion-function nil t))

(add-hook 'org-mode-hook #'add-easy-templates-to-capf)
;; configure the calendar

(setq calendar-week-start-day 1)
(setq calendar-intermonth-text
	  '(propertize
		 (format "%2d"
				 (car
				   (calendar-iso-from-absolute
					 (calendar-absolute-from-gregorian (list month day year)))))
		 'font-lock-face 'font-lock-warning-face))


(setq calendar-intermonth-header
	  (propertize "Wk"                  ; or e.g. "KW" in Germany
				  'font-lock-face 'font-lock-keyword-face))))





;;Swiper / Ivy / Counsel

; Swiper gives us a really efficient incremental search with regular expressions and Ivy / Counsel 
; replace a lot of ido or helms completion functionality

; [[https://oremacs.com/swiper][reference documentation]]
; C-M-j (ivy-immediate-done) Exits with the current input instead of the current candidate 
; (like other commands). This is useful e.g. when you call find-file to create a new file, but 
; the desired name matches an existing file. In that case, using C-j would select that existing 
; file, which isn’t what you want - use this command instead.


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






; White Space
(require 'whitespace)

(setq whitespace-style '(face empty tabs tab-mark lines-tail trailing))

;;Visualize tabs as a pipe character - "|"
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-tab ((t (:foreground "#636363")))))
(setq whitespace-display-mappings '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'

(global-whitespace-mode t)



; I do not care about system buffers
; See only buffers that are associated to a file buffer-predicate decides which buffers you want 
; to see in the cycle for windows in that frame. The function buffer-file-name returns nil for 
; buffers that are not associated to files and a non-nil value (the filename) for those that are. 
; After doing so, C-x <left> and C-x <right> called from windows in that frame will only cycle 
; through buffers with associated files. In short it will Cycle through buffers whose name does 
; not start with an asterisk
(add-to-list 'default-frame-alist '(buffer-predicate . buffer-file-name))





;Move line up/down
(defun tedi:move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun tedi:move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(meta up)]   'tedi:move-line-up)
(global-set-key [(meta down)] 'tedi:move-line-down)



;Go to line
(global-set-key (kbd "M-g") 'goto-line)


;Kill buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)




(use-package auto-complete
			 :ensure t
			 :init
			 (progn
			   (ac-config-default)
			   (add-hook 'makefile-gmake-mode-hook 'auto-complete-mode)
))





; (use-package aggressive-indent
;   :ensure t
;   :init
;   (setq aggressive-indent-comments-too t)
;   :config
;   (global-aggressive-indent-mode 1))





;Yasnippets

; (use-package yasnippet
;    :ensure t
;    :hook (prog-mode . yas-minor-mode)
;    :hook (org-mode . yas-minor-mode)
;    :config
;  (use-package yasnippet-snippets
;        :ensure t
;        :pin melpa)
;    :init
;    (yas-reload-all))


;Mark-down mode and enable auto-correction

(use-package markdown-mode
  :ensure  t
  :defer   t
  :mode    ("\\.\\(markdown\\|mdown\\|md\\)$" . markdown-mode)
  :hook  ((markdown-mode . visual-line-mode)
          (markdown-mode . writegood-mode)
          (markdown-mode . flyspell-mode))
  :config
  (progn
    (setq markdown-command "pandoc --smart -f markdown -t html")
  )
)






(defun toggle-term ()
  "Toggles between terminal and current buffer (creates terminal, if none exists)"
  (interactive)
  (if (string= (buffer-name) "*ansi-term*")
      (switch-to-buffer (other-buffer (current-buffer)))
    (if (get-buffer "*ansi-term*")
        (switch-to-buffer "*ansi-term*")
      (progn
        (ansi-term (getenv "SHELL"))
        (setq show-trailing-whitespace nil)))))
(global-set-key (kbd "<f12>") 'toggle-term)







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GDB ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
  ;; use gdb-many-windows by default
  gdb-many-windows t
  ;; ?
  gdb-use-separate-io-buffer t
  ;; Non-nil means display source file containing the main routine at startup
  gdb-show-main t
 )

;; Toggle window dedication
(defun tedi:toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;; Sets up the windows to make the command window dedicated
(advice-add 'gdb-setup-windows :after
            (lambda () (set-window-dedicated-p (selected-window) t)))

;; Prevent gdb from popping i/o window to the foreground on every output op
(setq-default gdb-display-io-nopopup t)


(defconst gud-window-register 123456)

(defun gud-quit ()
  (interactive)
  (gud-basic-call "quit"))

(add-hook 'gud-mode-hook
          (lambda ()
            (gud-tooltip-mode)
            (window-configuration-to-register gud-window-register)
            (local-set-key (kbd "C-c q") 'gud-quit)))

(advice-add 'gud-sentinel :after
            (lambda (proc msg)
              (when (memq (process-status proc) '(signal exit))
                (jump-to-register gud-window-register)
                (bury-buffer))))









