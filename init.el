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



(electric-pair-mode t)
(setq electric-pair-preserve-balance nil)

; Load Theme GruvBox
(load-theme 'gruvbox t)


; Clear Startup Screen Messages

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)


; User Name
(setq user-full-name "Muhammad Inam Ul Haq")



;; esc cancels everything.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


  ;; Cosmetics
(tool-bar-mode -1)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)


(setq-default
 display-line-numbers-type 'relative
 display-line-numbers-current-absolute t
 display-line-numbers-widen t)
(global-display-line-numbers-mode t)
(column-number-mode 1)

; Disable line numbers for some modes 
(dolist (mode '(org-mode-hook
				 term-mode-hook
				 shell-mode-hook
				 eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))


(show-paren-mode 1)

(setq-default tab-width 4)
(setq-default scroll-step 1)
(setq-default indent-tabs-mode nil)
(setq-default whitespace-style '(face lines-tail empty trailing))
(global-whitespace-mode 1)
(xterm-mouse-mode 1)
(setq make-backup-files nil)
(setq auto-save-default nil)


(use-package rainbow-delimeters
 			 :hook ( prog-mode . rainbow-delimiters-mode))

(setq ring-bell-function 'ignore)


;; Set Encodings

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;; ;; Enable Pretty Mode. Converts lambda to actual symbols (Package CL is deprecated)
;; (use-package pretty-mode
;; 			 :ensure t
;; 			 :config
;; 			 (global-pretty-mode t))


;; (fset 'yes-or-no-p 'y-or-n-p)
;; (set-variable 'confirm-kill-emacs 'yes-or-no-p)
(global-set-key (kbd "<f5>") 'revert-buffer)


; (use-package eterm-256color
; 			 :hook (term-mode . eterm-256color-mode))

(require 'elisp-slime-nav)
(defun my-lisp-hook ()
  (elisp-slime-nav-mode)
  (turn-on-eldoc-mode)
    )
(add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)





;; Doom Mode Line
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))


(use-package mode-icons
  :ensure t
  :init (mode-icons-mode)
  :config
  (progn
    (setq doom-modeline-height 10)
    (setq doom-modeline-project-detection 'projectile)
    (setq doom-modeline-buffer-file-name-style 'file-name)
    (setq doom-modeline-icon (display-graphic-p))
    (setq doom-modeline-major-mode-icon t)
    (setq doom-modeline-major-mode-color-icon t)
    (setq doom-modeline-buffer-state-icon t)
    (setq doom-modeline-buffer-modification-icon t)
    (setq doom-modeline-indent-info nil)
    (setq doom-modeline-modal-icon 'evil)
    (setq doom-modeline-env-version t)
    )
)

; (use-package nlinum-hl
; 			 :ensure t)
; (require 'linum)                        ;For its face

;  (use-package nlinum-relative
; 			  :ensure t
;               :config
;  			  (nlinum-relative-setup-evil)
;               (add-hook 'prog-mode-hook 'nlinum-relative-mode)
;               (setq nlinu-widen 30)
;               (setq nlinum-relative-redisplay-delay 0)      ;; delay
;               (setq nlinum-relative-current-symbol "")      ;; or "" for display current line number
;               (setq nlinum-relative-offset 0))                 ;; 1 if you want 0, 2, 3...')

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer dw/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer dw/ctrl-c-keys
    :prefix "C-c")
  )

(dw/leader-key-def 
  "z" '(hydra-text-scale/body :which-key "scale-text")
  "s" '(shell)
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Download Evil
(unless (package-installed-p 'evil)
    (package-install 'evil))


(use-package evil-leader
      :ensure t
      :commands (evil-leader-mode)
      :init
      (setq evil-want-keybinding nil)
      (global-evil-leader-mode)
      :config
      (progn
		(evil-leader/set-leader ",")
		(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
		(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
		(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
		(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

		(evil-leader/set-key
          "s" 'save-buffer
          "k" 'kill-buffer-and-window
          "h" 'dired-jump
          "v" 'split-window-right
          "f" 'find-file
          "e" 'pp-eval-last-sexp
          "," 'other-window
          "b" 'ibuffer
          "x" 'helm-M-x
          "p" 'helm-projectile
          "g" 'magit-status
        )
        )
      )

(defun inam/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
  (add-to-list 'evil-emacs-state-modes mode)))


; Enable Evil
(use-package evil
  :ensure t
  :after evil-leader
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-move-beyond-eol t)
  (setq evil-cross-lines t)
  (setq evil-undo-system 'undo-tree)
  (setq evil-respect-visual-line-mode t)
  (global-undo-tree-mode)
  (evil-mode 1)
  :config
  (progn
    (add-hook 'evil-mode-hook 'inam/evil-hook)
    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal)
    (when evil-want-C-u-scroll
      (define-key evil-insert-state-map (kbd "C-u") 'evil-scroll-up)
      (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
      (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
      (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up))
    )
  )


	  


(use-package evil-collection
			 :after evil
			 :config
			 (evil-collection-init))



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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; C  --  C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package lsp-mode
			 :commands (lsp lsp-deffered)
			 :init
			 (setq lsp-keymap-prefix "C-c l")
			 :config 
			 (lsp-enable-which-key-integration t)
			 :hook (sh-mode . lsp))

(setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp
                                               projectile hydra flycheck company avy which-key helm-xref dap-mode))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
    (package-refresh-contents)
      (mapc #'package-install package-selected-packages))

;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
(helm-mode)
(require 'helm-xref)
(setq xref-show-xrefs-function 'helm-xref-show-xrefs)

(require 'helm)
(require 'helm-config)
(require 'helm-grep)
(helm-projectile-on)

(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
                      (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
                      (require 'dap-cpptools)
                      (yas-global-mode))


(use-package flycheck
             :ensure t
             :init (global-flycheck-mode))


(setq sp-ui-sideline-show-diagnostics 0)
;(setq lsp-ui-sideline-show-hover 1)
(setq lsp-ui-sideline-show-code-actions 1)
(setq lsp-ui-sideline-update-mode 1)
(setq lsp-ui-sideline-delay 0)

(setq lsp-ui-peek-enable 1)
(setq sp-ui-peek-jump-backward 1)
(setq lsp-ui-peek-jump-forward 1)


(setq c-basic-offset 4)
(setq c-default-style "linux")
(setq-default indent-tabs-mode nil)


;; Tell cc-mode not to check for old-style (K&R) function declarations.
;; This speeds up indenting a lot.
(setq c-recognize-knr-p nil)

(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))


;; Change the indentation amount to 4 spaces instead of 2.
;; You have to do it in this complicated way because of the
;; strange way the cc-mode initializes the value of `c-basic-offset'.
(add-hook 'c-mode-hook (lambda () (setq c-basic-offset 4)))
(add-hook 'c++-mode-hook (lambda () (setq c-basic-offset 4)))

;(add-hook 'c++-mode-hook (lambda () (highlight-lines-matching-regexp ".\{91\}" "hi-green-b")))


(add-hook 'c-mode-hook (lambda () (show-paren-mode 1)))
(add-hook 'cc-mode-hook (lambda () (show-paren-mode 1)))
(add-hook 'c++-mode-hook (lambda () (show-paren-mode 1)))

(add-hook 'c-mode-hook 'projectile-mode)
(add-hook 'cc-mode-hook 'projectile-mode)
(add-hook 'c++-mode-hook 'projectile-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PYTHON 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i"))


(use-package lsp-jedi
  :ensure t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;











;Yasnippets

(use-package yasnippet
   :ensure t
   :hook (prog-mode . yas-minor-mode)
   :hook (org-mode . yas-minor-mode)
   :config
   (use-package yasnippet-snippets
       :ensure t
       :pin melpa)
   :init
   (yas-reload-all))


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


