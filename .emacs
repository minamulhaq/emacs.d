(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
						 ("melpa-stable" . "https://stable.melpa.org/packages/")
						 ("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(require 'use-package)




(load-theme 'gruvbox t)
(set-default-coding-systems 'utf-8)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)

;(set-frame-parameter (selected-frame) 'alpha '(85 . 80))
;(add-to-list 'default-frame-alist '(alpha . (85 . 50)))

(add-to-list 'load-path "~/.emacs.d/modules/")
(show-paren-mode 1)
(setq-default scroll-step 1)
(setq-default indent-tabs-mode nil)
(line-number-mode 1)
(column-number-mode 1)
(setq-default whitespace-style '(face lines-tail empty trailing))
(global-whitespace-mode 1)
(xterm-mouse-mode 1)
(setq make-backup-files nil)
(setq auto-save-default nil)
(show-paren-mode 1)
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)
(setq electric-pair-pairs
      '(
        (?\" . ?\")
        (?\{ . ?\})))






(require 'linum) 
(use-package nlinum-relative
             :config
             ;; something else you want
             (add-hook 'prog-mode-hook 'nlinum-relative-mode)
			 (setq nlinu-widen 20)
			 (setq nlinum-relative-redisplay-delay 0)      ;; delay
			 (setq nlinum-relative-current-symbol "")      ;; or "" for display current line number
			 (setq nlinum-relative-offset 0))                 ;; 1 if you want 0, 2, 3...')





(use-package lsp-mode
			 :commands (lsp lsp-deffered)
			 :init
			 (setq lsp-keymap-prefix "C-c l")
			 :config 
			 (lsp-enable-which-key-integration t))

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
(setq projectile-completion-system 'helm)
(helm-projectile-on)


(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(which-key-mode)
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





(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)



(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  (setq doom-modeline-height 15)
  (setq doome-modeline-width 6)
  (setq doom-modeline-window-width-limit fill-column)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-lsp t)
  (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
  (setq doom-modeline-major-mode-icon 't))




(use-package evil-leader
      :commands (evil-leader-mode)
      :ensure evil-leader
      :demand evil-leader
      :init
      (global-evil-leader-mode)
      :config
      (progn
		(evil-leader/set-leader ",")))        ;; bindings from earlier

;; Download Evil
(unless (package-installed-p 'evil)
    (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)


(evil-leader/set-leader ",")
(evil-leader/set-key "w" 'save-buffer)
(evil-leader/set-key "q" 'kill-buffer-and-window)

(evil-leader/set-key "h" 'dired-jump)
(evil-leader/set-key "v" 'split-window-right)
(evil-leader/set-key "e" 'pp-eval-last-sexp)
(evil-leader/set-key "," 'other-window)
(evil-leader/set-key "b" 'ibuffer)
(evil-leader/set-key "x" 'helm-M-x)





(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))





(use-package avy
  :ensure t
  :bind
    ("M-s" . avy-goto-char))




(use-package magit
  :ensure magit
  :config
  (progn
    (evil-set-initial-state 'magit-mode 'normal)
    (evil-set-initial-state 'magit-status-mode 'normal)
    (evil-set-initial-state 'magit-diff-mode 'normal)
    (evil-set-initial-state 'magit-log-mode 'normal)
    (evil-define-key 'normal magit-mode-map
        "j" 'magit-goto-next-section
        "k" 'magit-goto-previous-section)
    (evil-define-key 'normal magit-log-mode-map
        "j" 'magit-goto-next-section
        "k" 'magit-goto-previous-section)
    (evil-define-key 'normal magit-diff-mode-map
        "j" 'magit-goto-next-section
        "k" 'magit-goto-previous-section)))



(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))



(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)




(use-package eterm-256color
			 :hook (term-mode . eterm-256color-mode))




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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-smartparens smartparens lsp-mode yasnippet lsp-treemacs helm-lsp projectile hydra flycheck company avy which-key helm-xref dap-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
