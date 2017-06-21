;;; init.el --- Joe's personal emacs setup

;; Update our load path to include any custom modules
(add-to-list 'load-path (directory-file-name "~/.emacs.d/elisp"))

(require 'face-remap)

;; Define any local functions here
(defun my-org-buffer-mode-face-custom ()
  "Set org mode to a font without ligatures."
  (interactive)
  (setq buffer-face-mode-face '(:family "Helvetica Neue" :height 150))
  (buffer-face-mode))

;; Setup theme first to avoid annoying window flashes on load
(add-to-list 'custom-theme-load-path "~/.emacs.d/noctilux-theme")
(load-theme 'noctilux t)

;; I dig the Fira Code font
(set-frame-font "Fira Code:style=Retina" nil t)

;; Also enable ligatures on mac
(if (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode))

;;; Now tune up emacs:
;; Set the default directory to home
(setq default-directory "~/")

;; Gimme 20mb of GC (number chosen out of thin air)
(setq gc-cons-threshold 20000000)

;; Keep backups and autosaves from cluttering up the filesystem
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Delete all trailing whitespace on save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Always follow symlinks already
(setq vc-follow-symlinks t)

;; Save files starting with #! as executable
(add-hook 'after-save-hook
	  #'executable-make-buffer-file-executable-if-script-p)

;; Get that typewriter out of here
(setq sentence-end-double-space nil)

;; I think this is always enabled, but if not let's turn it on
(transient-mark-mode t)

;; Files should end in new line
(setq require-final-newline t)

;; Ask before closing emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; On that note, use y-or-n-p everywhere
(fset 'yes-or-no-p 'y-or-n-p)

;; Keep buffers up to date
(global-auto-revert-mode t)

;; NO BELL UGH
(setq visible-bell t)

(toggle-frame-maximized)

;; Default line length is 80
(setq-default fill-column 80)

;; Human-readable file sizes
(setq-default dired-listing-switches "-alh")

;; Set up mac rebindings as advised by http://david.rothlis.net/emacs/osx.html
(setq mac-command-modifier 'meta)

;; Force tab to indent, then run complete commands if already indented
(setq tab-always-indent 'complete)

;; Not a fan of the scrollbars or the toolbar
(scroll-bar-mode -1)
(tool-bar-mode -1)

;;; LOCAL PACKAGES
(require 'rwd-bell)

;;; NON-LOCAL PACKAGES
;; Set up package initialization
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Ensure that use-package is installed (if it's not, refresh and install)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
 (require 'use-package))
(require 'bind-key)

;; Ensure that all packages mentioned in use-package are installed
(setq use-package-always-ensure t)
(setq use-package-verbose t)

;; Any generic shortcuts go here
(use-package bind-key
  :config
  (bind-key "M-`" #'other-frame)
  (bind-key "M-o" #'other-window)
  (bind-key "C-x C-m" #'execute-extended-command)
  (bind-key "C-c C-m" #'execute-extended-command)
  (bind-key "C-w" #'backward-kill-word)
  (bind-key "C-x C-w" #'kill-region)
  (bind-key "C-c C-w" #'kill-region))

(use-package paren
  :config
  (show-paren-mode t)
  (setq show-paren-delay 0.0))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package yaml-mode
  :mode "\\.yml")

(use-package enh-ruby-mode
  :mode "\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
  :interpreter ("ruby" "ruby1.9" "ruby1.8" "jruby" "rbx")
  :init
  (add-hook 'enh-ruby-mode-hook #'eldoc-mode)
  :config
  ;; (setq enh-ruby-deep-indent-paren nil)
  ) ; Turns off strange indenting on hashes

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "multimarkdown"))

(use-package inf-ruby
  :init
  (add-hook 'enh-ruby-mode-hook #'inf-ruby-minor-mode)
  (add-hook 'compilation-filter-hook #'inf-ruby-auto-enter))

(use-package yard-mode
  :init
  (add-hook 'enh-ruby-mode-hook #'yard-mode))

(use-package nyan-mode
  :config
  (setq nyan-animate-nyancat t)
  (setq nyan-wavy-trail t)
  (nyan-mode))

(use-package company
  :config
  (setq company-tooltip-limit 20)                      ; bigger popup window
  (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                          ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (global-company-mode)
  (bind-key "C-w" 'backward-kill-word company-active-map)
  (bind-key "<tab>"
	    (lambda ()
	      (interactive)
	      (if (looking-at "\\_>")
		  (company-complete-common)
		(indent-according-to-mode)))
	    company-active-map))

(use-package company-inf-ruby)
(use-package company-go
  :init
  (eval-after-load 'company
    '(push 'company-go company-backends)))

(use-package smartparens
  :init
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'enh-ruby-mode-hook #'smartparens-mode)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings))

(use-package ag
  :config
  (setq ag-highlight-search t))

(use-package ace-window
  :bind (("M-p" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package org
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :init
  (add-hook 'org-mode-hook #'my-org-buffer-mode-face-custom)
  :config
  (setq org-special-ctrl-a/e t)
  (setq org-todo-keywords
	'((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)")))
  (setq org-log-done 'time)
  (setq org-agenda-files '("~/org/todo.org")))

(use-package projectile
  :config
  (projectile-mode))

(use-package projectile-rails
  :config
  (projectile-rails-global-mode))

(use-package whole-line-or-region)

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit))

(use-package robe
  :init
  (add-hook 'enh-ruby-mode-hook 'robe-mode)
  (eval-after-load 'company
    '(push 'company-robe company-backends)))

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package minitest
  :init
  (add-hook 'enh-ruby-mode-hook 'minitest-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook #'subword-mode)
  :config
  (put-clojure-indent 's/fdef 1))

(use-package clj-refactor
  :init
  (add-hook 'clojure-mode-hook (lambda ()
				 (clj-refactor-mode 1)
				 (yas-minor-mode 1)
				 (cljr-add-keybindings-with-prefix "C-c C-m")))) ;TODO: use bind-key here

(use-package clojure-cheatsheet)

(use-package rainbow-delimiters
  :init
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(use-package aggressive-indent
  :init
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(use-package cider
  :init
  (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
  :config
  (setq cider-prompt-for-symbol nil))

(use-package which-key
  :config
  (which-key-mode))

(use-package flx-ido
  :config
  (flx-ido-mode t)
  (setq ido-use-faces nil))

(use-package ido-completing-read+)

(use-package magit
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch-popup))
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status)
  (setq magit-completing-read-function 'magit-ido-completing-read))

(use-package avy
  :bind* (("C-:" . avy-goto-word-or-subword-1)))

(use-package ido
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-use-virtual-buffers t))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package recentf
  :config
  (recentf-mode t)
  (setq recentf-max-saved-items 200))

(use-package winner
  :config
  (winner-mode 1))

(use-package goto-last-change
  :bind (("C-'" . goto-last-change)))

(use-package rbenv
  :config
  (global-rbenv-mode))

(use-package go-mode
  :init
  (add-hook 'go-mode-hook (lambda ()
			    (setq tab-width 4)))
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-eldoc
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(background-color "#202020")
 '(background-mode dark)
 '(cursor-color "#cccccc")
 '(custom-safe-themes
   (quote
    ("0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "14f0fbf6f7851bfa60bf1f30347003e2348bf7a1005570fd758133c87dafe08f" "4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" default)))
 '(foreground-color "#cccccc")
 '(package-selected-packages
   (quote
    (go-eldoc go-mode buffer-move projectile-rails web-mode rbenv evil-search-highlight-persist default-text-scale evil-visual-mark-mode fill-column-indicator sos ido-completing-read+ zenburn-theme ido-vertical-mode markdown-mode yaml-mode clj-refactor ace-window goto-last-change emacs-sensible use-package-chords use-package avy enh-ruby-mode company-inf-ruby yasnippet minitest ruby-compilation ruby-test-mode robe inf-ruby flycheck whole-line-or-region org ag exec-path-from-shell yard-mode which-key smartparens rainbow-delimiters projectile paredit nyan-mode magit flx-ido company clojure-cheatsheet aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-todo ((t (:background "#202020" :foreground "#ccaaff" :inverse-video nil :underline nil :slant normal :weight bold)))))
