;;; init.el --- Joe's personal emacs setup

;; Update our load path to include any custom modules
(add-to-list 'load-path (directory-file-name "~/.emacs.d/elisp"))

(require 'face-remap)

;; Global settings (defaults)
					;
;; Define any local functions here
(defun my-org-buffer-mode-face-custom ()
  "Set org mode to a font without ligatures."
  (interactive)
  (setq buffer-face-mode-face '(:family "Menlo"))
  (buffer-face-mode))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
	(file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
	(if (neo-global--window-exists-p)
	    (progn
	      (neotree-dir project-dir)
	      (neotree-find file-name)))
      (message "Could not find git project root."))))

(defconst fira-code-font-lock-keywords-alist
  (mapcar (lambda (regex-char-pair)
            `(,(car regex-char-pair)
              (0 (prog1 ()
                   (compose-region (match-beginning 1)
                                   (match-end 1)
                                   ;; The first argument to concat is a string containing a literal tab
                                   ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))))))
          '(("\\(www\\)"                   #Xe100)
            ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
            ("\\(\\*\\*\\*\\)"             #Xe102)
            ("\\(\\*\\*/\\)"               #Xe103)
            ("\\(\\*>\\)"                  #Xe104)
            ("[^*]\\(\\*/\\)"              #Xe105)
            ("\\(\\\\\\\\\\)"              #Xe106)
            ("\\(\\\\\\\\\\\\\\)"          #Xe107)
            ("\\({-\\)"                    #Xe108)
            ("\\(\\[\\]\\)"                #Xe109)
            ("\\(::\\)"                    #Xe10a)
            ("\\(:::\\)"                   #Xe10b)
            ("[^=]\\(:=\\)"                #Xe10c)
            ("\\(!!\\)"                    #Xe10d)
            ("\\(!=\\)"                    #Xe10e)
            ("\\(!==\\)"                   #Xe10f)
            ("\\(-}\\)"                    #Xe110)
            ("\\(--\\)"                    #Xe111)
            ("\\(---\\)"                   #Xe112)
            ("\\(-->\\)"                   #Xe113)
            ("[^-]\\(->\\)"                #Xe114)
            ("\\(->>\\)"                   #Xe115)
            ("\\(-<\\)"                    #Xe116)
            ("\\(-<<\\)"                   #Xe117)
            ("\\(-~\\)"                    #Xe118)
            ("\\(#{\\)"                    #Xe119)
            ("\\(#\\[\\)"                  #Xe11a)
            ("\\(##\\)"                    #Xe11b)
            ("\\(###\\)"                   #Xe11c)
            ("\\(####\\)"                  #Xe11d)
            ("\\(#(\\)"                    #Xe11e)
            ("\\(#\\?\\)"                  #Xe11f)
            ("\\(#_\\)"                    #Xe120)
            ("\\(#_(\\)"                   #Xe121)
            ("\\(\\.-\\)"                  #Xe122)
            ("\\(\\.=\\)"                  #Xe123)
            ("\\(\\.\\.\\)"                #Xe124)
            ("\\(\\.\\.<\\)"               #Xe125)
            ("\\(\\.\\.\\.\\)"             #Xe126)
            ("\\(\\?=\\)"                  #Xe127)
            ("\\(\\?\\?\\)"                #Xe128)
            ("\\(;;\\)"                    #Xe129)
            ("\\(/\\*\\)"                  #Xe12a)
            ("\\(/\\*\\*\\)"               #Xe12b)
            ("\\(/=\\)"                    #Xe12c)
            ("\\(/==\\)"                   #Xe12d)
            ("\\(/>\\)"                    #Xe12e)
            ("\\(//\\)"                    #Xe12f)
            ("\\(///\\)"                   #Xe130)
            ("\\(&&\\)"                    #Xe131)
            ("\\(||\\)"                    #Xe132)
            ("\\(||=\\)"                   #Xe133)
            ("[^|]\\(|=\\)"                #Xe134)
            ("\\(|>\\)"                    #Xe135)
            ("\\(\\^=\\)"                  #Xe136)
            ("\\(\\$>\\)"                  #Xe137)
            ("\\(\\+\\+\\)"                #Xe138)
            ("\\(\\+\\+\\+\\)"             #Xe139)
            ("\\(\\+>\\)"                  #Xe13a)
            ("\\(=:=\\)"                   #Xe13b)
            ("[^!/]\\(==\\)[^>]"           #Xe13c)
            ("\\(===\\)"                   #Xe13d)
            ("\\(==>\\)"                   #Xe13e)
            ("[^=]\\(=>\\)"                #Xe13f)
            ("\\(=>>\\)"                   #Xe140)
            ("\\(<=\\)"                    #Xe141)
            ("\\(=<<\\)"                   #Xe142)
            ("\\(=/=\\)"                   #Xe143)
            ("\\(>-\\)"                    #Xe144)
            ("\\(>=\\)"                    #Xe145)
            ("\\(>=>\\)"                   #Xe146)
            ("[^-=]\\(>>\\)"               #Xe147)
            ("\\(>>-\\)"                   #Xe148)
            ("\\(>>=\\)"                   #Xe149)
            ("\\(>>>\\)"                   #Xe14a)
            ("\\(<\\*\\)"                  #Xe14b)
            ("\\(<\\*>\\)"                 #Xe14c)
            ("\\(<|\\)"                    #Xe14d)
            ("\\(<|>\\)"                   #Xe14e)
            ("\\(<\\$\\)"                  #Xe14f)
            ("\\(<\\$>\\)"                 #Xe150)
            ("\\(<!--\\)"                  #Xe151)
            ("\\(<-\\)"                    #Xe152)
            ("\\(<--\\)"                   #Xe153)
            ("\\(<->\\)"                   #Xe154)
            ("\\(<\\+\\)"                  #Xe155)
            ("\\(<\\+>\\)"                 #Xe156)
            ("\\(<=\\)"                    #Xe157)
            ("\\(<==\\)"                   #Xe158)
            ("\\(<=>\\)"                   #Xe159)
            ("\\(<=<\\)"                   #Xe15a)
            ("\\(<>\\)"                    #Xe15b)
            ("[^-=]\\(<<\\)"               #Xe15c)
            ("\\(<<-\\)"                   #Xe15d)
            ("\\(<<=\\)"                   #Xe15e)
            ("\\(<<<\\)"                   #Xe15f)
            ("\\(<~\\)"                    #Xe160)
            ("\\(<~~\\)"                   #Xe161)
            ("\\(</\\)"                    #Xe162)
            ("\\(</>\\)"                   #Xe163)
            ("\\(~@\\)"                    #Xe164)
            ("\\(~-\\)"                    #Xe165)
            ("\\(~=\\)"                    #Xe166)
            ("\\(~>\\)"                    #Xe167)
            ("[^<]\\(~~\\)"                #Xe168)
            ("\\(~~>\\)"                   #Xe169)
            ("\\(%%\\)"                    #Xe16a)
	    ;; ("\\(x\\)"                   #Xe16b) This ended up being hard to do properly so i'm leaving it out.
            ("[^:=]\\(:\\)[^:=]"           #Xe16c)
            ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
            ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

(defun add-fira-code-symbol-keywords ()
  (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

(add-hook 'prog-mode-hook
          #'add-fira-code-symbol-keywords)

;; Setup theme first to avoid annoying window flashes on load
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/noctilux-theme")
;;(load-theme 'noctilux t)

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
(setq mac-option-modifier 'meta)

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

(use-package delight)

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
  :diminish company-mode
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

(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode))

(use-package eldoc
  :config
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round))

(use-package lispy)

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
  (setq org-agenda-files '("~/org/todo.org"))
  (setq org-capture-templates
	'(("t" "Todo" entry (file+olp "~/org/todo.org" "Tasks" "Captured")
	   "* TODO %?\n  %i\n  %a"))))

(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name)))
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
  :delight
  :config
  (yas-global-mode 1))

(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook #'subword-mode)
  :config
  (put-clojure-indent 's/fdef 1)
  (setq clojure-align-forms-automatically t))

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
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
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
  (setq ido-use-virtual-buffers t)
  (setq ido-auto-merge-work-directories-length -1))

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
  ;; (global-rbenv-mode)
  )

(use-package go-mode
  :init
  (add-hook 'go-mode-hook (lambda ()
			    (setq tab-width 4)
			    (if (not (string-match "go" compile-command))
				(set (make-local-variable 'compile-command)
				     "go build -v && go test -v && go vet"))))
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (electric-pair-local-mode))

(use-package go-eldoc
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
	 ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :init
  (dumb-jump-mode))

(use-package all-the-icons)

(use-package neotree
  :bind (("<f8>" . neotree-project-dir))
  :config
  (setq projectile-switch-project-action 'neotree-projectile-action))

(use-package terraform-mode)

(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  ;; I dig the Fira Code font

  ;; Also enable ligatures
  (if (fboundp 'mac-auto-operator-composition-mode)
      (progn (set-frame-font "Fira Code:style=Retina" nil t)
	     (mac-auto-operator-composition-mode))
    ;; This works when using emacs --daemon + emacsclient
    (add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
    ;; This works when using emacs without server/client
    (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))

(use-package typescript-mode
  :config
  (setq typescript-indent-level 2))

(use-package tide
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (setq tide-format-options '(:indentSize 2 :tabSize 2)))

(use-package ng2-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "14f0fbf6f7851bfa60bf1f30347003e2348bf7a1005570fd758133c87dafe08f" "4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" default)))
 '(package-selected-packages
   (quote
    (dockerfile-mode ng2-mode tide delight neotree doom-themes dumb-jump go-eldoc go-mode buffer-move projectile-rails web-mode rbenv evil-search-highlight-persist default-text-scale evil-visual-mark-mode fill-column-indicator sos ido-completing-read+ zenburn-theme ido-vertical-mode markdown-mode yaml-mode clj-refactor ace-window goto-last-change emacs-sensible use-package-chords use-package avy enh-ruby-mode company-inf-ruby yasnippet minitest ruby-compilation ruby-test-mode robe inf-ruby flycheck whole-line-or-region org ag exec-path-from-shell yard-mode which-key smartparens rainbow-delimiters projectile paredit nyan-mode magit flx-ido company clojure-cheatsheet aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
