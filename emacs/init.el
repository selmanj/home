;; Setup noctilux (a rad theme)
(add-to-list 'custom-theme-load-path "~/.emacs.d/noctilux-theme")
(load-theme 'noctilux t)

;; Set up mac rebindings as advised by http://david.rothlis.net/emacs/osx.html
(setq mac-command-modifier 'meta)

(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "M-o") 'mode-line-other-buffer)

;; I find sensible defaults very sensible
(load-file "~/.emacs.d/sensible-defaults.el/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)

;; Fix that annoying bell
(load-file "~/.emacs.d/rwd-bell.el")

;; Ido mode and recentf seem pretty sweet
(ido-mode t)
(setq ido-enable-flex-matching t)
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 200)
(setq ido-use-virtual-buffers t)

;; Define list of packages to install
(setq package-list '(clojure-mode rainbow-delimiters aggressive-indent))

;; Configure melpa-stable (stay away from melpa, been in trouble too much from that!)
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; If this is a clean install, do a package refresh
; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; List of packages to install goes here
(dolist (pkg package-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Require clojure-goodies
(add-hook 'clojure-mode-hook #'subword-mode)
;; TODO decide on paredit or smartparens!
;; (add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
