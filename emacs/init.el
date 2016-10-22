;; Dark-themes are best
(load-theme 'tango-dark)

;; Set up mac rebindings as advised by http://david.rothlis.net/emacs/osx.html
(setq mac-command-modifier 'meta)
(global-set-key (kbd "M-`") 'other-frame)

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

;; Configure melpa-stable (stay away from melpa, been in trouble too much from that!)
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Require clojure-goodies
(unless (package-installed-p 'clojure-mode)
  (package-install 'clojure-mode))
;; TODO add clj-refactor
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(unless (package-installed-p 'rainbow-delimiters)
  (package-install 'rainbow-delimiters))
(unless (package-installed-p 'aggressive-indent)
  (package-install 'aggressive-indent))
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
