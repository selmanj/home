;; Dark-themes are best
(load-theme 'tango-dark)

;; Set up mac rebindings as advised by http://david.rothlis.net/emacs/osx.html
(setq mac-command-modifier 'meta)
(global-set-key (kbd "M-`") 'other-frame)

;; I find sensible defaults very sensible
(load-file "~/.emacs.d/sensible-defaults.el/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)

;; Ido mode and recentf seem pretty sweet
(ido-mode t)
(setq ido-enable-flex-matching t)
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 200)
(setq ido-use-virtual-buffers t)
