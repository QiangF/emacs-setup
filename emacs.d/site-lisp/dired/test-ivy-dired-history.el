(require 'package)
(setq package-enable-at-startup nil
      package-user-dir "~/.emacs.d/elpa/")
(package-initialize)

(setq savehist-file "~/.emacs.d/history")
(require 'savehist)
(add-to-list 'savehist-additional-variables 'ivy-dired-history-variable)
(savehist-mode 1)

(with-eval-after-load 'dired
  (require 'ivy-dired-history)
  ;; if you are using ido,you'd better disable ido for dired
  ;; (define-key (cdr ido-minor-mode-map-entry) [remap dired] nil) ;in ido-setup-hook
  (define-key dired-mode-map "," 'dired))
