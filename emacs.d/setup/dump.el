;; -*- lexical-binding: t -*-
;; emacs --dump-file="$HOME/.emacs.d/dump/dump-file"
;;; Packages

(add-to-list 'load-path (expand-file-name "dump" user-emacs-directory))

(require 'my-init)

(package-initialize)

(setq luna-dumped-load-path load-path
      luna-dumped t)

(dolist (package my-dumped-packages)
  (require package))

(require 'modus-themes)
(load-theme 'modus-vivendi t t)

(setq luna-env-vars (exec-path-from-shell-getenvs
                     exec-path-from-shell-variables))

(message "Dumping to %s" luna-dump-file)
(dump-emacs-portable luna-dump-file)
