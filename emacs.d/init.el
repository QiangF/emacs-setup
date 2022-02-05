; -*- coding:utf-8 -*-

(push (expand-file-name "setup" user-emacs-directory) load-path)

(require 'dump-init)

(luna-if-dump
    (progn
      (setq load-path luna-dumped-load-path)
      (global-font-lock-mode)
      (transient-mark-mode))
  ;; add load-path’s and load autoload files
  (package-initialize))

(require 'init-functions)
(require 'init-elpa)
(require 'init-gcmh)
(require 'init-basics)
(require 'init-miscs)

;; (setq debug-on-error t)
;; (require 'benchmark-init-modes)
;; (require 'benchmark-init)
;; (benchmark-init/activate)
;; To disable collection of benchmark data after init is done.
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

(setq exwm_enable (string= (getenv "exwm_enable") "yes"))
;; (require 'init-exwm)
(require 'init-gui)
(require 'init-modeline)
(require 'init-input-method)
(require 'init-emacs-anywhere)
;; (require 'rime)
(require 'init-autosave)
(require 'init-locale)
(require 'init-kill)
(require 'init-hydra)
;; (require 'init-evil)
(require 'init-multiple-cursors)
;; (require 'init-god-mode)
(require 'init-ibuffer)
;; (require 'init-exwmx)
(require 'init-minibuffer)
(require 'init-dired)
(require 'init-image-dired)
(require 'init-dired-ranger)
(require 'init-dired-async)
(require 'init-dired-history)
(require 'init-find)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-ivy)
;; (require 'init-selectrum)
(require 'init-term)
;; (require 'init-vterm)
(require 'init-term-keys)
(require 'init-xml)
(require 'init-lisp)
(require 'init-debug)
(require 'init-help)
(require 'init-file)
(require 'init-outline)
(require 'init-undo)
(require 'init-iedit)
(require 'init-pdf)
(require 'init-my-mode)
(require 'init-paren)
(require 'init-dictionary)
(require 'init-internet)
(require 'init-org)
(require 'init-org-odt)
;; (require 'init-git)
(require 'init-paradox)
(require 'init-shackle)
(require 'init-winner)
(require 'init-bookmark)
(require 'init-sys-utils)
(require 'init-gpg)
(require 'init-highlight)
;; (require 'init-slime)
;; (require 'init-shell)
(require 'init-cae)

;; * language support
;; (require 'init-eglot)
(require 'init-python)
(require 'init-eaf)

(toggle-debug-on-error)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
