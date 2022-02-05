; -*- coding:utf-8 -*-

(defvar luna-dumped nil
  "non-nil when a dump file is loaded.
(Because dump.el sets this variable).")

(defvar luna-dumped-load-path nil
  "By default dump files doesn’t save ‘load-path’.
We need to manually save and restore it. See manual for more info.")

(defvar luna-env-vars nil
  "When dumping, load environment from shell into this variable.
At startup, load environment from this variable.")

(defvar luna-dump-file (expand-file-name "~/.emacs.d/dump/dump-file")
  "Dump file location.")

(defmacro luna-if-dump (then &rest else)
  "Evaluate IF if running with a dump file, else evaluate ELSE."
  (declare (indent 1))
  `(if luna-dumped
       ,then
       ,@else))

(setq my-dumped-packages '(ag
                           ace-pinyin
                           avy
                           bookmark+
                           bookmark+-lit
                           company
                           elec-pair
                           exec-path-from-shell
                           expand-region
                           fm-bookmarks
                           helpful
                           hydra
                           ibuffer
                           iedit
                           isearch-mb
                           ivy
                           org
                           ox-odt
                           pinyinlib
                           savehist
                           setup
                           wgrep
                           which-func
                           which-key
                           winner
                           yasnippet
                           ;; lib
                           subr-x cl-lib seq pcase svg))

(defalias 'qrr 'query-replace-regexp)
(defalias 'qr 'query-replace)
(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (ignore-errors (server-start))))

(setq custom-file "~/.emacs.d/custom.el"
      initial-scratch-message nil
      inhibit-startup-message t
      inhibit-startup-echo-area-message nil
      ;; Disable the site default settings. See (info "(emacs) Init File")
      inhibit-default-init t
      inhibit-startup-screen t)
(load custom-file :noerror)

(require 'package)
(require 'cl-lib)

(defun my-add-subdirs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
            (append
             (cl-remove-if-not
              (lambda (dir) (file-directory-p dir))
              (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
             load-path)))))

(add-to-list 'load-path (concat "~/.dotfiles/usr/" (getenv "my_distrb") "/" (getenv "bin") "/emacs"))
;; package with the same name in site-lisp will shadow packages in elpa
(my-add-subdirs-to-load-path (expand-file-name "site-lisp/" user-emacs-directory))

(defun edump ()
  "Dump Emacs."
  (interactive)
  (let ((buf "*dump process*"))
    (make-process
     :name "dump"
     :buffer buf
     :command (list "emacs" "--batch" "-q"
                    "-l" (expand-file-name "~/.emacs.d/dump/dump.el")))
    (display-buffer buf)))

(provide 'dump-init)
