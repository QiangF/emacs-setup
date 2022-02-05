; -*- coding:utf-8 -*-

(require 'init-miscs)
(require 'cl-macs)

(setq confirm-kill-emacs 'y-or-n-p)
(setq confirm-kill-processes nil)

(defun ask-before-kill (orig-func &rest args)
  (when
      (yes-or-no-p "Kill emacs now? ")
    (apply orig-func args)))

(advice-add #'kill-emacs :around #'ask-before-kill)

;; (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
;;   "Prevent annoying \"Active processes exist\" query when you quit Emacs."
;;   (lispy-flet (process-list ()) ad-do-it))

(when (require 'so-long nil :noerror)
  (global-so-long-mode 1))

;; see also http://anirudhsasikumar.net/blog/2005.01.21.html
;; https://github.com/m00natic/vlfi
(setup files
  (defun my-always-yes (&rest _) t)
  (defun no-confirm (fun &rest args)
    "Apply FUN to ARGS, skipping user confirmations."
    (cl-letf (((symbol-function 'y-or-n-p) #'my-always-yes)
              ((symbol-function 'yes-or-no-p) #'my-always-yes))
      (apply fun args)))

  ;; (cl-flet ((always-yes (&rest _) t))
  ;;   (defun no-confirm (fun &rest args)
  ;;     "Apply FUN to ARGS, skipping user confirmations."
  ;;     (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
  ;;               ((symbol-function 'yes-or-no-p) #'always-yes))
  ;;       (apply fun args))))

  (defun my-large-file-hook ()
    "If a file is over a given size, make the buffer read only."
    (when (> (buffer-size) (* 1024 1024))
      (set (make-local-variable 'backup-inhibited) t)
      (buffer-disable-undo)
      (highlight-indent-guides-mode -1)
      (undo-tree-mode -1)
      ;; (font-lock-mode -1)
      (company-mode -1)
      (message "my-large-file-hook applied")))

  (setq line-number-display-limit large-file-warning-threshold)
  (setq line-number-display-limit-width 200)
  (setq display-line-numbers-width-start t)

  (defun my-huge-file-hook ()
    "If a file is over a given size, make the buffer read only."
    (when (> (buffer-size) (* 4096 1024))
      (buffer-disable-undo)
      (set (make-local-variable 'backup-inhibited) t)
      (setq buffer-auto-save-file-name nil) ; disable autosave
      (font-lock-mode -1)
      (company-mode -1)
      (undo-tree-mode -1)
      (highlight-indent-guides-mode -1)
      ;; (setq buffer-read-only t)
      (setq bidi-display-reordering nil)
      (jit-lock-mode nil)
      (set (make-local-variable 'global-hl-line-mode) nil)
      ;; (set (make-local-variable 'line-number-mode) nil)
      (set (make-local-variable 'column-number-mode) nil)
      ;; cf. make-variable-buffer-local
      (fundamental-mode)
      (message "my-huge-file-hook applied")))

  (defun my-maybe-recover-this-file ()
    (when (file-newer-than-file-p (or buffer-auto-save-file-name
                                      (make-auto-save-file-name))
                                  buffer-file-name)
      (remove-hook 'find-file-hook 'my-maybe-recover-this-file)
      (if (yes-or-no-p (format "Recover auto save file %s? " buffer-file-name))
          ;; (call-interactively 'recover-this-file)
          (no-confirm 'recover-this-file)
          (message "Autosaved file available!")))
    (add-hook 'find-file-hook 'my-maybe-recover-this-file))

  (add-hook 'find-file-hook 'my-maybe-recover-this-file)

  (defun no-ask-before-recover (orig-func &rest args)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t))
              ((symbol-function 'y-or-n-p) (lambda (&rest args) t)))
      (apply orig-func args)))

  (defun dired-omit-mode-off ()
    (when dired-omit-mode
      (dired-omit-mode -1)))

  (advice-add #'recover-session :after #'dired-omit-mode-off)

  (advice-add #'recover-session-finish :around #'no-ask-before-recover)

  (add-hook 'find-file-hook 'my-large-file-hook)
  (add-hook 'find-file-hook 'my-huge-file-hook))

(provide 'init-file)
