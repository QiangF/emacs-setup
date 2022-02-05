; -*- coding:utf-8 -*-
;; Use a prefix argument (C-u <RET>) to see the whole call tree below a function.

(setup debug
  (:with-map debugger-mode-map
    (:bind "J" #'debugger-jump
           "j" #'next-line
           "k" #'previous-line
           "Q" #'top-level
           "q" #'bury-buffer
           "Z" #'ora-debug-set-frame))

  (:option debugger-stack-frame-as-list t
            profiler-max-stack-depth 64
            profiler-report-cpu-line-format
                 '((120 left)
                   (24 right ((19 right)
                              (5 right)))))
  (:when-loaded
    (defun my-debug ()
      (interactive)
      (if (profiler-cpu-running-p)
          (progn (profiler-report) (profiler-reset))
          (profiler-start 'cpu)))

    (defun ora-debug-set-frame ()
      (interactive)
      (let* ((debugger-window (selected-window))
             (nframe (1+ (debugger-frame-number 'skip-base)))
             (base (debugger--backtrace-base))
             (locals (backtrace--locals nframe base))
             wnd)
        (push-button)
        (setq wnd (current-window-configuration))
        (run-with-timer
         0 nil
         `(lambda ()
            (mapc (lambda (x!) (set (car x!) (cdr x!))) ',locals)
            (set-window-configuration ,wnd)
            (when (get-buffer "*Backtrace*")
              (kill-buffer "*Backtrace*"))))
        (top-level)))))
;; Search init file for bugs
(setup bug-hunter
  (defun my-test-emacs ()
    (interactive)
    (require 'async)
    (async-start
     (lambda () (shell-command-to-string
                 "emacs --batch --eval \"
    (condition-case e
        (progn
        (load \\\"~/.emacs\\\")
        (message \\\"-OK-\\\"))
    (error
    (message \\\"ERROR!\\\")
    (signal (car e) (cdr e))))\""))
     `(lambda (output)
        (if (string-match "-OK-" output)
            (when ,(called-interactively-p 'any)
              (message "All is well"))
            (switch-to-buffer-other-window "*startup error*")
            (delete-region (point-min) (point-max))
            (insert output)
            (search-backward "ERROR!"))))))

(defun kill-buffer-volatile (buf)
  "Kill current buffer, even if it has been modified."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-buffer buf))

;; get hanging emacs back to response: switch to a tty
;; 1. interrupt-emacs, toggle-debug-on-quit after get back control
;; 2. kill-emacs-buffer
;; or use emacsclient -e '(buffer-list)' to list buffer names and kill the buffer that hangs emacs

(defun emacsclient-kill-buffer ()
  (let ((buf (nth 0 (buffer-list))))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (file-directory-p (buffer-file-name))))
        (save-buffer)))
    (kill-buffer buf)))

;; Kill the current buffer immediatly, saving it if needed.
(defvar kill-save-buffer-delete-windows t
  "*Delete windows when `kill-save-buffer' is used.
If this is non-nil, then `kill-save-buffer' will also delete the corresponding
windows.  This is inverted by `kill-save-buffer' when called with a prefix.")

(defun kill-save-buffer (arg)
  "Save the current buffer (if needed) and then kill it.
Also, delete its windows according to `kill-save-buffer-delete-windows'.
A prefix argument ARG reverses this behavior."
  (interactive "P")
  (let ((del kill-save-buffer-delete-windows))
    (when arg (setq del (not del)))
    (when (and (buffer-file-name) (not (file-directory-p (buffer-file-name))))
      (save-buffer))
    (let ((buf (current-buffer)))
      (when del (delete-windows-on buf))
      (kill-buffer buf))))

(provide 'init-debug)

; * debug
;; *Backtrace* has to be quited before new backtrace is shown
;; interaction-log, command-log-mode, view-lossage

;; * profiler
;; type d to see function description
;; M-x profiler-start
;; M-x profiler-report
