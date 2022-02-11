; -*- coding:utf-8 -*-

;; Comes from https://github.com/noctuid/general.el
(defun save-rime-userdb ()
  ;; (call-process-shell-command "/usr/bin/fcitx-remote -r" nil 0)
  (call-process-shell-command "/usr/bin/ibus exit" nil 0))

(defun stop-aria2c ()
  (call-process-shell-command
   (concat "/usr/bin/systemctl --user "
           "aria2c@"
           (getenv "DISPLAY")
           ".service")
   nil 0))

(defun my-clean-up ()
  (save-rime-userdb)
  (stop-aria2c))

(defun lock ()
  (interactive)
  (my-clean-up)
  (call-process-shell-command "xautolock -locknow" nil 0))

;; (start-process "loginctl" nil "loginctl" "suspend")
(defun suspend ()
  (interactive)
  (let ((suspend-command
          (concat "/usr/bin/dbus-send --system --print-reply --dest=org.freedesktop.login1 "
                  "/org/freedesktop/login1 'org.freedesktop.login1.Manager.Suspend' boolean:true")))
    (bookmark-save)
    (recentf-save-list)
    (my-clean-up)
    (call-process-shell-command suspend-command nil 0)))

(defun poweroff ()
  (interactive)
  (let ((poweroff-command
          (concat "/usr/bin/dbus-send --system --print-reply --dest=org.freedesktop.login1 "
                  "/org/freedesktop/login1 'org.freedesktop.login1.Manager.PowerOff' boolean:true")))
    (bookmark-save)
    ;; (recentf-save-list)
    (save-some-buffers)
    (run-hooks 'kill-emacs-hook)
    (my-clean-up)
    (call-process-shell-command poweroff-command nil 0)))

(defun reboot ()
  (interactive)
  (let ((reboot-command
          (concat "/usr/bin/dbus-send --system --print-reply --dest=org.freedesktop.login1 "
                  "/org/freedesktop/login1 'org.freedesktop.login1.Manager.Reboot' boolean:true")))
    (bookmark-save)
    ;; (recentf-save-list)
    (save-some-buffers)
    (my-clean-up)
    (run-hooks 'kill-emacs-hook)
    (call-process-shell-command reboot-command nil 0)))

(setup dmenu
  (defun exwm-app-launcher ()
    "Launches an application in your PATH.
Can show completions at point for COMMAND using helm or ido"
    (interactive)
    (unless dmenu--cache-executable-files
      (dmenu--cache-executable-files))
    (ivy-read "Run a command: " dmenu--cache-executable-files
              :action (lambda (command) (start-process-shell-command command nil command))
              :caller 'exwm-app-launcher)))

(defun my-dispatch-key (key)
  "list key-binding's command in all active keymaps"
  (interactive "kDispatch Key: ")
  (require 'seq)
  (let* ((commands (seq-filter #'commandp
                               (mapcar (lambda (map) (lookup-key map key))
                                       (current-active-maps))))
         (collection (mapcar #'symbol-name commands))
         (command (intern (completing-read
                           (concat (key-description key) " ")
                           collection nil t nil nil (cadr collection)))))
    (command-execute command)))

(defun pager-read-pipe (fname)
  (let ((buf (generate-new-buffer "*pager*"))
        (pname (concat "pager-" fname)))
    (with-current-buffer buf (read-only-mode))
    (switch-to-buffer buf)

    (let ((proc (start-process pname buf "/usr/bin/cat" fname)))
      (set-process-sentinel proc (lambda (proc e) ()))
      (set-process-filter proc (lambda (proc string)
                                 (when (buffer-live-p (process-buffer proc))
                                   (with-current-buffer (process-buffer proc)
                                     (save-excursion
                                       ;; Insert the text, advancing the process marker.
                                       (let ((inhibit-read-only t))
                                         (goto-char (process-mark proc))
                                         (insert string)
                                         (set-auto-mode)
                                         (set-marker (process-mark proc) (point))))))))
      proc)))

(provide 'init-miscs)
