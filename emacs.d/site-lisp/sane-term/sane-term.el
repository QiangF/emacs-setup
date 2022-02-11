;;; sane-term.el --- Multi Term is crazy. This is not.

;; Copyright (C) 2015 Adam Patterson

;; Author: Adam Patterson <adam@adamrt.com>
;; URL: http://github.com/adamrt/sane-term
;; Version: 0.4
;; Package-Requires: ((emacs "24.1"))

;;; Commentary:

;; You can set it up like this:

;;     (require 'sane-term)
;;     (global-set-key (kbd "C-x t") 'sane-term)
;;     (global-set-key (kbd "C-x T") 'sane-term-create)

;;; Code:
(require 'vterm-toggle)

(defgroup sane-term nil
  "Multi Term is crazy. This is not."
  :group 'term)

(defcustom sane-term-shell-command (or (getenv "SHELL")
                                       "/bin/sh")
  "Specify which shell to use."
  :type 'string
  :group 'sane-term)

(defcustom sane-term-initial-create t
  "Creates a term if one doesn't exist."
  :type 'boolean
  :group 'sane-term)

(defcustom sane-term-kill-on-exit t
  "Kill term buffer on exit (C-d or `exit`)."
  :type 'boolean
  :group 'sane-term)

(defcustom sane-term-next-on-kill t
  "When killing a term buffer, go to the next one.
Depends on sane-term-kill-on-exit."
  :type 'boolean
  :group 'sane-term)

(defun my-term-mode-p ()
  (and (or (derived-mode-p 'term-mode) (derived-mode-p 'vterm-mode))
       (not (eq (current-buffer) vterm-toggle--vterm-dedicated-buffer))))

(defun sane-term-buffer-exists-p ()
  "Boolean if term-mode buffers exist."
  (catch 'loop
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (let ((exwm--framep (if exwm_enable
                                (eq exwm--frame exwm-workspace--current) t)))
          (when (and (my-term-mode-p) exwm--framep)
            (throw 'loop t)))))))

(defun sane-term-cycle (reverse)
  (unless reverse
    (when (my-term-mode-p)
      (bury-buffer)))
  (let ((buffers (buffer-list)))
    (when reverse
      (setq buffers (nreverse buffers)))
    (catch 'loop
      (dolist (buf buffers)
        (with-current-buffer buf 
          (let ((exwm--framep (if exwm_enable
                                  (eq exwm--frame exwm-workspace--current) t)))
            (when (and (my-term-mode-p) exwm--framep)
              (switch-to-buffer buf)
              (throw 'loop nil))))))))

(defun sane-term-prev ()
  "Cycle through term buffers, in reverse."
  (interactive)
  (sane-term-cycle t))

(defun sane-term-next ()
  "Cycle through term buffers."
  (interactive)
  (sane-term-cycle nil))

;;;###autoload
(defun sane-term-create ()
    "Create new term buffer."
    (interactive)
    (unless (ignore-errors (file-directory-p default-directory)) (setq-local default-directory "/tmp"))
    (if (and (require 'vterm nil :noerror) (require 'vterm-module nil :noerror))
        (vterm)
    (ansi-term "/bin/bash" (concat "Term::" (read-from-minibuffer "Buffer name (optional): " "ansi-term")))))

;; (defun sane-term-create ()
;;   "Create new term buffer."
;;   (interactive)
;;   (start-process-shell-command "termite" nil "termite"))
;;   (ansi-term sane-term-shell-command (read-string "Buffer name (optional):" nil nil "ansi-term")))


;;;###autoload
(defun sane-term ()
  "Cycle through term buffers, creating if necessary."
  (interactive)
  (when sane-term-initial-create
    (unless (sane-term-buffer-exists-p)
      (sane-term-create)))
  (sane-term-next))

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  "Kill term buffers on exiting term (C-d or `exit`).
Optionally go to next term buffer."
  (when sane-term-kill-on-exit
    (kill-buffer)
    (when sane-term-next-on-kill
      (sane-term-next))))


(provide 'sane-term)

;;; sane-term.el ends here
