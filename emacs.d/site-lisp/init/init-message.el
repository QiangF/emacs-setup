; -*- coding:utf-8 -*-

(defun messages-auto-tail (&rest _)
  "Make *Messages* buffer auto-scroll to the end after each message."
  (let* ((buf-name "*Messages*")
         ;; Create *Messages* buffer if it does not exist
         (buf (get-buffer-create buf-name)))
    ;; Activate this advice only if the point is _not_ in the *Messages* buffer
    ;; to begin with. This condition is required; otherwise you will not be
    ;; able to use `isearch' and other stuff within the *Messages* buffer as
    ;; the point will keep moving to the end of buffer :P
    (when (not (string= buf-name (buffer-name)))
      ;; Go to the end of buffer in all *Messages* buffer windows that are
      ;; *live* (`get-buffer-window-list' returns a list of only live windows).
      (dolist (win (get-buffer-window-list buf-name nil :all-frames))
        (with-selected-window win
          (goto-char (point-max))))
      ;; Go to the end of the *Messages* buffer even if it is not in one of
      ;; the live windows.
      (with-current-buffer buf
        (goto-char (point-max))))))

(advice-add 'message :after #'messages-auto-tail)

;; (setup posframe
;;    (setq posframe-arghandler #'my-posframe-arghandler)
;;    (defun my-posframe-arghandler (buffer-or-name arg-name value)
;;      (let ((info '(:internal-border-width 10
;;                    :foreround-color "yello"
;;                    :background-color "gray10")))
;;        (or (plist-get info arg-name) value))))

(provide 'init-message)
