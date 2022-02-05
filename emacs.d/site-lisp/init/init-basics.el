; -*- coding:utf-8 -*-

;; http://stackoverflow.com/a/6133921/24998
;; don't want to add delete word to the kill-ring
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun forward-delete-word (arg)
  "Delete characters forward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun prelude-shift-left-visual ()
  "Shift left and restore visual selection."
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun prelude-shift-right-visual ()
  "Shift right and restore visual selection."
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun join-region (beg end)
  "Apply join-line over region."
  (interactive "r")
  (if mark-active
      (let ((beg (region-beginning))
            (end (copy-marker (region-end))))
        (goto-char beg)
        (while (< (point) end)
          (join-line 1)))))

(setup (:pkg move-text)
  (:global "C->" #'prelude-shift-right-visual
           "C-<" #'prelude-shift-left-visual)

  (move-text-default-bindings))

(setup (:pkg centimacro)
  (:autoload centi-assign))

(setup (:pkg exec-path-from-shell)
  (if luna-dumped
      (dolist (var luna-env-vars)
        (exec-path-from-shell-setenv (car var) (cdr var)))
      (exec-path-from-shell-initialize)))

(provide 'init-basics)
