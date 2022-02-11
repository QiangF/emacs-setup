;; deform-mode.el
;; see deform-mode

(defvar deform-mode-hook nil)

(defvar deform-ruler "**..:....1....:....2....:....3....:....4....:....5....:....6....:....7....:....8"
  "*The ruler `deform-insert-ruler' inserts."
)

(defun deform-insert-ruler ()
  "Insert a ruler with comments."
  (interactive)
  (end-of-line)  
  (insert deform-ruler)
)

(defvar deform-font-lock-defaults
  `((
   ("^[*].*$" . font-lock-comment-face)
   ("^[ \t]+$" . highlight)
  )))

(defvar deform-comment-prefix "$"
  "*The comment `deform-insert-comment' inserts."
)

(define-derived-mode deform-mode fundamental-mode "deform input file"
    ;; for comments
    ;; overriding these vars gets you what (I think) you want
    ;; they're made buffer local when you set them
    (setq font-lock-defaults deform-font-lock-defaults)
    (setq comment-start "$ ")
    (setq comment-end "")
    ;; example heading
    ;; *  Process Definition
    (setq outline-regexp "\\$  [*]\\{0,8\\}")
    (setq require-final-newline  t)
    (run-mode-hooks 'deform-mode-hook))

(defun non-alphabet-outline ()
    (setq-local outline-minor-mode-hook nil)
    (outline-minor-mode t)
    (setq outline-level (lambda()
           (let* ((data (match-data))
             (start (car data))
             (end (cadr data))
             (level (- end start)))
             (if (looking-at "[*]\\{1,8\\} ")
             (- level 1) 12)))
          outline-regexp "[*]\\{1,8\\} \\|^[a-zA-Z]+.*"))

(add-hook 'deform-mode-hook 'non-alphabet-outline)

(provide 'deform-mode)
