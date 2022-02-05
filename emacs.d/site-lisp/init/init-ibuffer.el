; -*- coding:utf-8 -*-
; * ibuffer
; ** tips
;; * s (mark star buffers) D (delete all marked buffers)
;; * r k (remove read-only buffers from ibuffer)
;; % n (mark buffers by regex) U (regex replace)
;; q close current window
;; clean-buffer-list kill buffers that haven’t been visited in 3 days, or in the last hour in the case of special buffers

; ** setting
(setup ibuffer
  (:option ibuffer-formats '((mark modified read-only " "
                              (name 30 30 :left :elide) ; change: 30s were originally 18s
                              " " (size 9 -1 :right) " " (mode 10 12 :left :elide)
                              " " filename-and-process)
                             (mark " " (name 16 -1) " " filename))
           ibuffer-expert t
           ibuffer-show-empty-filter-groups nil)
  (:global* "C-x C-b" #'ibuffer)
  (:bind "s p" #'ibuffer-do-sort-by-pathname
         "e" #'ibuffer-ediff-marked-buffers)

  ;; add another sorting method for ibuffer (allow the grouping of
  ;; filenames and dired buffers
  ;; (define-ibuffer-sorter pathname
  ;;    "Sort the buffers by their mode and pathname."
  ;;    (:description "mode plus filenames")
  ;;    (string-lessp
  ;;    (with-current-buffer (car a)
  ;;    (or (concat (symbol-name major-mode) buffer-file-name)
  ;;    (if (eq major-mode 'dired-mode)
  ;;    (concat (symbol-name major-mode) (expand-file-name dired-directory)))
  ;;    ;; so that all non pathnames are at the end
  ;;    "~"))
  ;;    (with-current-buffer (car b)
  ;;    (or (concat (symbol-name major-mode) buffer-file-name)
  ;;    (if (eq major-mode 'dired-mode)
  ;;    (c-name major-mode) (expand-file-name dired-directory)))
  ;;    ;; so that all non pathnames are at the end
  ;;    "~")))

  ;; (defun ibuffer-ediff-marked-buffers ()
  ;;  (interactive)
  ;;  (let* ((marked-buffers (ibuffer-get-marked-buffers))
  ;;     (len (length marked-buffers)))
  ;;    (unless (= 2 len)
  ;;      (error (format "%s buffer%s been marked (needs to be 2)"
  ;;             len (if (= len 1) " has" "s have"))))
  ;;    (ediff-buffers (car marked-buffers) (cadr marked-buffers))))

  ;; ;; Ensure ibuffer opens with point at the current buffer's entry.
  ;; (defadvice ibuffer
  ;;    (around ibuffer-point-to-most-recent) ()
  ;;    "Open ibuffer with cursor pointed to most recent buffer name."
  ;;    (let ((recent-buffer-name (buffer-name)))
  ;;    ad-do-it
  ;;      (when (called-interactively-p 'interactive)
  ;;        (ibuffer-jump-to-buffer recent-buffer-name))))
  ;; (ad-activate 'ibuffer)
  )

;; hide certain buffer
;; (setup ibuf-ext
;;   (:load-after ibuffer)
;;   (add-hook 'ibuffer-mode 'ibuffer-auto-mode)
;;   ;; Enable ibuffer-filter-by-filename to filter on directory names too.
;;   (define-ibuffer-filter filename
;;      "Toggle current view to buffers with file or directory name matching QUALIFIER."
;;      (:description "filename"
;;       :reader (read-from-minibuffer "Filter by file/directory name (regexp): "))
;;      (ibuffer-awhen (or (buffer-local-value 'buffer-file-name buf)
;;             (buffer-local-value 'dired-directory buf))
;;        (string-match qualifier it)))
;;   (add-to-list 'ibuffer-never-show-predicates "^\\*helm"))

(setup uniquify            ; Make buffer names unique
  (:option uniquify-buffer-name-style 'forward))

(provide 'init-ibuffer)
