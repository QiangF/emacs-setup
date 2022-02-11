; -*- coding:utf-8 -*-

;; After opening your_file.csv using Emacs, you might want to use
;; M-x toggle-truncate-lines to disable the warping of long
;; lines. Then, you can use M-x csv-align-fields to align fields.
;; You can set the variable csv-separators to change the separator

;; align just visible lines:
(setup csv-mode
  (:file-match "\\.csv\\'")
  (:autoload csv-align-visible align-whitespace align-quote-space align-comma)
  (:bind "C-c C-c" #'csv-align-visible)

  (:when-loaded
    (defun csv-align-visible (&optional arg)
      "Align visible fields"
      (interactive "P")
      (csv-align-fields nil (window-start) (window-end)))

    (defun align-whitespace (start end)
      "Align columns by whitespace"
      (interactive "r")
      (align-regexp start end
                    "\\(\\s-*\\)\\s-" 1 0 t))

    (defun align-quote-space (start end)
      "Align columns by quote and space"
      (interactive "r")
      (align-regexp start end
                    "\\(\\s-*\\).*\\s-\"" 1 0 t))

    (defun align-comma (start end)
      "Align columns by comma"
      (interactive "r")
      (align-regexp start end
                    "\\(\\s-*\\)," 1 1 t))))

(provide 'init-csv)
