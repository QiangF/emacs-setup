;; -*- coding:utf-8 -*-

(setup (:pkg simple-paren)
   (:bind
    "<" #'simple-paren-lesser-than))

(setup (:pkg region-bindings-mode)
   (:bind "q" #'region-bindings-mode-off
          "(" #'simple-paren-parentize
          "{" #'simple-paren-brace
          "[" #'simple-paren-bracket
          "'" #'simple-paren-singlequote
          "\"" #'simple-paren-doublequote
          ;; < is used by lispy
          ;; ("<" #'simple-paren-lesser-than)
          ;; (">" #'simple-paren-greater-than)
          )

   (require 'simple-paren)
   (defun simple-paren-quote-cn (arg)
     (interactive "*P")
     (simple-paren--intern ?“ ?” arg))

   (with-eval-after-load 'my-mode
     (add-to-list 'my-minor-mode-list 'region-bindings-mode))

   ;; (evil-make-intercept-map region-bindings-mode-map)
   (with-eval-after-load 'evil-core
     (progn
       (evil-make-overriding-map region-bindings-mode-map 'visual)
       (add-hook 'region-bindings-mode-hook #'evil-normalize-keymaps)))

   (require 'region-bindings-mode)
   (region-bindings-mode-enable))

(setup (:pkg puni)
   (:hook-into org-mode prog-mode sgml-mode nxml-mod tex-mode)
   (:global "M-\\" #'toki-shrink-whitespace)
   (:autoload toki-shrink-whitespace)
   (:with-map puni-mode-map
     (:bind "C-k" #'my-puni-kill-line))
   (:when-loaded
     (defun toki-shrink-whitespace ()
       "Intelligently shrink whitespaces around point.

When in the middle of a line, delete whitespaces around point, or
add a space if there are no whitespaces.

When in an empty line, leave only one blank line, or delete it if
it's the only one.

When at the beginning/end of a line, first delete whitespaces
around it, then delete empty lines before/after it.  Finally join
with the line before/after it."
       (interactive)
       (let* ((beg
                (save-excursion (while (member (char-before) '(?\s ?\t))
                                  (backward-char))
                                (point)))
              ;; el = empty lines
              (beg-with-el
                (save-excursion (while (member (char-before) '(?\n ?\s ?\t))
                                  (backward-char))
                                (point)))
              (end
                (save-excursion (while (member (char-after) '(?\s ?\t))
                                  (forward-char))
                                (point)))
              (end-with-el
                (save-excursion (while (member (char-after) '(?\n ?\s ?\t))
                                  (forward-char))
                                (point))))
         (cond
           ((puni--line-empty-p)
            (delete-blank-lines))
           ((eq beg end)
            (cond
              ((eq beg-with-el end-with-el)
               (insert-char ?\s))
              (t
               (cond
                 ((eq (point) beg-with-el)
                  (save-excursion
                    (forward-char)
                    (if (puni--line-empty-p)
                        (delete-blank-lines)
                        (delete-char -1))))
                 (t
                  (save-excursion
                    (backward-char)
                    (if (puni--line-empty-p)
                        (delete-blank-lines)
                        (delete-char 1))))))))
           (t
            (delete-region beg end)))))

     (defun my-puni-kill-line ()
       "Kill a line forward while keeping expressions balanced.
If nothing can be deleted, kill backward.  If still nothing can be
deleted, kill the pairs around point."
       (interactive)
       (let ((bounds (puni-bounds-of-list-around-point)))
         (if (eq (car bounds) (cdr bounds))
             (when-let ((sexp-bounds (puni-bounds-of-sexp-around-point)))
               (puni-delete-region (car sexp-bounds) (cdr sexp-bounds) 'kill))
             (if (eq (point) (cdr bounds))
                 (puni-backward-kill-line)
                 (puni-kill-line)))))))

;; (setup (:pkg) evil-matchit
;;  :bind* ("<M-return>" #'evilmi-jump-items)
;;  :config
;;    (with-eval-after-load "evil-matchit-org"
;;      (push '("^[ \t]*\\\\\\([a-zA-Z_]+\\)" 1) evilmi-org-extract-keyword-howtos)
;;      (push '(("begin_section") () ("end_section") "MONOGAMY") evilmi-org-match-tags)
;;      (push '(("begin") () ("end") "MONOGAMY") evilmi-org-match-tags)))

(provide 'init-paren)
