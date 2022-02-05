; -*- coding:utf-8 -*-

;; deadgrep
;;  ;; C-g to abort
;;    (setq deadgrep-project-root-function '(lambda () default-directory))
;;    (setq deadgrep--hidden-files t)

(setup (:pkg ag))

(setup (:pkg wgrep)
  (:option wgrep-auto-save-buffer t
           wgrep-enable-key "r"
           wgrep-change-readonly-file t))

(setup (:pkg wgrep-ag)
  (:autoload wgrep-ag-setup)
  (add-hook 'ag-mode-hook #'wgrep-ag-setup))

(setup grep
  (:require wgrep-mode)
  (:bind "C-x C-q" #'wgrep-change-to-wgrep-mode
         "C-c C-c" 'wgrep-finish-edit))

(setup (:pkg visual-regexp))

(setup re-builder
  ;; Escape 的工作就交给 Emacs 了
  (:option reb-re-syntax 'string))

(setup (:pkg xr))

(setup (:pkg find-file-in-project)
  (:option ffip-use-rust-fd t))

;; (defun swiper-toggle-color-rg ()
;;   (interactive)
;;   (ivy-exit-with-action
;;    (lambda (_)
;;      (when (looking-back ivy--old-re (line-beginning-position))
;;        (goto-char (match-beginning 0)))
;;      (unless (string= ivy-text "")
;;        (color-rg-search-input ivy-text)))))
(setup color-rg
  (:autoload color-rg-search-symbol
    color-rg-search-input
    color-rg-search-symbol-with-type))

;; "^$" matches empty line

; * ag
; ** tips
;; ag to search for all files and ag-same to search for files of the same type
;; as the current buffer.
;; next-error and previous-error can be used to jump to the matches.
;; ag-find-file and ag-find-same-file use ag to list the files in the current
;; project. It's a convenient, though slow, way of finding files.
; ** setting

;; (setup (:pkg ) ag-and-a-half
;;   :commands (ag-and-a-half ag-and-a-half-same ag-and-a-half-find-file ag-and-a-half-find-file-same)
;;   :init
;;     (defalias 'ag 'ag-and-a-half)
;;     (defalias 'ag-same 'ag-and-a-half-same)
;;     (defalias 'ag-find-file 'ag-and-a-half-find-file)
;;     (defalias 'ag-find-file-same 'ag-and-a-half-find-file-same)
;; )

;; (setup (:pkg ivy-xref)
;;   (:load-after ivy xref)
;;   (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
;;   (setq xref-show-definitions-function #'ivy-xref-show-defs))

;; (setup (:pkg ) ctrlf
;;  :ensure
;;  :init (ctrlf-mode +1)
;;  :config
;;    (setq search-invisible nil)
;;    (setq isearch-invisible nil)

;;    (defun re-builder-extended-pattern (input)
;;      (let ((case-fold-search nil))
;;        (concat input "\\|" (pinyinlib-build-regexp-string input t))))

;;    (add-to-list 'ctrlf-style-alist
;;                 '(pinyin-regexp . (:prompt "pinyin-regexp"
;;                                    :translator re-builder-extended-pattern
;;                                    :case-fold ctrlf-no-uppercase-regexp-p
;;                                    :fallback (isearch-forward-regexp
;;                                               . isearch-backward-regexp))))

;;    (set-face-attribute 'ctrlf-highlight-line nil :underline t)
;;    (setq ctrlf-default-search-style 'pinyin-regexp)

;;    (setq completion-styles '(substring)))

;; (:map 'search-mb-minibuffer-map
;;       ("r" . (isearch-query-replace :wk "Replace"))
;;       ("C-j" . (newline :wk "[Newline]"))
;;       ("." . (toki-search-insert-\.* :wk "[.*]"))
;;       ("a" . (toki-search-insert-anychar :wk "[Anychar]"))
;;       ("g" . (toki-search-insert-group :wk "(Group)"))
;;       ("w" . (toki-search-insert-word-boundary :wk "(Word Bounds)"))
;;       ("s" . (toki-search-insert-symbol-boundary :wk "(Symbol Bounds)"))
;;       ("C" . (isearch-toggle-case-fold :wk "<> Case Fold"))
;;       ("R" . (isearch-toggle-regexp :wk "<> Regexp Search")))

(setup (:pkg isearch-mb :fetcher git :url "https://github.com/astoff/isearch-mb")
   (:option isearch-allow-scroll t
            ;; Match count next to the minibuffer prompt
            isearch-lazy-count t
            ;; Don't be stingy with history; default is to keep just 16 entries
            search-ring-max 200
            regexp-search-ring-max 200
            isearch-search-fun-function 'isearch-function-with-pinyin
            search-invisible nil)

   (:with-map isearch-mb-minibuffer-map
     (:bind
      "C-n" 'isearch-repeat-forward
      "C-p" 'isearch-repeat-backward
      "C-l" 'ctrlf-recenter-top-bottom
      "C-." 'toki-search-insert-\.*))

   (defun isearch-function-with-pinyin ()
     "Wrap for Pinyin searching."
     ;; Return the function to use for pinyin search
     `(lambda (string bound noerror)
        (funcall (if ,isearch-forward
                     're-search-forward
                     're-search-backward)
                 (re-builder-extended-pattern string) bound noerror)))

   (defadvice isearch-update (before my-isearch-update activate)
     (sit-for 0)
     (if (and
          ;; not the scrolling command
          (not (eq this-command 'isearch-other-control-char))
          ;; not the empty string
          (> (length isearch-string) 0)
          ;; not the first key (to lazy highlight all matches w/o recenter)
          (> (length isearch-cmds) 2)
          ;; the point in within the given window boundaries
          (let ((line (count-screen-lines (point) (window-start))))
            (or (> line (* (/ (window-height) 4) 3))
                (< line (* (/ (window-height) 9) 1)))))
         (let ((recenter-position 0.3))
           (recenter '(4)))))

   (defun ctrlf-recenter-top-bottom ()
     "Display current match in the center of the window (by default).
Successive calls, or calls with prefix argument, may have
different behavior, for which see `recenter-top-bottom'."
     (interactive)
     (with-selected-window
         (minibuffer-selected-window)
       (recenter-top-bottom)))

   (define-advice isearch-mb--update-prompt (:around (fn &rest args) show-case-fold-info)
     "Show case fold info in the prompt."
     (cl-letf* ((isearch--describe-regexp-mode-orig
                 (symbol-function 'isearch--describe-regexp-mode))
                ((symbol-function 'isearch--describe-regexp-mode)
                 (lambda (regexp-function &optional space-before)
                   (concat (if isearch-case-fold-search "[Case Fold] " "")
                           (funcall isearch--describe-regexp-mode-orig
                                    regexp-function space-before)))))
       (funcall fn args)))

   (isearch-mb-mode))

(setup (:pkg plur)
  (:global "C-c M-%" plur-query-replace)
  (:with-map isearch-mode-map
    (:bind "M-{" #'plur-isearch-query-replace)))

(provide 'init-find)
