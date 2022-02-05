; -*- coding:utf-8 -*-

;; paradox-list-packages paradox-menu-visit-homepage paradox-menu-copy-homepage-as-kill
;; paradox--refresh-remote-data to get star count
;; use ~ in package list to mark obsolete package for deletion

;; better to disable paradox for complex packages like anaconda-mode, pdf-view-mode etc.
(setup (:pkg paradox)
   (:bind-into paradox-menu-mode-map
     "s" #'package-sort-by-version)
   (:delay)
   (:when-loaded
     ;; click on the heading also do sorting
     ;; evil:package help page f/ to go to link, enter to open
     (defun my-package-sort-by-version ()
       (interactive)
       ;; version is column 1
       (tabulated-list-sort 1)
       (goto-char (point-min))
       (goto-char (word-search-forward "melpa")))

     ;; (add-hook 'package-menu-mode-hook
     ;; (lambda () (setq tabulated-list-format
     ;;     (vconcat (mapcar (lambda (arg) (list (nth 0 arg) (nth 1 arg)
     ;;     (or (nth 2 arg) t)))
     ;;     tabulated-list-format)))))

     ;; not using mode-line
     (defun paradox--update-mode-line ())

     ;; (setq browse-url-browser-function 'eww-browse-url)
     (setq paradox-execute-asynchronously t)
     (setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "firefox")
     (paradox-enable)))

(provide 'init-paradox)
