; -*- coding:utf-8 -*-

;; C-M-{p . n} Sync previous, selected or next notes
(setup (:pkg org-noter)
   (:option org-noter-doc-split-fraction '(0.7 . 0.7)
            org-noter-notes-search-path "/home/q/my_lib/annex/notes/"
            org-noter-always-create-frame nil))

;; run pdf-tools-install on first use
;; o outline, tab to expand
;; e to open file in dired buffer, if pdf-view-mode is not enabled, enable it manually
(setq doc-view-continuous t)

(setup pdf-tools
   (:disabled)
   (:file-match "\\.pdf\\'")
   (:file-match "\\.PDF\\'")
   (:autoload my-org-noter)
   (:bind-into pdf-view-mode-map
     "j" #'my-scroll-other-window-down
     "k" #'my-scroll-other-window-up
     "C-M-." #'my-org-noter)
   (:when-loaded
     (defun my-org-noter ()
       (interactive)
       (let ((notes-file (concat (file-name-sans-extension (pdf-view-buffer-file-name)) ".org")))
         (message "%s" notes-file)
         (unless (file-exists-p notes-file) (write-region "" nil notes-file))
         (org-noter)))
     (autoload 'pdf-view-mode "pdf-view" "\
    Major mode in PDF buffers.

    PDFView Mode is an Emacs PDF viewer.  It displays PDF files as
    PNG images in Emacs buffers." t nil)

     (add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdf-view-mode))

     ;; solarized dark
     (setq pdf-view-midnight-colors '("#ffffc0" . "#101500"))
     (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
     (defun my-scroll-other-window-down (&optional arg)
       (interactive "P")
       (pdf-view-scroll-up-or-next-page)
       (pdf-view-scroll-up-or-next-page)
       (other-window 1)
       (pdf-view-scroll-up-or-next-page)
       (pdf-view-scroll-up-or-next-page)
       (other-window 1))

     (defun my-scroll-other-window-up (&optional arg)
       (interactive "P")
       (pdf-view-scroll-down-or-previous-page)
       (pdf-view-scroll-down-or-previous-page)
       (other-window 1)
       (pdf-view-scroll-down-or-previous-page)
       (pdf-view-scroll-down-or-previous-page)
       (other-window 1))))

;; scroll-other-window
;; (sow-scroll-other-window sow-scroll-other-window-down)

(provide 'init-pdf)
