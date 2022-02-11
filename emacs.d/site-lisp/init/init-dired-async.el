; -*- coding:utf-8 -*-

;; (setup dired-async
;;   :disabled
;;   :ensure async
;;   :commands dired-async-mode
;;   :hook (dired-mode #'my-set-dired-async)
;;   :config
;;   (define-minor-mode dired-async--modeline-mode
;;     "Notify mode-line that an async process run."
;;     :group 'dired-async
;;     :global t
;;     (unless dired-async--modeline-mode
;;       (let ((visible-bell t)) (ding))
;;       (setq mode-line-process
;;             (let ((n (length (dired-async-processes))))
;;               (when (> n 0)
;;                 (format " [%s Async job(s) running]" n))))))

;;   (defun my-set-dired-async ()
;;     (dired-async-mode 1)
;;     (dired-async--modeline-mode 1)))

;; (setup dired-rsync
;;     ;; support mark file in dired
;;     (bind-key "C-c C-r" 'dired-rsync dired-mode-map)
;; )

(setup tmtxt-dired-async
   (:load-after dired)
   (:with-map dired-mode-map
     (:bind "C-c c" #'tda/rsync-multiple-mark-file
            "C-c p" #'my-dired-paste
            "C-c m" #'my-dired-move
            ;; ("C-c C-a" #'tda/rsync-multiple-mark-file)
            "C-c e" #'tda/rsync-multiple-empty-list
            "C-c d" #'tda/rsync-multiple-remove-item
            ;; ("C-c C-v" #'tda/rsync-multiple)
            "C-c s" #'tmtxt/dired-async-get-files-size))
   (:require tmtxt-async-tasks)
   (:when-loaded
     (defun dired-rsync (dest)
       (interactive
        (list
         (expand-file-name
          (read-file-name
           "Rsync to:"
           (dired-dwim-target-directory)))))
       ;; store all selected files into "files" list
       (let ((files (dired-get-marked-files
                     nil current-prefix-arg))
             ;; the rsync command
             (tmtxt/rsync-command
               "rsync -arvz --progress "))
         ;; add all selected file names as arguments
         ;; to the rsync command
         (dolist (file files)
           (setq tmtxt/rsync-command
                 (concat tmtxt/rsync-command
                         (shell-quote-argument file)
                         " ")))
         ;; append the destination
         (setq tmtxt/rsync-command
               (concat tmtxt/rsync-command
                       (shell-quote-argument dest)))
         ;; run the async shell command
         (async-shell-command tmtxt/rsync-command "*rsync*")
         ;; finally, switch to that window
         (other-window 1)))))

(provide 'init-dired-async)
