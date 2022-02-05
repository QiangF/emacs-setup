; -*- coding:utf-8 -*-

(setup (:pkg s))

(setup (:pkg dired-ranger)
   (:load-after dired)
   (:with-map dired-mode-map
     (:bind "[" hydra-dired-ranger/body))
   (:when-loaded
     (:require s)
     (defhydra hydra-dired-ranger (:color blue)
       ("c" dired-ranger-copy)
       ("p" dired-ranger-paste)
       ("h" jmm/dired-ranger-hardlink)
       ("s" jmm/dired-ranger-symlink)
       ("r" jmm/dired-ranger-relsymlink)
       ("m" dired-ranger-move)
       ("d" jmm/latest-download-to-ranger)
       ("<escape>" nil "quit"))

     (defmacro jmm/make-dired-ranger-action (newfuncname actionname functiontocall char)
       "Make dired-ranger paste commands, but for other things like
symlink, hardlink, relsymlink, etc."
       `(defun ,newfuncname (arg)
          ,(format "%s the items from copy ring to current directory.

With raw prefix argument \\[universal-argument], do not remove
the selection from the stack so it can be copied again.

With numeric prefix argument, %s the n-th selection from the
copy ring." (s-capitalize actionname) (s-downcase actionname))
          (interactive "P")
          (let* ((index (if (numberp arg) arg 0))
                 (data (ring-ref dired-ranger-copy-ring index))
                 (files (cdr data))
                 (target-directory (dired-current-directory))
                 (pasted-files 0))
            (--each files (when (file-exists-p it)
                            (,functiontocall it target-directory)
                            (cl-incf pasted-files)))
            (dired-ranger--revert-target ,char target-directory files)
            (unless arg (ring-remove dired-ranger-copy-ring 0))
            (message (format ,(format "%sed %%d/%%d item%%s from copy ring." (s-capitalize actionname))
                             pasted-files
                             (length files)
                             (if (> (length files) 1) "s" ""))))))

     (jmm/make-dired-ranger-action jmm/dired-ranger-hardlink "Hardlink" dired-hardlink ?H)
     (jmm/make-dired-ranger-action jmm/dired-ranger-symlink "Symlink" make-symbolic-link ?S)
     (jmm/make-dired-ranger-action jmm/dired-ranger-relsymlink "Relsymlink" dired-make-relative-symlink ?R)

     (defcustom jmm/downloads-directory "~/Downloads"
       "Directory where downloads are usually stored."
       :type 'directory)

     (defun jmm/latest-download-to-ranger ()
       "Add the latest file in `jmm/downloads-directory' to dired-ranger's copy ring.
Like `dired-ranger-copy' but I don't have to manually select the latest download."
       (interactive)
       (let ((latest-file (josh/latest-file jmm/downloads-directory "[:alpha:]")))
         (ring-insert
          dired-ranger-copy-ring
          (cons (list (dired-noselect jmm/downloads-directory)) (list latest-file)))
         (message (format "Copied %s into copy ring." latest-file))))))

(provide 'init-dired-ranger)
