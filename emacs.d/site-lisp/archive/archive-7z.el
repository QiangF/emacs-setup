;;; archive-7z.el --- Archive mode extensions for 7z

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords:
;; Emacs: GNU Emacs
;; Package-Requires: ()


;;; Commentary:
;; 

;;; Code:

(defvar archive-7z-mode-hook nil)

(defcustom archive-7z-program
  (cond
   ((memq system-type '(windows-nt))
    (if (getenv "ProgramFiles")
	(expand-file-name "7-zip/7z.exe" (getenv "ProgramFiles"))
      "c:/Program Files/7-Zip/7z.exe"))
   (t
    ;;TODO
    "7z"))
  "*Program name of 7-zip.")

(defadvice archive-find-type 
  (around archive-find-type-ad () activate)
  (condition-case err
      ad-do-it
    (error
     (setq ad-return-value
	   (cond
	    ;;TODO correct or not
;; 	    ((looking-at "7z\274\257") '7z)
 	    ((looking-at "7z") '7z)
	    (t 
	     (signal (car err) (cdr err))))))))
  
(defun archive-7z-summarize ()
  (let ((filename buffer-file-name)
	files visual
	header footer)
    (with-temp-buffer
      ;;todo archive file in archive.
      (call-process archive-7z-program nil t nil "l" filename)
      (goto-char (point-min))
      (when (re-search-forward "^--" nil t)
	(setq header (buffer-substring (line-beginning-position) (line-end-position)))
	(save-excursion
	  (forward-line -1)
	  (setq header (concat (buffer-substring (point) (line-end-position)) "\n" header)))
	(forward-line 1)
	(while (looking-at "^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\) \\(.....\\) \\([ 0-9]+\\) \\([ 0-9]+\\)  \\(.*\\)" )
	  (let (ifnname text 
			;; TODO
			efnname fiddle mode
			)
	    (setq text (match-string 0)
		  ifnname (match-string 5))
	    (setq efnname ifnname) ;;TODO
	    (setq mode ?\777)
	    (setq files (cons 
			 (vector efnname ifnname fiddle mode)
			 files))
	    (setq visual (cons (vector text
				       (- (length text) (length ifnname))
				       (length text))
			       visual))
	    (forward-line 1)))
	(setq footer (concat (buffer-substring (line-beginning-position) (line-end-position)) "\n"))
	(forward-line 1)
	(setq footer (concat footer (buffer-substring (line-beginning-position) (line-end-position))))))
    (insert header "\n")
    (archive-summarize-files (nreverse visual))
    (insert footer "\n")
    (apply 'vector (nreverse files))))

(defun archive-7z-extract (archive ename)
  ;; TODO
  (unless (=(call-process archive-7z-program nil '(t nil) nil  "e" archive ename "-so") 0)
    (error "Failed extracting"))
  t)

;; (defun archive-7z-add-new-member ()
;;   ;; TODO
;;   )

;; (defun archive-7z-write-file-member ()
;;   ;; TODO
;;   )

;; (defun archive-7z-chmod-entry ()
;;   ;; TODO
;;   )

;; (defun archive-7z-chown-entry ()
;;   ;; TODO
;;   )

;; (defun archive-7z-chgrp-entry ()
;;   ;; TODO
;;   )

;; (defun archive-7z-expunge ()
;;   ;; TODO
;;   )

;; (defun archive-7z-rename-entry ()
;;   ;; TODO
;;   )

(add-to-list 'auto-mode-alist '("\\.7[zZ]$" . archive-mode))

(provide 'archive-7z)

;;; archive-7z.el ends here
