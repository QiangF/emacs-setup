;;; openwith.el --- Open files with external programs -*- lexical-binding: t; -*-

;;; Commentary:

;; This lets you associate external applications with files so that
;; you can open them via shortcut in dired and wdired.

;; Copy openwith.el to your load-path and add to your .emacs:

;;    (require 'openwith)

;; To customize associations etc., use:

;;    M-x customize-group RET openwith RET

;;; Code:

(defgroup openwith nil
  "Associate external applications with file name patterns."
  :group 'files
  :group 'processes)

(defcustom openwith-associations 'nil
  "Associations of file patterns to external programs.
File pattern is a regular expression describing the files to
associate with a program. The program arguments are a list of
strings and symbols and are passed to the program on invocation,
where the symbol 'file' is replaced by the file to be opened."
  :group 'openwith
  :type '(repeat (list (regexp :tag "Files")
                       (string :tag "Program")
                       (sexp :tag "Parameters"))))

(defcustom openwith-confirm-invocation nil
  "Ask for confirmation before invoking external programs."
  :group 'openwith
  :type 'boolean)

(defun openwith-make-extension-regexp (strings)
  "Make a regexp that matches a string that starts with a '.',
has any of the supplied STRINGS, and is at the end of the
string."
  (concat "\\." (regexp-opt strings) "$"))

(defun openwith-open-unix (command arglist)
  "Run external command COMMAND, in such a way that it is
  disowned from the parent Emacs process.  If Emacs dies, the
  process spawned here lives on.  ARGLIST is a list of strings,
  each an argument to COMMAND."
  (let ((shell-file-name "/bin/sh"))
    (start-process-shell-command
     "openwith-process" nil
     (concat
      "exec nohup " command " " 
      (mapconcat 'shell-quote-argument arglist " ")
      " >/dev/null"))
     ))

(defun openwith-open-windows (file)
  "Run external command COMMAND, in such a way that it is
  disowned from the parent Emacs process.  If Emacs dies, the
  process spawned here lives on.  ARGLIST is a list of strings,
  each an argument to COMMAND."
  (w32-shell-execute "open" file))

(defun dired-openwith ()
  "Open file with external program, if an association is configured."
  (interactive)
  (when (or (equal major-mode 'dired-mode) (equal major-mode 'wdired-mode))
    (let* ((assocs openwith-associations)
           (file (dired-get-file-for-visit)))
      ;; do not use `dolist' here, since some packages (like cl)
      ;; temporarily unbind it
      (while assocs
        (setq oa (car assocs)
              assocs (cdr assocs))
        (when (save-match-data (string-match (car oa) file))
          (let ((params (mapcar (lambda (x) (if (eq x 'file) file x))
                                (nth 2 oa))))
            (when (or (not openwith-confirm-invocation)
                      (y-or-n-p (format "%s %s? " (cadr oa)
                                        (mapconcat #'identity params " "))))
	      (if (eq system-type 'windows-nt)
		  (openwith-open-windows file)
	        (openwith-open-unix (cadr oa) params))

              (when (featurep 'recentf)
                (recentf-add-file file))
              ;; inhibit further actions
              (error "Opened %s in external program"
                     (file-name-nondirectory file)))))))))

(provide 'openwith)

;;; openwith.el ends here
