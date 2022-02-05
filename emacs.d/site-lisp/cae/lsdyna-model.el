;; lsdyna-model.el
;; $Id: lsdyna.el,v 1.1.1.1 2003/06/30 01:07:51 jorgen Exp $
;;
;; Author:   Jorgen S Bergstrom <jorgen@polymerFEM.com>
;; URL:      http://polymerFEM.com
;; 
;; Installation:
;;    add the following lines to your .emacs file:
;;
;;       ; lsdyna
;;       (autoload 'lsdyna-mode "lsdyna" "Enter ls-dyna mode." t)
;;       (setq auto-mode-alist (cons '("\\.k\\'" . lsdyna-mode) auto-mode-alist))
;;
;;    copy this file the emacs site-lisp directory:
;;
;;       cp lsdyna.el [path to emacs site-lisp directory]

(defvar lsdyna-mode-hook nil)
(defvar lsdyna-load-hook nil)

(defvar lsdyna-ruler "
$...:....1....:....2....:....3....:....4....:....5....:....6....:....7....:....8
"
  "*The ruler `lsdyna-insert-ruler' inserts."
)

(defun lsdyna-insert-ruler ()
  "Insert a ruler with comments."
  (interactive)
  (end-of-line)
  (insert lsdyna-ruler)
)

(defvar lsdyna-font-lock-keywords
  (list
   '("^\$.*$" . font-lock-comment-face) ;; comments
   '("^*.*[^a-zA-Z]" . font-lock-keyword-face) ;; keywords
   '("[\t]+" . highlight) ;; tabs
   '("[ ]+$" . highlight) ;; spaces just before end of line
  )
)

(defun lsdyna-mode ()
  "Major mode for editing ls-dyna files."
  (interactive)
  (setq mode-name "lsdyna")
  (setq major-mode 'lsdyna-mode)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(lsdyna-font-lock-keywords))
  (run-hooks 'lsdyna-mode-hook)
)

(provide 'lsdyna-mode)
(run-hooks 'lsdyna-load-hook)
