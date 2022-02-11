;; abaqus.el
;; $Id: abaqus.el,v 1.5 2002/07/03 12:13:43 jorgen Exp $
;;
;; Author:   Jorgen S Bergstrom <jorgen@polymerFEM.com>
;; URL:      http://www.polymerFEM.com
;;
;; Modified by: James Lockley <safricanjames@yahoo.co.uk>
;;              Martin Lüthi
;;
;; Installation:
;;    add the following lines to your .emacs file:
;;
;;       ; abaqus
;;       (add-hook 'abaqus-mode-hook 'turn-on-font-lock)
;;       (autoload 'abaqus-mode "abaqus" "Enter abaqus mode." t)
;;       (setq auto-mode-alist (cons '("\\.inp\\'" . abaqus-mode) auto-mode-alist))
;;
;;    copy this file the emacs site-lisp directory:
;;
;;       cp abaqus.el [path to emacs site-lisp directory]

(defvar abaqus-mode-hook nil)

(defvar abaqus-ruler "**..:....1....:....2....:....3....:....4....:....5....:....6....:....7....:....8"
  "*The ruler `abaqus-insert-ruler' inserts."
)

(defun abaqus-insert-ruler ()
  "Insert a ruler with comments."
  (interactive)
  (end-of-line)  
  (insert abaqus-ruler)
)

(defvar abaqus-font-lock-defaults
  `((
   ("^[*][*].*$" . font-lock-comment-face)
   ("^#.*$" . font-lock-comment-face)
   ("^\*[a-zA-Z].*[^a-zA-Z]" . font-lock-keyword-face)
   ("^[ \t]+$" . highlight)
  )))

(defvar abaqus-comment-prefix "** "
  "*The comment `abaqus-insert-comment' inserts."
)

(define-derived-mode abaqus-mode fundamental-mode "abaqus input file"
    ;; for comments
    ;; overriding these vars gets you what (I think) you want
    ;; they're made buffer local when you set them
    (setq font-lock-defaults abaqus-font-lock-defaults)
    (setq comment-start abaqus-comment-prefix)
    (setq comment-end "")
	(setq require-final-newline  t)
    (run-mode-hooks 'abaqus-mode-hook))

(provide 'abaqus-mode)
