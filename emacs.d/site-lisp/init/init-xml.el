; -*- coding:utf-8 -*-

; odt shema
; install nxml-mode
;; (use-package rng-loc
;;   :init
;;   (add-to-list 'rng-schema-locating-files "~/.dotfiles/emacs/org/schema/schemas.xml")
;; )

(defun pprint-xml (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this. The function inserts linebreaks to separate tags that have
nothing but whitespace between them. It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n") (setq end (1+ end)))
    (indent-region begin end)))

(defun my-xml-format ()
  "Format an XML buffer with `xmllint' (sudo apt-get install libxml2-utils)."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           "xmllint -format -"
                           (current-buffer) t
                           "*Xmllint Error Buffer*" t))

(defun my-pprint-xml ()
  (interactive)
  (pprint-xml (point-min) (point-max)))

(defun sgml-setup ()
  (setq sgml-xml-mode t)
  ;; (toggle-truncate-lines) ; This seems to slow down Emacs.
  (turn-off-auto-fill))
(add-hook 'sgml-mode-hook 'sgml-setup)

(provide 'init-xml)
