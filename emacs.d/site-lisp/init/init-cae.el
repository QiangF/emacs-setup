; -*- coding:utf-8 -*-

; fortran
(setq fortran-comment-region "c")
(define-coding-system-alias 'mbcs 'utf-8)

(setup abaqus-mode
  (:file-match "\\.inp\\'")
  (:file-match "\\.pes\\'")
  (:hook turn-on-font-lock outline-minor-mode))

(setup deform-mode
  (:file-match "\\.key\\'")
  (:hook turn-on-font-lock outline-minor-mode))

(setup marc-mode
  (:file-match "\\.dat\\'")
  (:hook turn-on-font-lock outline-minor-mode))

(setup gcode
  (:file-match "\\.nc\\'")
  (:file-match "\\.cnc\\'")
  (:hook turn-on-font-lock outline-minor-mode))

(provide 'init-cae)
