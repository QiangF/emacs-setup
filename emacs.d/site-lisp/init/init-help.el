; -*- coding:utf-8 -*-
; * which key
;; C-h followed by
;; The commands are:
;;     Cycle through the pages forward with n (or C-n)
;;     Cycle backwards with p (or C-p)
;;     Undo the last entered key (!) with u (or C-u)
;;     Call the default command bound to C-h, usually describe-prefix-bindings, with h (or C-h)
;; This is especially useful for those who like helm-descbinds but also want to use C-h as their which-key paging key.
;; Note C-h is by default equivalent to ? in this context.

;; use tab to jump between links
(setup help-mode
  (:bind "j" 'next-line
         "k" 'previous-line
         "H" 'describe-mode
         "h" 'backward-char
         "L" 'help-go-back
         "l" 'forward-char
         "v" 'recenter-top-bottom
         "C-M-i" nil
         "c" 'counsel-ace-link))

(setup info
   (:with-map Info-mode-map
     (:bind
      "w" 'forward-word
      "b" 'backward-word
      "t" 'hydra-info-to/body
      "u" 'Info-history-back
      "c" 'counsel-ace-link
      "H" 'Info-history-back)))

(setup man
  (:with-map Man-mode-map
    (:bind
     "w" 'forward-word
     "b" 'backward-word)))

;; helpful-variable, which-key-show-full-keymap
(setup which-key
  (setq which-key-idle-delay 1.0)
  (which-key-mode))

                                        ; C-M-q reindent, C-M-f,b,u,d work on s-expression
(setup elisp-demos
  (:load-after helpful)
  (:when-loaded
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
    (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)))

(setup auto-compression-mode
  (:hook-into help-mode)
  (:when-loaded
    (auto-compression-mode 1)))

;; https://www.emacswiki.org/emacs/download/help-fns%2b.el
;; https://www.emacswiki.org/emacs/HelpPlus
(setup (:pkg helpful)
  (:delay))

(provide 'init-help)
