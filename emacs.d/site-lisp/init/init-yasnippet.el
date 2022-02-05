; -*- coding:utf-8 -*-
(setup (:pkg dropdown-list))

(setup (:pkg yasnippet)
   (:delay)
   ;; (:bind "<tab>" #'nil)
   (:with-map yas-keymap
     (:bind
      ;; yas map when expansion in action
      "M-p" #'yas-prev-field
      "TAB" #'nil
      "<tab>" #'nil
      "M-n" #'yas-next-field
      ;; Keys can be written by their ASCII code, using a backslash
      ;; followed by up to six octal digits. This is the only way to
      ;; represent keys with codes above \377.
      "\7" #'nil
      "M-ESC" #'yas-abort-snippet))
   (:global "C-c l" #'yas-insert-snippet)
   ;; @see http://stackoverflow.com/questions/7619640/emacs-latex-yasnippet-why-are-newlines-inserted-after-a-snippet
   ;; (setq-default mode-require-final-newline nil)
   (:option yas-snippet-dirs (list "~/.emacs.d/snippets")
            yas-fallback-behavior nil
            ;; give yas/dropdown-prompt in yas/prompt-functions a chance
            yas-prompt-functions '(yas-dropdown-prompt
                                   yas-ido-prompt yas-completing-prompt))
   (:hook (lambda ()
            (yas-activate-extra-mode 'fundamental-mode)))

   (:when-loaded
     ;; use yas/completing-prompt when ONLY when `M-x yas-insert-snippet'
     ;; thanks to capitaomorte for providing the trick.
     (defadvice yas-insert-snippet (around use-completing-prompt activate)
       "Use `yas-completing-prompt' for `yas-prompt-functions' but only here..."
       (let ((yas-prompt-functions '(yas-completing-prompt))) ad-do-it))

     (yas-global-mode 1)))

(setup (:pkg auto-yasnippet)
   ;; usage:
   ;; 1 one line field$ = document.getElementById("|"); at | call aya-create, the aya-expand, M-n move to the next field
   ;; 2 multi placeholder count_of_~red = get_total("~red");
   ;; 3 arbitrary text, Emacs-style backticks:
   ;; `red'_total = get_total("`red'_values")
   (:autoload aya-create aya-open-line))

; * tiny
;  a single C-_ will undo the whole thing and allow you to edit the code
; m{range start:=0}{separator:= }{range end}{Lisp expr:=indentity}|{format expr:=%d}

; ** example 1 %x hex
; m1;\n7*xx|hex: 0x%x
;; integer range start: 1
;; integer range end: 7
;; separator to join the expressions: ";\n"
;; Elisp expression to transform the linear range: (* x x)
;; format expression for the result: "hex: 0x%x"
; ** example 2 %d integer
;; m1\n\n20x|@article{%d,title={%d},}

;; (mapconcat
;;  (lambda (x)
;;    (format "hex: 0x%x"
;;            (* x x)))
;;  (number-sequence 1 7)
;;  ";\n")

(setup (:pkg tiny)
   (:autoload tiny-expand))

(setup (:pkg hippie-completing-read)
   (:global [remap dabbrev-expand] #'hippie-completing-read)
   (:autoload hippie-completing-read))

;; (setup (:pkg hippie-exp)                ; Powerful expansion and completion
;;    (:option hippie-expand-try-functions-list
;;             '(try-expand-dabbrev
;;               try-expand-dabbrev-all-buffers
;;               try-expand-dabbrev-from-kill
;;               try-complete-file-name-partially
;;               try-complete-file-name
;;               try-expand-all-abbrevs
;;               try-expand-list
;;               try-complete-lisp-symbol-partially
;;               try-complete-lisp-symbol)))

(provide 'init-yasnippet)
