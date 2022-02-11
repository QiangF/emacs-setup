; -*- coding:utf-8 -*-

;; evil in iedit mode
;; D: clear the region
;; C: clear to end-of-region and go into insert mode
;; A: go into insert mode at end-of-region
;; I: go into insert mode at start-of-region
;; V: select the region
;; iedit-show/hide-unmatched-lines
;; org-copy-visible (C-c C-x v) to copy the visible content of the current region to the kill ring
;; (buffer-substring-of-unpropertied start end 'invisible)
;; M-;" 'iedit-toggle-selection
(setup (:pkg iedit)
   (:global* "C-;" #'iedit-mode)
   (:with-map iedit-mode-keymap
     (:bind "C-;" #'iedit-mode
            "C-h" nil))
   (:option iedit-search-invisible nil))

;; You search for a word in the buffer/region, type in the
;; replacement and confirm each one by pressing y or n or just press
;; ! to apply this to everything.
(setup (:pkg visual-regexp-steroids)
  (:autoload vr/select-query-replace))

;; (("M-m SPC SPC" #'vr/query-replace))
(setup (:pkg visual-regexp))

; renaming workflow
; use rgrep or ag-project-regexp to search for pattern, wgrep and iedit to rename

; * allignment
; ** zop-to-char
; ** tip
;; Works in minibuffer
;; You can change direction with C-b and C-f. When starting at end of buffer zop-to-char search automatically backward.
;; You can use zop-to-char to move to a place (use C-q).
;; Hit repetitively the character you are searching will move to next.
;; You can copy or kill region from point to last search point.
;; C-g will quit and bring you back to initial position.

;; (setup zop-to-char                ; Better zapping
;;  :pkg t
;;  :bind (("M-z" #'zop-to-char)
;;         ("M-Z" #'zop-up-to-char)))

(setup (:pkg avy-zap)
  (:global "M-z" #'avy-zap-to-char-dwim
           "M-Z" #'avy-zap-up-to-char-dwim))

(setup (:pkg align)                     ; Align text in buffers
  (:global "C-c x a a" #'align
           "C-c x a c" #'align-current))
; * regexp 
(setq reb-re-syntax 'string)

; * expand region
; ** tips
; C-M-h select defun
;; evil
;; There’s a text object for every “step” of expansion that expand-region provides (and more). To select the word at point = viw, symbol at point = vio, line at point = V, the block at point (by indentation) = vii, the block at point (by braces) = vib, sentence at point = vis, paragraph = vip, and so on.

;; Selection expansion can be emulated by using text objects consecutively: viw to select a word, followed by io to expand to a symbol, then ib expands to the surrounding brackets/parentheses, etc. There is no reverse of this however; you’d have to restart visual state.

(setup (:pkg expand-region)
  (:global "C-," #'er/expand-region))

;; (eval-after-load "evil" '(setq expand-region-contract-fast-key "z"))

(provide 'init-iedit)
