; -*- coding:utf-8 -*-

; * highlight
(global-hl-line-mode)
(setq global-hl-line-sticky-flag t)

(setup (:pkg rainbow-delimiters)        ; Highlight delimiters by depth
       (:hook-into text-mode prog-mode))

(setup (:pkg hi-lock)                    ; Custom regexp highlights
       (global-hi-lock-mode))

(setup (:pkg highlight-numbers)        ; Fontify number literals
       (:hook-into prog-mode))

;; 加一个prefix key高亮的时候输入symbol-overlay-map字符
(setup (:pkg symbol-overlay)
  (:hook-into prog-mode)
  ;; (:bind "h" nil)
  (:when-loaded
    (defhydra hydra-highlight (:color pink)
      ("<" symbol-overlay-switch-first)
      (">" symbol-overlay-switch-last)
      ("h" symbol-overlay-put)
      ("n" symbol-overlay-jump-next)
      ("p" symbol-overlay-jump-prev)
      ("b" symbol-overlay-switch-backward)
      ("f" symbol-overlay-switch-forward)
      ("s" symbol-overlay-mode)
      ("r" symbol-overlay-remove-all)
      ("<escape>" nil "quit"))

    (defun symbol-overlay-switch-first ()
      (interactive)
      (let* ((symbol (symbol-overlay-get-symbol))
             (keyword (symbol-overlay-assoc symbol))
             (a-symbol (car keyword))
             (before (symbol-overlay-get-list a-symbol 'car))
             (count (length before)))
        (symbol-overlay-jump-call 'symbol-overlay-basic-jump (- count))))

    (defun symbol-overlay-switch-last ()
      (interactive)
      (let* ((symbol (symbol-overlay-get-symbol))
             (keyword (symbol-overlay-assoc symbol))
             (a-symbol (car keyword))
             (after (symbol-overlay-get-list a-symbol 'cdr))
             (count (length after)))
        (symbol-overlay-jump-call 'symbol-overlay-basic-jump (- count 1))))))

;; nav-flash
(setup (:pkg beacon)
  (:option beacon-blink-when-point-moves t
           beacon-color "#00f600"
           blink-when-window-changes t
           beacon-blink-when-point-moves-vertically 1
           beacon-blink-when-point-moves-horizontally 8
           beacon-blink-delay 0.4)
  (beacon-mode 1))

;; (setup (:pkg) highlight-symbol        ; Highlighting and commands for symbols
;;   (("C-c s %" . highlight-symbol-query-replace)
;;    ("C-c s n" . highlight-symbol-next-in-defun)
;;    ("C-c s p" . highlight-symbol-prev-in-defun))
;;   ;; Navigate occurrences of the symbol under point with M-n and M-p, and
;;   ;; highlight symbol occurrences
;;   :init
;;   (dolist (fn '(highlight-symbol-nav-mode highlight-symbol-mode))
;;     (add-hook 'prog-mode-hook fn))
;;   :config
;;   (setq highlight-symbol-idle-delay 0.4        ; Highlight almost immediately
;;     highlight-symbol-on-navigation-p t) ; Highlight immediately after
;;                     ; navigation
;;   :diminish highlight-symbol-mode)

;; ; * highlight column
;; ;; cpu intensive
;; (setup (:pkg) col-highlight
;;     :if (display-graphic-p)
;;     :config
;;     (column-highlight-mode)
;;     (global-hl-line-mode 1)
;;     (set-face-attribute hl-line-face nil :background "gray30" :foreground nil :overline t)
;;     (set-face-attribute col-highlight-face nil :background "gray10" :foreground nil :overline nil)
;; )

(setup (:pkg stripe-buffer)             ; Add stripes to a buffer
  ;; (org-mode . turn-on-stripe-table-mode)
  ;; band alternate line shading in dired
  (:hook-into dired-mode)

  ;; (add-hook 'stripe-buffer-mode-hook #'hl-line-mode)
  ;; (defun stripe-set-global-hl ()
  ;;   (when (or stripe-buffer-mode stripe-table-mode)
  ;;     (overlay-put global-hl-line-overlay 'priority 10)))
  ;; see stripe-buffer.el
  ;; in hl-line.el global-hl-line-overlay has priority -50
  ;; (advice-add 'global-hl-line-highlight :after #'stripe-set-global-hl)
  ;; (advice-add 'hl-line-highlight :after #'stripe-set-global-hl)
  (:when-loaded
    (set-face-attribute stripe-highlight-face nil :background "gray10" :foreground nil)

    ;; Decrease sb/overlay priority
    (defadvice sb/redraw-region (after stripe-set-priority activate)
      (when (or stripe-buffer-mode stripe-table-mode)
        (dolist (overlay sb/overlays)
          (overlay-put overlay 'priority -100))))

    (defun list-overlays-at (&optional pos)
      "Describe overlays at POS or point."
      (interactive)
      (setq pos (or pos (point)))
      (let ((overlays (overlays-at pos))
            (obuf (current-buffer))
            (buf (get-buffer-create "*Overlays*"))
            (props '(priority window category face mouse-face display
                     help-echo modification-hooks insert-in-front-hooks
                     insert-behind-hooks invisible intangible
                     isearch-open-invisible isearch-open-invisible-temporary
                     before-string after-string evaporate local-map keymap
                     field))
            start end text)
        (if (not overlays)
            (message "None.")
            (set-buffer buf)
            (erase-buffer)
            (dolist (o overlays)
              (setq start (overlay-start o)
                    end (overlay-end o)
                    text (with-current-buffer obuf
                           (buffer-substring start end)))
              (when (> (- end start) 13)
                (setq text (concat (substring text 1 10) "...")))
              (insert (format "From %d to %d: \"%s\":\n" start end text))
              (dolist (p props)
                (when (overlay-get o p)
                  (insert (format " %15S: %S\n" p (overlay-get o p))))))
            (pop-to-buffer buf))))))

;; (setup (:pkg) crosshairs
;;     :ensure t
;;     :config
;;     (setq col-highlight-overlay-priority 900)
;      (set-face-attribute col-highlight-face nil :background "gray10" :foreground nil)
;      (set-face-attribute vline-visual-face nil :background "gray10" :foreground nil)
;; )

; ** scroll indicator on-screen
(setup (:pkg on-screen)
  (:option on-screen-highlight-method 'narrow-line
           on-screen-inverse-flag t
           on-screen-highlighting-to-background-delta nil
           on-screen-delay 5)
  (on-screen-global-mode 1)
  (set-face-underline 'on-screen-narrow-line t))
       
;; diredful

; ** indentguide
;; package indent-guide too slow
;; (setup (:pkg highlight-indent-guides)
;;   :commands highlight-indent-guides-mode
;;   :hook (prog-mode . highlight-indent-guides-mode)
;;   ;; (add-hook 'text-mode-hook 'highlight-indent-guides-mode)
;;   ;; highlight-indent-guides is very slow for large org file
;;   :config
;;     (setq jit-lock-defer-time 0.1)
;;     (setq highlight-indent-guides-method 'character)
;;     (setq highlight-indent-guides-character ?\|)
;;     (setq highlight-indent-guides-auto-odd-face-perc 15)
;;     (setq highlight-indent-guides-auto-even-face-perc 15)
;;     (setq highlight-indent-guides-auto-character-face-perc 20)
;;     ;; (setq highlight-indent-guides-responsive 'stack)
;;     (setq highlight-indent-guides-responsive nil)
;;     (setq highlight-indent-guides-delay 1))

(setup electric-indent
  (:hook-into prog-mode))

(setup (:pkg indent-tools))
     
; * smart shift
(setup (:pkg smart-shift))

(setq-default tab-width 4
              indent-tabs-mode nil)
; indent
; (tab-to-tab-stop) indent to the next tab position, according to tab-width
; (indent-relative)

(provide 'init-highlight)
