; -*- coding:utf-8 -*-

;; In Emacs, "region active" means transient-mark-mode is on, which
;; roughly means you selected some text. In this state, you can bind
;; some "easy" bindings, i.e., bindings without modifiers.

(setq kill-do-not-save-duplicates t)

;; (defvar kill-ring-entry-length 3)

;; (string-match-p "^\\s-*$" last)
;; (string-empty-p last)
;; (defun my-kill-new (orig-fun string &optional replace)
;;   ;; use condition-case to prevent error buffer from popping up
;;   (condition-case nil
;;       (let ((last (substring-no-properties string)))
;;         (when (and last (> (length last) kill-ring-entry-length))
;;           (apply orig-fun string replace)))
;;     ((debug error) nil)))

;; (advice-add 'kill-new :around #'my-kill-new)

(defconst angel-transient-mode-map-alist
  `((mark-active
     ,@(let ((map (make-sparse-keymap)))
         ;; operations
         (define-key map "p" (lambda (b e)
                               (interactive "r") (delete-region b e) (yank)))
         (define-key map "x" #'exchange-point-and-mark)
         (define-key map ";" #'comment-dwim)
         (define-key map "y" #'kill-ring-save)
         (define-key map (kbd "C-y") #'kill-ring-save)
         (define-key map "Y" (lambda
                                 (b e)
                               (interactive "r")
                               (kill-new (buffer-substring b e))
                               (message "Region saved")))
         ;; isolate
         ;; (define-key map "s" #'isolate-quick-add)
         ;; (define-key map "S" #'isolate-long-add)
         ;; (define-key map "d" #'isolate-quick-delete)
         ;; (define-key map "D" #'isolate-long-delete)
         ;; (define-key map "c" #'isolate-quick-change)
         ;; (define-key map "C" #'isolate-long-change)
         ;; mark things
         ;; (define-key map "f" #'er/mark-defun)
         ;; (define-key map "w" #'er/mark-word)
         ;; (define-key map "W" #'er/mark-symbol)
         ;; (define-key map "P" #'mark-paragraph)
         ;; inner & outer
         ;; (define-key map "C-i" inner-map)
         ;; (define-key map "C-a" outer-map)
         ;; (define-key inner-map "q" #'er/mark-inside-quotes)
         ;; (define-key outer-map "q" #'er/mark-outside-quotes)
         ;; (define-key inner-map "b" #'er/mark-inside-pairs)
         ;; (define-key outer-map "b" #'er/mark-outside-pairs)
         ;; (define-key map "q" #'er/mark-inside-quotes)
         ;; (define-key map "b" #'er/mark-inside-pairs)

         ;; expand-region
         ;; (define-key map (kbd "C--") #'er/contract-region)
         ;; (define-key map (kbd "C-=") #'er/expand-region)
         map))))

;; (add-to-list 'emulation-mode-map-alists
;;              'angel-transient-mode-map-alist t)

; ** tips
; repeat C-y do yank-pop, to see the kill ring use M-y
;; M-w w: save word at point
;; M-w s: save sexp at point
;; M-w l: save list at point (enclosing sexp)
;; M-w d: save defun2 at point
;; M-w D: save current defun’s name
;; M-w f: save filename at point
;; M-w b: save buffer-file-name or default-directory.
;; SPC: cycle through things in easy-kill-alist (word at point, sexp at point, etc)
;; C-SPC: turn selection into an active region.
;; C-g: abort ?: help

(setup (:pkg easy-kill)                 ; Easy killing and marking on C-w
  (:global [remap kill-ring-save] #'easy-kill
           [remap mark-sexp] #'easy-mark
           ;; ("C-w" . backward-kill-word) ; it 's more efficient to kill word type again than backspace
           "C-x C-k" #'kill-region)

  (put 'kill-region 'interactive-form
       '(interactive
         (if (use-region-p)
             (list (region-beginning) (region-end))
             (list (line-beginning-position)
                   (line-beginning-position 2))))))

;; apt-get install gpaste
;; alternative 
;; https://github.com/cdown/clipmenu
;; ivy-clipmenu.el
;; (use-package gpastel
;;  :ensure t
;;  ;; :if (and window-system exwm_enable)
;;  :if window-system
;;  :init
;;    (require 'gpastel)
;;    (gpastel-mode 1)
;;  :config
;;    (defun gpastel--update-handler (action target index)
;;      (when (gpastel--handle-event-p action target index)
;;        (let ((interprogram-cut-function nil)
;;              (copied-text (gpastel-get-copied-text)))
;;          (unless (string= copied-text (car kill-ring))
;;            (push copied-text kill-ring)
;;            (run-hooks 'gpastel-update-hook))))	))

(setup delsel
  (:option delete-selection-mode t))

;; x-select-enable-primary - default nil; set this to t if you want the Emacs commands C-w and C-y to use the primary selection.
;; x-select-enable-clipboard - default t; set this to nil if you want the Emacs commands C-w and C-y to use the clipboard selection.
(setq x-select-enable-clipboard t
      x-select-enable-primary nil       ; when set to t, delete of active region will be copied to kill ring
      mouse-drag-copy-region t
      yank-pop-change-selection t
      save-interprogram-paste-before-kill nil) ; avoid non text clipboard error (error "Selection owner couldn't convert" UTF8_STRING)

;; (use-package simpleclip)

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

(provide 'init-kill)
