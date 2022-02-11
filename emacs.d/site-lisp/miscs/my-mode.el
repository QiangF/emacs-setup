;; Source: http://nullprogram.com/blog/2013/02/06    
;;;; My minor mode
;; Main use is to have my key bindings have the highest priority
(defvar my-mode-map (make-sparse-keymap)
  "Keymap while my-mode is active.")

;;;###autoload
;; (define-minor-mode my-mode
;;   "A minor mode so that my key settings override annoying major modes."
;;   nil
;;   my-mode-map)
(define-minor-mode my-mode
  "Global minor mode"
  nil " Gl" my-mode-map)
;; :global 1)

(defvar my-minor-mode-list '(my-mode))

(defun my-set-minor-mode-keymap-priority (&rest args)
    "Try to ensure that my keybindings always have priority."
    (mapc (lambda (mode)
            (if (not (eq (car (car minor-mode-map-alist)) 'mode))
                (let ((mykeys (assq 'mode minor-mode-map-alist)))
                  (assq-delete-all 'mode minor-mode-map-alist)
                  (add-to-list 'minor-mode-map-alist mykeys))))
          my-minor-mode-list))

(advice-add 'load :after #'my-set-minor-mode-keymap-priority)

; * functions
;;;###autoload
(defun my-kill-buffer()
  (interactive)
	  (kill-buffer (current-buffer))
    (if (> (count-windows) 1)(delete-window)))

;;;###autoload
(defun complete-indent-fold-flyspell ()
  (interactive)
  (if mark-active (indent-for-tab-command)
	(if (and (equal major-mode 'org-mode)
    		(or (looking-at org-outline-regexp)
            (looking-at org-block-regexp)
            (looking-at org-drawer-regexp)
            (org-at-table-p)))
      (org-cycle)
      (if (looking-at outline-regexp)
          (my-outline-cycle)
          (if (or (looking-at "\\_>") (looking-back "[a-zA-Z0-9_]\." (- (point) 2)))
              (company-complete)
              (if (string-match "^[[:space:]]*$" (buffer-substring-no-properties
                                                  (line-beginning-position) (line-end-position)))
                                        ; line with all spaces
                  (insert-char (string-to-char " ") tab-width)
                  (if (string-match "^[[:space:]]*$"
                                    (buffer-substring-no-properties (line-beginning-position) (point)))
                                        ; at the beginning of a non all space line
                      (indent-for-tab-command)
                      (if (and (bound-and-true-p flyspell-mode) (not (flyspell-word)))
                          (flyspell-correct-word) ; run flyspell-word at point
                          (insert "\t")))))))))

;;;###autoload
(defun my-toggle-fill-paragraph ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

;; (global-set-key [remap fill-paragraph]
;;                 #'endless/fill-or-unfill)

;;;###autoload
(defun my-home ()
  "Move to the beginning of the current line on the first key stroke,
and to the beginning of the buffer if there is a second key stroke
within `my-double-key-timeout' seconds."
  (interactive)
  (let ((last-called (get this-command 'my-last-call-time)))
    (if (and (eq last-command this-command)
             last-called
             (<= (time-to-seconds (time-since last-called))
                 my-double-key-timeout))
        (beginning-of-buffer)
      (move-beginning-of-line nil)))
  (put this-command 'my-last-call-time (current-time)))

;;;###autoload
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

;;;###autoload
(defun my-eval()
  (interactive)
  (if (region-active-p)
      (eval-region (region-beginning) (region-end) t)
    (call-interactively #'pp-eval-last-sexp)))

;;;###autoload
(defun toggle-mode-line () "toggles the modeline on and off"
  (interactive)
  (setq mode-line-format
    (if (equal mode-line-format nil)
        (default-value 'mode-line-format)) )
  (redraw-display))

;;;###autoload
(defun switch-to-scratch-and-back (&optional arg)
  "Toggle between *scratch-MODE* buffer and the current buffer.
If a scratch buffer does not exist, create it with the major mode set to that
of the buffer from where this function is called. (http://stackoverflow.com/a/7539787/1219634)"
  (interactive "p")
  (if (string-match-p "\\*scratch" (buffer-name))
    (switch-to-buffer (other-buffer))
    (let* ((mode-str (if (derived-mode-p 'prog-mode)
                         (format "%s" major-mode) "org-mode"))
           (buf (concat "*scratch-" mode-str "*")))
      (if (get-buffer buf)
          (switch-to-buffer buf)
        (progn
         (switch-to-buffer
         (find-file (concat "~/.emacs.d/scratch/" buf))
         (funcall (intern mode-str))))))))

;;;###autoload
(defun turn-on-my-mode ()
  "Turns on my-mode."
  (interactive)
  (my-mode t))

;;;###autoload
(defun turn-off-my-mode ()
  "Turns off my-mode."
  (interactive)
  (my-mode -1))

;;;###autoload
(define-globalized-minor-mode global-my-mode my-mode turn-on-my-mode)

;;;; my functions
(defun my-rename-buffer ()
  (interactive)
  (let ((my-buffer-name (read-from-minibuffer "New buffer name:")))
    (if (derived-mode-p 'exwm-mode)
        (exwm-workspace-rename-buffer (concat exwm-class-name "::" my-buffer-name))
        (rename-buffer my-buffer-name))))

;;;; term
(defun last-term-buffer (l)
  "Return most recently used term buffer."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
	(car l) (last-term-buffer (cdr l)))))
;; (defun get-term ()
;;   "Switch to the term buffer last used, or create a new one if
;;     none exists, or if the current buffer is already a term."
;;   (interactive)
;;   (let ((b (last-term-buffer (buffer-list))))
;;     (if (or (not b) (eq 'term-mode major-mode))
;; 	(ansi-term (getenv "SHELL"))
;;       (switch-to-buffer b))
;;  (get-buffer-process b)))

;; (defun start-or-switch-to (function buffer-name)
;;   "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
;; Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
;; the current buffer."
;;   (if (not (get-buffer buffer-name))
;;       (progn
;;         ;; (split-window-sensibly (selected-window))
;;         ;; (other-window 1)
;;         (funcall function))
;;     (switch-to-buffer buffer-name)))

;; (defun visit-term-buffer ()
;;   "Create or visit a terminal buffer."
;;   (interactive)
;;   (start-or-switch-to (lambda ()
;;                          (ansi-term (getenv "SHELL")))
;;                       "*ansi-term*"))

;; (defun visit-ielm ()
;;   "Switch to default `ielm' buffer.
;; Start `ielm' if it's not already running."
;;   (interactive)
;;   (prelude-start-or-switch-to 'ielm "*ielm*"))
;; (defun comment-dwim-line (&optional arg)
;; "Replacement for the comment-dwim command.
;; If no region is selected and current line is not blank and we are not at the end of the line,
;; then comment current line.
;; Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
;; (interactive "*P")
;; (comment-normalize-vars)
;; (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
;; (progn
;; (comment-or-uncomment-region (line-beginning-position) (line-end-position))
;; (next-line))
;; (comment-dwim arg)))

;;;; outline-minor-mode

(defvar outline-hs-toggle nil
  "Keeps the state of how the buffer was last toggled by TABing.")

;; hide show all
;;;###autoload
(defun outline-cycle-all()
  (interactive)
  (if outline-hs-toggle
      (show-all) (hide-body))
  (setq outline-hs-toggle (not outline-hs-toggle)))

;;;###autoload
(defun shift-region(numcols)
"Trick to expand the region to the beginning and end of the area selected
 much in the handy way I liked in the Dreamweaver editor."
  (if (< (point)(mark))
    (if (not(bolp))    (progn (beginning-of-line)(exchange-point-and-mark) (end-of-line)))
    (progn (end-of-line)(exchange-point-and-mark)(beginning-of-line)))
  (setq region-start (region-beginning))
  (setq region-finish (region-end))
  (save-excursion
    (if (< (point) (mark)) (exchange-point-and-mark))
    (let ((save-mark (mark)))
      (indent-rigidly region-start region-finish numcols)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil)
	  )))

;;;###autoload
(defun my-backtab()
  (interactive) 
  (if mark-active (shift-region (- tab-width))
    (if (equal major-mode 'org-mode) (org-shifttab)
      (outline-cycle-all))
  ))

;;;###autoload
(defun copy_work_path()
  (interactive)
  (kill-new default-directory)
  (message "Current directory path Copied"))

;;;; tab
(defvar my-double-key-timeout 0.25
  "The number of seconds to wait for a second key press.")

;;;###autoload
(defun my-tab ()
  "Move to the beginning of the current line on the first key stroke,
and to the beginning of the buffer if there is a second key stroke
within `my-double-key-timeout' seconds."
  (interactive)
  (let ((last-called (get this-command 'my-last-call-time))
       ;; (is-term (derived-mode-p 'term-mode)
       (is-term (string= "term-mode" major-mode)))
       (if (and is-term (term-in-char-mode))
         (term-send-raw-string "\t")
         (if (and (eq last-command this-command)
	  (<= (time-to-seconds (time-since last-called))
	  my-double-key-timeout))
	  (ivy-yasnippet)
	  (if (sit-for my-double-key-timeout)
	    (if (bound-and-true-p iedit-mode) (iedit-next-occurrence)
            (complete-indent-fold-flyspell)))))
    (put this-command 'my-last-call-time (current-time))))

;; Pull from PRIMARY (same as middle mouse click)
(defun get-primary ()
  (interactive)
  (insert
   (gui-get-primary-selection)))
(global-set-key "\C-c\C-y" 'get-primary)

;; (setq select-enable-primary t)

(defun my-lauch (command)
    (interactive (list (read-shell-command "$ ")))
    (start-process-shell-command command nil command))

; todo don't change tab for evil normal mode
;; (global-unset-key (kbd "C-1"))
;; (define-key my-mode-map (kbd "C-1") 'get-term)
;; (define-key my-mode-map [backtab] 'my-backtab)
;; (define-key my-mode-map (kbd "S-<backspace>") 'evil-shift-left-line)
;; (global-unset-key (kbd "M-;"))
;; (define-key my-mode-map (kbd "M-;") 'comment-dwim-2)
;; (define-key my-mode-map (kbd "<tab>") 'my-tab)
(provide 'my-mode)

;;; my-mode ends here
