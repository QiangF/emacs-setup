; -*- coding:utf-8 -*-

;; (use-package zoutline
;;  :ensure t)

;; * imenu
;; ** tips
; imenu-max-item-length
;; ** settings
(setq imenu-auto-rescan t
      imenu-auto-rescan-maxout 600000) ;; 600kb

;; side-hustle.el is similar to imenu-tree, it has no folding and narrowing
;; side-hustle use display-buffer-in-side-window
(setup imenu-tree
   (:with-hook prog-mode-hook
     (:hook imenu-my-heading))

   ;; see outline minor mode
   (defun imenu-my-heading ()
     (setq imenu-sort-function 'imenu--sort-by-name
           imenu-prev-index-position-function nil)
     (add-to-list 'imenu-generic-expression `("Sections"
                                              ,(concat (regexp-quote comment-start) " \\* \\(.+\\)$") 1))))

;; * outline
;; see deform-mode.el for level by indent

(setup outline
   (:global "<S-left>" #'outline-promote
            "<S-right>" #'outline-demote
            "M-C--" #'outline-hide-sublevels)
   (:autoload my-outline-cycle my-heading)
   (:with-feature outline-minor-mode
     (:hook-into text-mode prog-mode)
     (:hook my-heading))
   (:when-loaded
     (defun my-heading ()
       "custom heading for all progn mode, heading must ends in
    outline-heading-end-regexp,which is : for python mode
    * in heading to differentiate from comment, custom heading has higher level "
       (when (and (not (equal major-mode 'org-mode)) comment-start)
         (make-local-variable 'my-outline-regexp)
         (make-local-variable 'my-outline-max-level)
         (setq my-outline-max-level 8)
         ;; (let ((my-heading-start (comment-padright comment-start comment-add)))
         ;; same as lispy-mode
         (let ((my-heading-start comment-start))
           ;; (setq my-outline-regexp (concat (regexp-quote (substring comment-start 0 1))
           ;; match space before heading ?
           ;; (setq my-outline-regexp (concat "\\s-" (regexp-quote comment-start) " [*]\\{1,8\\}"))
           (setq my-outline-regexp (concat (regexp-quote my-heading-start) "[*]\\{1,8\\}"))
           (setq outline-heading-alist (list)) ;; make outline promote work
           (let ((level 0)
                 (level-prefix my-heading-start))
             ;; (level-prefix (concat (substring comment-start 0 1) " ")))
             (while (< level my-outline-max-level)
               (setq outline-heading-alist (cons (cons level-prefix level) outline-heading-alist)
                     level (1+ level)
                     level-prefix (concat level-prefix "*")))
             (setq outline-heading-alist (nreverse outline-heading-alist)))
           (setq outline-regexp (concat my-outline-regexp "\\|" outline-regexp)
                 outline-level (lambda ()
                                 (let* ((data (match-data))
                                        (start (car data))
                                        (end (cadr data))
                                        (level (- end start)))
                                   (if (looking-at my-outline-regexp)
                                       (- level 2)                     ;; subtract two spaces
                                       (+ level my-outline-max-level)) ;; 8 is the maximum custom heading levels
                                   ))))))

     (defun toggle-selective-display (&optional level)
       "Fold text indented same of more than the cursor.
    If level is set, set the indent level to LEVEL.
    If 'selective-display' is already set to LEVEL, clicking
    F5 again will unset 'selective-display' by setting it to 0."
       (interactive "P")
       (if (eq selective-display (1+ (current-column)))
           (set-selective-display 0)
           (set-selective-display (or level (1+ (current-column))))))

     (defun my-outline-cycle ()
       (interactive)
       (when (outline-on-heading-p)
         (cond ((not (outline-subheadings-visible-p))
                (show-children))
               ((and (outline-body-p) (not (outline-body-visible-p)))
                (show-entry))
               ((not (outline-subtree-visible-p))
                (show-subtree))
               ((and (outline-subheadings-p)
                     (outline-subheadings-visible-p))
                (hide-subtree))
               ((and (outline-body-p)
                     (outline-body-visible-p))
                (hide-entry)
                (hide-leaves)))))

     (defun outline-body-p ()
       (save-excursion
         (outline-back-to-heading)
         (outline-end-of-heading)
         (and (not (eobp))
              (progn (forward-char 1)
                     (not (outline-on-heading-p))))))

     (defun outline-body-visible-p ()
       (save-excursion
         (outline-back-to-heading)
         (outline-end-of-heading)
         (not (outline-invisible-p))))

     (defun outline-subtree-visible-p ()
       (interactive)
       (save-excursion
         (outline-back-to-heading)
         (let ((level (funcall outline-level)))
           (outline-next-heading)
           (cond ((and (not (eobp)) (< level (funcall outline-level)))
                  (outline-end-of-heading) (not (outline-invisible-p)))
                 (t t)))))

     (defun outline-subheadings-p ()
       (save-excursion
         (outline-back-to-heading)
         (let ((level (funcall outline-level)))
           (outline-next-heading)
           (and (not (eobp))
                (< level (funcall outline-level))))))

     (defun outline-subheadings-visible-p ()
       (interactive)
       (save-excursion
         (outline-next-heading)
         (not (outline-invisible-p))))

     (defun my-outline-next ()
       (interactive)
       (end-of-line)
       (let (found)
         (while (not found)
           (setq found (re-search-forward my-outline-regexp nil nil))
           (if found (goto-char found)
               (error "before first heading"))
           (if (not (outline-on-heading-p)) (setq found nil))))
       (previous-line))

     (defun my-outline-body-start ()
       (interactive)
       (condition-case nil
           (progn (my-outline-start) (next-line))
         (error (goto-char (point-min)))))

     (defun my-outline-end ()
       (interactive)
       (my-outline-start)
       (condition-case nil
           (outline-forward-same-level 1)
         (error (condition-case nil
                    (my-outline-next)
                  (error (goto-char (point-max)))))))

     (with-eval-after-load 'evil-commands
       (defadvice evil-goto-line (after expand-after-goto-line activate compile)
         "hideshow-expand affected block when using goto-line in a collapsed buffer"
         (save-excursion (outline-show-all) (outline-show-all))))

     ;; *** evil object
     ;; (with-eval-after-load 'evil-macros
     ;;    (progn
     ;;      (defun my-outline-start ()
     ;;        (interactive)
     ;;        (end-of-line)
     ;;        (let (found)
     ;;          (while (not found)
     ;;            (setq found (re-search-backward my-outline-regexp nil nil))
     ;;            (if found (goto-char found)
     ;;                (error "before first heading"))
     ;;            (if (not (outline-on-heading-p)) (setq found nil)))))


     ;;      (defun my-evil-object-range (backward forward &optional type)
     ;;        (interactive)
     ;;        (save-excursion
     ;;          (setq beg (progn (funcall backward) (point))))
     ;;        (save-excursion
     ;;          (funcall forward) (setq end (point)))
     ;;        (evil-range beg end))

     ;;      (defun evil-an-object-range (count forward &optional backward type newlines)
     ;;        "Return a text object range (BEG END) of COUNT objects with whitespace.
     ;; See `evil-inner-object-range' for more details."
     ;;        (let ((range (evil-inner-object-range count forward backward type)))
     ;;          (save-excursion
     ;;            (save-restriction
     ;;              (if newlines
     ;;                  (evil-add-whitespace-to-range range count)
     ;;                  (narrow-to-region
     ;;                   (save-excursion
     ;;                     (goto-char (evil-range-beginning range))
     ;;                     (line-beginning-position))
     ;;                   (save-excursion
     ;;                     (goto-char (evil-range-end range))
     ;;                     (line-end-position)))
     ;;                  (evil-add-whitespace-to-range range count))))))

     ;;      (evil-define-text-object evil-inner-outline (count &optional beg end type)
     ;;                               "Select around my outline.Return an outer text object range."
     ;;                               (my-evil-object-range #'my-outline-body-start #'my-outline-end))

     ;;      (evil-define-text-object evil-outer-outline (count &optional beg end type)
     ;;                               "Select around my outline.Return an outer text object range."
     ;;                               (my-evil-object-range #'my-outline-start #'my-outline-end))

     ;;      (define-key evil-outer-text-objects-map "l" 'evil-outer-outline)
     ;;      (define-key evil-inner-text-objects-map "l" 'evil-inner-outline)))

     (with-eval-after-load 'hydra
       (defhydra hydra-outline (:color pink :hint nil)
         "
    ^Hide^             ^Show^           ^Move
    ------------------------------------------------------
    _q_: sublevels     _a_: all         _u_: up
    _t_: body          _e_: entry       _n_: next visible
    _o_: other         _i_: children    _p_: previous visible
    _c_: entry         _k_: branches    _f_: forward same level
    _l_: leaves        _s_: subtree     _b_: backward same level
    _d_: subtree "
         ;; Hide
         ("q" hide-sublevels)                   ; Hide everything but the top-level headings
         ("t" hide-body)                        ; Hide everything but headings (all body lines)
         ("o" hide-other)                       ; Hide other branches
         ("c" hide-entry)                       ; Hide this entry's body
         ("l" hide-leaves)                      ; Hide body lines in this entry and sub-entries
         ("d" hide-subtree)                     ; Hide everything in this entry and sub-entries
         ;; Show
         ("a" show-all)                         ; Show (expand) everything
         ("e" show-entry)                       ; Show this heading's body
         ("i" show-children)                    ; Show this heading's immediate child sub-headings
         ("k" show-branches)                    ; Show all sub-headings under this heading
         ("s" show-subtree)                     ; Show (expand) everything in this heading & below
         ;; Move
         ("u" outline-up-heading)               ; Up
         ("n" outline-next-visible-heading)     ; Next
         ("p" outline-previous-visible-heading) ; Previous
         ("f" outline-forward-same-level)       ; Forward - same level
         ("b" outline-backward-same-level)      ; Backward - same level
         ("<escape>" nil)))))

;; * dumb-jump
;; cp. ctags/ggtags
;; 1. Don’t have to rebuild ctags
;; 2. Easier to debug and tweak: Since dump-jump is built on elisp and regular expressions
;; 3. Easy customizations like adapting regex to your custom Scala setup.
;; Example debugging session
;; If dumb-jump-go fails, search the same string with helm-projectile-ag.
;; If helm-projectile-ag fails, see helm-ag--last-command and make it from command line.
;; If it fails, try it with plain ag without all the flags.
;; If it fails, try it with alternative tool like grep or ack.

(provide 'init-outline)
