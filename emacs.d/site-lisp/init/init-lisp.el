; -*- coding:utf-8 -*-

;; I have to tell you that probably of all the languages I've used, definitely, Emacs Lisp has been the most fun. – John Wiegley

(setup (:pkg easy-escape)
  (:with-hook emacs-lisp-mode
    (:hook easy-escape-minor-mode)))

; * paredit
; M-\ use " to enclose selection or word
;; (setup (:pkg) paredit-menu
;;     :ensure t)

;; (setup (:pkg) paredit-everywhere
;;  :ensure t
;;  :bind
;;    ("M-\\" . paredit-meta-doublequote)
;;  :hook
;;    ((text-mode . paredit-everywhere-mode)
;;     (prog-mode . paredit-everywhere-mode)))

(setup (:pkg elec-pair)
   (:with-feature electric-pair-mode
     (:hook-into text-mode prog-mode))
   (:when-loaded
     ;; disable chinese quote, interfere with rime pair
     ;; (setq electric-pair-inhibit-predicate
     ;;       `(lambda (c)
     ;;          (if (char-equal c ?\“) t (,electric-pair-inhibit-predicate c))))

     ;; make electric-pair-mode work on more brackets
     (setq electric-pair-pairs '((?\` . ?\`)
                                 (?\' . ?\')
                                 (?\‘ . ?\’)
                                 (?\" . ?\")
                                 (?\“ . ?\”)
                                 (?\( . ?\))
                                 (?\{ . ?\})
                                 (?\[ . ?\])))

     ;; 把弯引号左右对调重新添加到补全列表
     (setq electric-pair-pairs
           `(,@electric-pair-pairs
             (?’ . ?‘)
             (?” . ?“)))

     ;; 修正左右对调的补全
     (define-advice electric-pair--insert (:around (orig-fn c) fix-curved-quotes)
       (let* ((qpair (rassoc c electric-pair-pairs))
              (reverse-p (and qpair (> (car qpair) (cdr qpair)))))
         (if reverse-p
             (run-with-timer 0 nil
                             `(lambda ()
                                (backward-char 1)
                                (insert (char-to-string ,c))))
             (funcall orig-fn c))))))

; error loading
;; electric-operator
; * evil-matchit
;; (setup (:pkg) evil-matchit
;;   ;; :ensure t
;;   :hook (evil-mode . evil-matchit-mode)
;; )
; * corral,evil surround,embrace

;; wrap delimiters around the sexp at point. Repeated calls of the
;; same command, backward or forward, will shift the delimiters in
;; the respective direction

;; (setup (:pkg) corral
;;   :bind
;;     (("M-9" . corral-parentheses-backward)
;;     ("M-0" . corral-parentheses-forward)
;;     ("M-[" . corral-brackets-backward)
;;     ("M-]" . corral-brackets-forward)
;;     ("M-{" . corral-braces-backward)
;;     ("M-}" . corral-braces-forward)
;;     ("M-\"" . corral-double-quotes-backward))
;;   :config
;;     ; ** solve conflict with paredit
;;     (define-key paredit-mode-map (kbd "M-]") nil)
;;     (define-key paredit-mode-map (kbd "M-{") nil)
;;     (define-key paredit-mode-map (kbd "M-}") nil)
;;     (define-key paredit-mode-map (kbd "M-\"") nil)
;;     (define-key paredit-everywhere-mode-map (kbd "M-]") nil)
;;     (define-key paredit-everywhere-mode-map (kbd "M-{") nil)
;;     (define-key paredit-everywhere-mode-map (kbd "M-}") nil)
;;     (define-key paredit-everywhere-mode-map (kbd "M-\"") nil)
;; )

; * lisp
(setup (:pkg sly-el-indent)
   (:delay)
   (:with-hook emacs-lisp-hook
     (:hook sly-el-indent-setup))
   (put 'setup 'common-lisp-indent-function '(4 &rest lisp-indent-tagbody)))

(setup (:pkg eldoc)
   (:with-hook emeacs-lisp-mode
     (:hook my-emeacs-lisp-mode-hook))

   (:hook (lambda ()
            (remove-hook 'pre-command-hook 'eldoc-pre-command-refresh-echo-area)))

   (:option eldoc-idle-delay 1
            lisp-indent-function 'common-lisp-indent-function)

   (defun my-emeacs-lisp-mode-hook ()
     (progn
       (eldoc-mode 1)
       (setq tab-width 2)
       (setq lisp-body-indent 2))))

; * lispy
;; eleminate parenthes matches to wrong position, some enclosed function definition is void, which is hard to debug
;; you are in normal mode when the point is before/after paren or the region is active
; ** universal (use c-q to input these characters)
;; ]   lispy-forward - move to the end of the closest list, analogous to C-M-n (forward-list)
;; [   lispy-backward - move to the start of the closest list, analogous to C-M-p (backward-list)
;; C-3   lispy-right - exit current list forwards, analogous to up-list
;; )   lispy-right-nostring exit current list forwards, but self-insert in strings and comments
;; C-,   lispy-kill-at-point

;; C-1   lispy-describe-inline
;; C-2   lispy-arglist-inline
;; C-3   lispy-right
;; C-4   lispy-x

;; C-M-,   lispy-mark
;; M-d   lispy-kill-word
;; M-k   lispy-kill-sentence
;; M-m   lispy-mark-symbol

; ** special
;; When special, the digit keys call digit-argument
;; digit-argument
;; mark the third element of the list with 3m
;; then mark third through fifth element (three total) with 2> or >>
;; then move the selection to the last three elements of the list with 99j
;; chain apply command continuously by holding the key. Some useful hold-able keys are "jkf<>cws;"

;; b moves back in history for all move commands
;;  m mark or deactivate mark

;; key   command         key    command
;; j    lispy-down       k      lispy-up
;; s    lispy-move-down  w      lispy-move-up
;; >    lispy-slurp       <      lispy-barf
;; c    lispy-clone       C-d    or DEL
;; C    lispy-convolute   C      reverses itself
;; d    lispy-different   d      reverses itself
;; M-j lispy-split       +      lispy-join
;; O    lispy-oneline     M      lispy-multiline
;; S    lispy-stringify   C-u "  lispy-quotes
;; ;    lispy-comment     C-u ;  lispy-comment
;; xi  lispy-to-ifs     xc      lispy-to-cond

;; avy-related commands
;; key   command
;; q   lispy-ace-paren
;; Q   lispy-ace-char
;; a   lispy-ace-symbol
;; H   lispy-ace-symbol-replace
;; -   lispy-ace-subword

;; IDE-like features for Elisp, Clojure, Scheme, Common Lisp, Hy, Python and Julia:
;;     e evals
;;     E evals and inserts
;;     g jumps to any tag in the current directory with semantic
;;     G jumps to any tag in the current file
;;     M-. jumps to symbol, M-, jumps back
;;     F jumps to symbol, D jumps back
;;     C-1 shows documentation in an overlay
;;     C-2 shows arguments in an overlay
;;     Z breaks out of edebug, while storing current function's arguments

;; Code manipulation:
;;     i prettifies code (remove extra space, hanging parens ...)
;;     xi transforms cond expression to equivalent if expressions
;;     xc transforms if expressions to an equivalent cond expression
;;     xf flattens function or macro call (extract body and substitute arguments)
;;     xr evals and replaces
;;     xl turns current defun into a lambda
;;     xd turns current lambda into a defun
;;     O formats the code into one line
;;     M formats the code into multiple lines

;; Misc. bindings:
;;     outlines navigation/folding (J, K, I, i)
;;     narrow/widen (N, W)
;;     ediff (b, B)
;;     ert (T)
;;     edebug (xe)

;; Use xe to edebug a function.
;; Call the function (or a command that eventually calls the function) and press Z to exit edebug. All the state is now stored in globals.
;; Use f and hjkl to step through expressions and p to eval them.
;; Or put (debug) (or (debug nil ARGS)) in code where you want breakpoints.

(setup (:pkg lispy)
   (:hook-into emacs-lisp-mode)
   (:autoload conditionally-enable-lispy)
   (:with-hook minibuffer-setup-hook
     (:hook conditionally-enable-lispy))
   (:when-loaded
     (define-key lispy-mode-map-c-digits (kbd "C-5") 'lispy-parens-down)
     (define-key lispy-mode-map-c-digits (kbd "C-8") nil)
     (define-key lispy-mode-map-c-digits (kbd "C-9") nil)
     (defun conditionally-enable-lispy ()
       (when (eq this-command 'eval-expression)
         (lispy-mode 1)))

     (with-eval-after-load 'elisp-mode
       (require 'elispfl)
       (elispfl-mode))

     (advice-add 'lispy-occur :around 'my-lispy-occur)

     (defun my-lispy-occur (orig-func &rest args)
       (let ((ivy-initial-inputs-alist
               (list (cons 'lispy-occur (if (region-active-p)
                                            (buffer-substring-no-properties (region-beginning) (region-end))
                                            "")))))
         (apply orig-func args)))))

;; `company-elisp` may no longer be needed as emacs-lisp-mode now sets up a function in the
;; completion-at-point-functions hook which is used by company-capf.
;; (eval-after-load 'company-mode
;;     (my-set-company-backends 'emacs-lisp-mode-hook 'company-elisp))

; * lispyville

(provide 'init-lisp)
