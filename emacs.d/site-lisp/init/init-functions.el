;; -*- coding:utf-8 -*-

(defun check_or_create (directory)
  (interactive)
  (unless (file-exists-p directory)
    (make-directory directory))
  directory)

(defmacro my-setq (&rest settings)
  "A simplified `customize-set-variable' with the syntax of `setq'.
It calls the setter of the variable, if it's defined.  Once the
package which defines the variable is loaded, using `setq' will
not call the setter, so the desired effect may not happen.  This
is where `toki/setq' should be used.

See the docstring of `general-setq' for details."
  `(progn
     ,@(cl-loop for (var val) on settings by 'cddr
                collect `(funcall (or (get ',var 'custom-set) #'set)
                                  ',var ,val))))

(defmacro my-setq-default (&rest settings)
  "A `setq-default' version of the `toki-setq' macro.
See its docstring for details."
  `(progn
     ,@(cl-loop for (var val) on settings by 'cddr
                collect `(funcall (or (get ',var 'custom-set) #'set-default)
                                  ',var ,val))))

;; Comes from https://github.com/hlissner/doom-emacs and
;; https://framagit.org/citreu/emacs.d/
(defmacro my-add-trigger (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.
FORMS are evaluated once, when that function/hook is first
invoked, then never again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted
function (which will be advised).  FORMS can start with `:after',
which means when HOOK-OR-FUNCTION is a function, run the rest of
FORMS after the finishing the function."
  (declare (indent 1))
  (let ((append (if (eq (car forms) :after) (pop forms)))
        (fn (gensym "toki|after-call")))
    `(progn
       (fset ',fn
             (lambda (&rest _)
               ,@forms
               (cond ((functionp ,hook-or-function)
                      (advice-remove ,hook-or-function #',fn))
                     ((symbolp ,hook-or-function)
                      (remove-hook ,hook-or-function #',fn)))
               (unintern ',fn nil)))
       (cond ((functionp ,hook-or-function)
              (advice-add ,hook-or-function ,(if append :after :before) #',fn))
             ((symbolp ,hook-or-function)
              (put ',fn 'permanent-local-hook t)
              (add-hook ,hook-or-function #',fn ,append))))))


(setq async-shell-command-buffer 'new-buffer)

;; `C-a' first takes you to the first non-whitespace char as
;; `back-to-indentation' on a line, and if pressed again takes you to
;; the actual beginning of the line.
(defun smarter-move-beginning-of-line (arg)
  "Move depending on ARG to beginning of visible line or not.
  From https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/."
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

(defmacro radian-defadvice (name arglist where place docstring &rest body)
  "Define an advice called NAME and add it to a function.
ARGLIST is as in `defun'. WHERE is a keyword as passed to
`advice-add', and PLACE is the function to which to add the
advice, like in `advice-add'. PLACE should be sharp-quoted.
DOCSTRING and BODY are as in `defun'."
  (declare (indent 2)
           (doc-string 5))
  (unless (stringp docstring)
    (error "Radian: advice `%S' not documented'" name))
  (unless (and (listp place)
               (= 2 (length place))
               (eq (nth 0 place) 'function)
               (symbolp (nth 1 place)))
    (error "Radian: advice `%S' does not sharp-quote place `%S'" name place))
  `(progn
     ;; You'd think I would put an `eval-and-compile' around this. It
     ;; turns out that doing so breaks the ability of
     ;; `elisp-completion-at-point' to complete on function arguments
     ;; to the advice. I know, right? Apparently this is because the
     ;; code that gets the list of lexically bound symbols at point
     ;; tries to `macroexpand-all', and apparently macroexpanding
     ;; `eval-and-compile' goes ahead and evals the thing and returns
     ;; only the function symbol. No good. But the compiler does still
     ;; want to know the function is defined (this is a Gilardi
     ;; scenario), so we pacify it by `eval-when-compile'ing something
     ;; similar (see below).
     (defun ,name ,arglist
       ,(let ((article (if (string-match-p "^:[aeiou]" (symbol-name where))
                           "an"
                         "a")))
          (format "%s\n\nThis is %s `%S' advice for `%S'."
                  docstring article where
                  (if (and (listp place)
                           (memq (car place) ''function))
                      (cadr place)
                    place)))
       ,@body)
     (eval-when-compile
       (declare-function ,name nil))
     (advice-add ,place ',where #',name)
     ',name))

(defmacro radian-defhook (name arglist hooks docstring &rest body)
  "Define a function called NAME and add it to a hook.
ARGLIST is as in `defun'. HOOKS is a list of hooks to which to
add the function, or just a single hook. DOCSTRING and BODY are
as in `defun'."
  (declare (indent 2)
           (doc-string 4))
  (unless (listp hooks)
    (setq hooks (list hooks)))
  (dolist (hook hooks)
    (unless (string-match-p "-\\(hook\\|functions\\)$" (symbol-name hook))
      (error "Symbol `%S' is not a hook" hook)))
  (unless (stringp docstring)
    (error "Radian: no docstring provided for `radian-defhook'"))
  (let ((hooks-str (format "`%S'" (car hooks))))
    (dolist (hook (cdr hooks))
      (setq hooks-str (format "%s\nand `%S'" hooks-str hook)))
    `(progn
       (defun ,name ,arglist
         ,(format "%s\n\nThis function is for use in %s."
                  docstring hooks-str)
         ,@body)
       (dolist (hook ',hooks)
         (add-hook hook ',name)))))

(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(defun always-yes-p (prompt) t)

;; elisp version of try...catch...finally
(defmacro safe-wrap (fn &rest clean-up)
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))

(defun read-lines (file)
  "Return a list of lines of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun read-nonempty-lines (file)
  (with-temp-buffer
    (insert-file-contents file)
    (split-string s "[\r\n]+" t)))

(defun string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

;; use (aref string 0) to get char code
(defun my-show-key ()
  (interactive)
  (let ((key (read-key-sequence-vector "Type key sequence:")))
    (message "%s is %s" key
             (key-description key))))

(defun my-show-event ()
  (interactive)
  (let* ((event (read-event "type a key to see its event"))
         (modifiers (event-modifiers event))
         (type (event-basic-type event)))
    (message "mdodifiers %s ttype %s event %s" modifiers type event)))

(defun slegetank/dump-var-to-file (var filepath)
  "Dump var to file."
  (save-excursion
    (let ((buf (find-file-noselect filepath)))
      (set-buffer buf)
      (erase-buffer)
      (prin1 var buf)
      (save-buffer)
      (kill-buffer))))

(defun slegetank/read-var-from-file (filePath)
  "Read var from file."
  (if (file-exists-p filePath)
      (read (with-temp-buffer
              (insert-file-contents filePath)
              (buffer-string)))
    '()))

(defun my-same-file-systemp (path1 path2)
  (let ((path1 (file-name-directory (expand-file-name path1)))
        (path2 (file-name-directory (expand-file-name path2))))
    (string= (shell-command-to-string
              (concat "df --output=target " (shell-quote-argument (expand-file-name path1))))
             (shell-command-to-string
              (concat "df --output=target " (shell-quote-argument (expand-file-name path2)))))))

(defun fake-module-reload (module)
  (interactive "fReload Module file: ")
  (let ((tmpfile (make-temp-file
                  (file-name-nondirectory module) nil module-file-suffix)))
    (copy-file module tmpfile t)
    (module-load tmpfile)))

(defmacro my/with-advice (adlist &rest body)
  "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  (SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to `advice-add'.  The BODY is wrapped in an
`unwind-protect' form, so the advice will be removed even in the
event of an error or nonlocal exit."
  (declare (debug ((&rest (&rest form)) body))
           (indent 1))
  `(progn
     ,@(mapcar (lambda (adform)
                 (cons 'advice-add adform))
               adlist)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (adform)
                   `(advice-remove ,(car adform) ,(nth 2 adform)))
                 adlist))))

(defun my-rm-at-point:hook|advice|alist ()
  (interactive)
  (save-excursion
    (let ((form (read (progn
                        (my-goto-left-paren)
                        (buffer-substring-no-properties (point)
                                                        (progn
                                                          (forward-sexp)
                                                          (point)))))))
      (pcase (car form)
        (`add-hook
         (pcase (length form)
           (5
            ;; (add-hook 'x-hook 'fn nil-or-append :local)
            (eval `(remove-hook ,(cadr form)
                                ,(nth 2 form)
                                :local)))
           (_ (eval `(remove-hook ,(cadr form)
                                  ,(nth 2 form)))))
         (prin1 (symbol-value (eval (cadr form)))))
        ;; (advice-add 'ffx :before 'f1)
        (`advice-add
         (eval `(advice-remove ,(cadr form) ,(nth 3 form)))
         (message "rm %S advice => %S" (cadr form) (nth 3 form)))
        ;; (add-to-list 'xx-alist '("a" . 1))
        (`add-to-list
         (cl-symbol-macrolet ((al (eval (cadr form))))
           (eval `(setq ,al
                        (delete ,(nth 2 form) ,al)))
           (prin1 (eval al))))
        (_
         (error "unkown %S" form))))))

(defun my-eval-buffer ()
  "Execute the current buffer as Lisp code.
Top-level forms are evaluated with `eval-defun' so that `defvar'
and `defcustom' forms reset their default values."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (eval-defun nil))))


;; (defmacro measure-time (&rest body)
;;   "Measure the time it takes to evaluate BODY."
;;   `(let ((time (current-time)))
;;      ,@body
;;      (message "%.06f" (float-time (time-since time)))))

;; (benchmark-run 10 (sit-for 0.1))
;; It's used like this:

;; (measure-time
;;   (dotimes (i 100000)
;;     (1+ 1)))

(defun my-goto-left-paren ()
  "a(...|....) => a|(...)"
  (interactive)
  (or (eq (char-after) ?\()
      (let ((innermost-paren (nth 1 (syntax-ppss))))
        (if innermost-paren
            (goto-char innermost-paren)
            (beginning-of-defun)))))

(defun my/bypass-confirmation-all (function &rest args)
  "Call FUNCTION with ARGS, bypassing all prompts.
This includes both `y-or-n-p' and `yes-or-no-p'."
  (my/with-advice
      ((#'y-or-n-p    :override (lambda (prompt) t))
       (#'yes-or-no-p :override (lambda (prompt) t)))
    (apply function args)))

(defun lineup (start end)
  "Align columns by whitespace."
  (interactive "r")
  ;; From emacs-devel.
  (align-regexp start end "\\(\\s-*\\)\\s-" 1 0 t))

(defun my-rename-buffer-file ()
  ;; https://stackoverflow.com/questions/384284/how-do-i-rename-an-open-file-in-emacs
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;; http://endlessparentheses.com/define-context-aware-keys-in-emacs.html
(defmacro my-define-conditional-key (keymap condition &rest args)
  "In KEYMAP, define key sequences ARGS conditionally.
This is like `define-key', except the definition
\"disappears\" whenever CONDITION evaluates to nil."
  (declare (indent 3)
           (debug (form form form &rest sexp)))
  `(let (key def (bindings ',args))
     (while bindings
       (setq key (pop bindings)
             def (pop bindings))
       (define-key ,keymap key
         '(menu-item
           (format "maybe-%s" (or (car (cdr-safe def)) def))
           nil
           :filter (lambda (&optional _)
                     (when (macroexp-progn ,condition)
                       def)))))))

(defun my-suppress-messages (oldfn &rest args) ; from pkal
  "Advice wrapper for suppressing `message'.
OLDFN is the wrapped function, that is passed the arguments
ARGS."
  (let ((msg (current-message)))
    (prog1
        (let ((inhibit-message t))
          (apply oldfn args))
      (when msg
        (message "%s" msg)))))

;; https://github.com/cstby/emacs.d/blob/main/init.el#L67
(defun my-clean-empty-lines (&optional begin end)
  "Remove duplicate empty lines from BEGIN to END.
Called interactively, this function acts on the region, if
active, or else the entire buffer."
  (interactive "*r")
  (unless (region-active-p)
    (setq begin (point-min)
          end (save-excursion
                (goto-char (point-max))
                (skip-chars-backward "\n[:space:]")
                (point))))
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (goto-char (point-min))
      (while (re-search-forward "\n\n\n+" nil :move)
        (replace-match "\n\n"))
      ;; Insert a newline at the end.
      (goto-char (point-max))
      (unless (or (buffer-narrowed-p)
                  (= (line-beginning-position) (line-end-position)))
        (insert "\n")))))

(defun my-ensure-after-init (function)
  "Ensure FUNCTION runs after init, or now if already initialized.
If Emacs is already started, run FUNCTION.  Otherwise, add it to
`after-init-hook'.  FUNCTION is called with no arguments."
  (if after-init-time
      (funcall function)
    (add-hook 'after-init-hook function)))

(provide 'init-functions)
