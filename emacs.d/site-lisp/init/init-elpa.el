;;; init-elpa.el --- Settings and helpers for package.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ("gnu"   . "http://elpa.emacs-china.org/gnu/")

(defun use-official ()
  (interactive)
  (setq package-archives
        '(("gnu"   . "https://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/"))))

(defun use-emacs-china ()
  (interactive)
  (setq package-archives
        '(("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
          ("org-cn"   . "http://elpa.emacs-china.org/org/")
          ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/"))))

(defun use-tencent ()
  (interactive)
  (setq package-archives
        '(("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
          ("org-cn"   . "http://mirrors.cloud.tencent.com/elpa/org/")
          ("gnu-cn"   . "http://mirrors.cloud.tencent.com/elpa/gnu/"))))

(defun use-163 ()
  (interactive)
  (setq package-archives
        '(("melpa-cn" . "http://mirrors.163.com/elpa/melpa/")
          ("org-cn"   . "http://mirrors.163.com/elpa/org/")
          ("gnu-cn"   . "http://mirrors.163.com/elpa/gnu/"))))


(defun use-tuna ()
  (interactive)
  (setq package-archives
        '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
          ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
          ("nongnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
          ;; ("ox-odt" . "https://kjambunathan.github.io/elpa/")
          ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/"))))

(defun use-ustc ()
  (interactive)
  (setq package-archives
        '(("gnu"   . "http://mirrors.ustc.edu.cn/elpa/gnu/")
          ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
          ;; ("ox-odt" . "https://kjambunathan.github.io/elpa/")
          ("org"   . "http://mirrors.ustc.edu.cn/elpa/org/"))))

(defun use-zju ()
  (interactive)
  (setq package-archives
        '(("gnu"   . "http://mirrors.zju.edu.cn/elpa/gnu/")
          ("melpa" . "http://mirrors.zju.edu.cn/elpa/melpa/")
          ("emacswiki" . "http://mirrors.zju.edu.cn/elpa/emacswiki/")
          ;; ("ox-odt" . "https://kjambunathan.github.io/elpa/")
          ("org"   . "http://mirrors.zju.edu.cn/elpa/org/"))))

(use-tuna)

; ** tips
; :after will lazy loads the package being configured once the after target has been loaded
;; similar to (require 'package-being-configured) after target
;; but the above doesn't apply to dired+, dired-extension, which needs a :demand t to load
;; in the above packages' :config, a require is needed if non lazy loading is requried (full) 
; :demand t :config equals to :init (require 'package) (functions in the package)
; :commands specify functions used in :init or for package in site-lisp/miscs that doesn't have package-autoloads.el 
; :defines declares variables, use if emacs complain variable is not available
;; :functions for function names
;; use :commands in (use-package package B) if package A has a binding that uses a command in package B, see dired+
; :bind define autoloads for the bound commands, also true when used with :map
;; https://github.com/jwiegley/use-package/issues/705

;; (unless (package-installed-p 'quelpa)
;;   (package-install 'quelpa))

(require 'quelpa)

(setq quelpa-checkout-melpa-p nil
      quelpa-update-melpa-p nil
      quelpa-self-upgrade-p nil
      quelpa-git-clone-depth t
      quelpa-initialized-p t
      quelpa-use-async-p t)

;; (quelpa '(setup :fetcher git
;;           :url "https://git.sr.ht/~pkal/setup"))

(require 'setup)
(defmacro setc (&rest vars-and-vals)
  "Set VARS-AND-VALS by customizing them or using set-default.
Use like `setq'."
  `(progn ,@(cl-loop for (var val) on vars-and-vals by #'cddr
                     if (null val) return (user-error "Not enough arguments")
                     collecting `(funcall (or (get ',var 'custom-get)
                                              #'set-default)
                                          ',var ',val)
                     into ret
                     finally return ret)))

;; local-or-package
(setup-define :pkg
  (lambda (package &rest args)
    `(unless (locate-file (symbol-name ',package) load-path (get-load-suffixes))
       (if ,args
           (quelpa '(,package ,@args))
           (:package ,package))))
  :documentation "Install PACKAGE if it is not available locally."
  :shorthand #'cadr)

(setup-define :advise
    (lambda (symbol where function)
      `(advice-add ',symbol ,where ,(setup-ensure-function function)))
  :documentation "Add a piece of advice on a function.
See `advice-add' for more details."
  :after-loaded t
  :debug '(sexp sexp function-form)
  :repeatable t)

(setup-define :load-after
    (lambda (&rest features)
      (let ((body `(require ',(setup-get 'feature))))
        (dolist (feature (nreverse features))
          (setq body `(with-eval-after-load ',feature ,body)))
        body))
  :documentation "Load the current feature after FEATURES.")

(setup-define :load-from
    (lambda (path)
      `(let ((path* (expand-file-name ,path)))
         (if (file-exists-p path*)
             (add-to-list 'load-path path*)
           ,(setup-quit))))
  :documentation "Add PATH to load path.
This macro can be used as NAME, and it will replace itself with
the nondirectory part of PATH.
If PATH does not exist, abort the evaluation."
  :shorthand (lambda (args)
               (intern
                (file-name-nondirectory
                 (directory-file-name (cadr args))))))

(setup-define :delay
  (lambda (&optional time)
    `(run-with-idle-timer ,(or time 1) nil
                          (lambda () (require ',(setup-get 'feature)))))
  :documentation "Delay loading the feature until a certain amount of idle time has passed.")

(setup-define :disabled
  (lambda ()
    `,(setup-quit))
  :documentation "Always stop evaluating the body.")

(setup-define :autoload
  (lambda (function)
    `(autoload ',function (symbol-name ',(setup-get 'feature)) nil t))
  :documentation "autoload function"
  :repeatable t)

;; Create override-global-mode to force key remappings

(defvar override-global-map (make-keymap)
  "override-global-mode keymap")

(define-minor-mode override-global-mode
  "A minor mode so that keymap settings override other modes."
  t "")

;; the keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists
             `((override-global-mode . ,override-global-map)))

(setup-define :global*
  (lambda (key command)
    `(define-key override-global-map ,key ,command))
  :documentation "Bind KEY to COMMAND in current map."
  :after-loaded nil
  :debug '(form sexp)
  :ensure '(kbd func)
  :repeatable t)

(setup-define :needs
    (lambda (executable)
      `(unless (executable-find ,executable)
         ,(setup-quit)))
  :documentation "If EXECUTABLE is not in the path, stop here."
  :repeatable 1)

(setup-define :if-host
    (lambda (hostname)
      `(unless (string= (system-name) ,hostname)
         ,(setup-quit)))
  :documentation "If HOSTNAME is not the current hostname, stop evaluating form.")

;; (setup-define :autoload
;;   (lambda (&rest funcs)
;;     (let ((body '())
;;           (feature-string (symbol-name (setup-get 'feature))))
;;       (dolist (single-func (nreverse funcs))
;;         (add-to-list 'body `(autoload ',single-func ,feature-string nil t)))
;;       (add-to-list 'body 'progn)
;;       body))
;;   :documentation "Load the current feature after FEATURES.")

;; (setup auto-compile
;;   (auto-compile-on-load-mode)
;;   (auto-compile-on-save-mode))

;; (setup bind-key)

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (or (package-installed-p package min-version)
      (let* ((known (cdr (assoc package package-archive-contents)))
             (best (car (sort known (lambda (a b)
                                      (version-list-<= (package-desc-version b)
                                                       (package-desc-version a)))))))
        (if (and best (version-list-<= min-version (package-desc-version best)))
            (package-install best)
            (if no-refresh
                (error "No version of %s >= %S is available" package min-version)
                (package-refresh-contents)
                (require-package package min-version t)))
        (package-installed-p package min-version))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

(let ((package-check-signature nil))
  (require-package 'gnu-elpa-keyring-update))

(setup (:pkg async))

(provide 'init-elpa)
;;; init-elpa.el ends here
