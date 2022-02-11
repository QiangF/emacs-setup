; -*- coding:utf-8 -*-

;; https://github.com/realgud/realgud/
;; https://github.com/pappasam/jedi-language-server
;; use language server for large project
;; https://github.com/microsoft/pyright

(setup (:pkg jedi)
  (:when-loaded
    (setq jedi:setup-function nil)
    ;; (setq jedi:complete-on-dot t)
    (setf (symbol-function #'jedi:handle-post-command) (lambda nil nil))))

(setup (:pkg function-args))

(setup company-jedi
  (eval-after-load 'company-mode
    (my-set-company-backends 'python-mode-hook 'company-jedi)))

(setup lpy
   (:load-after python)
   (:with-hook python-mode-hook
     (:hook lpy-python-hook))
   (:when-loaded
     (require 'soap)
     (defun lpy-python-hook ()
       (lpy-mode)
       (company-mode)
       (jedi:setup)
       ;; (setq-local company-backends '(company-jedi company-dabbrev-code company-keywords))
       (setq-local completion-at-point-functions '(lispy-python-completion-at-point t))
       ;; (setq-local lispy-outline-header "# ")
       ;; (setq-local lispy-outline (concat "^" "# \\*+"))
       (setq-local outline-heading-end-regexp "\n")
       (setq-local fill-paragraph-function 'lpy-fill-paragraph)
       ;; (setq-local fill-forward-paragraph-function 'lpy-fill-forward-paragraph-function)
       ;; (setq-local forward-sexp-function 'lpy-forward-sexp-function)
       ;; (electric-indent-mode -1)
       (setq indent-tabs-mode nil
             tab-width 4))

     (defun lpy-forward-sexp-function (arg)
       (let* ((forward-sexp-function nil)
              (bnd
                (let ((forward-sexp-function nil))
                  (lpy-listp))))
         ;; inside function arglist
         (cond (bnd
                (save-restriction
                  (narrow-to-region (1+ (car bnd))
                                    (1- (cdr bnd)))
                  (if (> arg 0)
                      (lispy-dotimes arg
                        (cond ((= (point) (point-min))
                               (lpy-arg-forward))
                              ((or (lpy-arg-rightp)
                                   (lpy-arg-leftp))
                               (forward-char 1)
                               (lpy-arg-forward))
                              (t
                               (forward-sexp arg))))
                      (lispy-dotimes (- arg)
                        (if (or (= (point) (point-max))
                                (or (lpy-arg-rightp)
                                    (lpy-arg-leftp)))
                            (progn
                              (forward-sexp -1)
                              (skip-chars-backward ", \n")
                              (while (and (> (point) (point-min))
                                          (not (looking-at ",")))
                                (forward-sexp -1)
                                (skip-chars-backward ", \n"))
                              (skip-chars-forward ", \n")
                              (unless (bobp)
                                (backward-char)))
                            (backward-sexp))))))
               ((looking-at " +\\*")
                (goto-char (match-end 0)))
               (t
                (forward-sexp arg)))))

     (defun lpy-backward ()
       (interactive)
       (lispy--remember)
       (if (looking-back "^ *" (line-beginning-position))
           (let ((pt (point)))
             (when (= (char-after) 32)
               (forward-char))
             (python-nav-backward-up-list)
             (cond ((/= pt (point))
                    (when (looking-back "^ +" (line-beginning-position))
                      (backward-char 1)))
                   ((bolp)
                    (outline-back-to-heading)
                    nil)
                   (t
                    (python-nav-beginning-of-statement))))
           (back-to-indentation)
           (backward-char 1)))))

;; The package is "python" but the mode is "python-mode":
;; (setenv "PYTHONPATH" "/home/my_usr/bin_repo/conda3/bin")
;; (setq python-shell-exec-path '("/home/my_usr/bin_repo/conda3/bin"))
;; (setq python-shell-virtualenv-path "/home/my_usr/bin_repo/conda3/envs")
;; (setq python-shell-virtualenv-path nil)
;; (setq python-shell-virtualenv-root nil)
;; :interpreter ("python" . python-mode)
;; (require 'py-smart-operator)
;; (add-hook 'python-mode-hook 'py-smart-operator-mode)

(setup python
   (:file-match "\\.py\\'")
   (:when-loaded
     ;; see ~/.dotfiles/emacs/emacs.d/site-lisp/lpy/targets/plain.el
     (setq python-shell-interpreter "ipython"
           python-shell-prompt-detect-failure-warning t
           ;; fix ipython>=5 auto-complete feature from breaking the emacs sub-shell
           ;; python-shell-interpreter-args "-i --pylab --colors=Linux --profile=default --simple-prompt"
           ;; 解决lpy绘图后 plt.show() 显示图形窗口，emacs 会 freeze
           python-shell-interpreter-args "-i --simple-prompt --no-color-info --pylab"
           ;; python.el : a lot of IPython support is built into the default config.
           ;; python-shell-prompt-regexp "In \\[[0-9]+\\]: "
           ;; python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
           ;; python-shell-completion-setup-code
           ;; "from IPython.core.completerlib import module_completion"
           ;; python-shell-completion-module-string-code
           ;; "';'.join(module_completion('''%s'''))\n"
           ;; python-shell-completion-string-code
           ;; "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
           python-indent-offset 4)

     (dolist (k '("+" "-" "*" "/" "%" "&" "|" "<" "=" ">" ","))
       (define-key python-mode-map (kbd k) 'soap-command))


     (defun ora-get-py-fname ()
       "Get the file name of a visibile `python-mode' buffer."
       (let ((b (window-buffer
                 (cl-find-if (lambda (w)
                               (with-current-buffer (window-buffer w)
                                 (eq major-mode 'python-mode)))
                             (window-list)))))
         (if b
             (file-name-nondirectory
              (buffer-file-name b))
             "")))

     (defun ora-python-shell-send-region (start end &optional nomain)
       "Send the region delimited by START and END to inferior Python process."
       (interactive "r")
       (let* ((python--use-fake-loc
                (not buffer-file-name))
              (string (python-shell-buffer-substring start end nomain))
              (process (python-shell-get-or-create-process))
              (_ (string-match "\\`\n*\\(.*\\)" string)))
         (let* ((temp-file-name (python-shell--save-temp-file string))
                (file-name (or (buffer-file-name) temp-file-name)))
           (python-shell-send-file file-name process temp-file-name t)
           (unless python--use-fake-loc
             (with-current-buffer (process-buffer process)
               (compilation-fake-loc (copy-marker start) temp-file-name
                                     2))))))

     (defun ora-inferior-python-hook ()
       (setq next-error-function 'ora-comint-next-error-function))

     (defun ora-comint-next-error-function (n &optional reset)
       (interactive "p")
       (when reset
         (setq compilation-current-error nil))
       (let* ((msg (compilation-next-error (or n 1) nil
                                           (or compilation-current-error
                                               compilation-messages-start
                                               (point-min))))
              (loc (compilation--message->loc msg))
              (file (caar (compilation--loc->file-struct loc)))
              (buffer (find-file-noselect file)))
         (pop-to-buffer buffer)
         (goto-char (point-min))
         (forward-line (1- (cadr loc)))
         (back-to-indentation)
         (unless (bolp)
           (backward-char))))

     (defun my-switch-to-jupyter ()
       "Switch to using Jupyter shell."
       (interactive)
       (setq python-shell-interpreter "jupyter"
             python-shell-interpreter-args "console --simple-prompt"
             python-shell-prompt-detect-failure-warning nil))))

;; (local-set-key (kbd "M-n") 'flymake-goto-next-error)
;; (local-set-key (kbd "M-p") 'flymake-goto-prev-error)

;; auto-virtualenvwrapper
;;   (add-hook 'python-mode-hook #'auto-virtualenvwrapper-activate)

;; setup poetry

;; conda-anaconda-home is the equivalent to the ANACONDA_HOME environment variable (i.e. contains all files of your Anaconda installation)
;; conda-env-home-directory - is the directory where your virtual environments get stored (within the envs subdirectory)
;; (add-to-list 'exec-path "/home/my_usr/bin_repo/conda3")
;; (setenv "PATH" "/home/my_usr/bin_repo/conda3" '("PATH"))

;; 1. M-x conda activate python
;; conda activate base:
;; (conda-env-activate "..")
(setup pythonic)

(setup conda
  (custom-set-variables '(conda-anaconda-home "/home/my_usr/bin_repo/conda3"))
  (:when-loaded
    ;; interactivate shell support
    (conda-env-initialize-interactive-shells)
    ;; eshell support
    (conda-env-initialize-eshell)
    ;; (conda-env-autoactivate-mode t)
    (defun conda--get-path-prefix (env-dir)
      "Get a platform-specific path string to utilize the conda env in ENV-DIR.
It's platform specific in that it uses the platform's native path separator."
      (s-trim
       (with-output-to-string
           (with-current-buffer standard-output
             (process-file shell-file-name nil '(t nil) nil shell-command-switch
                           (format "%s/bin/conda ..activate \"%s\" %s"
                                   conda-anaconda-home
                                   (if (eq system-type 'windows-nt)
                                       "cmd.exe"
                                       "bash")
                                   env-dir))))))))

;; By setting the WORKON_HOME environment variable we can select
;; which pyenv virtual environment we want to use by calling M-x
;; pyvenv-workon. One can also call M-x pyvenv-activate to choose an
;; environment via manual filesystem navigation.

;; 2. Activate your virtualenv with:
;; (pyvenv-activate "C:\Program Files\Anaconda3\envs\python2")
;; If the binaries in your virtualenv are names "python2", you will need to set
;; (setq python-shell-interpreter "python2")

;; (setup pyvenv
;;   :ensure t
;;   :init
;;    ;; (setenv "WORKON_HOME" "~/.pyenv/versions")
;;    ;; (setenv "WORKON_HOME" (concat (getenv "CONDA_PREFIX") "/envs"))
;;    (setenv "WORKON_HOME"  "/home/my_usr/bin_repo/conda3/envs")
;;    (pyvenv-mode 1)
;; )

;; (setup realgud-ipdb)

(provide 'init-python)
