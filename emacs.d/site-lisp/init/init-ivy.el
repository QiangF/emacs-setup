; -*- coding:utf-8 -*-

; * ivy
;; The default matcher will use a .* regex wild card in place of
;; each single space in the input.
;; If you want to use the fuzzy matcher, which instead uses a .*
;; regex wild card between each input letter,
; The ivy-initial-inputs-alist variable is pretty useful in conjunction with the
; default matcher.

;; ivy-switch-buffer, you get a list of not only the currently open
;; buffers but also bookmarks, recently opened files, and window
;; layouts. Ivy calls the window layouts “views”, Because of the way
;; views are named, it's easy to limit your choices to just views
;; when you call ivy-switch-buffer.


(setup (:pkg flx))

(setup (:pkg ivy-avy)
  (:autoload ivy-avy)
  (:load-after ivy)
  (:when-loaded
    (defun ivy-avy--action (pt)
      "Select the candidate represented by PT."
      (when (number-or-marker-p pt)
        (let ((bnd (ivy--minibuffer-index-bounds ivy--index ivy--length ivy-height)))
          (ivy--done
           (nth (+ (car bnd) (- (line-number-at-pos pt) 2)) ivy--old-cands)))))))

;; https://github.com/oantolin/icomplete-vertical
;; counsel-ibuffer does not handle frame switch? ido-switch-buffer does.
;; ("C-x b" #'counsel-ibuffer)
;; ("C-c v" #'ivy-push-view)
;; ("C-c V") . ivy-pop-view)

;; 当前路径确定是目录，直接按/或者C-j (ivy-alt-done)来补全目录路径。
;; Use Enter on a directory to navigate into the directory, not open it with dired.
;; (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)
;; "C-m" 'ivy-done
;; "C-j" 'ivy-alt-done
;; ivy-initial-inputs-alist by default matches only the beginning,
;; just hit space if you do want to search for a ^ substring.
;; search behaviour is controlled by ivy--regex-function and ivy-re-builders-alist
;; Press C-p and Enter to select current input as candidate

(setup (:pkg ivy)
   (:delay)
   (:require flx)
   (:bind "C-'" #'ivy-avy)
   (:global "C-c C-r" #'ivy-resume)
   (:with-map ivy-minibuffer-map
     (:bind "TAB" #''ivy-partial-or-done
            ;; ("<return>" #'ivy-alt-done)
            "C-<" #'ivy-minibuffer-shrink
            "C->" #'ivy-minibuffer-grow
            "C-g" #'my-ivy-keyboard-quit-dwim
            "M-y" #'ivy-next-line))
   (:when-loaded
     ;; omit space in the input string:
     ;; (add-to-list 'ivy-re-builders-alist '(counsel-M-x . ivy--regex-fuzzy))
     ;; (add-to-list 'ivy-re-builders-alist '(counsel-describe-function . ivy--regex-ignore-order))
     ;; (add-to-list 'ivy-re-builders-alist '(counsel-describe-variable . ivy--regex-ignore-order))
     ;; (setq ivy-re-builders-alist '((t . re-builder-extended-pattern)))

     (add-to-list 'ivy-height-alist '(counsel-yank-pop . 10))

     (defun ivy-with-thing-at-point (cmd &optional dir)
       "Wrap a call to CMD with setting "
       (let ((ivy-initial-inputs-alist
               (list
                (cons cmd (substring-no-properties (thing-at-point 'symbol))))))
         (funcall cmd nil dir)))

     (defun counsel-ag-from-here (&optional dir)
       "Start ag but from the directory the file is in."
       (interactive "D")
       (ivy-with-thing-at-point
        'counsel-ag
        (or dir (file-name-directory (buffer-file-name)))))

     (defun my-set-exwm-workspace ()
       (when (and (not (bound-and-true-p exwm--frame)) (not (derived-mode-p 'exwm-mode)) exwm_enable)
         (setq exwm--frame exwm-workspace--current)))

     (defun my-other-workspace-p (buffer-name)
       (if (get-buffer buffer-name)
           (with-current-buffer buffer-name
             (when (bound-and-true-p exwm--frame)
               (not (eq exwm--frame exwm-workspace--current))))
           nil))

     (when exwm_enable
       (add-hook 'buffer-list-update-hook 'my-set-exwm-workspace)
       (add-to-list 'ivy-ignore-buffers 'my-other-workspace-p)
       (advice-add 'ivy--switch-buffer-action :around 'my-exwm-workspace-switch-to-buffer))
     ;; when Debugger entered--Lisp error: (quit), toggle-debug-on-quit
     (defun my-ivy-keyboard-quit-dwim ()
       "If region active, deactivate. If there's content, clear the minibuffer. Otherwise quit."
       (interactive)
       (cond ((and delete-selection-mode (region-active-p))
              (setq deactivate-mark t))
             ((> (length ivy-text) 0)
              (delete-minibuffer-contents))
             (t (minibuffer-keyboard-quit))))

     (setq ivy-display-style 'fancy)
     (setq ivy-extra-directories nil) ;; Don't like seeing ./ and ../ in completions
     (setq ivy-use-virtual-buffers t)
     ;; better performance on everything (especially windows), ivy-0.10.0 required
     (setq ivy-dynamic-exhibit-delay-ms 250)
     ;; Press C-p and Enter to select current input as candidate
     (setq ivy-use-selectable-prompt t)
     ;; better performance on everything (especially windows), ivy-0.10.0 required
     ;; @see https://github.com/abo-abo/swiper/issues/1218
     (setq ivy-dynamic-exhibit-delay-ms 250)
     (setq ivy-use-selectable-prompt t)
     (ivy-mode 1)))

; * counsel
 ;select the current input instead of the current candidate?
 ;Use C-M-j (ivy-immediate-done)
;; counsel-locate
;; force an update of locate db with: sudo updatedb
;; on a candidate C-o open hydra option panel s and w to sroll action
;; c to toggle making the current action execute each time a new candidate is selected.
;; C-M-n to apply action on the next candidate

(setup (:pkg amx))

(setup (:pkg counsel)
  (:global "C-c C-/" #'counsel-bookmark
           "C-c r" #'counsel-linux-app
           "C-C s" #'counsel-grep
           "C-x C-f" #'counsel-find-file
           ;; ("C-x d" #'counsel-find-file)
           "C-x l" #'counsel-locate
           "C-c k" #'counsel-ag
           "C-x C-r" #'counsel-recentf
           "M-x" #'counsel-M-x
           "C-\"" #'counsel-imenu)
  (:with-feature helpful (:when-loaded
                      (setq counsel-describe-function-function 'helpful-callable
                            counsel-describe-variable-function 'helpful-variable)))

  (:when-loaded
    (with-eval-after-load 'multiple-cursors-core
      (add-to-list 'mc/cmds-to-run-once 'counsel-M-x))

    ;; ivy-sort-functions-alist
    (setq ivy-sort-matches-functions-alist
          '((t . nil)
            (plain-org-wiki . ivy--shorter-matches-first)
            (ivy-completion-in-region . ivy--shorter-matches-first)
            (ivy-switch-buffer . ivy-sort-function-buffer)))

    (setq counsel-yank-pop-truncate-radius 5)

    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

    (defun my-prepare-candidate-fit-into-screen (s)
      (let* ((w (frame-width))
             ;; display kill ring item in one line
             (key (replace-regexp-in-string "[ \t]*[\n\r]+[ \t]*" " ## " s)))
        ;; strip the whitespace
        (setq key (replace-regexp-in-string "^[ \t]+" "" key))
        ;; fit to the minibuffer width
        (if (> (length key) w)
            (setq key (concat (substring key 0 (- w 4)) "...")))
        (cons key s)))

    (defmacro my-select-from-kill-ring (fn)
      "If N > 1, yank the Nth item in `kill-ring'.
If N is nil, use `ivy-mode' to browse `kill-ring'."
      (interactive "P")
      `(let* ((candidates (cl-remove-if
                           (lambda (s)
                             (or (< (length s) 5)
                                 (string-match-p "\\`[\n[:blank:]]+\\'" s)))
                           (delete-dups kill-ring)))
              (ivy-height (/ (frame-height) 2)))
         (ivy-read "Browse `kill-ring':"
                   (mapcar #'my-prepare-candidate-fit-into-screen candidates)
                   :action #',fn)))

    (defun counsel--yank-pop-format-function (cand-pairs)
      "Transform CAND-PAIRS into a string for `counsel-yank-pop'."
      (ivy--format-function-generic
       (lambda (str)
         (mapconcat
          (lambda (s)
            (ivy--add-face s 'ivy-current-match))
          (split-string
           (counsel--yank-pop-truncate str) "\n" t)
          "\n"))
       (lambda (str)
         (car (my-prepare-candidate-fit-into-screen str)))
       cand-pairs
       (propertize counsel-yank-pop-separator 'face 'ivy-separator)))

    ;; @see https://oremacs.com/2015/07/23/ivy-multiaction/
    ;; press "M-o" to choose ivy action
    (ivy-set-actions
     'counsel-find-file
     '(("b" counsel-find-file-cd-bookmark-action "cd bookmark")
       ("x" counsel-find-file-extern "open externally")
       ("d" delete-file "delete")
       ("r" counsel-find-file-as-root "open as root")))

    (define-key read-expression-map (kbd "C-r") 'counsel-minibuffer-history)
    ;; Modify to return true for files you're interested in.
    (defun autosave-symlink-match (str)
      (and (not (cl-remove-if-not (lambda (x) (string-suffix-p x str))
                                  completion-ignored-extensions))
           (not (string-prefix-p ".#" str))))

    ;; Don't show autosave files in the completion...
    ;; Add the advice after completion.
    (defadvice completion-file-name-table (after ignoring-backups-f-n-completion
                                                 activate)
      (if (and (listp ad-return-value) (stringp (car ad-return-value))
               (cdr ad-return-value))
          (let ((newlis (cl-remove-if-not 'autosave-symlink-match ad-return-value)))
            ;; If there are no qualifiers, list everything.
            (if (and (listp newlis) (cdr newlis))
                (setq ad-return-value newlis)))))))

;; yanked text should not end with / for path
;; (defun counsel-recoll-function (string &rest _unused)
;;       "Issue recallq for STRING."
;;       (if (< (length string) 3)
;;           (counsel-more-chars 3)
;;           (counsel--async-command
;;            (format "recollq -b '%s'" string))
;;           nil))

;; (defun counsel-recoll (&optional initial-input)
;;   "Search for a string in the recoll database.
;;     You'll be given a list of files that match.
;;     Selecting a file will launch `swiper' for that file.
;;     INITIAL-INPUT can be given as the initial minibuffer input."
;;   (interactive)
;;   (ivy-read "recoll: " 'counsel-recoll-function
;;             :initial-input initial-input
;;             :dynamic-collection t
;;             :history 'counsel-git-grep-history
;;             :action (lambda (x)
;;                       (when (string-match "file://\\(.*\\)\\'" x)
;;                         (let ((file-name (match-string 1 x)))
;;                           (find-file file-name)
;;                           (unless (string-match "pdf$" x)
;;                             (swiper ivy-text)))))))

;; (defvar counsel-network-manager-history nil
;;      "Network manager history.")
;; (defun counsel-network-manager (&optional initial-input)
;;      "Connect to wifi network."
;;      (interactive)
;;      (shell-command "nmcli device wifi rescan")
;;      (let ((networks-list (s-split "\n" (shell-command-to-string "nmcli device wifi list"))))
;;        (ivy-read "Select network" networks-list
;;                  :initial-input initial-input
;;                  :require-match t
;;                  :history counsel-network-manager-history
;;                  :sort nil
;;                  :caller 'counsel-network-manager
;;                  :action (lambda (line)
;;                            (let ((network (car (s-split " " (s-trim (s-chop-prefix "*" line)) t))))
;;                              (message "Connecting to \"%s\".." network)
;;                              (async-shell-command
;;                               (format "nmcli device wifi connect %s" (shell-quote-argument network))))))))

(setup (:pkg ivy-yasnippet))

(provide 'init-ivy)
