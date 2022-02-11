; -*- coding:utf-8; lexical-binding: t -*-

; * git-annex 

;; (use-package dired-k
;;  :ensure
;;  :bind (:map dired-mode-map
;;              ("g" . dired-k))
;;  ;; :hook (dired-initial-position . dired-k)
;;  :config
;;    (setq dired-k-human-readable t))

;; Colourful dired; conflict with git annex
(setup (:pkg diredfl)
  (diredfl-global-mode))

;; (setup magit-annex)

;; https://github.com/mm--/dot-emacs
;; use the patched git-annex at ~/.dotfiles/emacs/emacs.d/site-lisp/git-annex
;; better to unlock file explicitly to prevent file corruption
;; git-annex sync --content in repo with unlocked files will make a commit
;; if the commit are made in repo outdated may cause files being unwantly delted

;; toggle read-only unlock the file for editing:
;; (defadvice read-only-mode (before git-annex-edit-file activate)
;;   (git-annex--toggle-unlock))
;; :commands git-annex--toggle-unlock
;; :defines git-annex-dired-annexed-invisible

(setup git-annex
  (:needs "git-annex")
  (:load-after dired)
  (:option git-annex-commit nil
           vc-follow-symlinks t)
  (:with-hook before-save-hook
    (:hook my-maybe-unlock-file))
  (:with-map dired-mode-map
    (:bind "% a" #'jmm/dired-mark-files-git-annex-matching))
  (:with-map git-annex-dired-map
    (:bind "l" #'git-annex-dired-lock-files
           "u" #'git-annex-dired-unlock-files
           "f" #'magit-annex-file-action
           "t" #'jmm/dired-git-annex-tag
           "s" #'jmm/dired-git-annex-print-human-file-size
           "S" #'jmm/dired-git-annex-add-real-file-sizes
           "* u" #'jmm/dired-mark-git-annex-unavailable-files))
  (:when-loaded
    (git-annex-dired-do-to-files "drop" "Annex: dropped %d file(s)")
    (git-annex-dired-do-to-files "edit" "Annex: unlocked %d file(s) for editing")
    (git-annex-dired-do-to-files "get" "Annex: got %d file(s)")
    (git-annex-dired-do-to-files "lock" "Annex: lock %d file(s)" t)
    ;; disable the “buffer is read-only” warning for locked annex files
    ;; (defun barf-if-buffer-read-only () t)

    (defun my-file-writable-p (orig-fun file-name)
      (let ((file-list (list file-name)))
        (or (apply orig-fun file-list)
	        (let ((target (nth 0 (file-attributes file-name))))
	          (and (stringp target)
                   (string-match "\\.git/annex/" target))))))

    (advice-add 'file-writable-p :around #'my-file-writable-p)

    (require 'cl-macs)
    (defun my-maybe-unlock-file (&optional ARG)
      (when (and buffer-file-name
                 (file-symlink-p buffer-file-name)
                 (string= (vc-backend buffer-file-name) "Git"))
	    (let ((target (nth 0 (file-attributes buffer-file-name))))
	      (cl-assert (stringp target))
	      (when (string-match "\\.git/annex/" target)
		    (call-process "git-annex" nil nil nil "edit"
					      (file-relative-name buffer-file-name default-directory))))))
    ;; not working
    ;; (advice-add 'save-buffer :before 'my-maybe-unlock-file)
    ;; (add-hook 'find-file-hook 'my-maybe-unlock-file -100)

    (defun jmm/git-annex-find-files (&rest args)
      "Generate a list of git annex files that match ARGS.
    For example, ARGS could be \"--in=here\""
      (-remove #'s-blank?
               (s-split "\0"
                        (shell-command-to-string (mapconcat #'identity
                                                            (append '("git annex find --print0") args)
                                                            " ")))))
    (defun eshell/dga (&rest args)
      "Show a `dired' buffer of git annex files that match ARGS.
    For example, ARGS could be \"--in=here\""
      (dired (cons "." (apply #'jmm/git-annex-find-files args))))

    (defun eshell/gaf (&rest args)
      "Return a list of git annex files that match ARGS.
    For example, ARGS could be \"--in=here\""
      (apply #'jmm/git-annex-find-files args))

    (defvar-local jmm/git-annex-directory-tags nil
      "Current git-annex tags set in the directory, as a list.")

    (defun jmm/dired-git-annex-current-tags (file-list &optional intersection)
      "Get current git-annex tag for each file in FILE-LIST. With
    optional argument INTERSECTION, only show tags all files share in common."
      (let* ((metadata (with-output-to-string
                           (with-current-buffer
                               standard-output
                             (apply #'process-file "git" nil t nil "annex" "metadata" "--json" file-list))))
             (json-array-type 'list)
             (jsonout (-map 'json-read-from-string (split-string metadata "\n" t))))
        (-reduce (if intersection '-intersection '-union) (--map (cdr (assoc 'tag (cdr (assoc 'fields it)))) jsonout))))

    (defun jmm/dired-git-annex-tag (file-list tags &optional arg)
      "Add git-annex TAGS to each file in FILE-LIST.
    Used as an interactive command, prompt for a list of tags for all
    files, showing the current tags all files currently have in common."
      (interactive
       (let* ((files (dired-get-marked-files t current-prefix-arg))
              (shared-tags (jmm/dired-git-annex-current-tags files t))
              ;; Cache directory tags
              (current-tags (or jmm/git-annex-directory-tags
                                (setq jmm/git-annex-directory-tags
                                      (or (jmm/dired-git-annex-current-tags '("--all")) '("")))))
              (crm-separator " ")
              (crm-local-completion-map
                (let ((map (make-sparse-keymap)))
                  (set-keymap-parent map crm-local-completion-map)
                  (define-key map " " 'self-insert-command)
                  map))
              (tags (completing-read-multiple
                     "Tags: " (--map (concat it crm-separator) current-tags)
                     nil nil
                     (when shared-tags (mapconcat 'identity shared-tags " ")))))
         (setq jmm/git-annex-directory-tags (-union tags jmm/git-annex-directory-tags))
         (list files tags current-prefix-arg)))
      (let ((args (cl-loop for x in tags
                           append (list "-t" x))))
        (-each file-list
          (lambda (file)
            (apply #'call-process "git" nil nil nil "annex" "metadata" (append args (list file)))))
        (message (format "Tagged %d file(s)" (length file-list)))))

    (defun jmm/dired-mark-git-annex-unavailable-files ()
      "Mark git-annex files that are not present."
      (interactive)
      (dired-mark-if
       (and (looking-at-p ".* -> \\(.*\\.git/annex/.+\\)")
            (not (file-exists-p (file-truename (dired-get-filename t)))))
       "unavailable file"))

    (defun jmm/dired-mark-files-git-annex-matching (matchingoptions &optional marker-char)
      "Mark all files that match git annex's MATCHINGOPTIONS for use in later commands.
    A prefix argument means to unmark them instead.
    `.' and `..' are never marked."
      (interactive
       (list (read-string (concat (if current-prefix-arg "Unmark" "Mark")
                                  " files matching (git annex match expression): ")
                          nil 'jmm-dired-annex-matchingoptions-history)
             (if current-prefix-arg ?\040)))
      (let ((dired-marker-char (or marker-char dired-marker-char)))
        (dired-mark-if
         (and (not (looking-at-p dired-re-dot))
              (not (eolp))              ; empty line
              (let ((fn (dired-get-filename nil t)))
                (when (and fn (not (file-directory-p fn)))
                  (message "Checking %s" fn)
                  (s-present? (shell-command-to-string
                               (mapconcat
                                #'identity
                                (list "git annex find" matchingoptions (shell-quote-argument fn))
                                " "))))))
         "matching file")))

    (defun jmm/git-annex-file-target (filename)
      "If FILENAME is a git annex file, return its symlink target."
      (-when-let (symname (and filename
                               (file-symlink-p filename)))
        (when (string-match-p ".*\\.git/annex/.+" symname)
          symname)))

    (defun jmm/dired-git-annex-file-target ()
      "If the dired file at point is a git annex file, return its symlink target."
      (jmm/git-annex-file-target (dired-get-filename nil t)))

    (defun jmm/git-annex-file-size (filename)
      "Try to determine the size of the git annex file FILENAME."
      (-when-let (target (jmm/git-annex-file-target filename))
        (or (save-match-data
              (when (string-match "SHA256E-s\\([0-9]+\\)--" target)
                (string-to-number (match-string 1 target))))
            (-some-> (expand-file-name target (file-name-directory filename))
              file-attributes
              file-attribute-size))))

    (defun jmm/dired-git-annex-print-human-file-size ()
      "Try to print the human readable file size of the dired git-annex file at point."
      (interactive)
      (let* ((filename (dired-get-filename nil t))
             (string-file (file-name-nondirectory filename)))
        (-if-let (filesize (-some-> (jmm/git-annex-file-size filename)
                             file-size-human-readable))
            (message "%s - %s" filesize string-file)
          (message "Can't determine git annex file size of %s" string-file))))

    ;; Based off of `dired--align-all-files'
    (defun jmm/dired-git-annex-add-real-file-sizes ()
      "Go through all the git-annex files in dired, replace the
    symlink file size with the real file size, then try to align
    everything."
      (interactive)
      (require 'dired-aux)
      (let ((regexp directory-listing-before-filename-regexp))
        (save-excursion
          (goto-char (point-min))
          (dired-goto-next-file)
          (while (or (dired-move-to-filename)
                     (progn (save-restriction
                              (narrow-to-region (dired-subdir-min) (dired-subdir-max))
                              (dired--align-all-files))
                            (dired-next-subdir 1 t)
                            (dired-goto-next-file)
                            (dired-move-to-filename)))
            (let ((inhibit-read-only t))
              (when (and (jmm/dired-git-annex-file-target)
                         (re-search-backward regexp (line-beginning-position) t))
                (goto-char (match-beginning 0))
                (-when-let (newsize (-some-> (jmm/git-annex-file-size (dired-get-filename nil t))
                                      file-size-human-readable))
                  (search-backward-regexp "[[:space:]]" nil t)
                  (when (re-search-forward "[[:space:]]+\\([^[:space:]]+\\)[[:space:]]" nil t)
                    (goto-char (match-beginning 1))
                    (delete-region (point) (match-end 1))
                    (insert-and-inherit newsize))))
              (forward-line))))))

    ;; (add-hook 'dired-after-readin-hook #'jmm/dired-git-annex-add-real-file-sizes)

    (defun jmm/dired-dir-files-beginning ()
      "First point where there's a filename on the line. Beginning of line."
      (save-excursion
        (goto-char (dired-subdir-min))
        (dired-goto-next-file)
        (beginning-of-line)
        (point)))

    (defun jmm/dired-dir-files-end ()
      "Last point where there's a filename. End of line."
      (save-excursion
        (goto-char (dired-subdir-max))
        (while (not (dired-get-filename nil t))
          (dired-previous-line nil))
        (end-of-line)
        (point)))

    (defun jmm/dired-file-size ()
      "Return the file size of a file at point (for sorting). Takes
    into account git-annex files."
      (let* ((filename (dired-get-filename nil t))
             (string-file (file-name-nondirectory filename)))
        (or (jmm/git-annex-file-size filename)
            (file-attribute-size (file-attributes filename)))))

    (defun jmm/dired-sort-size (&optional ascending)
      "Sort some dired lines by size (consider annex sizes).
    With optional argument ASCENDING, sort by ascending file size. (I
    like going the other way around usually.)"
      (interactive "P")
      (if (string= "Git" (vc-responsible-backend default-directory))
          (let (buffer-read-only
                (beg (jmm/dired-dir-files-beginning))
                (end (jmm/dired-dir-files-end)))
            (save-excursion
              (save-restriction
                (narrow-to-region beg end)
                (goto-char (point-min))
                (sort-subr (not ascending)
                           'forward-line 'end-of-line
                           #'jmm/dired-file-size nil))))
          (progn
            (setq dired-listing-switches (concat dired-default-listing-switches " -t"))
            (dired-sort-other dired-listing-switches))))))

; * dired extension
; install ranger
;; 1 mark files, 2 ranger command, 3 go to target dir, 4 ranger copy or move
; ** tips
;; dired-do-search string in marked files
;; dired-undo to undo file moving
;; M-+ M-i diredp-insert-subdirs-recursive (show contents of subdir in one buffer)
; *** wdired makes file names editable
; C-x C-q to enter, C-c C-c to save
;; find-dired https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired-and-Find.html
;; counsel locate
; *** hide file
; `C-x d f*.html' opens Dired on only files that start with `f' and end
; with `.html'
; C-x d binds to ido-dired, use M-x dired open full path
; *** short keys
; s sort, / filter, * or % mark
; C-x C-j jump to file or link
; D delete to trash, C-u D delete for ever to do
; S is symlink
; ** setting

(setup (:pkg dash))

(setup (:pkg dired-extension)
  (:load-after dired))

(setup dired-hacks-utils
  (:load-after dired))

; * diredp 
;; (use-package dired+
;;  :after dired
;;  :demand t
;;  :commands my-dired-do-delete
;;  :config
;;    (setq-default diredp-hide-details-initially-flag t)
;;    ;; delete marked files permanently
;;    (defun my-dired-do-delete (&optional arg)  ; Bound to `D'
;;      "Delete all marked (or next ARG) files.
;;    NOTE: This deletes marked, not flagged, files.
;;    `dired-recursive-deletes' controls whether deletion of
;;    non-empty directories is allowed."
;;      (interactive "P")
;;      ;; This is more consistent with the file-marking feature than
;;      ;; `dired-do-flagged-delete'.  But it can be confusing to the user,
;;      ;; especially since this is usually bound to `D', which is also the
;;      ;; `dired-del-marker'.  So offer this warning message:
;;      (unless arg
;;        (ding)
;;        (message "NOTE: Deletion of files marked `%c' (not those flagged `%c')."
;;                 dired-marker-char dired-del-marker))
;;      (diredp-internal-do-deletions
;;       ;; This can move point if ARG is an integer.
;;       (dired-map-over-marks (cons (dired-get-filename) (point)) arg) arg nil))

;;    (diredp-toggle-find-file-reuse-dir nil)
;;    (diredp-make-find-file-keys-reuse-dirs) 
;;    (set-face-attribute 'diredp-omit-file-name nil
;;                        :strike-through nil)
;;    (set-face-attribute 'diredp-flag-mark nil
;;                        :foreground "white")
;;    )

; * dired
;; auto-reverted if it has no inserted subdirs
;; (setq dired-auto-revert-buffer  (lambda (_dir) (null (cdr dired-subdir-alist))))
;; But keep in mind that dired-auto-revert-buffer has an effect only
;; when you "revisit" an existing Dired buffer (C-x d) . It is not
;; enough, for example, to just reselect its window or make its
;; buffer current.
(setup dired
   (:also-load dired-x dired-aux)
   (:option dired-omit-verbose nil
            dired-omit-files (concat dired-omit-files "\\|^\\..+$\\|\\*~$")
            dired-recursive-copies 'always
            dired-recursive-deletes 'always
            dired-create-destination-dirs 'always
            dired-do-revert-buffer t
            dired-hide-details-hide-symlink-targets nil
            dired-isearch-filenames 'dwim
            delete-by-moving-to-trash t
            dired-auto-revert-buffer t
            dired-listing-switches "-alh --group-directories-first --time-style=iso"
            dired-default-listing-switches "-alh --group-directories-first --time-style=iso"
            ;; dired-listing-switches "-Al"
            ;; dired-default-listing-switches "-alhtvgoLBX --group-directories-first"
                                        ; set omit first, others use the setting
            ;; symlink
            find-file-visit-truename nil
            ls-lisp-dirs-first t
            dired-ls-F-marks-symlinks t
            dired-clean-confirm-killing-deleted-buffers nil
            dired-no-confirm '(byte-compile
                               load chgrp chmod chown
                               copy move hardlink symlink
                               shell touch)
            dired-dwim-target t)
   (:local-set truncate-lines t)
   (:bind "C-j" #'dired-jump-other-window
          "@" #'git-annex-dired-map
          ;; ("M-<" #'dired-move-to-first-file)
          ;; ("M->" #'dired-move-to-last-file)
          "s" #'hydra-dired-sort/body
          "D" #'my-dired-do-delete
          "C-x M-o" #'dired-omit-switch
          ;; ("C-x d" #'ido-dired)
          "C-x d" #'dired
          "=" #'ora-ediff-files
          "0" #'dired-back-to-start-of-files
          "<backspace>" #'dired-up-directory
          "TAB" #'dired-subtree-cycle
          "i" #'dired-subtree-toggle
          ")" #'dired-git-info-mode)
   ;; (:hook #'dired-hide-details-mode)
   (:when-loaded (with-eval-after-load 'frowny
                   (add-to-list 'frowny-inhibit-modes #'dired-mode))

                 (defun my-ignore-cwd ()
                   (interactive)
                   (shell-command "cp ~/.dotfiles/shell/gitignore .gitignore"))

                 ;; delete marked files permanently
                 (defun my-dired-do-delete (&optional arg) ; Bound to `D'
                   (interactive "P")
                   (unless arg
                     (ding)
                     (message "NOTE: Deletion of files marked `%c' (not those flagged `%c')."
                              dired-marker-char dired-del-marker))
                   (dired-internal-do-deletions
                    ;; This can move point if ARG is an integer.
                    (dired-map-over-marks (cons (dired-get-filename) (point-marker)) arg) arg nil))

                 ;; ls-lisp also works in windows
                 ;; (require 'ls-lisp)
                 ;; (setq ls-lisp-use-insert-directory-program t)
                 ;; (setq ls-lisp-verbosity nil)

                 ;; git-annex.el clobbers dired-marked-face and dired-flagged-face fix
                 ;; diredp-font-lock-keywords-1 has a higher priority than dired-font-lock-keywords
                 (defun my-rename-file (file newname &optional ok-if-already-exists)
                   "fix path for relative symlink"
                   (let* ((file-name (file-name-nondirectory file))
                          (new-dir-name (file-name-directory newname))
                          (new-file-path (expand-file-name file-name new-dir-name))
                          (relative-symlink (file-symlink-p new-file-path)))
                     (when (and relative-symlink (string-equal system-type "gnu/linux"))
                       (dired-make-relative-symlink
                        ;; overwrite the invalid symlink
                        (expand-file-name relative-symlink (file-name-directory file)) new-file-path t))))
                 (advice-add #'rename-file :after #'my-rename-file)

                 ;; disable ido for dired
                 ;; (add-hook 'ido-setup-hook (lambda () (define-key (cdr ido-minor-mode-map-entry) [remap dired] nil)))
                 (defun dired-back-to-start-of-files ()
                   (interactive)
                   (backward-char (- (current-column) 2)))
                 ;; as you navigate around the directory tree you accumulate Dired buffers,
                 ;; one for each directory you visit. set it to t to DiredReuseDirectoryBuffer

                 ;; *** dired-sort
                 ;; dired remember current sort state
                 (defun dired-sort-ctime ()
                   "Dired sort by create time."
                   (interactive)
                   (progn
                     (setq dired-listing-switches (concat dired-default-listing-switches " -ct"))
                     (dired-sort-other dired-listing-switches)))

                 (defun dired-sort-utime ()
                   "Dired sort by access time."
                   (interactive)
                   (progn
                     (setq dired-listing-switches (concat dired-default-listing-switches " -ut"))
                     (dired-sort-other dired-listing-switches)))

                 (defun dired-sort-extension ()
                   "Dired sort by extension."
                   (interactive)
                   (progn
                     (setq dired-listing-switches (concat dired-default-listing-switches " -X"))
                     (dired-sort-other dired-listing-switches)))

                 (defun dired-sort-size ()
                   "Dired sort by time."
                   (interactive)
                   (if (and  (fboundp 'jmm/dired-sort-size) (string= (vc-backend buffer-file-name) "Git"))
                       (jmm/dired-sort-size)
                       (progn
                         (setq dired-listing-switches (concat dired-default-listing-switches " -S"))
                         (dired-sort-other dired-listing-switches))))

                 (defun dired-sort-time ()
                   "Dired sort by time."
                   (interactive)
                   (progn
                     (setq dired-listing-switches (concat dired-default-listing-switches " -t"))
                     (dired-sort-other dired-listing-switches)))

                 (defun dired-sort-name ()
                   "Dired sort by name."
                   (interactive)
                   (progn
                     (setq dired-listing-switches (concat dired-default-listing-switches ""))
                     (dired-sort-other dired-listing-switches)))

                 (defun diredp-next-dirline (arg &optional opoint) ; Bound to `>'
                   "Goto ARGth next directory file line.
If `diredp-wrap-around-flag' is non-nil then wrap around if none is
found before the buffer beginning (buffer end, if ARG is negative).
Otherwise, raise an error or, if NO-ERROR-IF-NOT-FOUND is nil, return
nil."
                   (interactive (let ((narg  (prefix-numeric-value current-prefix-arg)))
                                  (when (and (boundp 'shift-select-mode)  shift-select-mode) (handle-shift-selection)) ; Emacs 23+
                                  (list narg))) ; Equivalent to "^p"
                   (or opoint  (setq opoint  (point)))
                   (if (if (> arg 0)
                           (re-search-forward dired-re-dir nil t arg)
                           (beginning-of-line)
                           (re-search-backward dired-re-dir nil t (- arg)))
                       (dired-move-to-filename) ; user may type `i' or `f'
                       (if diredp-wrap-around-flag
                           (let ((diredp-wrap-around-flag  nil))
                             (goto-char (if (< arg 0) (point-max) (point-min)))
                             (diredp-next-dirline arg opoint))
                           (goto-char opoint)
                           (error "No more subdirectories"))))

                 (defun diredp-prev-dirline (arg) ; Bound to `<'
                   "Goto ARGth previous directory file line."
                   (interactive (let ((narg  (prefix-numeric-value current-prefix-arg)))
                                  (when (and (boundp 'shift-select-mode)  shift-select-mode) (handle-shift-selection)) ; Emacs 23+
                                  (list narg))) ; Equivalent to "^p"
                   (diredp-next-dirline (- arg)))

                 (defun my-dired-go-to-first-file ()
                   (interactive)
                   (dired-move-to-last-file)
                   (condition-case err
                       (progn (diredp-prev-dirline 1)
                              (dired-next-file-line))
                     ('error (beginend-dired-mode-goto-beginning))))
                 ;; use :post to run function on exit
                 (defhydra hydra-dired-sort (:color blue)
                   ("f" my-dired-go-to-first-file "go-to-first-file")
                   ("<" dired-move-to-first-file "go-to-first-line")
                   (">" dired-move-to-last-file "go-to-last-line")
                   ("s" dired-sort-size "size")
                   ("x" dired-sort-extension "extension")
                   ("t" dired-sort-time "modified")
                   ("c" dired-sort-ctime "created")
                   ("a" dired-sort-utime "accessed")
                   ("n" dired-sort-name "name")
                   ("<escape>" nil "quit"))

                 ;; *** dired-omit (mess up with git annex font lock)
                 (defvar v-dired-omit t
                   "If dired-omit-mode enabled by default. Don't setq me.")

                 (defun dired-omit-switch ()
                   "This function is a small enhancement for `dired-omit-mode', which will
   \"remember\" omit state across Dired buffers."
                   (interactive)
                   (if (eq v-dired-omit t)
                       (setq v-dired-omit nil)
                       (setq v-dired-omit t))
                   (dired-omit-caller)
                   (revert-buffer))

                 (defun dired-omit-caller ()
                   (if v-dired-omit
                       (setq dired-omit-mode t)
                       (setq dired-omit-mode nil)))

                 (add-hook 'dired-mode-hook 'dired-omit-caller)

                 ;; folder rights info on . and .. is helpful, . and .. can be hidden with
                 ;; (setq dired-omit-files
                 ;;     (rx (or (seq bol (? ".") "#")
                 ;;             (seq bol "." eol)
                 ;;             (seq bol ".." eol)
                 ;;             )))

                 ;; file and folder size
                 (defun dired-get-size ()
                   (interactive)
                   (let ((files (dired-get-marked-files)))
                     (with-temp-buffer
                       (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
                       (message "Size of all marked files: %s"
                                (progn
                                  (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                                  (match-string 1))))))

                 (defun my-set-xref-buffer ()
                   (setq-local outline-minor-mode-hook nil)
                   (outline-minor-mode t)
                   (setq outline-level (lambda() 1)
                         outline-regexp "^/.*")
                   (font-lock-add-keywords nil
                                           '(("^/.*" . font-lock-function-name-face)
                                             ("^[ \t]*[0-9]+:" . font-lock-keyword-face)) 'set))

                 (add-hook 'xref--xref-buffer-mode-hook #'my-set-xref-buffer)

                 (defun my-create-non-existent-directory ()
                   (let ((parent-directory (file-name-directory buffer-file-name)))
                     (when (and (not (file-exists-p parent-directory))
                                (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
                       (make-directory parent-directory t))))
                 (add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

                 (defvar dired-ediff-files nil)

                 (defun ora-ediff-files ()
                   (interactive)
                   (let* ((files (dired-get-marked-files))
                          (files-num (length files))
                          (current-file-path (ignore-errors (dired-get-filename)))
                          file1 file2
                          (wnd (current-window-configuration)))

                     (cond
                       ((< files-num 2)
                        (if (equal dired-ediff-files current-file-path)
                            (setq dired-ediff-files nil)
                            (if dired-ediff-files
                                (setq file1 dired-ediff-files
                                      file2 current-file-path)
                                (setq dired-ediff-files current-file-path)
                                (message "Add to ediff: %s" current-file-path)
                                (dired-next-line 1))))
                       ((= files-num 2)
                        (progn
                          (setq file1 (car files))
                          (setq file2 (cadr files))))
                       (t (error "no more than 2 files should be marked")))

                     ;; (file-newer-than-file-p file1 file2)
                     (when (and file1 file2)
                       (ediff-files file1 file2)
                       (setq dired-ediff-files nil)
                       (add-hook 'ediff-after-quit-hook-internal
                                 (lambda ()
                                   (setq ediff-after-quit-hook-internal nil)
                                   (set-window-configuration `,wnd))))))

                 ;; make sure git-annex-dired-annexed-invisible is defined
                 (eval-after-load "dired+"
                   '(progn
                     ;; the following conflict with show size hack
                     (add-to-list 'diredp-font-lock-keywords-1
                      (list "^[ ]+.* -> .*\\.git/annex/"
                       '("\\(.+\\)\\( -> .+\\)" (dired-move-to-filename) nil
                         (1 dired-symlink-face)
                         (2 git-annex-dired-annexed-invisible))))
                     (add-to-list 'diredp-font-lock-keywords-1
                      (list "^[*].* -> .*\\.git/annex/"
                       '("\\(.+\\)\\( -> .+\\)" (dired-move-to-filename) nil
                         (1 dired-marked-face)
                         (2 git-annex-dired-annexed-invisible))))
                     (add-to-list 'diredp-font-lock-keywords-1
                      (list "^[D].+ -> .*\\.git/annex/"
                       '("\\(.+\\)\\( -> .+\\)" (dired-move-to-filename) nil
                         (1 dired-flagged-face)
                         (2 git-annex-dired-annexed-invisible))))))))

;; *** sudo
;; (use-package sudo-edit            ; Edit files as root, through Tramp
;;   :ensure t
;;   :bind (("C-c f s" . sudo-edit)
;;          ("C-c f S" . sudo-edit-current-file))
;;   :init
;;     (defun su-buffer ()
;;       (interactive)
;;       (require 'tramp)
;;       (let ((pos (point))
;;             (dir (expand-file-name default-directory)))
;;         (if (eq major-mode 'dired-mode)
;;             (dired (concat "/sudo:root@localhost:" dir))
;;           (find-alternate-file
;;            (concat "/sudo:root@localhost:" (buffer-file-name (current-buffer)))))
;;         (goto-char pos)))
;; )

;; (use-package sudo-ext
;;  :ensure t)

;; automatic switch to root is a bad idea
;; su-mode cause annexed file saving without unlock
;; (use-package su
;;  :ensure t
;;  :init
;;    (su-mode +1))

;; (use-package auto-sudoedit
;;  :ensure t
;;  :commands auto-sudoedit)

(setup (:pkg beginend)
  (:load-after dired)
  (:with-map dired-mode-map
    (:bind "M-<" #'beginend-dired-mode-goto-beginning
           "M->" #'beginend-dired-mode-goto-end))
  (:when-loaded
    (beginend-global-mode)))

(setup dired-toggle-sudo)

;; use file extension to select pack program 
;; set 7z solid archvie off (-ms=off) 
(setup (:pkg pack)
  (:with-map dired-mode-map
    (:bind "P" #'pack-dired-dwim))
  (:option pack-dired-default-extension ".7z"
           pack-program-alist '(("\\.7z\\'"
                                 :pack
                                 ("7z" "a" "-ms=off" archive sources)
                                 :pack-append
                                 ("7z" "a" archive sources)
                                 :unpack
                                 ("7z" "x" archive))
                                ("\\.zip\\'"
                                 :pack
                                 ("zip" "-r" archive sources)
                                 :pack-append
                                 ("zip" "-r" archive sources)
                                 :unpack
                                 ("unzip" archive)))))

;; apt-get install avfs
;; run `mountavfs' before using this
(setup (:pkg dired-avfs)
  (:when-loaded
    (setq dired-avfs-file-size-threshold 1000
          dired-avfs-archives (append dired-avfs-archives '("dar" "7z")))
    ;; attention: create file under ~/.avfs will freeze emacs
    ;; don't hide avfs root
    (defun dired-avfs--hide-root ())))

(setup (:pkg fd-dired))

;; (use-package dired-open
;;  :ensure t
;; )

(setup openwith
  (:load-after dired)
  (:with-map dired-mode-map
    (:bind "C-c o" #'my-ext-open))
  (:when-loaded
    (defun my-ext-open () ;;dired-open-file ()
      "In dired, open the file named on this line."
      (interactive)
      (let* ((file (if (equal major-mode 'dired-mode)
                       (dired-get-filename nil t)
                       (if mark-active (buffer-substring (region-beginning) (region-end)) nil))))
        ;; (message "Opening %s..." file)
        (openwith-open-unix "/home/my_usr/scripts/mimi/xdg-open" (list file))
        ;; (call-process "/home/my_usr/scripts/linopen/xdg-open" nil 0 nil file)
        ;; libfile-mimeinfo-perl
        ;; (call-process "/home/my_usr/scripts/mime/mimeopen" nil 0 nil "--no-ask" file)
        ))))

;;    (setq openwith-associations
;;          (list (list (openwith-make-extension-regexp
;;                       '("flac" "mpg" "mpeg" "mp3" "mp4"
;;                         "avi" "wmv" "wav" "mov" "flv"
;;                         "ogm" "ogg" "mkv" "webm"))
;;                      "mpv"
;;                      '(file))
;;                (list (openwith-make-extension-regexp
;;                       '("xbm" "pbm" "pgm" "ppm" "pnm"
;;                         "png" "gif" "bmp" "tif" "jpeg" "jpg"))
;;                      "feh"
;;                      '(file))
               
;;                (list (openwith-make-extension-regexp
;;                       '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
;;                      "libreoffice"
;;                      '(file))

;;                (list (openwith-make-extension-regexp
;;                       '("html" "htm"))
;;                      (getenv "BROWSER")
;;                      '(file))

;;                (list (openwith-make-extension-regexp
;;                       '("pdf" "ps" "ps.gz" "dvi" "epub"))
;;                      "okular"
;;                      '(file))))
;;    (define-key dired-mode-map (kbd "S-<return>") 'dired-openwith))

(setup (:pkg dired-single)
  (:load-after dired)
  (:when-loaded
    (define-key dired-mode-map [remap dired-find-file]
      'dired-single-buffer)
    (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
      'dired-single-buffer-mouse)
    (define-key dired-mode-map [remap dired-up-directory]
      'dired-single-up-directory)))

(provide 'init-dired)
