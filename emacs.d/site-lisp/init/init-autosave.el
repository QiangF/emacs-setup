; -*- coding:utf-8 -*-
;; recover-session doesn't delete auto saved file in ~/.cache/auto_save

;; Automatically save and restore sessions
; emacs 25 (save-place-mode 1) saveplace is auto-loaded by save-place-mode. So you do not need to explicitly require it.
(setq-default desktop-dirname "~/.emacs.d/desktop/"
              desktop-path (list desktop-dirname)
              desktop-files-not-to-save "^$" ;reload tramp paths
              save-place t
              save-place-forget-unreadable-files nil ; don't check if filereadable before save place
              save-place-file "~/.emacs.d/places"
              ;; desktop-load-locked-desktop nil
              desktop-load-locked-desktop "ask")

(defvar --temp-directory "~/.cache/")
(check_or_create --temp-directory)
(setq tramp-auto-save-directory (check_or_create (concat --temp-directory "auto_save")))
;; (setq tramp-auto-save-directory (check_or_create "/tmp/auto_save"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "\\2" tramp-auto-save-directory) t)))
      ;; `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "~/.emacs.d/temp/" t)))
      ;; `(("\\(?:[^/]*/\\)*\\(.*\\)" ,(concat --temp-directory "\\1") t))) ; for windows
; things run much quicker
(setq auto-save-directory tramp-auto-save-directory
      ;; auto-save-list-file-prefix (concat temp-directory "/autosave-")
      ;; auto-save-hash-p nil
      auto-save-default t               ; auto-save every buffer that visits a file
      ;; auto save either after 30s or after 200 keystrokes
      auto-save-timeout 30              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

;; auto-save-list-file-name
;; When Emacs exits normally (or killed with pkill), it deletes this file, if Emacs
;; crashes, the recover-session command uses this file.

;; (defvar --backup-directory (concat --temp-directory "backups"))
;; (check_or_create --backup-directory)
;; (setq backup-directory-alist `(("." . ,--backup-directory)))
;; (add-to-list 'backup-directory-alist
;;              (cons tramp-file-name-regexp nil))

; * backup
;; (setq make-backup-files t        ; backup of a file the first time it is saved.
;;       backup-by-copying-when-linked t)
(setq backup-by-copying t               ; don't clobber symlinks
      kept-new-versions 10              ; keep 10 latest versions
      kept-old-versions 0               ; don't bother with old versions
      delete-old-versions t             ; don't ask about deleting old versions
      version-control t                 ; number backups
      vc-make-backup-files t) ; backup version controlled files

;; backup every save                                                      ;;
;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
;; https://www.emacswiki.org/emacs/backup-each-save.el
(defvar my/backup-file-size-limit (* 1 1024 1024)
  "Maximum size of a file (in bytes) that should be copied at each savepoint.
If a file is greater than this size, don't make a backup of it.
Default is 1 MB")

(defvar my/backup-location (check_or_create (concat --temp-directory "backups")) "Base directory for backup files.")
(defvar my/backup-trash-dir (check_or_create (concat --temp-directory "trash")) "Directory for unwanted backups.")
(defvar my/backup-exclude-regexp "\\[Gmail\\]\\|.*dhashcache.*\\|recentf"
  "Don't back up files matching this regexp. Files whose full name matches this regexp are backed up to `my/backup-trash-dir'. Set to nil to disable this.")
(setq my/backup-trash-dir nil)

;; Default and per-save backups go here:
;; N.B. backtick and comma allow evaluation of expression
;; when forming list
(setq backup-directory-alist
      `(("" . ,(expand-file-name "per-save" my/backup-location))))

;; add trash dir if needed
(if my/backup-exclude-regexp
    (add-to-list 'backup-directory-alist `(,my/backup-exclude-regexp . ,my/backup-trash-dir)))

(defun my/backup-every-save ()
  "Backup files every time they are saved.
Files are backed up to `my/backup-location' in subdirectories \"per-session\" once per Emacs session, and \"per-save\" every time a file is saved.
Files whose names match the REGEXP in `my/backup-exclude-regexp' are copied to `my/backup-trash-dir' instead of the normal backup directory.
Files larger than `my/backup-file-size-limit' are not backed up."

  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  ;; (when (not buffer-backed-up)
  ;;   ;; Override the default parameters for per-session backups.
  ;;   (let ((backup-directory-alist
  ;;          `(("." . ,(expand-file-name "per-session" my/backup-location))))
  ;;         (kept-new-versions 3))
  ;;     ;; add trash dir if needed
  ;;     (if my/backup-exclude-regexp
  ;;         (add-to-list
  ;;          'backup-directory-alist
  ;;          `(,my/backup-exclude-regexp . ,my/backup-trash-dir)))
  ;;     ;; is file too large?
  ;;     (if (<= (buffer-size) my/backup-file-size-limit)
  ;;         (progn
  ;;           (message "Made per session backup of %s" (buffer-name))
  ;;           (backup-buffer))
  ;;       (message "WARNING: File %s too large to backup - increase value of my/backup-file-size-limit" (buffer-name)))))

  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    ;; is file too large?
    (if (<= (buffer-size) my/backup-file-size-limit)
        (progn
          (message "Made per save backup of %s" (buffer-name))
          (backup-buffer))
      (message "WARNING: File %s too large to backup - increase value of my/backup-file-size-limit" (buffer-name)))))

;; add to save hook
(add-hook 'before-save-hook 'my/backup-every-save)

;; The typical workflow is:
;;
;;   1) I'm in a buffer and realize I need to check some backups.
;;
;;        M-x backup-walker-start
;;
;;   2) I press <p> to go backwards in history until I see something
;;      interesting.  Then I press <enter> to bring it up.  OOPs this isn't
;;      it, I go back to the backup-walker window and find the right file.
;;
;;   3) I get what I need from the backup, go back to backup-walker, and press
;;      <q> and kill all open backups.
;;
;;   4) the end.
;;
;; Additionally, note that all the diff-mode facilities are available in the
;; `backup-walker' buffer.

(setup (:pkg backup-walker))

;; ediff
;; to do use a custom theme

(setup ediff
  ;; (add-hook 'ediff-load-hook
  ;;   (lambda ()
  ;;     (set-face-underline ediff-current-diff-face-A "black")
  ;;     (set-face-underline ediff-current-diff-face-B "black")
  ;;     (set-face-underline ediff-current-diff-face-C "black")))
  (:option ediff-window-setup-function 'ediff-setup-windows-plain
           ediff-split-window-function 'split-window-horizontally)

  (defun ediff-dwim ()
    (interactive)
    (let* ((num-win (safe-length (window-list)))
           (buffers-to-do (buffer-list))
           (bufa (current-buffer))
           (filename-a (file-name-nondirectory (buffer-file-name bufa)))
           bufb filename-b)
      (if (region-active-p)
          (call-interactively #'ediff-regions-wordwise)

          (when (= 2 num-win)
            (save-excursion
              (other-window 1)
              (setq bufb (current-buffer))))

          (while (and (not bufb) buffers-to-do)
            (if (buffer-file-name (car buffers-to-do))
                (setq filename-b (file-name-nondirectory (buffer-file-name (car buffers-to-do))))
                (setq filename-b nil))
            (when (and (not (eq bufa (car buffers-to-do)))
                       filename-b
                       (string-equal filename-b filename-a))
              (setq bufb (car buffers-to-do)))
            (setq buffers-to-do (cdr buffers-to-do)))

          (if bufb
              (progn (message "Running (ediff-buffers \"%s\" \"%s\") .." bufa bufb)
                     (ediff-buffers bufa bufb))
              (call-interactively #'ediff-buffers))))))

(setup (:pkg ws-butler)
  (ws-butler-global-mode 1))

(provide 'init-autosave)
