; -*- coding:utf-8 -*-

; * ivy-dired-history
;; run dired to match in dir history

(setup savehist
  (:option savehist-file "~/.emacs.d/history"
           ;; (savehist-load)
           history-delete-duplicates t
           savehist-autosave-interval 120)
  (savehist-mode 1))

(setup recentf
  ;; disable before we start recentf!
  (:option recentf-auto-cleanup 'never
           recentf-max-saved-items 300)
  (:delay)
  (:when-loaded
    ;; 使用org-publish发布静态博客时，生成的html路径会把recentf-list塞满
    (defsubst file-was-visible-p (file)
      "Return non-nil if FILE's buffer exists and has been displayed."
      (let ((buf (find-buffer-visiting file)))
        (if buf
            (let ((display-count (buffer-local-value 'buffer-display-count buf)))
              (if (> display-count 0) display-count nil)))))

    (let ((r-list recentf-list))
      (defsubst keep-default-old-and-visible-recentf-p (file)
        "Decide whether to keep file in recentf-list.
        Return non-nil if recentf would, by default, keep FILE, and
        either FILE name was loaded from recentf file on disk or FILE
        has been displayed in this session."
        (if (recentf-keep-default-predicate file)
            (or (member file r-list)
                (file-was-visible-p file)))))

    (setf recentf-keep '(keep-default-old-and-visible-recentf-p))

    ;;    (defvar buffer-creation-time nil)
    ;;    (make-variable-buffer-local 'buffer-creation-time)

    ;;    (defun recentf-track-opened-file ()
    ;;      "Insert the name of the file just opened or written into the recent list."
    ;;      (and buffer-file-name
    ;;           (recentf-add-file buffer-file-name)
    ;;           (message "set creation time")
    ;;           (setq buffer-creation-time (time-convert nil 'integer)))
    ;;      ;; Must return nil because it is run from `write-file-functions'.
    ;;      nil)

    ;;    (defsubst recentf-remove-if-non-kept (filename)
    ;;      "Remove FILENAME from the recent list, if file is not kept.
    ;; Return non-nil if FILENAME has been removed."
    ;;      (when (or (not (recentf-keep-p filename))
    ;;                ;; buffer opened for less than a set time
    ;;                (< (- (time-convert nil 'integer) buffer-creation-time)
    ;;                   20))
    ;;        (let ((m (recentf-string-member
    ;;                  (recentf-expand-file-name filename) recentf-list)))
    ;;          (and m (setq recentf-list (delq (car m) recentf-list))))))

    ;; (run-with-idle-timer 30 t (lambda () (recentf-save-list)))
    (recentf-mode 1)))

;; press tab to complete, keep typing to match in sudirs
;; type path only, do not type seperator "/"
;; if you are using ido,you'd better disable ido for dired
;; (define-key (cdr ido-minor-mode-map-entry) [remap dired] nil) ;in ido-setup-hook

(setup ivy-dired-history
  (:load-after dired)
  (:with-map dired-mode-map
    (:bind "," 'dired))
  (:when-loaded
    (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable)
    ;; or if you use desktop-save-mode
    ;; (add-to-list 'desktop-globals-to-save 'ivy-dired-history-variable)

    (defvar ivy-dired-history-update-timer nil)

    (defun ivy-dired-history-update ()
      "Update variable `ivy-dired-history-variable'."
      (when ivy-dired-history-update-timer
        (cancel-timer ivy-dired-history-update-timer))
      ;; don't add intermediate dir to the history list during a successive dir opening
      (setq ivy-dired-history-update-timer
            (let ((dir (dired-current-directory)))
              (run-with-timer 15 nil `(lambda ()
                                        (ivy-dired-history--update ,dir))))))))

(provide 'init-dired-history)
