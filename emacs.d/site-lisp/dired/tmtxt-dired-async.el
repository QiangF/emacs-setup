;;; an collection of functions that I developed to execute some commands
;;; asynchronously
;;; only run ob unix-based systems

;;; TODO: stick the output window with the result buffer
;;; using dedicated window
;;; TODO: shortcut keys for close the result window
;;; TODO: check process exit status, if not success, not close the result window
;;; TODO: undo function

;;; ----------------------------------------------
;;; ----------------------------------------------
(require 'dash)
(require 'init-functions)

(defvar my-dired-mutex (make-mutex "my-dired mutex"))

(defvar tda/rsync-multiple-file-list
  () "The list of the files to be copied")

(defvar tda/rsync-multiple-failed-list
  () "The alist (dir . failed-file-list) of the files to be manualled processed")

(defvar tda/rsync-modified-buffer-name-list
  ())

(defvar tda/rsync-multiple-target-dir nil)

(defun my-dired-utils-goto-line (filename)
  "Go to line describing FILENAME in listing.

Should be absolute file name matched against
`dired-get-filename'."
  (goto-char (point-min))
  (let (stop)
    (while (and (not stop)
                (= (forward-line) 0))
      (when (equal filename (dired-get-filename nil t))
        (setq stop t)
        (dired-move-to-filename)))
    stop))

;;;###autoload
(defun dired-ranger--revert-target (char target-directory files)
  "Revert the target buffer and mark the new files.

CHAR is the temporary value for `dired-marker-char'.

TARGET-DIRECTORY is the current dired directory.

FILES is the list of files (from the `dired-ranger-copy-ring') we
operated on."
  (let ((buf (dired-find-buffer-nocreate target-directory)))
    (when buf 
      (with-current-buffer buf
        (let ((current-file (dired-get-filename nil t)))
          (revert-buffer)
          (let ((dired-marker-char char))
            (--each (-map 'file-name-nondirectory files)
              (my-dired-utils-goto-line (concat target-directory it))
              (dired-mark 1)))
          (my-dired-utils-goto-line current-file))))))

;; use thread
;;;###autoload
(defun my-clean-failed-tasks ()
  "Create a dired buffer for tda/rsync-multiple-failed-list and process it manually"
  (interactive)
  (let* ((item (pop tda/rsync-multiple-failed-list)))
    (when item
      (dired (car item))
      (delete-other-windows)
      (split-window-right)
      (dired (cons "failed file list" (cdr item)))
      )))

(defun my-dired-move-simple (arg)
  "(git mv rsyn) move tda/rsync-multiple-file-list to current directory."
  (interactive "P")
  (let* ((file-list tda/rsync-multiple-file-list)
         (modified-buffer-list tda/rsync-modified-buffer-name-list)
         (target-directory (dired-current-directory))
         moved-files failed-list)
    (tda/rsync-multiple-empty-list)

    (--each file-list
      (let (exit-status)
        (setq exit-status
              (call-process-shell-command
               (concat "git mv " (shell-quote-argument it) " " (shell-quote-argument target-directory))
               nil " *git mv*"))
        (unless (= 0 exit-status)
          ;; https://gnu.huihoo.org/emacs/24.4/emacs-lisp/Standard-Errors.html
          (if (my-same-file-systemp it target-directory)
              ;; on the same file system
              (ignore-errors
                (progn
                  (rename-file it target-directory nil)
                  (setq exit-status 0)))
              ;; on different file systems
              (setq exit-status
                    (call-process-shell-command
                     (concat "rsync -a --partial --remove-source-files " (shell-quote-argument it) " " (shell-quote-argument target-directory))))))
        (if (= exit-status 0)
            (let ((buf (get-file-buffer it)))
              (add-to-list 'moved-files it)
              (when buf 
                ;; redirect opened buffer to new file path see vc-rename-file
                (with-current-buffer buf
                  (set-visited-file-name (concat target-directory (file-name-nondirectory it)) nil t))))
            (add-to-list 'failed-list it))))

    (with-mutex my-dired-mutex
      ;; mark moved files in target dir
      (when moved-files
        (dired-ranger--revert-target ?M target-directory moved-files))
      ;; add failed-list to tda/rsync-multiple-failed-list
      (if failed-list
          (progn
            (setf (alist-get target-directory tda/rsync-multiple-failed-list nil t 'equal)
                  (delete-dups (append failed-list (alist-get target-directory tda/rsync-multiple-failed-list nil nil 'equal))))
            (message "Warning: some tasks failed, please run my-clean-failed-tasks!"))
          (message (format "Moved %d/%d to %s"
                           (length moved-files)
                           (length file-list)
                           target-directory)))
      ;; revert affected dired buffers
      (--each modified-buffer-list
        (when (buffer-live-p it)
          (with-current-buffer it (revert-buffer)))))
    ))

(defun my-dired-move ()
  (interactive)
  (setq tda/rsync-multiple-target-dir (expand-file-name default-directory))
  (async-start
   `(lambda ()
      ,(async-inject-variables "^load-path$")
      ,(async-inject-variables "^exec-path$")
      ,(async-inject-variables "^tda/rsync-multiple-file-list$")
      ,(async-inject-variables "^tda/rsync-multiple-target-dir$")
      ,(async-inject-variables "^tda/rsync-modified-buffer-name-list$")
      (require 'use-package)
      (require 'init-functions)
      (require 'dired-x)
      (require 'notify)
      (setq notify-delay '(0 0 1))

      (defun my-dired-move-file (it target-directory)
        (let* ((file-name (file-name-nondirectory it))
               ;; absolute file path
               (symlink (file-symlink-p it))
               (new-file-path (expand-file-name file-name target-directory))
               exit-status)
          (setq exit-status
                (call-process-shell-command
                 ;; git mv doesn't work for dir
                 (concat "git mv " (shell-quote-argument it) " " (shell-quote-argument target-directory))
                 nil " *git mv*"))
          (if (= 0 exit-status)
              (when symlink
                (dired-make-relative-symlink (expand-file-name symlink (file-name-directory it)) new-file-path t))
              (if (my-same-file-systemp it target-directory)
                  ;; on the same file system
                  (condition-case err
                      (progn (if symlink
                                 (progn (dired-make-relative-symlink (expand-file-name symlink (file-name-directory it)) new-file-path)
                                        (dired-delete-file it))
                                 (rename-file it target-directory))
                             (setq exit-status 0))
                    ('error (notify "Error in renaming file: " (format "%s" err))
                            (setq exit-status 1)))
                  (notify "Moving files with rsync in the background!" (format "it %s target %s" (shell-quote-argument it) (shell-quote-argument target-directory)))
                  (setq exit-status
                        (call-process-shell-command
                         (concat "rsync -a --copy-links --partial --remove-source-files "
                                 (shell-quote-argument it) " " (shell-quote-argument target-directory)) nil " *rsync*"))))
          exit-status))

      (defun my-git-work-treep (dir)
        (equal 0 (string-match-p (regexp-quote "true")
                        (shell-command-to-string (format "cd %s; git rev-parse --is-inside-work-tree"
                                                         (shell-quote-argument (expand-file-name dir)))))))

      (defun my-dired-move-dir (dir target-directory)
        (let* ((dir-file-name (file-name-nondirectory (directory-file-name dir)))
               (new-dir-path (expand-file-name dir-file-name target-directory))
               (new-target-directory (concat new-dir-path "/"))
               exit-status)
          (if (and (my-same-file-systemp dir target-directory)
                   ;; at lease one dir is not managed by git or they are different repository
                   (or (not (vc-call-backend 'git 'root dir))
                       (not (vc-call-backend 'git 'root target-directory))
                       (not (equal (vc-call-backend 'git 'root dir)
                                   (vc-call-backend 'git 'root target-directory)))))
              (condition-case err
                  (progn (rename-file dir new-dir-path)
                         (setq exit-status 0))
                ('error (notify "Error in renaming dir: " (format "%s" err))
                        (setq exit-status 1)))

              (condition-case err
                  (progn
                    (when (not (file-exists-p new-target-directory))
                      (make-directory new-target-directory t))
                    (setq exit-status 0))
                ('error (notify "Error in mkdir: " (format "%s" err))
                        (setq exit-status 1)))
              (dolist (it (directory-files dir t "^[^\\.]"))
                (if (and (file-directory-p it) (not (file-symlink-p it)))
                    (my-dired-move-dir it new-target-directory)
                    (setq exit-status (my-dired-move-file it new-target-directory))))
              (delete-directory dir t t))
          exit-status))

      (let ((file-list tda/rsync-multiple-file-list)
            (modified-buffer-name-list tda/rsync-modified-buffer-name-list)
            (target-directory tda/rsync-multiple-target-dir)
            (notification-timer (timer-create))
            moved-list failed-list exit-status)
        (dolist (it file-list)
          (setq notification-timer
                (run-at-time 20 nil
                             (lambda ()
                               (notify "Move file in the background:" (format "%d/%d to %s"
                                                                              (length moved-list)
                                                                              (length file-list)
                                                                              target-directory)))))
          (if (and (file-directory-p it) (not (file-symlink-p it)))
              (setq exit-status (my-dired-move-dir it target-directory))
              (setq exit-status (my-dired-move-file it target-directory)))
          (if (and exit-status (= exit-status 0))
              (add-to-list 'moved-list it)
              (add-to-list 'failed-list it)))
        (notify "All files moved!" "")
        ;; modified-buffer-name-list is a list of buffer names, async.el has problems in sending buffer variable
        (list target-directory moved-list failed-list modified-buffer-name-list)))

   (lambda (result)
     (let ((target-directory (nth 0 result))
           (moved-list (nth 1 result))
           (failed-list (nth 2 result))
           (modified-buffer-name-list (nth 3 result)))
       (with-mutex my-dired-mutex
         ;; mark moved files in target dir
         (when moved-list
           (dolist (it moved-list)
             (let ((buf (get-file-buffer it)))
               (when buf 
                 ;; redirect opened buffer to new file path see vc-rename-file
                 (with-current-buffer buf
                   (set-visited-file-name (concat target-directory (file-name-nondirectory it)) nil t)))))
           (dired-ranger--revert-target ?M target-directory moved-list))
         ;; add failed-list to tda/rsync-multiple-failed-list
         (if failed-list
             (progn
               (setf (alist-get target-directory tda/rsync-multiple-failed-list nil t 'equal)
                     (delete-dups (append failed-list (alist-get target-directory tda/rsync-multiple-failed-list nil nil 'equal))))
               (message "Warning: some tasks failed, please run my-clean-failed-tasks!"))
             (message (format "Moved %d/%d to %s"
                              (length moved-list)
                              (+ (length moved-list) (length failed-list))
                              target-directory)))
         ;; revert affected dired buffers
         (dolist (it modified-buffer-name-list nil)
           (when (get-buffer it)
             (with-current-buffer (get-buffer it) (revert-buffer))))))))

  (tda/rsync-multiple-empty-list))

(defun my-dired-paste ()
  (interactive)
  (setq tda/rsync-multiple-target-dir (expand-file-name default-directory))
  (async-start
   `(lambda ()
      ,(async-inject-variables "^load-path$")
      ,(async-inject-variables "^exec-path$")
      ,(async-inject-variables "^tda/rsync-multiple-file-list$")
      ,(async-inject-variables "^tda/rsync-multiple-target-dir$")
      (require 'use-package)
      (require 'init-functions)
      (require 'dired-x)
      (require 'dired+)
      (require 'notify)

      (defun my-dired-paste-file (it target-directory)
        (let* ((file-name (file-name-nondirectory it))
               ;; absolute file path
               (symlink (file-symlink-p it))
               (new-file-path (expand-file-name file-name target-directory))
               exit-status)
          (condition-case err
              (progn (if symlink
                         (dired-make-relative-symlink (expand-file-name symlink (file-name-directory it)) new-file-path)
                         (notify "Hardlinking!" "")
                         (dired-hardlink it new-file-path))
                     (setq exit-status 0))
            ('error (notify "Caught exception: " (format "%s" err))
                    (setq exit-status 1)))
          exit-status))

      (defun my-dired-paste-dir (dir target-directory)
        (let* ((dir-file-name (file-name-nondirectory (directory-file-name dir)))
               (new-target-directory (concat (expand-file-name dir-file-name target-directory) "/"))
               exit-status)
          (condition-case err
              (progn
                (when (not (file-exists-p new-target-directory))
                  (make-directory new-target-directory t))
                (setq exit-status 0))
            ('error (notify "Error in mkdir: " (format "%s" err))
                    (setq exit-status 1)))
          (dolist (it (directory-files dir t "^[^\\.]"))
            (if (and (file-directory-p it) (not (file-symlink-p it)))
                (my-dired-paste-dir it new-target-directory)
                (setq exit-status (my-dired-paste-file it new-target-directory))))
          exit-status))

      ;; ensure all notification are sent
      (setq notify-delay '(0 0 1))
      (let ((file-list tda/rsync-multiple-file-list)
            (target-directory tda/rsync-multiple-target-dir)
            (notification-timer (timer-create))
            moved-list failed-list exit-status)
        ;; (notify "Copying files using rsync in the background!" "")
        (dolist (it file-list)
          (setq notification-timer
                  (notify "Copy file in the background:" (format "%d/%d to %s"
                                                   (length moved-list)
                                                   (length file-list)
                                                   target-directory)))

          (if (my-same-file-systemp it target-directory)
              (if (and (file-directory-p it) (not (file-symlink-p it)))
                  (setq exit-status (my-dired-paste-dir it target-directory))
                  (setq exit-status (my-dired-paste-file it target-directory)))
              (setq exit-status
                    (call-process-shell-command
                     (concat "rsync -a --partial --copy-links " (shell-quote-argument it) " "
                             (shell-quote-argument target-directory)))))

        (if (and exit-status (= exit-status 0))
            (add-to-list 'moved-list it)
            (add-to-list 'failed-list it)))
        (notify (format "Fail/All: %d/%d" (length failed-list) (length file-list)) "")
        (list target-directory moved-list failed-list)))

   (lambda (result)
     (let ((target-directory (nth 0 result))
           (moved-list (nth 1 result))
           (failed-list (nth 2 result)))
       (with-mutex my-dired-mutex
         ;; mark moved files in target dir
         (when moved-list
           (dired-ranger--revert-target ?M target-directory moved-list))
         ;; add failed-list to tda/rsync-multiple-failed-list
         (if failed-list
             (progn
               (setf (alist-get target-directory tda/rsync-multiple-failed-list nil t 'equal)
                     (delete-dups (append failed-list (alist-get target-directory tda/rsync-multiple-failed-list nil nil 'equal))))
               (message "Warning: some tasks failed, please run my-clean-failed-tasks!"))
             (message (format "Moved %d/%d to %s"
                              (length moved-list)
                              (+ (length moved-list) (length failed-list))
                              target-directory)))))))
  (tda/rsync-multiple-empty-list))

;;; get file size
(defvar tda/get-files-size-command "du"
  "The name of \"du\" command (or the path to the \"du\" command)")
(defvar tda/get-files-size-arguments "-hc"
  "The arguments for passing into the \"du\" command")

;;; get file size
(defun tda/get-files-size ()
  "Calculate files size for all the marked files"
  (interactive)
  (let ((files (dired-get-marked-files)) command)
    ;; the get files size command
    (setq command tda/get-files-size-command)
    (setq command (concat command " " tda/get-files-size-arguments " "))
    ;; add selected file names as arguments to the command
    (dolist (file files)
      (setq command (concat command (shell-quote-argument file) " ")))
    ;; execute the command
    (tat/execute-async command "file size")))

;;; ----------------------------------------------
;;; ----------------------------------------------
;;; Async Rsync
(defvar tda/rsync-command-name "rsync"
  "The name of rsync command (or the path to the rsync command).")
(defvar tda/rsync-arguments "-avz --progress"
  "The arguments for passing into the rsync command")

(defun tda/rsync (dest)
  "Asynchronously copy file using Rsync for dired.
    This function runs only on Unix-based system.
    Usage: same as normal dired copy function."
  (interactive ;; offer dwim target as the suggestion
   (list (expand-file-name (read-file-name "Rsync to:" (dired-dwim-target-directory)))))
  (let ((files (dired-get-marked-files nil current-prefix-arg))
        command)
    ;; the rsync command
    (setq command
          (concat tda/rsync-command-name " " tda/rsync-arguments " "))
    ;; add all selected file names as arguments to the rsync command
    (dolist (file files)
      (setq command (concat command (shell-quote-argument file) " ")))
    ;; append the destination to the rsync command
    (setq command (concat command (shell-quote-argument dest)))
    ;; execute the command asynchronously
    (tat/execute-async command "rsync")))

(defun tda/rsync-sudo (dest)
  "Asynchronously copy file using Rsync for dired.
    This function runs only on Unix-based system.
    Usage: same as normal dired copy function."
  (interactive ;; offer dwim target as the suggestion
   (list (expand-file-name (read-file-name "Rsync to:" (dired-dwim-target-directory)))))
  (let ((files (dired-get-marked-files nil current-prefix-arg))
        command)
    ;; the rsync command
    (setq command
          (concat "sudo " tda/rsync-command-name " " tda/rsync-arguments " "))
    ;; add all selected file names as arguments to the rsync command
    (dolist (file files)
      (setq command (concat command (shell-quote-argument file) " ")))
    ;; append the destination to the rsync command
    (setq command (concat command (shell-quote-argument dest)))
    ;; execute the command asynchronously
    (tat/execute-async command "rsync")))

;; --delete-after remove the destination files after the transfer, not the source files.
;; --delete delete extraneous files from destination directory. (ones that aren’t on the sending
;; --side), but only for the directories that are being synchronized.
(defun tda/rsync-delete (dest)
  "Asynchronously copy file using Rsync for dired include the delete option
    This function runs only on Unix-based system.
    Usage: same as normal dired copy function."
  (interactive ;; offer dwim target as the suggestion
   (list (expand-file-name (read-file-name "Rsync delete to:" (dired-dwim-target-directory)))))
  (let ((files (dired-get-marked-files nil current-prefix-arg))
        command)
    ;; the rsync command
    (setq command
          (concat tda/rsync-command-name " " tda/rsync-arguments " --delete "))
    ;; add all selected file names as arguments to the rsync command
    (dolist (file files)
      (setq command (concat command (shell-quote-argument file) " ")))
    ;; append the destination to the rsync command
    (setq command (concat command (shell-quote-argument dest)))
    ;; execute the command asynchronously
    (tat/execute-async command "rsync")))

(defun tda/rsync-delete-sudo (dest)
  "Asynchronously copy file using Rsync for dired include the delete option
    This function runs only on Unix-based system.
    Usage: same as normal dired copy function."
  (interactive ;; offer dwim target as the suggestion
   (list (expand-file-name (read-file-name "Rsync delete to:" (dired-dwim-target-directory)))))
  (let ((files (dired-get-marked-files nil current-prefix-arg))
        command)
    ;; the rsync command
    (setq command
          (concat "sudo " tda/rsync-command-name " " tda/rsync-arguments " --delete "))
    ;; add all selected file names as arguments to the rsync command
    (dolist (file files)
      (setq command (concat command (shell-quote-argument file) " ")))
    ;; append the destination to the rsync command
    (setq command (concat command (shell-quote-argument dest)))
    ;; execute the command asynchronously
    (tat/execute-async command "rsync")))

;;; ----------------------------------------------
;;; ----------------------------------------------
;;; async zip files
(defvar tda/zip-command "zip"
  "The command name (or the path to the zip command")
(defvar tda/zip-arguments
  "-ru9" "The compression level for dired async zip command, from 0-9. This variable is a string, so if you change this value, please set it as a string.")

(defun tda/zip (output)
  "Asynchronously compress marked files to the output file"
  (interactive
   (list (expand-file-name (read-file-name "Add to file: "))))

  (let (command
        (files (dired-get-marked-files nil current-prefix-arg)))
    ;; the zip command
    (setq command
          (concat tda/zip-command " " tda/zip-arguments " "))
    ;; append the output file
    (setq command
          (concat command (shell-quote-argument output) " "))
    ;; add all selected files as argument
    (dolist (file files)
      (setq command
            (concat command
                    (shell-quote-argument
                     (file-name-nondirectory file)) " ")))
    (message command)
    ;; execute the command asynchronously
    (tat/execute-async command "zip")))

;;; ----------------------------------------------
;;; ----------------------------------------------
;;; Uncompress function
(defvar tda/unzip-command "unzip"
  "The command name (or path to the unzip command)")
(defvar tda/unzip-arguments ""
  "The arguments for passing into the unzip command")

(defun tda/unzip ()
  "Asynchronously decompress the zip file at point"
  (interactive)

  (let (command
        output-directory
        (file (dired-get-filename 'verbatim)))

    ;; new directory name for the output files
    (setq output-directory
          (file-name-sans-extension
           (dired-get-filename 'verbatim)))

    ;; the unzip command
    (setq command (concat tda/unzip-command " " tda/unzip-arguments " "))
    ;; append the file name
    (setq command
          (concat command
                  (shell-quote-argument file) " "))
    ;; append the output directory name
    (setq command
          (concat command "-d "
                  (shell-quote-argument output-directory)))

    ;; execute the command asynchronously
    (tat/execute-async command "unzip")))

;;; ----------------------------------------------
;;; ----------------------------------------------
;;; Rsync from multiple directories
(defun tda/rsync-multiple-mark-file ()
  "Add file to waiting list for copying"
  (interactive)
  ;; Add file to the list
  (let ((files (or (dired-get-marked-files nil current-prefix-arg) (list (dired-get-filename)))))
    ;; expand-file-name at adding, not need to expand it again during operation
    (mapc (lambda (item) (add-to-list 'tda/rsync-multiple-file-list (expand-file-name item))) files)
    ;; Message for user
    (add-to-list 'tda/rsync-modified-buffer-name-list (buffer-name))
    (message "Marked file added to waiting list.")))

(defun tda/rsync-multiple-empty-list ()
  "Empty the waiting list"
  (interactive)
  ;; Empty the list
  (setq tda/rsync-multiple-file-list '())
  (setq tda/rsync-modified-buffer-name-list '())
  ;; message for the user
  (message "Waiting list empty."))

(defun tda/rsync-multiple-remove-item ()
  "Remove the file at point from the waiting list if it is in"
  (interactive)
  (let ((files (dired-get-filename)))
    ;; remove the item from the list
    (setq tda/rsync-multiple-file-list
          (dolist (file files)
            (remove file tda/rsync-multiple-file-list)))
    ;; message for the use
    (message
     (concat "Marked file(s) removed from the list."))))

;; Copy file from multiple directories
(defun tda/rsync-multiple ()
  "Mark file in multiple places and then paste in 1 directory"
  (interactive)

  (let (command)
    (if (equal tda/rsync-multiple-file-list ())
        (progn
          (message "Please add file to the waiting list."))
      (progn
        ;; the rsync command
        (setq command (concat tda/rsync-command-name " " tda/rsync-arguments " "))
        ;; add all selected file names as arguments to the rsync command
        (dolist (file tda/rsync-multiple-file-list)
          (setq command
                (concat command (shell-quote-argument file) " ")))
        ;; append the destination to the rsync command
        (setq command
              (concat command
                      (shell-quote-argument (expand-file-name default-directory))))
        ;; execute the command asynchronously
        (tat/execute-async command "rsync")
        ;; empty the waiting list
        (tda/rsync-multiple-empty-list)))))

;; tda/rsync-multiple replacement
(defun my-dired-paste-with-thread ()
  (interactive)
  (make-thread
   (lambda ()
     (let* ((file-list tda/rsync-multiple-file-list)
            (modified-buffer-list tda/rsync-modified-buffer-name-list)
            (target-directory (dired-current-directory))
            moved-list failed-list)
       (tda/rsync-multiple-empty-list)

       (--each file-list
         (let (exit-status)
           (setq exit-status
                 (call-process-shell-command
                  (concat "rsync -a --partial --copy-links " (shell-quote-argument it) " " (shell-quote-argument target-directory))))
           (if (= exit-status 0)
               (let ((buf (get-file-buffer it)))
                 (add-to-list 'moved-list it)
                 (with-current-buffer (dired-find-buffer-nocreate target-directory)
                   (setq mode-line-process
                         (format "%d/%d "
                                 (length moved-list)
                                 (length file-list))))
                 (message (format "Moved %d/%d to %s"
                                  (length moved-list)
                                  (length file-list)
                                  target-directory))
                 (when buf 
                   ;; redirect opened buffer to new file path see vc-rename-file
                   (with-current-buffer buf
                     (set-visited-file-name (concat target-directory (file-name-nondirectory it)) nil t))))
             (add-to-list 'failed-list it))))

       (with-mutex my-dired-mutex
         ;; add failed-list to tda/rsync-multiple-failed-list
         (if failed-list
             (progn
               (setf (alist-get target-directory tda/rsync-multiple-failed-list nil t 'equal)
                     (delete-dups (append failed-list (alist-get target-directory tda/rsync-multiple-failed-list nil nil 'equal))))
               (message "Warning: some tasks failed, please run my-clean-failed-tasks!"))
           (message (format "Copied %d/%d to %s"
                            (length moved-list)
                            (length file-list)
                            target-directory)))
         ;; add failed-list to tda/rsync-multiple-failed-list
         ;; mark moved files in target dir
         (when moved-list
           (dired-ranger--revert-target ?M target-directory moved-list))
         ;; revert affected dired buffers
         (--each modified-buffer-list
           (when (buffer-live-p it)
             (with-current-buffer it (revert-buffer)))))))))

;;; ----------------------------------------------
;;; ----------------------------------------------
;;; download file to current dir
(defvar tda/download-command "wget"
  "The download program to download to current dir. The default is wget, ou can replace it to curl, aria2c,...")
(defun tda/download-to-current-dir (src)
  "Read the link and download the file to current directory"
  (interactive (list (read-from-minibuffer "Link: ")))
  (let ((command ""))
    ;; create the command
    (setq command (concat command tda/download-command " "))
    ;; append the link
    (setq command (concat command (shell-quote-argument src)))
    ;; execute
    (tat/execute-async command "download")))
(defun tda/download-clipboard-link-to-current-dir ()
  "Read the clipboard link and download it into the current dir"
  (interactive)
  (tda/download-to-current-dir (x-get-clipboard)))

(provide 'tmtxt-dired-async)
