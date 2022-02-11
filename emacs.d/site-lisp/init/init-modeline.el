; -*- coding:utf-8 -*-

;; https://github.com/Bad-ptr/common-header-mode-line.el

;; (when (display-graphic-p)
;;   (setf (alist-get 'mode-line face-remapping-alist)
;;         'default))

;; (defun mini-modeline-windows-change-hook  ()
;;     (if (> (count-windows) 1)
;;       (when (display-graphic-p)
;;         (setf (alist-get 'mode-line face-remapping-alist)
;;               'mini-modeline-mode-line-inactive)
;;         (setf (alist-get 'mode-line-inactive face-remapping-alist)
;;               'default))
;;       (when (display-graphic-p)
;;         (setf (alist-get 'mode-line face-remapping-alist)
;;               'default))))

;; (add-hook 'window-configuration-change-hook #'mini-modeline-windows-change-hook)

;; custom-theme-set-faces
;;  'user
;;  `(mini-modeline-mode-line ((t (:inherit default :height 0.1 :box nil))) t)
;;  `(mini-modeline-mode-line-inactive ((t (:inherit default :height 0.1 :box nil))) t))

(setup mini-modeline
   (:global*
    "C-M-h" #'toki-tabs-previous
    "C-M-l" #'toki-tabs-next)
   
   (require 'mini-modeline)
   (add-hook 'focus-in-hook 'mini-modeline-display)

   (setq mini-modeline-right-padding 2)
   ;; remove pyim minibuffer message flashing
   ;; (setq resize-mini-windows nil)
   (setq mini-modeline-truncate-p nil)
   (setq display-time-day-and-date nil)
   (setq mini-modeline-echo-duration 4)
   (display-time-mode t)
   (if exwm_enable
       (defvaralias 'mini-modeline-frame 'exwm-workspace--current)
       (setq exwm-workspace--current nil))

   (defun mini-modeline-msg ()
     "Place holder to display echo area message."
     (let ((max-string-length 5000))
       (if (> (length mini-modeline--msg) max-string-length)
           (concat (substring mini-modeline--msg 0 (floor max-string-length 2))
                   "\n ...... \n"
                   (substring mini-modeline--msg-message (floor max-string-length -2)))
           mini-modeline--msg)))

   (defvar symon-refresh-rate 4)
   (defvar symon-linux--last-network-rx 0)
   (defvar symon-linux--last-network-tx 0)
   (defvar my-last-symon-message "")

   (defun symon-linux--read-lines (file reader indices)
     (with-temp-buffer
       (insert-file-contents file)
       (goto-char 1)
       (mapcar (lambda (index)
                 (save-excursion
                   (when (search-forward-regexp (concat "^" index "\\(.*\\)$") nil t)
                     (if reader
                         (funcall reader (match-string 1))
                         (match-string 1)))))
               indices)))

   ;; (defun my-update-symon ()
   ;;   (setq my-last-symon-message
   ;;         (concat
   ;;          ;; Receive speed in kb
   ;;          (with-temp-buffer
   ;;            (insert-file-contents "/proc/net/dev")
   ;;            (goto-char 1)
   ;;            (let ((rx 0) the-str)
   ;;              (while (search-forward-regexp "^[\s\t]*\\(.*\\):" nil t)
   ;;                (unless (string= (match-string 1) "lo")
   ;;                  (setq rx (+ rx (read (current-buffer))))))
   ;;              ;; warn the user if rx > 2000 kb/s
   ;;              (setq the-str (if (> (/ (- rx symon-linux--last-network-rx) symon-refresh-rate 1000) 2000) "R!!" ""))
   ;;              (setq symon-linux--last-network-rx rx)
   ;;              the-str))
   ;;          ;; Transmit speed
   ;;          (with-temp-buffer
   ;;            (insert-file-contents "/proc/net/dev")
   ;;            (goto-char 1)
   ;;            (let ((tx 0) the-str)
   ;;              (while (search-forward-regexp "^[\s\t]*\\(.*\\):" nil t)
   ;;                (unless (string= (match-string 1) "lo")
   ;;                  (forward-word 8)
   ;;                  (setq tx (+ tx (read (current-buffer))))))
   ;;              ;; warn the user if tx > 500 kb/s
   ;;              (setq the-str (if (> (/ (- tx symon-linux--last-network-tx) symon-refresh-rate 1000) 500) "T!!" ""))
   ;;              (setq symon-linux--last-network-tx tx)
   ;;              the-str))
   ;;          ;; memory
   ;;          (format "M:%s"
   ;;                  (cl-destructuring-bind (memtotal memavailable memfree buffers cached)
   ;;                      (symon-linux--read-lines
   ;;                       "/proc/meminfo" (lambda (str) (and str (read str)))
   ;;                       '("MemTotal:" "MemAvailable:" "MemFree:" "Buffers:" "Cached:"))
   ;;                    (if memavailable
   ;;                        (/ (* (- memtotal memavailable) 100) memtotal)
   ;;                        (/ (* (- memtotal (+ memfree buffers cached)) 100) memtotal))))
   ;;          ;; swapped
   ;;          (cl-destructuring-bind (swaptotal swapfree)
   ;;              (symon-linux--read-lines
   ;;               "/proc/meminfo" 'read '("SwapTotal:" "SwapFree:"))
   ;;            (let ((swapped (/ (- swaptotal swapfree) 1000)))
   ;;              (unless (zerop swapped) (format " %dMB Swapped" swapped)))))))

   (require 'battery)
   (if (battery--find-linux-sysfs-batteries)
       (display-battery-mode 1)
       (setq battery-mode-line-string nil))

   ;; (defun awesome-tray-module-symon-info ()
   ;;   (if (string-empty-p battery-mode-line-string)
   ;;       my-last-symon-message
   ;;       (concat my-last-symon-message " " (substring battery-mode-line-string 1 -4))))
   ;; (setq awesome-tray-symon-timer (run-with-idle-timer 0 symon-refresh-rate 'my-update-symon))

   (defun awesome-tray-module-workspace-info ()
     (let ((workspace-str ""))
       (when (boundp 'exwm-workspace-current-index)
         (setq workspace-str
               (concat workspace-str
                       (propertize (int-to-string exwm-workspace-current-index) 'face '((:background "#0000ff" :foreground "#ffff00"))))))
       (when (and (bound-and-true-p eyebrowse-mode)
                  (< 1 (length (eyebrowse--get 'window-configs))))
         (let* ((num (eyebrowse--get 'current-slot))
                (tag (when num (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
                (str (if (and tag (< 0 (length tag)))
                         tag
                         (when num (int-to-string num)))))
           (when str
             (setq workspace-str (concat workspace-str (format ":%s" str))))))
       (when (and (bound-and-true-p persp-mode)
                  (< 1 (length (hash-table-values (perspectives-hash)))))
         (setq workspace-str (concat workspace-str
                                     (propertize (format ":%s" (persp-current-name)) 'face '((:background "#004040" :foreground "#ff0000"))))))
       workspace-str))

   (defvar my-buffer-id-length 20 "")

   (defun my-trim-buffer-id (id suffix-length)
     (if (> (length id) my-buffer-id-length)
         (concat (substring id 0 (- my-buffer-id-length suffix-length 3))
                 "..."
                 (substring id (- 0 suffix-length)))
         id))

   (defun my-buffer-id ()
     (cond ((bound-and-true-p git-timemachine-mode) (buffer-name))
           ((buffer-file-name)
            (let* ((file-name (roife/shorten-path (buffer-file-name) my-buffer-id-length)))
              (if (<= (string-width file-name) my-buffer-id-length)
                  file-name
                  (my-trim-buffer-id (buffer-name) 4))))
           ((and exwm_enable (derived-mode-p 'exwm-mode))
            (my-trim-buffer-id exwm-title 10))
           (t (if (buffer-name) (my-trim-buffer-id (buffer-name) 4) ""))))

   (setq god-local-mode nil)
   (setq display-time-24hr-format t)
   (setq display-time-mail-string "")

   (set-face-attribute 'mode-line-buffer-id nil :foreground "red")

   ;; mode-line-mule-info: 当前内容的编码和输入法，U指UTF-8
   ;; mode-line-modified: **已经被修改，--没被修改，%%表示只读，%* 表示内容只读并且被修改

   ;; (:eval (format-mode-line (propertized-buffer-identification "%b")))
   ;; (vc-mode vc-mode)
   ;; (my-buffer-id) ;; 'font-lock-warning-face
   ;; (:eval (propertize (if god-local-mode "  God  " "") 'face '((:background "#000000" :foreground "red"))))
   ;; evil-mode-line-tag
   ;; display-time-string-forms

   ;; (defun my-toggle-mini-modeline-echo-duration (&rest args)
   ;;   (if pyim-translating (setq mini-modeline-echo-duration 4)
   ;;     (progn (my-clear-echo-area)
   ;;            (setq mini-modeline-echo-duration 9999))))

   ;; (eval-after-load "rime"
   ;;   (advice-add 'rime-terminate-translation :after #'my-clear-echo-area))
   (mini-modeline-mode)
   (setq resize-mini-windows t))


(setup toki-tabs
   (:hook-into mini-modeline-mode-hook)
   (:option toki-tabs-visible-buffer-limit 4)
   (require 'toki-tabs)
   (require 'cl-seq)

   (defun roife/shorten-path (path &optional max-len)
     "Shorten PATH to MAX-LEN."
     (unless max-len (setq max-len 0))
     (if (and path (not (eq path "")))
         (let* ((components (split-string (abbreviate-file-name path) "/"))
                (len (+ (1- (length components))
                        (cl-reduce '+ components :key 'length)))
                (str ""))
           (while (and (> len max-len)
                       (cdr components))
             (setq str (concat str (if (= 0 (length (car components)))
                                       "/"
                                       (string (elt (car components) 0) ?/)))
                   len (- len (1- (length (car components))))
                   components (cdr components)))
           (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components)))
         ""))

   (defun mini-modeline-buffer-name ()
     (concat
      (roife/shorten-path default-directory 30)
      (toki-buffer-name (current-buffer) t)))

   (defface toki-modeline-path-face
       '((((background light))
          :foreground "#ff0000" :italic t)
         (t
          :foreground "#ff0000" :italic t))
     "Face for file path.")

   (defvar my-modeline-background "black")
   (setq mini-modeline-r-format '("%e" mode-line-process
                                  (:eval (propertize (mini-modeline-buffer-name)
                                          'face 'toki-modeline-path-face))
                                  mode-line-position
                                  mode-line-remote
                                  mode-line-mule-info
                                  mode-line-modified
                                  (:eval (awesome-tray-module-workspace-info)
                                   'face `((:background ,my-modeline-background)))
                                  (:eval (propertize (format-time-string "%H:%M")
                                          'face `((:foreground "green" :background ,my-modeline-background))))
                                  (:eval (and battery-mode-line-string
                                          (propertize
                                           (concat "B" (substring battery-mode-line-string 1 -4))
                                           'face `((:foreground "plum3" :background ,my-modeline-background)))))))

   (defun toki-modeline-tabs ()
     "Return tabs."
     (if (bound-and-true-p toki-tabs-mode)
         (toki-tabs-string)
         ""))

   (setq mini-modeline-l-format '((:eval (toki-modeline-tabs))))

   ;; (setq mini-modeline--last-update (date-to-time "0001-01-01 00:00"))
   ;; (message "%s" (toki-modeline-tabs))
   ;; (mini-modeline--msg-message nil)

   (defun my-echo-tabs ()
     (let ((mini-modeline--msg nil)
           (mini-modeline--msg-message nil))
       (when (timerp mini-modeline--timer) (cancel-timer mini-modeline--timer))
       (mini-modeline-display 'force)
       (setq mini-modeline--timer
             (run-with-timer mini-modeline-echo-duration 0.3 #'mini-modeline-display 'force))))

   (defun keyboard-quit ()
     "Signal a `quit' condition.
During execution of Lisp code, this character causes a quit directly.
At top-level, as an editor command, this simply beeps."
     (interactive)
     ;; Avoid adding the region to the window selection.
     (setq saved-region-selection nil)
     (let (select-active-regions)
       (deactivate-mark))
     (if (fboundp 'kmacro-keyboard-quit)
         (kmacro-keyboard-quit))
     (when completion-in-region-mode
       (completion-in-region-mode -1))
     ;; Force the next redisplay cycle to remove the "Def" indicator from
     ;; all the mode lines.
     (if defining-kbd-macro
         (force-mode-line-update t))
     (setq defining-kbd-macro nil)
     (my-echo-tabs))

   (add-hook 'toki-tabs-update-hook 'my-echo-tabs))

;; (defun my-clear-echo-area ()
;;   (mini-modeline-display 'clear))
;; (message "%s" (toki-modeline-tabs))
;; (defun my-clear-echo-area ()
;;   (let ((mini-modeline--msg-message (toki-modeline-tabs)))
;;     (mini-modeline-display 'force)))

(provide 'init-modeline)
