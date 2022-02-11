; -*- coding:utf-8 -*-

; * image dired image
(setup image-file                       ; Visit images as images
  (:autoload auto-image-file-mode)
  (:with-hook image-dired-minor-mode-hook
    (:hook auto-image-file-mode)))

;; https://github.com/honmaple/dotfiles/blob/master/emacs.d/site-lisp/maple/maple-use-package.el
;; (setup image-dired
;;   (setq image-dired-dir (concat maple-cache-directory "image-dired")
;;         image-dired-thumbnail-storage 'standard)
;;   :evil-bind
;;   (:state normal :map image-dired-thumbnail-mode-map
;;           ("j" #'image-dired-next-line)
;;           ("k" #'image-dired-previous-line)
;;           ("l" #'image-dired-forward-image)
;;           ("h" #'image-dired-backward-image)
;;           ("q" #'image-dired-kill-buffer-and-window)
;;           ("RET" #'image-dired-display-thumbnail-original-image)))

;; (setup image-mode
;;   :evil-bind
;;   (:state normal :map image-mode-map
;;           ("j" #'image-next-file)
;;           ("k" #'image-previous-file)
;;           ("n" #'image-next-file)
;;           ("p" #'image-previous-file)
;;           ("q" #'quit-window)))

(setup (:pkg eimp)
  (:hook-into image-mode))

(setup (:pkg image-dired+)
  (:load-after image-mode)
  (:with-hook image-mode-hook
    (:hook my-image-mode-hook))
  (:with-hook image-dired-thumbnail-mode-hook
    (:hook my-thumbnail-mode-hook))
  (:with-map image-dired-thumbnail-mode-map
    (:bind
     "w" #'my-id-copy-file-name
     "j" #'my-id-jump-original-dired-buffer
     "\C-n" #'image-diredx-next-line
     "\C-p" #'image-diredx-previous-line
     "g" #'revert-buffer
     "x" #'image-diredx-flagged-delete))
  (:option
   image-dired-cmd-rotate-original-program "convert"
   image-dired-cmd-rotate-original-options "%p \"%o\" -rotate %d \"%t\""
   image-dired-thumb-size 'standard-large
   image-dired-track-movement nil) ;; Suppress unknown cursor movements:

  (:when-loaded
    (defun my-id-jump-original-dired-buffer ()
      "Jump to the dired buffer associated with the current image file."
      (interactive)
      (switch-to-buffer (image-dired-associated-dired-buffer))
      (if (not (dired-goto-file (image-dired-original-file-name)))
          (message "Could not track file")))

    (defun my-id-jump-thumbnail-buffer ()
      "Jump to thumbnail buffer."
      (interactive)
      (if (get-buffer image-dired-thumbnail-buffer)
          (switch-to-buffer image-dired-thumbnail-buffer)
          (image-dired default-directory)))

    (defun my-id-copy-file-name ()
      (interactive)
      (kill-new (expand-file-name (image-dired-original-file-name)))))

  (defun my-image-mode-hook ()
    (lambda ()
      (image-diredx-async-mode 1)
      (image-diredx-adjust-mode 1)
      (hl-line-mode -1)))

  (defun my-thumbnail-mode-hook ()
    (lambda ()
      (turn-off-my-mode)
      (hl-line-mode -1)))
  ;; "%p -rotate %d -copy all -outfile %t \"%o\""

  (defun image-dired-rotate-original (degrees)
    "Redefined function to rotate an original image by DEGREES degrees."
    (if (not (image-dired-image-at-point-p))
        (message "No image at point")
        (let ((file (image-dired-original-file-name))
              command)
          ;;(if (not (string-match "\.[jJ][pP[eE]?[gG]$" file))
          ;;    (error "Only JPEG images can be rotated!"))
          (setq command (format-spec
                         image-dired-cmd-rotate-original-options
                         (list
                          (cons ?p image-dired-cmd-rotate-original-program)
                          (cons ?o (expand-file-name file))
                          (cons ?d degrees)
                          (cons ?t image-dired-temp-rotate-image-file))))
          (if (not (= 0 (call-process shell-file-name nil nil nil
                                      shell-command-switch command)))
              (progn
                (error "Could not rotate image")
                ;;(message command)
                )
              (image-dired-display-image image-dired-temp-rotate-image-file)
              (if (or (and image-dired-rotate-original-ask-before-overwrite
                           (y-or-n-p
                            "Rotate to temp file OK.  Overwrite original image? "))
                      (not image-dired-rotate-original-ask-before-overwrite))
                  (progn
                    (copy-file image-dired-temp-rotate-image-file file t)
                    (image-dired-refresh-thumb))
                  (image-dired-display-image file)))))))

(provide 'init-image-dired)
