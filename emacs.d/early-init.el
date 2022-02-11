;; early-init.el

;; The early init file is read too early into the startup process,
;; and some important parts of the Emacs session, such as
;; 'window-system' and other GUI features, are not yet set up, which
;; could make some customization fail to work.

;; Better not touch it. In Emacs-22 we introduced gc-cons-percentage which provides the same benefit
;; as increasing gc-cons-threshold but without the drawbacks. And without having to fiddle with
;; it. I.e. I'd recommend users to remove any gc-cons-threshold settings from their .emacs."

;; There's also gc-cons-percentage which performs a gc if the amount of new memory used as a
;; percentage of the total has increased by a certain amount. If you set gc-cons-threshold to a
;; large number that effectively puts gc-cons-percentage into the driving seat.

(message "Now loading early-init.el")

(define-advice load (:before (feature &rest _))
  "Message the user when loading a library."
  (with-temp-message (format "Now loading: '%s'" feature)))

(setq init-file-name-handler-alist file-name-handler-alist
      init-gc-cons-threshold  gc-cons-threshold
      init-percentage gc-cons-percentage)

(setq file-name-handler-alist nil)
(setq gc-cons-threshold 100000000
      message-log-max 16384
      gc-cons-percentage 0.6
      auto-window-vscroll nil
      package-enable-at-startup nil ; don't auto-initialize!
      ;; don't add that `custom-set-variables' block to my init.el!
      ;; package--init-file-ensured t
      ;; package-quick-start t
      load-prefer-newer t
      inhibit-default-init t
      package-check-signature nil
      package-user-dir "~/.emacs.d/elpa/")

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)
;; (put 'narrow-to-page 'disabled nil)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(add-hook 'emacs-startup-hook
            (lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done)
              (setq file-name-handler-alist init-file-name-handler-alist
                    gc-cons-threshold init-gc-cons-threshold 
                    gc-cons-percentage init-percentage)
              (garbage-collect))
          t)

(push (expand-file-name "setup" user-emacs-directory) load-path)

(require 'dump-init)
