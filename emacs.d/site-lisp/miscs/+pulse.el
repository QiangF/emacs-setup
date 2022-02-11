;;; +pulse.el -*- lexical-binding: t; -*-

;;; Code:

(require 'pulse)

(defgroup +pulse nil
  "Extra customizations for `pulse'."
  :group 'pulse
  :prefix "+pulse-")

(defcustom +pulse-location-commands '(scroll-up-command
                                      scroll-down-command
                                      recenter-top-bottom
                                      other-window
                                      switch-to-buffer
                                      redraw-frame)
  "Commands to pulse the current line after.
Good for finding location."
  :type '(repeat function))

(defcustom +pulse-location-function '+pulse-line-current-window
  "What function to call after `+pulse-location-commands'."
  :type 'function)

;; XXX: this doesn't work yet.  I only want to pulse the line in the
;; active window, so when I have the same buffer viewed in multiple
;; windows I can still see where my cursor is.  To see the issue, C-x
;; 2 then C-x o a few times.
(defun +pulse-line-current-window (&rest _)
  "Pulse the current line, but only if this window is active."
  (pulse-momentary-highlight-one-line
   (window-point (selected-window))))

(defun +pulse--advice-remove (symbol where function &optional props)
  "Remove advice SYMBOL from FUNCTION.
This uses the same args as `advice-add' for easy toggling.
WHERE and PROPS are discarded."
  (ignore where props)
  (advice-remove symbol function))

(define-minor-mode +pulse-location-mode
  "After moving locations, pulse where we are."
  :global t
  :keymap nil
  (dolist (command +pulse-location-commands)
    (funcall
     (if +pulse-location-mode 'advice-add '+pulse--advice-remove)
     command :after +pulse-location-function)))

(provide '+pulse)
;;; +pulse.el ends here
