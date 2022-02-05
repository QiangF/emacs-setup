; -*- coding:utf-8 -*-

;; Translate the problematic keys to the function key Hyper:
(when (display-graphic-p)
  (keyboard-translate ?\C-I ?\H-I)
  (keyboard-translate ?\C-m ?\H-m)
  ;; Rebind then accordantly:
  (global-set-key [?\H-m] 'delete-backward-char))
;; (global-set-key [?\H-i] 'iswitchb-buffer)

;; linux console
;; # loadkeys /usr/share/kbd/keymaps/term-keys.keymap

;; However this keymap is only active for the current session. In
;; order to load the keymap at boot, specify the full path to the
;; file in the KEYMAP variable in /etc/vconsole.conf

(setup term-keys
   (require 'term-keys-linux)
   (term-keys-mode t)
   (defun my-term-key-init ()
     (interactive)
     (with-temp-buffer
       (insert (term-keys/linux-keymap))
       (write-region (point-min) (point-max) "~/.dotfiles/home/term-keys.keymap"))))

(provide 'init-term-keys)
