; -*- coding:utf-8 -*-

; * gpg gpg-agent is that it is similarly to ssh-agent just
;; stores the passwords you have, once you activate it. So you won't
;; be prompted for the password neither when you open a file, nor
;; when you save it (as long as the agent remembers the password)
(setup epa-file
  (add-hook 'after-init-hook 'epa-file-enable)
  ;; ask encyption password once
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)
  (setq epa-pinentry-mode 'loopback))

; prevent Emacs spread copies of files with sensitive data.
(setup sensitive)

(provide 'init-gpg)
