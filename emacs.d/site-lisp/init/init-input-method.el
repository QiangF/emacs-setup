; -*- coding:utf-8 -*-

(setup imbot
  (require 'imbot)
  (add-hook 'after-init-hook 'imbot-mode)
  (setq imbot--active-omit-check t))

(setup fuck
  (:autoload fuck))

(provide 'init-input-method)
