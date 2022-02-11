; -*- coding:utf-8 -*-

(setup (:pkg volume)
   (:bind "<escape>" #'volume-quit)
   (:autoload volume)
   (:option volume-backend 'volume-amixer-backend
            volume-electric-mode nil))

(setup (:pkg trashed)
   (:global* "C-x t" #'trashed))

(provide 'init-sys-utils)
