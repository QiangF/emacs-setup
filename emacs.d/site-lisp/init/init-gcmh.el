; -*- coding:utf-8 -*-

(setup gcmh
  (:option gcmh-idle-delay 5
           gcmh-high-cons-threshold (* 1000 1024 1024) ;1000Mb
           gcmh-low-cons-threshold (* 20 1024 1024))
  (gcmh-mode 1))

(provide 'init-gcmh)
