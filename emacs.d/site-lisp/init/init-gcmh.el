; -*- coding:utf-8 -*-

(setup gcmh
  (:option gcmh-idle-delay 5
           gcmh-high-cons-threshold (* 100 1024 1024) ;100Mb
           gcmh-low-cons-threshold (* 20 1024 1024))
  (gcmh-mode 1))

(provide 'init-gcmh)
