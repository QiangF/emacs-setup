; -*- coding:utf-8 -*-

;; This special method uses the Android Debug Bridge for accessing
;; Android devices. The Android Debug Bridge must be installed
;; locally. Some GNU/Linux distributions offer it for installation,
;; otherwise it can be installed as part of the Android SDK. If the
;; adb program is not found via the PATH environment variable, the
;; variable tramp-adb-program must point to its absolute path.

;; Tramp does not connect Android devices to adb. This must be
;; performed outside Emacs. If there is exactly one Android device
;; connected to adb, a host name is not needed in the remote file
;; name.

; The default TRAMP name to be used is /adb::

; connect over wifi: adb connect 192.168.1.100
; /adb:192.168.1.100:

;; therefore. Otherwise, one could find potential host names with
;; the command adb devices.
