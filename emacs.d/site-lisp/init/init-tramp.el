(setup (:pkg counsel-tramp)
   (:global "C-c s" #'counsel-tramp)
   (:option counsel-tramp-custom-connections '(/ssh:212.64.5.232|sudo:q@localhost:/)))

(setenv "SHELL" "/bin/bash")

(setup tramp
   (:option tramp-default-method "ssh"
            (append tramp-connection-properties)
	        (list (regexp-quote "android") "remote-shell" "sh")))
