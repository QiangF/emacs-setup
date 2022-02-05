;;; use-proxy-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "use-proxy" "use-proxy.el" (0 0 0 0))
;;; Generated autoloads from use-proxy.el

(defvar use-proxy-mode nil "\
Non-nil if Use-Proxy mode is enabled.
See the `use-proxy-mode' command
for a description of this minor mode.")

(custom-autoload 'use-proxy-mode "use-proxy" nil)

(autoload 'use-proxy-mode "use-proxy" "\
Toggle proxy mode.

If called interactively, enable Use-Proxy mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'use-proxy-toggle-proxies-global "use-proxy" "\
Toggle proxies globally by set/unset no_proxy key in `url-proxy-services'." t nil)

(autoload 'use-proxy-toggle-proto-proxy "use-proxy" "\
Toggle proxy on/off.
You can toggle proxy per protocol, and proxy status will show on mode-line.
This function will set/unset `url-proxy-services' to enable/disable proxies.
Argument PROTO protocol which you want to enable/disable proxy for.

\(fn PROTO)" t nil)

(autoload 'use-proxy-toggle-all-proxies "use-proxy" "\
Toggle all proxies on/off." t nil)

(autoload 'use-proxy-with-custom-proxies "use-proxy" "\
Use proxies on a group of S-expressions.
This function respects `use-proxy-<protocol>-proxy' variables,
and provide a local `url-proxy-services' to argument `BODY'.
Argument PROTOS protocol list such as '(\"http\" \"https\").

\(fn PROTOS &rest BODY)" nil t)

(autoload 'use-proxy-with-specified-proxies "use-proxy" "\
Use proxies on a group of S-expressions.
This function doesn't respect custom `use-proxy-<protocol>-proxy' variables.
It provides a local `url-proxy-services' to argument `BODY'.
Argument PROTOS-ASSOC protocol association list in the form of
'((\"http\" . \"localhost:1234\") (\"https\" . \"localhost:2345\")).

\(fn PROTOS-ASSOC &rest BODY)" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-proxy" '("use-proxy-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; use-proxy-autoloads.el ends here
