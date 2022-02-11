;;; ox-odt-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ox-jabref" "ox-jabref.el" (0 0 0 0))
;;; Generated autoloads from ox-jabref.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-jabref" '("org-jabref-" "ox-jabref-unload-function")))

;;;***

;;;### (autoloads nil "ox-odt" "ox-odt.el" (0 0 0 0))
;;; Generated autoloads from ox-odt.el

(put 'org-odt-preferred-output-format 'safe-local-variable 'stringp)

(autoload 'org-odt-export-as-odf "ox-odt" "\
Export LATEX-FRAG as OpenDocument formula file ODF-FILE.
Use `org-create-math-formula' to convert LATEX-FRAG first to
MathML.  When invoked as an interactive command, use
`org-latex-regexps' to infer LATEX-FRAG from currently active
region.  If no LaTeX fragments are found, prompt for it.  Push
MathML source to kill ring depending on the value of
`org-export-copy-to-kill-ring'.

\(fn LATEX-FRAG &optional ODF-FILE)" t nil)

(autoload 'org-odt-export-as-odf-and-open "ox-odt" "\
Export LaTeX fragment as OpenDocument formula and immediately open it.
Use `org-odt-export-as-odf' to read LaTeX fragment and OpenDocument
formula file." t nil)

(autoload 'org-odt-export-to-odt "ox-odt" "\
Export current buffer to a ODT or a OpenDocument XML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, write a OpenDocument
XML file holding just the contents.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

The function returns a file name in any one of the BACKEND
format, `org-odt-preferred-output-format' or XML format.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(autoload 'org-odt-export-to-odm "ox-odt" "\
Export current buffer to a ODM or a OpenDocument XML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, write a OpenDocument
XML file holding just the contents.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

The function returns a file name in any one of the BACKEND
format, `org-odt-preferred-output-format' or XML format.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(autoload 'org-odt-export-as-odt "ox-odt" "\
Export current buffer as a OpenDocument XML buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

The optional argument BODY-ONLY is ignored.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done to a buffer named \"*Org ODT Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(autoload 'org-odt-convert "ox-odt" "\
Convert IN-FILE to format OUT-FMT using a command line converter.
IN-FILE is the file to be converted.  If unspecified, it defaults
to variable `buffer-file-name'.  OUT-FMT is the desired output
format.  Use `org-odt-convert-process' as the converter.
If PREFIX-ARG is non-nil then the newly converted file is opened
using `org-open-file'.

\(fn &optional IN-FILE OUT-FMT OPEN)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-odt" '("org-odt-")))

;;;***

;;;### (autoloads nil nil ("ox-odt-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ox-odt-autoloads.el ends here
