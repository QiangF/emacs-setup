(defvar library-genesis-mirror "gen.lib.rus.ec"
  "The mirror for Library Genesis.
Other potential values if this one doesn't work:
libgen.is
libgen.lc
libgen.pw
bookfi.net")

(defvar library-genesis-search-types '("LibGen"
                                       "Scientific"
                                       "Fiction"
                                       "Comics"
                                       "Standards"
                                       "Magazines")
  "A list of search types that Library Genesis supports")

(defun library-genesis-format-type (type query)
  "Formats the search query for a specific type of search.
TYPE must be an element of the list `library-genesis-search-types'.
QUERY must be a string."
  (cond ((string-equal type "LibGen")
         (format "http://%s/search.php?req=%s&lg_topic=libgen" library-genesis-mirror query))
        ((string-equal type "Scientific")
         (format "http://%s/scimag/?q=%s" library-genesis-mirror query))
        ((string-equal type "Fiction")
         (format "http://%s/fiction/?q=%s" library-genesis-mirror query))
        ((string-equal type "Comics")
         (format "http://%s/comics/index.php?s=%s" library-genesis-mirror query))
        ((string-equal type "Standards")
         (format "http://%s/standarts/index.php?s=%s" library-genesis-mirror query))))

(defun library-genesis-search (type query)
  "Search Library Genesis. Interactively, prompt for a search
type and query.
If called from lisp, TYPE must be an element in
`library-genesis-search-types', and QUERY must be a string."
  (interactive (list
                (completing-read
                 "Library Search Type: "
                 library-genesis-search-types
                 nil t)
                (read-string
                 "Library Genesis: ")))
  ;; TODO: Parse results instead of `browse-url'
  (browse-url (library-genesis-format-type type query)))

(provide 'library-genesis)
