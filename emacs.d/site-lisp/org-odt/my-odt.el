;; -*- coding:utf-8 -*-

(defun org-odt--table-cell-widths (table info)
  (let* ((num-columns (cdr (org-export-table-dimensions table info)))
         (widths
           (cond
             ;; Case 1: Widths comes from `:widths'.
             ((org-odt--read-attribute table :widths)
              (mapcar (lambda (n) (* 1.0 (string-to-number n)))
                      (let ((widths (org-odt--read-attribute table :widths)))
                        (when (stringp widths)
                          (split-string widths "\\(?:,[[:space:]]*\\)" t "\\(?:[[:space:]]+\\)")))))
             ;; Case 2: Widths come from `:col-cookies'.
             (t (plist-get (org-odt--table-col-cookies table info) :widths)))))
    (setq widths (mapcar (lambda (w)
                           (if (or (null w) (zerop w)) 1 w))
                         (or widths (make-list num-columns 1))))

    (unless (= (length widths) num-columns)
      (user-error "You haven't specified widths of all columns"))

    (let* ((cum-width (apply #'+ widths))
           (normalized-cum-width 1000))
      (cl-loop for width in widths
               collect (/ (* normalized-cum-width width) cum-width)))))

(defun org-odt-link--inline-formula (element info)
  (let* ((src (let* ((_type (org-element-property :type element))
                     (raw-path (org-element-property :path element)))
                (cond
                  ((file-name-absolute-p raw-path)
                   (expand-file-name raw-path))
                  (t raw-path))))
         (src-expanded (if (file-name-absolute-p src) src
                           (expand-file-name src (file-name-directory
                                                  (plist-get info :input-file)))))
         (href
           (format
            "\n<draw:object %s xlink:href=\"%s\" xlink:type=\"simple\"/>"
            " xlink:show=\"embed\" xlink:actuate=\"onLoad\""
            (file-name-directory (org-odt--copy-formula-file
                                  info
                                  src-expanded
                                  (format "Formula-%04d/"
                                          (org-odt--count-object info :formulas))))))
         (standalone-link-p (org-odt--standalone-link-p element info))
         (embed-as (if standalone-link-p 'paragraph 'character))
         (captions (org-odt-format-label element info 'definition))
         ;; Check if this link was created by LaTeX-to-MathML
         ;; converter.
         (replaces (org-element-property
                    :replaces (if (not standalone-link-p) element
                                  (org-export-get-parent-element element))))
         ;; If yes, note down the type of the element - LaTeX Fragment
         ;; or LaTeX environment.  It will go in to frame title.
         (title (and replaces (capitalize
                               (symbol-name (org-element-type replaces)))))

         ;; If yes, note down its contents.  It will go in to frame
         ;; description.  This quite useful for debugging.
         (desc (and replaces (org-element-property :value replaces)))
         (width nil) (height nil))
    (cond
      ((eq embed-as 'character)
       (org-odt--render-image/formula "InlineFormula" href width height
                                      nil nil title desc))
      (t
       (let* ((equation (org-odt--render-image/formula
                         "CaptionedDisplayFormula" href width height
                         captions nil title desc))
              (label
                (car (org-odt-format-label element info 'definition :label-format))))
         (concat "<text:tab/>" equation "<text:tab/>" label))))))

(defun org-odt-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to ODT.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (setq contents (org-trim contents))
  (if (org-odt--transclude-link-p paragraph info) contents
    (let* ((parent (org-export-get-parent paragraph))
	   (_parent-type (org-element-type parent))
	   (genealogy (cons paragraph (org-element-lineage paragraph)))
	   (data (reverse genealogy))
	   (style
	    ;; Traverse the parse-tree from root element to this
	    ;; paragraph.  Use the following rule at each element to
	    ;; calculate the paragraph style applicable at that element.

	    ;; Case 1: If an element specifies an EXPLICIT STYLE of it's
	    ;; own via the #+ATTR_ODT line, use it.  PARAGRAPH and
	    ;; SPECIAL-BLOCK use the `:style' attribute for this
	    ;; purpose, while TABLE and PLAIN-LIST uses `:p-style'
	    ;; attribute.

	    ;; Case 2: If an element does not have an explicit style but
	    ;; has an IMPLICIT, PRE-CONFIGURE STYLE of it's own, use it.
	    ;; For example, paragraphs within a FOOTNOTE-DEFINITON,
	    ;; CENTER-BLOCK or QUOTE-BLOCK get pre-configured styles
	    ;; like "Footnote", "OrgCenter" or "Quotations" resply.

	    ;; Case 3: If an element specifies neither an IMPLICIT style
	    ;; or an EXPLICIT style, use the style from it's parent.
	    ;; For example, a paragraph within a TABLE and PLAIN-LIST
	    ;; (that doesn't specify a `:p-style' of it's own) inherit
	    ;; it's style from the it's parent.

	    ;; Case 4: If an element has no parent (i.e., root node),
	    ;; use the fallback style "Text_20_body".
	    (cl-loop for el in data
		     ;; Fallback style.
		     with style = (or
				   ;; Style set in `org-odt-link--inline-image'
				   ;; or `org-odt-link--inline-formula'.
				   (get-text-property 0 :p-style contents)
				   "Text_20_body")
		     with within-note-definition-p = nil do
		     (setq style
			   (or
			    ;; Case 1: Does this node IMPLICITLY or
			    ;; EXPLICITLY specify a style?  Use it.
			    (cl-case (org-element-type el)
			      (verse-block
			       (or (org-odt--read-attribute el :style)
				   "OrgVerse"))
			      (center-block
			       (or (org-odt--read-attribute el :style)
				   (cl-case within-note-definition-p
				     (footnote "OrgFootnoteCenter")
				     (endnote "OrgEndnoteCenter")
				     (t "OrgCenter"))))
			      (footnote-definition
			       (setq within-note-definition-p
				     (if (org-odt--endnote-p el info) 'endnote 'footnote))
			       (or (org-odt--read-attribute el :style)
				   (cl-case within-note-definition-p
				     (footnote "Footnote")
				     (endnote "Endnote")
				     (t (error "This shouldn't happen")))))
			      (paragraph
			       (or
				;; Case 1: Some paragraphs are "created"
				;; not by the user but by the
				;; pre-processing stage.  They use the
				;; `:style' property of the element rather
				;; than the style property from the
				;; attribute line.  See
				;; `org-odt--translate-description-lists/latex',
				;; `org-odt--translate-description-lists/html'
				;; `org-odt--translate-latex-fragments'.
				(org-element-property :style el)
				(org-odt--read-attribute el :style)))
			      (plain-list
			       ;; NOTE: ITEMs cannot have #+ATTR_ODT
			       ;; attached to them.  See
			       ;;
			       ;; http://lists.gnu.org/archive/html/emacs-orgmode/2013-08/msg00586.html
			       (org-odt--read-attribute el :p-style))
			      (quote-block
			       (or (org-odt--read-attribute el :style)
				   (cl-case within-note-definition-p
				     (footnote "OrgFootnoteQuotations")
				     (endnote "OrgEndnoteQuotations")
				     (t "Quotations"))))
			      (special-block
			       (let ((type (downcase (org-element-property :type el))))
				 (cond
				  ;; Case 1: Handle SPECIAL-BLOCKs that are
				  ;; well-known (and treated specially) by
				  ;; the ODT exporter.
				  ((string= type "textbox")
				   (org-odt--read-attribute el :p-style))
				  ((string= type "customshape")
				   (org-odt--read-attribute el :p-style))
				  ((string= type "section")
				   (org-odt--read-attribute el :p-style))
				  ;; Case 2: Handle user-specified
				  ;; SPECIAL-BLOCKs not known to the
				  ;; exporter.
				  (t (org-odt--read-attribute el :style)))))
			      (table-cell
			       ;; A table cell can have paragraphs, only if
			       ;; it is part of a list table.
			       (org-odt-table-cell--get-paragraph-styles el info)))
			    ;; Case 2: Element doesn't specify a style of
			    ;; it's own.  Use the parent style.
			    style))
		     finally return style)))
      ;; If this paragraph is a leading paragraph in an item and the
      ;; item has a checkbox, splice the checkbox and paragraph contents
      ;; together.
      (when (and (eq (org-element-type parent) 'item)
		 (eq paragraph (car (org-element-contents parent))))
	(setq contents (concat (org-odt--checkbox parent) contents)))

      (cond
       ;; Is this paragraph part of a paragraph block?
       ((and (eq (org-element-type parent) 'special-block)
	     (string= "paragraph" (downcase (org-element-property :type parent))))
	;; Yes.  If the paragraph is the last paragraph in the block,
	;; return it's contents, otherwise append a space to it.
	(if (eq paragraph (car (last (org-element-contents parent)))) contents
	  (concat contents " ")))
       (t
	(format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		(org-odt--get-derived-paragraph-style paragraph info style)
		contents))))))

(defun org-odt--table-compute-spans (table info)
  (let* ((cell-empty-p (lambda (table-cell)
			 (null (org-element-contents table-cell))))
	 (data-row-p (lambda (table-row)
		       (and (eq (org-element-property :type table-row) 'standard)
			    (not (org-export-table-row-is-special-p table-row 'ignore)))))
	 (get-span-length
	  (lambda (table-cells)
	    (cl-loop for table-cell in table-cells
		     while (funcall cell-empty-p table-cell)
		     counting t into n
		     finally (return n))))
	 (table (cl-loop for table-row in (org-element-contents table)
			 when (funcall data-row-p table-row)
			 collect table-row)))
    (cl-loop for table-row in table counting t into r
	     when (cl-loop for table-cell in (org-element-contents table-row)
			   counting t into c
			   when (and (not (funcall cell-empty-p table-cell))
				     (let* ((rowspan (funcall get-span-length
							      (cl-loop for table-row in (org-export-get-next-element table-row info t)
								       when (funcall data-row-p table-row)
								       collect (nth (1- c) (org-element-contents table-row)))))
					    (colspan (funcall get-span-length (org-export-get-next-element table-cell info t))))
				       (unless (and (zerop rowspan)
						    (zerop colspan))
					 (format "@%d$%d{%s:%s}"
						 r c
						 (number-to-string (1+ rowspan))
						 (number-to-string (1+ colspan))))))
			   collect it)
	     collect it)))

(defun org-odt-table-suggest-spans ()
  "Insert `:span' lines for a table at point.

Use empty cells to infer a spanned cell.  i.e., A table cell is
considered as spanned cell if it has an empty cell immediately
below it or to the right.  For a spanned cell, suggest the
`:span' attribute by counting the number of empty cells
immediately to it's right and immediately right below it.  This
command doesn't alter any prior attribute lines including the
`:span' lines.

Note that the `#+ATTR_ODT: :span ...' lines are merely
suggestions.  You may have to tweak the suggestions a bit to get
the desired typesetting.  This function generates multipe `:span'
lines, with each line containing *all* spanned cells on a unique
row.

To understand \"You may have to tweak the suggestions a bit ...\"
remark above above, consider the following example.

If you want to produce the following table

    #+begin_example
    +----------+----------+----------+
    |Column 1  |Column 2  |Column 3  |
    +----------+----------+----------+
    |A         |B                    |
    |          +----------+----------+
    |          |C         |D         |
    +----------+----------+----------+
    |E         |F                    |
    +----------+                     |
    |G         |                     |
    +----------+---------------------+
    |H                               |
    +--------------------------------+
    #+end_example

you will start with the following Org table:

    #+ATTR_ODT: :style \"GriddedTable\"
    |----------+----------+----------|
    | Column 1 | Column 2 | Column 3 |
    |----------+----------+----------|
    | A        | B        |          |
    |          | C        | D        |
    | E        | F        |          |
    | G        |          |          |
    | H        |          |          |
    |----------+----------+----------|

When you invoke `M-x org-odt-table-suggest-spans' on this table,
you will get the following result

    #+ATTR_ODT: :style \"GriddedTable\"
    #+ATTR_ODT: :span \"@1$3{2:1}\"
    #+ATTR_ODT: :span \"@2$1{2:1} @2$2{1:2}\"
    #+ATTR_ODT: :span \"@3$3{4:1}\"
    #+ATTR_ODT: :span \"@4$2{3:2}\"
    #+ATTR_ODT: :span \"@5$1{1:3}\"
    #+ATTR_ODT: :span \"@6$1{1:3}\"
    |----------+----------+----------|
    | Column 1 | Column 2 | Column 3 |
    |----------+----------+----------|
    | A        | B        |          |
    |          | C        | D        |
    | E        | F        |          |
    | G        |          |          |
    | H        |          |          |
    |----------+----------+----------|

If you export this table, you will get a table with col and
rowspans but in a \"wrong\" way. In order to get the desired
spans, you have to do the following \"edits\"

     #+ATTR_ODT: :style \"GriddedTable\"
    -#+ATTR_ODT: :span \"@1$3{2:1}\"
     #+ATTR_ODT: :span \"@2$1{2:1} @2$2{1:2}\"
    -#+ATTR_ODT: :span \"@3$3{4:1}\"
    -#+ATTR_ODT: :span \"@4$2{3:2}\"
    -#+ATTR_ODT: :span \"@5$1{1:3}\"
    +#+ATTR_ODT: :span \"@4$2{2:2}\"
     #+ATTR_ODT: :span \"@6$1{1:3}\"

That is,

    - Ignore `:span'-suggestions for first, third and fifth rows
    - Modify the `:span'-suggestions on fifth row
    - Retain `:span'-suggestions on other rows

and end up with the table like this:

    #+ATTR_ODT: :style \"GriddedTable\"
    #+ATTR_ODT: :span \"@2$1{2:1} @2$2{1:2}\"
    #+ATTR_ODT: :span \"@4$2{2:2}\"
    #+ATTR_ODT: :span \"@6$1{1:3}\"
    |----------+----------+----------|
    | Column 1 | Column 2 | Column 3 |
    |----------+----------+----------|
    | A        | B        |          |
    |          | C        | D        |
    | E        | F        |          |
    | G        |          |          |
    | H        |          |          |
    |----------+----------+----------|

Note that the `:span'-suggestions are split row-wise,
specifically to help with subsequent tweaks."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (org-with-wide-buffer
     (let* ((table (let* ((el (org-element-at-point)))
		     (when (memq (org-element-type el) '(table-row table))
		       (narrow-to-region (org-table-begin)
					 (org-table-end))
		       (org-element-map (org-element-parse-buffer) 'table #'identity nil t)))))
       (when table
	 (goto-char (org-table-begin))
	 (insert
	  (mapconcat #'identity
		     (cl-loop for row in (org-odt--table-compute-spans table nil)
			      collect (format "#+ATTR_ODT: :span \"%s\""
					      (mapconcat #'identity row " ")))
		     "\n")
	  "\n"))))))

(provide 'my-odt)
