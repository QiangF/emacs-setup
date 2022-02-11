;; -*- coding:utf-8 -*-

; ** table
;; add  :skipcols to keep the data while no export 
;  at table header, use "C-c -" to insert seperating lines
; 通过 "C-c | (create or convert from region)" 这个快捷键来快速创建指定大小的表格。
; 以逗号(,)分隔的 CSV 格式的数据，可以将其拷贝到当前在编辑的 Org mode 文档中，
; 选中然后使用 "C-c |" 这个快捷键，就能将其转换成表格形式
; C-|, to transform the CSV file to an org-mode table.
; 如果数据之间是用空格分隔的，该如何转换呢?选中后使用快捷键"C-u 1 C-c |"即可。
; S-return   当单元格无内容时，将其上方第一个非空内容拷贝过来;否则拷贝当前内容到下一行并随之移动
;; C-c C-c   强制表格重新排列, also move cursor to the beginning of the cell
;; C-c ^   表格排序
; 在表格下方手工添加以 "+TBLFM:" 开头的行，然后直接添加公式 such as $2=$1+$3
; org table recalculate C-c * and C-u C-c C-c
; 在Org mode的表格公式中，用 "@" 来表示行，用 "$" 来表示列，最简单的，"@3$2" 表
; 示的是第三行第二列的位置。使用快捷键 "C-c }" 可以开启表格的横纵坐标显示
; 左上角为第二行第一列单元格、右下角为第四行第三列单元格的区域， @2$1..@4$3
; "@#" 表示当前行的行号，用 "$#" 表示当前列的列号
; Org mode 默认使用的是 Emacs 中自带的 Calc 这个 package 来进行计算
; 可以在org文件中添加诸如这样的行(define variable): #+CONSTANTS: pi=3.14 eps=2.4e-6
; *** alignment
;; If you would like to overrule the automatic alignment of
;; number-rich columns to the right and of string-rich columns to
;; the left, you can use ‘<r>’, ‘<c>’1 or ‘<l>’ in a similar
;; fashion. You may also combine alignment and field width like
;; this: ‘<r10>’.

; ** list
; 1. a
; - b
; - c
; Now, hit C-c C-c at the first line "1. a". Unordered list becomes numbered
; list and vice versa

; *** style
; set org-odt-styles-file to A .odt or .ott file to
; Use the styles.xml contained in the specified OpenDocument Text or Template file
; * org babel
; ** tips
; use org-odt-export-to-odt to see errors 
; C-c ' to edit the current code block, C-c C-c to execute
; Open a REPL using C-c C-v C-z so that you get completion in Python buffers.
; py expand, :session and :file are mutual exclusive
; Be sure to use %matplotlib inline, otherwise graphics won’t work.

; ** org odt

;; run "java -jar ~/.dotfiles/emacs/org/jabref/JabRef-2.9.2.jar"
;; copy extracted plugins to ~/.jabref/plugins
;; net.sf.jabref.export.Chicago.ODF(English)-1.2.jar is already installed
;; which ships with following citation styles
;;    - "Numeric"
;;    - "Chicago (full-note)"
;;    - "Chicago (author-date)"

(setup ox-jabref
  (:option org-odt-citation-transcoders
           '(org-odt-citation-reference/numbered . org-jabref-odt-bibliography/numbered)
           org-jabref-command
           `("java" "-jar" ,(expand-file-name "~/.dotfiles/emacs/org/jabref/JabRef-2.9.2.jar") "-n" "true")))

(setup ox-odt
   (:autoload
       org-odt-export-to-odt
       my-org-odt-export-buffer
       my-org-odt-export-buffer-to-docx
       org-odt-table-suggest-spans)
   (:option org-odt-display-outline-level 1 ; level included in caption numbering
            org-odt-max-image-size '(100.0 . 200.0)
            org-entities-user '(("space" "\\ " nil " " " " " " " "))
            org-odt-experimental-features '(short-caption-as-label)
            org-odt-styles-file "~/.dotfiles/emacs/org/styles/text_cn.xml"
            org-odt-content-template-file "~/.dotfiles/emacs/org/styles/OrgOdtContentTemplate.xml"
            ;; org-odt-convert-processes "soffice --headless --convert-to %f%x --outdir %d %i"
            org-odt-prettify-xml nil
            ;; org-odt-styles-dir "~/.emacs.d/org-mode-ox-odt/etc/styles"
            ;; org-odt-schema-dir "~/.dotfiles/emacs/emacs.d/site-lisp/ox-odt/schema"
            org-odt-math-syntax "starmath"
            org-use-sub-superscripts nil
            org-export-with-sub-superscripts '{}
            org-export-initial-scope 'subtree
            org-export-backends (quote (ascii html latex odt)))
   (:when-loaded
     (require 'my-odt)
     (add-to-list 'org-export-filter-item-functions 'hjh-odt-filter-list-graph-style)
     (add-to-list 'org-odt-experimental-features 'transclude-sole-footnote-references-in-a-table)
     ;; 图表注释抬头
     (setq org-odt-caption-and-xref-settings
           '((:LISTING:
              :caption-position below
              :caption-format (category "" counter " " caption)
              :xref-format (value))
             (:DVIPNG-IMAGE:
              :caption-position nil
              :caption-format nil
              :xref-format (text)
              :label-format ("(" counter ")"))
             (:MATH-FORMULA:
              :caption-position nil
              :caption-format nil
              :xref-format (text)
              :label-format ("(" counter ")"))
             (:FIGURE:
              :caption-position below
              :caption-format (category counter " " caption)
              :xref-format (value))
             (:SUBENTITY:
              :caption-position below
              :caption-format ("(" counter ") " caption)
              :xref-format (value))
             (:TABLE:
              :caption-position above
              :caption-format (category "" counter " " caption)
              :xref-format (value))))

     (setq org-odt-caption-and-numbering-settings
           '((:TABLE:        :variable "Table"        :entity-name "表"       :caption-style "Table"     :use-outline-levelp nil   :seq-num-format "1")
             (:FIGURE:       :variable "Figure"       :entity-name "图"       :caption-style "Figure"    :use-outline-levelp nil   :seq-num-format "1")
             (:SUBENTITY:    :variable "SubEntity"    :entity-name ""         :caption-style "Figure"    :use-outline-levelp nil   :seq-num-format "a")
             (:MATH-FORMULA: :variable "Text"         :entity-name "Equation" :caption-style "Figure"    :use-outline-levelp t     :seq-num-format "1")
             (:DVIPNG-IMAGE: :variable "Equation"     :entity-name "Equation" :caption-style "Figure"    :use-outline-levelp nil   :seq-num-format "1")
             (:LISTING:      :variable "Listing"      :entity-name "Listing"  :caption-style "Listing"   :use-outline-levelp nil   :seq-num-format "1")))

     ;; odt separate list paragraph style from main paragraph style
     (defun hjh-odt-filter-list-graph-style (text backend info)
       "Replace Text_20_body with Text_20_list_20_body in lists"
       (when (org-export-derived-backend-p backend 'odt)
         (replace-regexp-in-string "Text_20_body" "Text_20_list_20_body" text)))

     ;; extension path is ~/.dotfiles/emacs/org/OrgModeUtilities.oxt
     ;; optimise column width takes time
     (setq org-odt-transform-processes
           '(("update field" "soffice" "--norestore" "--invisible" "--headless" "macro:///OrgMode.Utilities.UpdateAll(%I)")
             ;; ("reload" "soffice" "--norestore" "macro:///OrgMode.Utilities.SilentlyReload(%I)")
             ;; ("delete empty paragraphs" "soffice" "macro:///OrgMode.Utilities.DeleteEmptyPages(%I)")
             ;; ("optimise table column width" "soffice" "--norestore" "--invisible" "--headless" "macro:///OrgMode.Utilities.OptimizeColumnWidth(%I)")
             ))

     (defun my-org-odt-export-to-odt ()
       (let ((file-name)
             (org-confirm-babel-evaluate nil))
         (if global-so-long-mode
             (progn
               (so-long-disable)
               (setq file-name (org-odt-export-to-odt))
               (so-long-enable))
             (setq file-name (org-odt-export-to-odt)))
         ;; (call-process "soffice" nil 0 nil file-name "macro:///OrgMode.Utilities.DeleteEmptyPages")
         file-name))

     (defun my-org-odt-export-buffer ()
       (interactive)
       (let ((file-name (my-org-odt-export-to-odt)))
         (progn
           (call-process "soffice" nil 0 nil file-name "macro:///OrgMode.Utilities.SilentlyReload"))))

     (require 'cl)
     (defun my-org-odt-export-slide ()
       (interactive)
       ;; export content under the current slide tag
       (save-excursion
         (let* ((temp-buffer (generate-new-buffer "odt-temp"))
                (slide-tag "^# slide-tag")
                (current-slide-end (if (re-search-forward slide-tag nil 'move)
                                       (line-beginning-position)
                                       (point)))
                (current-slide-start (progn
                                       (beginning-of-line)
                                       (re-search-backward slide-tag nil 'move)
                                       (point)))
                (cover-end (progn (goto-char 1)
                                  (if (re-search-forward slide-tag nil 'move)
                                      (line-beginning-position)
                                      (point))))
                (cover (buffer-substring-no-properties 1 cover-end))
                (current-slide (buffer-substring-no-properties current-slide-start current-slide-end)))
           (if (<= cover-end current-slide-start)
               (flet ((buffer-file-name (&optional BUFFER)
                        (concat default-directory "temp.org")))
                 (with-current-buffer temp-buffer
                   (insert cover)
                   (insert current-slide)
                   (my-org-odt-export-to-odt)
                   (and (buffer-name temp-buffer)
                        (kill-buffer temp-buffer))))
               (my-org-odt-export-to-odt)))))

     (defun my-org-odt-export-heading ()
       (interactive)
       (save-excursion
         (let* ((temp-buffer (generate-new-buffer "odt-temp"))
                (slide-tag "^# slide-tag")
                (current-slide-start (progn
                                       (beginning-of-line)
                                       (outline-previous-heading)
                                       (point)))
                (current-slide-end (if (outline-get-next-sibling)
                                       (line-beginning-position)
                                       (point)))
                (cover-end (progn (goto-char 1)
                                  (if (outline-next-heading)
                                      (line-beginning-position)
                                      (point))))
                (cover (buffer-substring-no-properties 1 cover-end))
                (current-slide (buffer-substring-no-properties current-slide-start current-slide-end)))
           (if (<= cover-end current-slide-start)
               (flet ((buffer-file-name (&optional BUFFER)
                        (concat default-directory "temp.org")))
                 (with-current-buffer temp-buffer
                   (insert cover)
                   (insert current-slide)
                   (my-org-odt-export-to-odt)
                   (and (buffer-name temp-buffer)
                        (kill-buffer temp-buffer))))
               (my-org-odt-export-to-odt)))))

     (defun my-org-odt-export-buffer-to-docx ()
       (interactive)
       (let* ((out-file-name (my-org-odt-export-to-odt))
              (out-file-dir (file-name-directory out-file-name)))
         (let ((default-directory out-file-dir))
           (rename-file out-file-name "tmp.odt" t)
           ;; soffice can't convert some file names
           (org-odt-do-convert "tmp.odt" "docx")
           (rename-file "tmp.docx" (concat (file-name-sans-extension out-file-name) ".docx") t))))

     ;; odt files are saved to /tmp/my_tmp
     (defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
       (unless pub-dir
         (setq pub-dir "/tmp/my_tmp")
         (unless (file-directory-p pub-dir)
           (make-directory pub-dir)))
       (apply orig-fun extension subtreep pub-dir nil))
     (advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)

     (setq org-latex-to-mathml-convert-command "java -jar %j -unicode -force -df %o %I"
           org-latex-to-mathml-jar-file "~/.dotfiles/emacs/org/mathtoweb.jar")

     (defun org-odt--extract-starmath-from-latex-frag (&optional latex-frag)
       (let* ((latex-frag (or latex-frag (buffer-substring-no-properties (region-beginning) (region-end))))
	          (latex-frag (org-trim latex-frag)))
         (cond
           ((and (string-prefix-p "$" latex-frag)
	             (string-suffix-p "$" latex-frag))
            (substring latex-frag 2 -2))
           ((and (string-prefix-p "\\[" latex-frag)
	             (string-suffix-p "\\]" latex-frag))
            (substring latex-frag 2 -2))
           ((and (string-prefix-p "\\(" latex-frag)
	             (string-suffix-p "\\)" latex-frag))
            (substring latex-frag 2 -2))
           ((string-match (rx-to-string '(seq "\\begin{" (group (one-or-more (any "0-9A-Za-z" "*"))) "}")) latex-frag)
            (let* ((prefix (match-string 1 latex-frag))
	               (preamble (format "\\begin{%s}" prefix))
	               (postamble (format "\\end{%s}" prefix)))
	          (when (string-suffix-p postamble latex-frag)
	            (substring latex-frag (length preamble) (- (length postamble))))))
           (t (error "Couldn't match latex-frag: %S" latex-frag)))))

     (defun org-create-math-formula-from-starmath (latex-frag &optional mathml-file)
       "Convert LATEX-FRAG to MathML and store it in MATHML-FILE.
LATEX-FRAG here is in fact _not_ a latex fragment as such, but a
startmath formula delimited using .  If the conversion is
successful, return the portion between \"<math...> </math>\"
elements otherwise return nil.  When MATHML-FILE is specified,
write the results in to that file.  When invoked as an
interactive command, prompt for LATEX-FRAG, with initial value
set to the current active region and echo the results for user
inspection."
       (interactive (list (let ((frag (when (org-region-active-p)
					                    (buffer-substring-no-properties
					                     (region-beginning) (region-end)))))
			                (read-string "LaTeX Fragment: " frag nil frag))))
       (let* ((starmath  (org-odt--extract-starmath-from-latex-frag latex-frag))
	          (dummy (message "\n->%s<-\n\n-->%s<--\n" latex-frag starmath))

	          (math
		        (format "
<math xmlns=\"http://www.w3.org/1998/Math/MathML\" display=\"block\">
 <semantics>
  <annotation encoding=\"StarMath 5.0\">%s</annotation>
 </semantics>
</math>

"
			            starmath))
	          (mathml
		        (format "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n%s\n"
			            math)))
	     (when mathml-file
	       (message "Wrote %s" mathml-file)
	       (write-region mathml nil mathml-file))
	     math))

     ;; starmath fragment has to be enclosed in {}
     (advice-add 'org-create-math-formula :override 'org-create-math-formula-from-starmath)))

  ;; (defun org-odt-special-block (special-block contents info)
  ;; "Transcode a SPECIAL-BLOCK element from Org to ODT.
  ;; CONTENTS holds the contents of the block.  INFO is a plist
  ;; holding contextual information."
  ;; (let ((type (org-element-property :type special-block))
  ;; (attributes (org-export-read-attribute :attr_odt special-block)))
  ;;     (cond
  ;;     ;; Annotation.
  ;;     ;; Textbox.
  ;;     ((string= type "textbox")
  ;;         (let ((width (plist-get attributes :width))
  ;;         (height (plist-get attributes :height))
  ;;         (style (plist-get attributes :style))
  ;;         (extra (plist-get attributes :extra))
  ;;         (anchor (plist-get attributes :anchor)))
  ;;           (format "\n<text:p text:style-name=\"%s\">%s</text:p>" "Text_20_body"
  ;;                   (org-odt--textbox contents width height style extra anchor))))
  ;;     ((string= type "my-section")
  ;;         (let ((columns (plist-get attributes :columns)))
  ;;             (format "\n<text:section text:style-name=\"Sect%s\" text:name=\"%s\">%s</text:section>"
  ;;             columns (car (org-odt-add-automatic-style "Sect")) contents )))
  ;;     (t contents))))

  ;; (setq org-odt-with-latex 'imagemagick
  ;;       org-preview-latex-default-process 'imagemagick
  ;;       org-odt-pixels-per-inch 96)

  ;; (setq org-latex-to-mathml-convert-command
  ;;             "latexmlmath %i --presentationmathml=%o")

  ;; (setq org-latex-to-mathml-convert-command "latex2mathml -f %I > %o")

  ;; %j:     Executable file in fully expanded form as specified by
  ;;         `org-latex-to-mathml-jar-file'.
  ;; %I:     Input LaTeX file in fully expanded form.
  ;; %i:     The latex fragment to be converted.
  ;; %o:     Output MathML file.
  ;; (use-package ox-latex-chinese
  ;;     :ensure nil
  ;;     :config (oxlc/toggle-ox-latex-chinese t))

  ;; (defun my-with-theme (orig-fun &rest args)
  ;;   (let ((my-org-export-theme 'doom-nord-light))
  ;;     (load-theme my-org-export-theme ' t)
  ;;     (unwind-protect
  ;;          (apply orig-fun args)
  ;;       (disable-theme my-org-export-theme))))

  ;; (advice-add 'org-odt-export-to-odt :around 'my-with-theme)

(provide 'init-org-odt)
