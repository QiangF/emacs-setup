; -*- coding:utf-8 -*-

; * org-mode
; ** tips
; on heading C-c C-c set tags, C-c C-c on the top of the org file in dired.
; It updates the local config of the file: tags etc
;  C-c \     (org-match-sparse-tree) search tags
; helm-org-in-buffer-headings, there is counsel-outline

; ** settings
;; (use-package ob-ipython
;;  :ensure t
;;  :config
;;    (add-hook 'org-mode-hook 'ob-ipython-auto-configure-kernels)
;;    (setq ob-ipython-command (concat (getenv "my_usr") "/bin_repo/conda3/bin/jupyter")))

; ** org chinese
;; trun word wrap off in visual-line-mode
(setup org-chinese-utils
   (:option org-default-language "zh-CN"))

(setup org
   (:delay 5)
   (:option org-goto-interface 'outline-path-completion
            org-return-follows-link nil
            org-descriptive-links nil
            org-adapt-indentation nil
            org-startup-truncated nil          ;; wraps the lines in org-mode
            org-export-with-sub-superscripts t ;subscript ‘a_{b}’, superscript 'a^{b}'
            org-export-headline-levels 4       ; need to set for odt exporter with options :H 4
            org-list-allow-alphabetical t
            org-goto-max-level 8
            org-adapt-indentation t
            org-startup-indented nil
            org-hide-emphasis-markers t
            org-imenu-depth 5
            ;; org-startup-folded t
            fill-column 120
            org-log-done 'time ;; 记录完成时间
            org-blank-before-new-entry '((heading #'nil)
                                         (plain-list-item #'nil))
            org-image-actual-width 600
            org-confirm-babel-evaluate nil
            org-src-window-setup 'split-window-below
            org-src-fontify-natively t
            org-src-window-setup 'current-window ;; edit in current window
            org-src-preserve-indentation t       ;; do not put two spaces on the left
            org-src-tab-acts-natively t)
   (:file-match "\\.org\\'")
   (:hook add-pcomplete-to-capf turn-off-auto-fill my-org-mode-hook org-chinese-utils-enable)
   (:autoload my-org-mode-hook)
   (:when-loaded
     (require 'org-chinese-utils)

     (define-key org-mode-map (kbd "M-p")
       '(menu-item "" :filter
         (lambda (&rest _)
           (when (org-at-table-p)
             #'org-table-previous-field))))

     (define-key org-mode-map (kbd "M-n")
       '(menu-item "" :filter
         (lambda (&rest _)
           (when (org-at-table-p)
             #'org-table-next-field))))

     ;; (my-define-conditional-key org-mode-map (org-at-table-p)
     ;;     "M-p" #'org-table-previous-field
     ;;     "M-n" #'org-table-next-field)

     ;; variable for setting the default indentation:
     ;; (setq org-edit-src-content-indentation 0)
     ;; disable `evil-auto-indent' in org-mode:
     ;; (add-hook 'org-mode-hook (lambda () (setq evil-auto-indent nil)))

     (let ((pdf-reader (if (boundp 'do.minimal/pdf-reader) do.minimal/pdf-reader "xdg-open")))
       (setq org-file-apps `((auto-mode #'emacs)
                             ("\\.x?html?\\'" #'"firefox %s")
                             ("\\.pdf\\'" #',(concat pdf-reader " \"%s\""))
                             ("\\.pdf::\\([0-9]+\\)\\'" #',(concat pdf-reader " \"%s\" -p %1")))))

     (defun add-pcomplete-to-capf ()
       (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

     ;; display/update images in the buffer after I evaluate
     ;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
     (defun my-org-mode-hook ()
       "Stop the org-level headers from increasing in height relative to the other text."
       (progn
         (unless (org-at-heading-or-item-p)
           (org-next-visible-heading 1))
         ;; enable editing annexed files
         (when (and buffer-file-name buffer-read-only
                    (file-symlink-p buffer-file-name))
           (read-only-mode -1))))

     (eval-after-load 'company-mode
       (my-set-company-backends 'org-mode-hook 'company-ispell))

     (setq-default org-complete-tags-always-offer-all-agenda-tags t
                   org-startup-folded nil)
     ;; org-sort-entries on your * Tasks heading to sort tasks by todo order (type o to select that one).
     (setq org-agenda-files '("~/my_lib/annex/wiki/gtd.org"))
     ;; C-c C-w + refile target
     (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                    (file+headline "~/my_lib/annex/wiki/sinbox.org" "Tasks")
                                    "* TODO %i%?")
                                   ("T" "Tickler" entry
                                    (file+headline "~/my_lib/annex/wiki/tickler.org" "Tickler")
                                    "* %i%? \n %U")))
     (setq org-refile-targets '(("~/my_lib/annex/wiki/sgtd.org" :maxlevel #'3)
                                ("~/my_lib/annex/wiki/someday.org" :level #'1)
                                ("~/my_lib/annex/wiki/tickler.org" :maxlevel #'2)))
     ;; 时间戳内加如 +1d 、++1d/.+1d来使其可以每日循环，两者的差别是:+1d标记的事
     ;; 件如果有一天忘记做了，在agenda中依然会出现，而++1d/.+1d只会从你最后一次完
     ;; 成开始，之前没有做的都不再提示了。在 Agenda 视图中使用 t 来对选中的事项进
     ;; 行完成状态的改变。C-c C-s 在标题下面插入一个带有“SCHEDULED”关键字的时间戳。
     ;; C-c C-d “Deadline”时间戳。在给定的日期标题会列在议程中。另外，对于过期的
     ;; 日程安排会在编辑为 今天 并给出提醒，直到被标记为 DONE。也就是说，任务会自
     ;; 动推迟日期直到它被完成。有些任务需要一再重复出现。Org 模式在截止期限、计
     ;; 划安排和普通时间戳中用所谓的中继器来管理这种任务。

     ;; (org-archive-subtree-default), which moved the entry at point to an archive file.
     ;; setup archive location in archive directory in current folder
     (setq org-archive-location "archive/%s_archive::")

     (setq org-tag-alist '(("@work" #'119) ("@private" #'112)))
     (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
     ;; To select an agenda command to execute, press C-c a.
     (setq org-agenda-custom-commands
           '(("w" "work related" tags-todo "@work"
              ((org-agenda-overriding-header "work")
               (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))))

     (defun my-org-agenda-skip-all-siblings-but-first ()
       "Skip all but the first non-done entry."
       (let (should-skip-entry)
         (unless (org-current-is-todo)
           (setq should-skip-entry t))
         (save-excursion
           (while (and (not should-skip-entry) (org-goto-sibling t))
             (when (org-current-is-todo)
               (setq should-skip-entry t))))
         (when should-skip-entry
           (or (outline-next-heading)
               (goto-char (point-max))))))

     (defun org-current-is-todo ()
       (string= "TODO" (org-get-todo-state)))


                                        ; *** screenshot
     (defun my-img-maker ()
       "Make folder if not exist, define image name based on time/date"
       (let* ((org-mode-p (derived-mode-p 'org-mode))
              (img-folder-path
                (if org-mode-p (concat default-directory "img/")
                    "/tmp/img/"))
              (img-name (concat "img_" (format-time-string "%Y_%m_%d__%H_%M_%S") ".png"))
              (img-Abs-Path (concat img-folder-path img-name)) ;Relative to workspace.
              (relative-filename (concat "./img/" img-name)))
         ;; Make img folder if it doesn't exist.
         (if (not (file-exists-p img-folder-path)) ;[ ] refactor thir and screenshot code.
             (mkdir img-folder-path))
         (when org-mode-p
           (insert "[[" relative-filename "]]" "\n"))
         img-Abs-Path))

     (setq org-startup-with-inline-images nil)
     (setq org-display-inline-images nil)
     (setq org-redisplay-inline-images nil)

     ;; Overloading Commands Using the Prefix Argument command definition can examine the raw and
     ;; numeric prefix arguments, a typical way is to use ‘P’ for raw or ‘p’ for numeric in the
     ;; command’s ‘interactive’ spec.

     ;; https://github.com/dfeich/org-screenshot
     (defun my-org-screenshot (prefix-arg)
       "Take a screenshot into a time stamped unique-named file in the
      sub-directory (%filenameIMG) as the org-buffer and insert a link to this file.
      use C-g (three times) to cancel, press enter to capture"
       (interactive "P")
       (let* ((img-Abs-Path (my-img-maker))
              (shot-function `(lambda ()
                                (require 'notify)
                                (notify "org screenshot" "Please select the screen area now!")
                                (let ((default-directory ,(file-name-directory img-Abs-Path)))
                                  ;; scrot maim
                                  ;; (call-process "scrot" nil nil nil "-s" img-Abs-Path)))))
                                  ;; File is saved only when selection window closed by copying to clipboard.
                                  (shell-command ,(concat "flameshot gui --raw > " img-Abs-Path))))))
         (run-at-time 6 nil shot-function)))

     ;; * ditaa
     ;; -e  –encoding   指定编码
     ;; -E  –no-separation  嵌套矩形是否分隔,缺省有分隔,设置后无
     ;; -r  –round-corners  圆角矩形
     ;; -s  –scale  矩形大小 ,比如 0.8
     ;; -o  –overwrite  如果有同名文件覆盖
     ;; -S  –no-shadows
     ;; -A  –no-antialias
     ;; 圆角矩形: 用/ \ 作为图形的四个顶角
     ;; cxxx xxx别表示RGB hex number
     ;; 也可以采用预设的值: cRED cGRE cBLK cBLU cPNK cYEL
     ;; keep the lines straight, artist-mode ditaa-compatible diagrams
     ;; make anything dotted by putting either : (vertical) or = (horizontal) in it
     ;; add points to lines with *

     ;; ' o XXXXX' the 'o' is rendered as a bullet point. Note that there must
     ;; be a space before the 'o' as well as after it.

     ;; {c}   decision(Choice)
     ;; {d}   document
     ;; {io}   input/output, parallelogram
     ;; {mo}   manual operation
     ;; {o}   ellipse, circle
     ;; {s}   storage
     ;; {tr}   trapezoid (looks like an inverted {mo} )

     ;; +--------+   +-------+    +-------+
     ;; |        | --+ ditaa +--> |       |
     ;; |  Text  |   +-------+    |diagram|
     ;; |Document|   |!magic!|    |       |
     ;; |     {d}|   |       |    |       |
     ;; +---+----+   +-------+    +-------+
     ;;     :                         ^
     ;;     |       Lots of work      |
     ;;     +-------------------------+
     ;; use ruler-mode to align
     (org-babel-do-load-languages
      'org-babel-load-languages
      '((ditaa #'t)
        ;; (ipython #'t)
        (python #'t)))                  ; this line activates ditaa

     (defun my-org-babel-goto-block-corner (p)
       "Go to the beginning of the current block.
    If called with a prefix, go to the end of the block"
       (interactive "P")
       (let* ((element (org-element-at-point)))
         (when (or (eq (org-element-type element) 'example-block)
                   (eq (org-element-type element) 'src-block))
           (let ((begin (org-element-property :begin element))
                 (end (org-element-property :end element)))
             ;; Ensure point is not on a blank line after the block.
             (beginning-of-line)
             (skip-chars-forward " \r\t\n" end)
             (when (< (point) end)
               (goto-char (if p end begin))
               (when p
                 (skip-chars-backward " \r\t\n")
                 (beginning-of-line)))))))

     (setq org-ditaa-jar-path "~/.dotfiles/usr/scripts/ditaa.jar")))


;; (run-at-time nil 0.001 'my-gif-screencast-capture)
;; ;; code with actions to be captured
;; (cancel-function-timers 'my-gif-screencast-capture)

(setup gif-screencast
   (:disabled)
   (:bind "<f7>" #'my-gif-screencast-capture
          "<f8>" #'gif-screencast-toggle-pause
          "<f9>" #'gif-screencast-stop)
   (:autoload gif-screencast)
   (:option gif-screencast-program "scrot"
            gif-screencast-countdown 2
            gif-screencast-autoremove-screenshots nil)

   (defun my-gif-screencast-capture ()
     "non-interactive use of gif screencast"
     (let* ((time (current-time))
            (file (expand-file-name
                   (concat (format-time-string "%F-%T-%5N" time)
                           "."
                           gif-screencast-capture-format)
                   gif-screencast-screenshot-directory)))
       (unless (file-exists-p gif-screencast-screenshot-directory)
         (make-directory d 'parents))
       (apply 'start-process gif-screencast-program
              nil
              gif-screencast-program
              (append gif-screencast-args (list file))))))

; * artist
;; Artist mode says you can't change to another shape while drawing. Exit artist mode and then
;; reenter. Before drawing anything, click the mouse's middle button to display the pop-up menu and
;; select the desired shape from the Drawing menu.

;; artist mode type text before enclosing rectangles
;; 1 enable artist mode, 2 toggle rectangle, 3 evil insert mode enter to start, and enter to finish
;; 4 c-n, c-p move to another place repeat 3, 5 use line, and at corner insert +.

;; https://ivanceras.github.io/svgbob-editor/
; * svgbob
;; one column heigh box intersection : use -| instead of -+
;; double quote escapes parsing text that are in between the quotes
;; force CJK chars be treated as one text using escape quote ", to remove the extra spacing on words

(setup artist
   (:when-loaded
     (defun my-toggle-artist-rectangle ()
       (interactive)
       (if (advice-member-p 'my-replace-rect-corner 'artist-draw-rect)
	   (advice-remove 'artist-draw-rect #'my-replace-rect-corner)
	   (advice-add 'artist-draw-rect :after #'my-replace-rect-corner)))

     (defun my-replace-rect-corner (x1 y1 x2 y2)
       ;; round corner ?. ?' for svgbob
       (let ((nx1 (min x1 x2))
	     (nx2 (max x1 x2))
	     (ny1 (min y1 y2))
	     (ny2 (max y1 y2))
	     (c1 ?/)
	     (c2 ?\\))
	 (artist-move-to-xy nx1 ny1)
	 (artist-replace-char c1)
	 (artist-move-to-xy nx2 ny1)
	 (artist-replace-char c2)
	 (artist-move-to-xy nx1 ny2)
	 (artist-replace-char c2)
	 (artist-move-to-xy nx2 ny2)
	 (artist-replace-char c1)))))

; *** wiki
(setup plain-org-wiki
   (:autoload plain-org-wiki)
   (:global* "C-0" #'plain-org-wiki)
   (:option pow-directory "~/my_lib/annex/wiki"
	    ;; org-wiki-files (quote ("~/Nutstore/org/wiki")))
	    (append auto-mode-alist) '("\\.org.gpg\\'" #'org-mode)))

;; enable org-roam-mode to ensure that the cache is updated on file changes, renames and deletes
;; (setup org-roam
;;   (:option org-roam-directory "~/my_lib/annex/wiki"))

; ** org download
;org-download-yank to download links like url

(setup (:pkg org-download)
   (:autoload org-download-image)
   ;; (setq org-download-method 'attach)
   (:option org-download-method 'my-org-download-method
	    org-download-image-dir "./img")
   (:when-loaded
     (defun my-org-download-method (link)
       (let ((filename
	       (file-name-nondirectory
		(car (url-path-and-query
		      (url-generic-parse-url link))))))
	 (message "current-buffer: %s" (current-buffer))
	 (expand-file-name filename "~/")))))

;; (setq abbrev-file-name "~/.dotfiles/emacs/emacs.d/abbrev_defs.el")
;; (setq save-abbrevs t)                 ;; (ask) save abbrevs when files are saved
;; (setq-default abbrev-mode t)          ;; turn it on for all modes
;; add-global-abbrev
;; add-mode-abbrev

;; ; ** org reveal
;; ox-reveal
;;     (setq org-reveal-root "file://home/q/.dotfiles/emacs/org/reveal/js/reveal.js")
;;     (setq org-reveal-hlevel 2)

;;     (require 'ox)
;;     (require 'ox-reveal)
;;     (require 'ox-html)

;; htmlize


;; M-< go to the first heading
;; c-f-c act on the whole tree
(setup (:pkg worf)
   (:hook-into org-mode)
   (:bind "M-a" #'worf-backward
          "[" nil))

(setup valign
  (:autoload valign-mode valign-table))

(provide 'init-org)
