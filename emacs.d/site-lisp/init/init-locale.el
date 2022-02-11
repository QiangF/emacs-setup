; -*- coding:utf-8 -*-

; * coding
(set-locale-environment "zh_CN.UTF-8")
(setq enable-local-variables :safe)
;; (set-language-environment "UTF-8")
;; (set-language-environment "Chinese-GBK")

(set-default-coding-systems 'utf-8-unix)
;; set coding config, last is highest priority.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html#Recognize-Coding
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8-unix)

(set-buffer-file-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

(if (eq system-type 'windows-nt)
    (progn
      (set-selection-coding-system 'gbk-dos)
      (set-next-selection-coding-system 'gbk-dos)
      (set-clipboard-coding-system 'gbk-dos))
    (set-selection-coding-system 'utf-8-unix)
    (set-next-selection-coding-system 'utf-8-unix)
    (set-clipboard-coding-system 'utf-8-unix))

;; to prevent possible misinterpretations
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

; * font
;; install the font, restart Emacs, and look at the output
;; of (font-utils-list-names) to find the spelling of the font name
;; Font-lock stops when if finds an error: to single step your rules in font-lock-studio
;; t in keywords: override

; * unicode font
;; unicode-fonts-block-font-mapping
;; block-to-font mappings. Each block shows the order of its preferred font.
;; "CJK Unified Ideographs" you can add a new font in the first position. 
;; (use-package unicode-fonts
;;     :ensure t
;;     :config
;;         (unicode-fonts-setup)
;; )
; * chinese width
; east asian ambiguous character table
;; 汉字的unicode范围是：0x4E00~0x9FA5 (#x4E00 . #x9FA5)

;; If some characters appear on the screen as empty boxes or hex codes, this means that the fontset
;; in use for them has no font for those characters.

(defun unicode-char-range ()
  '((#x00 . #xFFFF)))

(setup (:pkg unicad)
   (:autoload unicad-enable)
   (unicad-enable))
;; (setq unicad-timer (run-with-idle-timer 10 nil #'unicad-enable "unicad"))

(defvar default-char-width-table)

(setq default-char-width-table char-width-table)

;; fixme: fonts only available if window-system is true
(defvar emacs-english-font "DejaVu Sans mono"
    "The font name of English.")

(defvar emacs-cjk-font "SimSun"
    "The font name for CJK.")

(defvar emacs-font-size-pair '(20 . 24)
    "Default font size pair for (english . chinese)")

(defvar emacs-font-size-pair-list
    '(( 5 .  6) (10 . 12)
    (13 . 16) (15 . 18) (17 . 20)
    (19 . 22) (20 . 24) (21 . 26)
    (24 . 28) (26 . 32) (28 . 34)
    (30 . 36) (34 . 40) (36 . 44))
    "This list is used to store matching (englis . chinese) font-size.")

(defun font-exist-p (fontname)
    "Test if this font is exist or not."
    (if (or (not fontname) (string= fontname ""))
        nil
    (if (not (x-list-fonts fontname)) nil t)))

(defun my-set-font (english chinese size-pair &optional all-frames)
  "Setup emacs English and Chinese font on x window-system."

  (if (font-exist-p english)
      ;; (set-frame-font
      ;;  (format "%s-%d" english (car size-pair)) nil all-frames)
    (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) nil all-frames)
    ;; unicode font
    ;; (let ((font-sets '("fontset-default" "fontset-standard" "fontset-startup")))
    ;;   (mapcar
    ;;    (lambda (font-set)
    ;;      ;; all the characters in that range (which is the full possible range)
    ;;      (set-fontset-font font-set '(#x000000 . #x3FFFFF) english)
    ;;      ;; for all characters without font specification
    ;;      ;; in another words it is a setting for lack of fallback font
    ;;      ;; if e.g. ℕ called DOUBLE-STRUCK CAPITAL N is not covered by our font
    ;;      ;; it will be displayed as placeholder-box,
    ;;      ;; because fallback for our font is now... our font :)
    ;;      (set-fontset-font font-set nil (font-spec :family english :size (cdr size-pair)))) font-sets))
    )

  (if (font-exist-p chinese)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font) charset
                          (font-spec :family chinese :size (cdr size-pair)) nil 'prepend)))
)

  ;; required to make line wrap in dired-rename minibuffer input work
  ;; (when (display-graphic-p)
  ;;   (set-frame-font
  ;;    ;; set font for all windows. don't keep window size fixed
  ;;    (format "%s" emacs-english-font nil nil))
  ;;   (dolist (charset '(kana han symbol cjk-misc bopomofo))
  ;;     (set-fontset-font (frame-parameter nil 'font) charset
  ;;                       (font-spec :family emacs-cjk-font :size (car emacs-font-size-pair)))))
  ;; (when exwm_enable
  ;;   (set-frame-width exwm-workspace--minibuffer
  ;;                    (nth 2 (mf-workarea exwm-workspace--minibuffer)) nil t))

(defun emacs-step-font-size (step)
    "Increase/Decrease emacs's font size."
    (let ((scale-steps emacs-font-size-pair-list))
      (if (< step 0) (setq scale-steps (reverse scale-steps)))
      (setq emacs-font-size-pair
            (or (cadr (member emacs-font-size-pair scale-steps))
                emacs-font-size-pair))
      (when emacs-font-size-pair
        (message "emacs font size set to %.1f" (car emacs-font-size-pair))
        (my-set-font emacs-english-font emacs-cjk-font emacs-font-size-pair t))
      (message nil)
      (when (and exwm_enable (exwm-workspace--minibuffer-own-frame-p)) (exwm-workspace-detach-minibuffer)
            (exwm-workspace-attach-minibuffer))))

(defun increase-emacs-font-size ()
    "Decrease emacs's font-size acording emacs-font-size-pair-list."
    (interactive) (emacs-step-font-size 1))

(defun decrease-emacs-font-size ()
    "Increase emacs's font-size acording emacs-font-size-pair-list."
    (interactive) (emacs-step-font-size -1))

;; make Emacs respect kinsoku rules when wrapping lines visually.
(setq word-wrap-by-category t)

; for over 23 (checked work in emacs 24)
;; default unicode char width is 2 anscii char width
(defun ditaa-set-char-width ()
  (interactive)
  (while (char-table-parent char-width-table)
         (setq char-width-table (char-table-parent char-width-table)))
  (let ((table (make-char-table nil))
        (width 1))
    (dolist (range (unicode-char-range))
      (set-char-table-range table range width))
    (optimize-char-table table)
    (set-char-table-parent table char-width-table)
    (setq char-width-table table)))

(defun ditaa-unset-char-width ()
  (interactive)
  (setq char-width-table default-char-width-table))

(defvar ditaa-font-active t)
(defun ditaa-toggle-font ()
    "Font size pair when draws with artist mode (english . chinese)"
    (interactive)
    (if ditaa-font-active
        (progn (message "ditaa font active")
                ;; (ditaa-set-char-width)
                (my-set-font emacs-english-font emacs-cjk-font '(12 . 7) nil))
    (progn ;; (ditaa-unset-char-width)
            (my-set-font emacs-english-font emacs-cjk-font emacs-font-size-pair nil)))
    (setq ditaa-font-active (not ditaa-font-active)))

(global-set-key (kbd "C-=") 'increase-emacs-font-size)
(global-set-key (kbd "C--") 'decrease-emacs-font-size)
;; (set-face-attribute 'default nil :font "DejaVu Sans mono-10:bold")

(provide 'init-locale)
