; -*- coding:utf-8 -*-

(xterm-mouse-mode 1)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)

(setq x-underline-at-descent-line t
      x-selection-timeout 300
      visible-bell t
      ;; visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
      visual-line-fringe-indicators '(nil right-curly-arrow)
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      scroll-margin 0
      scroll-error-top-bottom t
      scroll-in-place nil
      hscroll-step 1
      hscroll-margin 0
      line-move-visual t
      scroll-step 1
      auto-window-vscroll nil ;; no auto centering
      blink-cursor-mode t)

(setq-default cursor-type 'bar)

(when (display-graphic-p)
  (progn (toggle-frame-maximized)
         (set-frame-parameter nil 'undecorated t)
         (mouse-wheel-mode t)))
;; (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) nil 'fullboth))
(set-fringe-mode '(0 . 0))   ; Disable fringe because I use visual-line-mode

(setq indicate-buffer-boundaries 'right)

(set-face-inverse-video-p 'vertical-border nil)

(set-face-background 'vertical-border "#FC1B19")

(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?┃))

;;* frame
;; (use-package frame-cmds
;;     :ensure t)

;; also dimmer
;; https://github.com/larstvei/Focus/
;; (use-package dimmer
;;  :ensure
;;  :config
;;    (setq dimmer-fraction 0.3)
;;    (setq dimmer-adjustment-mode :foreground)
;;    (setq dimmer-use-colorspace :rgb)
;;    (dimmer-mode 1))

(setup (:require pulse)
  (:also-load +pulse)
  (:option pulse-flag nil
           pulse-delay 0.5
           pulse-iterations 1)
  (dolist (command '(pop-mark pop-globl-mark))
    (add-to-list '+pulse-location-commands command))
  (my-ensure-after-init #'+pulse-location-mode))

;; https://github.com/mina86/auto-dim-other-buffers.el
;; it uses face remapping in conjunction with
;; Emacs 27's :filtered face attribute.  This should avoid some of the visual
;; glitches and clashes with other packages that were hard to avoid while using
;; overlays.

(setup (:pkg auto-dim-other-buffers)
  ;; (:when-loaded (set-face-attribute 'auto-dim-other-buffers-face nil :background "#100000"))
  (auto-dim-other-buffers-mode t))

;; https://github.com/fishyfriend/hlwin
;; hiwin cause completion window in inferior-python-mode invisible
;;   (hiwin-activate)
;;   (set-face-background 'hiwin-face "#021")

;; (define-key input-decode-map (kbd "C-i") (kbd "H-i"))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; (display-line-numbers-mode 1)

; * fill guide
;; Set the number to the number of columns to use.
;; (when (fboundp 'adaptive-wrap-prefix-mode)
;;   (defun my-activate-adaptive-wrap-prefix-mode ()
;;     "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
;;     (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
;;   (add-hook 'visual-line-mode-hook 'my-activate-adaptive-wrap-prefix-mode))

;; (use-package adaptive-wrap
;;     :ensure t
;;     :preface
;;     ;; (global-visual-line-mode +1)
;;     :config
;;     ;; (setq-default adaptive-wrap-extra-indent 0)
;;     ;; (setq-default truncate-lines nil
;;     ;;       ;; Make wrapped lines start at half the available text width
;;     ;;       wrap-prefix (propertize (char-to-string ?\uE000)
;;     ;;                     'display '(space . (:width (0.5 . text)))))
;; )
;; fill-column-indicator

(setup display-fill-column-indicator
  (:hook prog-mode)
  (setq-default fill-column 100)
  (set-face-attribute 'fill-column-indicator nil :foreground "red")
  ;; (add-hook 'prog-mode-hook
  ;;   (lambda () (fci-mode 1)(setq fci-rule-column 101)))
  ;; (add-hook 'company-completion-started-hook 'company-turn-off-fci)
  ;; (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
  ;; (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
  ;; (defvar-local company-fci-mode-on-p nil)

  ;; (defun company-turn-off-fci (&rest ignore)
  ;; (when (boundp 'fci-mode)
  ;;     (setq company-fci-mode-on-p fci-mode)
  ;;     (when fci-mode (fci-mode -1))))

  ;; (defun company-maybe-turn-on-fci (&rest ignore)
  ;; (when company-fci-mode-on-p (fci-mode 1)))

  ;; (defun sanityinc/fci-enabled-p () (symbol-value 'fci-mode))

  ;; (defvar sanityinc/fci-mode-suppressed nil)
  ;; (make-variable-buffer-local 'sanityinc/fci-mode-suppressed)

  ;; (defadvice popup-create (before suppress-fci-mode activate)
  ;; "Suspend fci-mode while popups are visible"
  ;; (let ((fci-enabled (sanityinc/fci-enabled-p)))
  ;;     (when fci-enabled
  ;;     (setq sanityinc/fci-mode-suppressed fci-enabled)
  ;;     (turn-off-fci-mode))))

  ;; (defadvice popup-delete (after restore-fci-mode activate)
  ;; "Restore fci-mode when all popups have closed"
  ;; (when (and sanityinc/fci-mode-suppressed
  ;;     (null popup-instances))
  ;;     (setq sanityinc/fci-mode-suppressed nil)
  ;;     (turn-on-fci-mode)))
  )

;; Add Autofill mode to mode hooks.
(add-hook 'text-mode-hook #'(lambda ()
                              (turn-on-auto-fill)))

; * disable mouse
(setup (:pkg disable-mouse)
  (:only-if (display-graphic-p))
  (global-disable-mouse-mode))

; * GUI settings
;; Charset 设置
;; (use-package mule)

;; base16-theme
;; srcery-theme
;; following doesn't work in minibuffer and modeline
;; find face name with face-explorer-tooltip-global-mode in face-explorer.el
;; (global-set-key [s-mouse-1] 'facemenup-customize-face-at-mouse)

;; (add-to-list 'custom-theme-load-path "~/.dotfiles/emacs/emacs.d/site-lisp/tao-theme-emacs")

;; (use-package doom-themes
;;  :config
;;    ;; Global settings (defaults)
;;    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;          doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;    (load-theme 'doom-one t)
;;    ;; Corrects (and improves) org-mode's native fontification.
;;    (doom-themes-org-config))

;; (use-package all-the-icons
;;   :ensure t
;;   :if window-system
;;   :config
;;     ;; icons
;;     ;; (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
;;     ;; (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
;;     ;; (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
;;     ;; (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
;;     ;; (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append)
;;     ;; (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append)
;; )

;; (setq-default wrap-prefix (propertize "↳" 'face '(:foreground "orange")))

;; visual-fill-column wrap the line at fill column width
;; (add-hook 'prog-mode-hook #'enable-truncate-lines)
;; (add-hook 'text-mode-hook #'enable-truncate-lines)
(add-hook 'text-mode-hook #'visual-line-mode)
;; (add-hook 'minibuffer-setup-hook 'visual-line-mode)
(define-key visual-line-mode-map [remap kill-line] 'kill-visual-line)

;; ; * column marker
;; (use-package column-marker
;;   :if (display-graphic-p))

(setf custom-safe-themes t)
(setq-default cursor-in-non-selected-windows 'hollow)

;; underline tabs
;; (set-face-background 'region "gray90")  ;; Color for selected lines

;; remove face customisation in ~/.emacs.d/custom.el
;; don't use custom-set-faces, do customisation in theme
;; (custom-set-faces
 ;; '(my-trailing-space-face ((((class color)) (:background "black"))) t)
 ;; '(mini-modeline-mode-line ((t (:height 0.14 :box nil))))
 ;; '(diredp-flag-mark-line ((t (:foreground "red"))))
 ;; '(ivy-current-match ((t (:foreground "#FF0000" :background nil))))
 ;; '(swiper-line-face ((t (:foreground "magenta" :background "gray20"))))
 ;; '(my-tab-face ((((class color)) (:weight bold :underline t))) t))

;; (add-hook 'font-lock-mode-hook
;;             (function
;;             (lambda ()
;;             (setq font-lock-keywords
;;                     (append font-lock-keywords
;;                             '(("\t+" (0 'my-tab-face t))
;;                             ;; ("[ \t]+$" (0 'my-trailing-space-face t)))
;;                             ;; ("^.\\{81\\}\\(.+\\)$" (1 'my-long-line-face t)))
;;                             ))))))

;; (setq whitespace-global-modes '(not vterm-mode))
;; (set (make-local-variable 'whitespace-style) nil)
(setq whitespace-style '(face tabs spaces trailing space-before-tab
                         indentation empty space-after-tab space-mark tab-mark))
;;   '(my-long-line-face ((((class color)) (:background "gray90"))) t))
;; (set-face-attribute 'hl-line nil :background "#252000")
;; (set-face-background 'hl-line "#252000")
;; (set-face-attribute hl-line-face nil
;;                     :strike-through nil
;;                     ;; :background "#352000"
;;                     ;; :foreground nil
;;                     :overline nil)
;; find color name with list-colors-display
(defvar my-default-mode-line-bg "#000")
(defvar my-attention-mode-line-bg "#210")
;; (set-face-background 'mode-line my-default-mode-line-bg)
;; Setup font size based on emacs-font-size-pair
;; (setq base16-highlight-mode-line 'contrast)
(luna-if-dump
    (enable-theme 'modus-vivendi)
  (setup modus-themes
    (:option modus-themes-italic-constructs t
             modus-themes-headings '((1 . (background rainbow))
                                     (2 . (rainbow))
                                     (t . (semibold)))
             modus-themes-syntax '(faint alt-syntax green-strings yellow-comments)
             modus-themes-bold-constructs t
             modus-themes-region '(accented bg-only no-extend)
             modus-themes-org-blocks 'tinted-background
             modus-themes-intense-markup t
             modus-themes-hl-line '(intense accented)
             modus-themes-paren-match '(intense bold)
             modus-themes-subtle-line-numbers t)
    ;; Load the theme files before enabling a theme
    (modus-themes-load-themes)
    (set-face-attribute 'modus-themes-search-success nil :background "red")
    (modus-themes-load-vivendi)))

(defun my-set-theme ()
  (when (get-buffer "*scratch*")
    (kill-buffer "*scratch*"))
  ;; (load-theme 'srcery t)
  ;; (load-theme 'doom-one t)
  ;; (load-theme 'zenburn t)
  (when window-system
    (my-set-font emacs-english-font emacs-cjk-font emacs-font-size-pair t)))

(if exwm_enable
    (add-hook 'exwm-init-hook 'my-set-theme t)
    (add-hook 'after-init-hook 'my-set-theme t))

(provide 'init-gui)
