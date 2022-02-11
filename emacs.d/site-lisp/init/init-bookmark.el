; -*- coding:utf-8 -*-

;; - set mark :: "C-SPC C-SPC"
;; - jump to previous mark :: "C-u C-SPC"  after it, just "C-SPC" to continue jumping
;; - jump to mark saved in global-mark ring :: "C-x C-SPC"
;; - exchange the cursor and the previous mark :: "C-x C-x"
;; counsel-mark-ring

;; Registers:
;; C-x r SPC runs point-to-register
;; C-x r j runs jump-to-register

;; Bookmarks:
;; C-x r m runs bookmark-set
;; C-x r b runs bookmark-jump

;; Mark ring:
;; C-SPC C-SPC pushes the current position to the mark ring (without leaving it active).
;; C-u C-SPC pops the mark ring, jumping to the previous position.
;; You can use this repeatedly to navigate through the entire ring.

;; Note that some commands (especially ones which are liable to move
;; you an an unknown or arbitrary distance from your original
;; location) will automatically push to the mark ring so that you
;; can use C-uC-SPC to return afterwards. This includes isearch, so
;; after using C-s to go somewhere, you can easily jump back again.

;; (advice-add 'ggtags-find-tag-dwim :before #'backward-forward-push-mark-wrapper))

(setup (:pkg backward-forward)
  (:option backward-forward-mark-ring-max 100
           backward-forward-mode t)
  (:global*
   ;; ("s-," #'my/backward-forward-previous-location)
   ;; ("s-." #'my/backward-forward-next-location)
   "C-o" #'my/backward-forward-next-location
   "H-i" #'my/backward-forward-previous-location)

  (defun my/backward-forward-previous-location ()
    "A `backward-forward-previous-location' wrap for skip invalid locations."
    (interactive)
    (let ((purge (< backward-forward-mark-ring-traversal-position (1- (length backward-forward-mark-ring))))
          (recent (point-marker)))
      (backward-forward-previous-location)
      (when (and (equal recent (point-marker)) purge)
        (my/backward-forward-previous-location))))
  (defun my/backward-forward-next-location ()
    "A `backward-forward-next-location' wrap for skip invalid locations."
    (interactive)
    (let ((purge (> backward-forward-mark-ring-traversal-position 0))
          (recent (point-marker)))
      (backward-forward-next-location)
      (when (and (equal recent (point-marker)) purge)
        (my/backward-forward-next-location)))))

(setq tramp-default-method "ssh")
(setq password-cache-expiry nil)
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
(require 'bookmark)
(setq bookmark-completion-ignore-case nil)
(bookmark-maybe-load-default-file)

(setup (:pkg headlong)
  (:autoload headlong-bookmark-jump))

(defun bmk/function (bookmark)
  "Handle a function bookmark BOOKMARK."
  (funcall (bookmark-prop-get bookmark 'function)))

(defun bmk/scratch ()
  "Bookmark for *scratch*."
  (interactive)
  (switch-to-buffer
   (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

;; (defun ora-remote-hosts ()
;;   (require 'tramp)
;;   (let ((default-directory "~"))
;;     (delq nil (mapcar
;;                (lambda (x) (and x (cdr x) (cadr x)))
;;                (tramp-parse-sconfig "~/.ssh/config")))))

;; (defun bmk/remote-shell ()
;;   (interactive)
;;   (ivy-read "ssh: " (cons "localhost" (ora-remote-hosts))
;;             :action (lambda (h)
;;                       (let ((default-directory
;;                              (if (string= h "localhost")
;;                                  default-directory
;;                                (concat "/ssh:" h ":/"))))
;;                         (ora-dired-open-term)))))

;; (defun bmk/remote-dired ()
;;   (interactive)
;;   (ivy-read "ssh: " (ora-remote-hosts)
;;             :action (lambda (h)
;;                       (dired (concat "/ssh:" h ":/")))))

(defun ora-add-bookmark-command-action (cmd)
  (let ((entry `(,(concat ": " cmd)
                  (filename . "   - no file -")
                  (position . 0)
                  (function . ,(intern cmd))
                  (handler . bmk/function))))
    (cl-pushnew entry bookmark-alist)))

(defun ora-add-bookmark-command ()
  "Add a command action."
  (interactive)
  (let ((ivy-inhibit-action #'ora-add-bookmark-command-action))
    (counsel-M-x)))

(setup (:pkg bookmark+-lit)
  (require 'bookmark+-lit)
  ;; bmkp-light-style-autonamed
  ;; (setq bmkp-auto-light-when-set 'autonamed-in-buffer)
  (setq bmkp-auto-light-when-set 'any-bookmark)
  (setq bmkp-auto-light-when-jump 'any-bookmark))

;; see also https://github.com/joodland/bm
;; bookmark+ can be a content organizer
(setup (:pkg bookmark+)
  (:delay 5)
  (:option bookmark-version-control t
           bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks"
           bmkp-auto-idle-bookmark-mode-delay 15
           bmkp-auto-idle-bookmark-min-distance 250
           bmkp-save-new-location-flag t
           ;; auto-save bookmarks
           bookmark-save-flag 1)
  (:autoload bmkp-turn-on-auto-idle-bookmark-mode hydra-bookmark/body)
  ;; evil-jump-forward evil-jump-backward
  ;; ("C-c C-/" #'counsel-bookmark)
  (:global* "C-c SPC" #'bmkp-set-autonamed-bookmark
            "<C-right>" #'bmkp-next-bookmark-this-file/buffer-repeat
            "<C-left>" #'bmkp-previous-bookmark-this-file/buffer-repeat)
  (:when-loaded
    (defhydra hydra-bookmark (:color pink)
      ("<escape>" nil "exit")
      ("C-a" bmkp-annotate-bookmark "add annotation")
      ("C-d" bmkp-set-desktop-bookmark "bookmark desktop")
      ("C-s" bookmark-bmenu-show-all-annotations "show annotation")
      ("C-t" bmkp-toggle-autonamed-bookmark-set/delete "set/delete automatic bookmark")
      ("C-l" bookmark-bmenu-list "list all")
      ("C-n" bmkp-next-bookmark-this-file/buffer-repeat "next in buffer")
      ("C-p" bmkp-previous-bookmark-this-file/buffer-repeat "previous in buffer")
      ("C-h" my-toggle-highlight "toggle highlight in buffer")
      ("C-f" bmkp-switch-bookmark-file-create "switch file"))

    (defvar my-bmkp-toggle nil
      "Keeps the state of how the bookmark was last toggled by TABing.")

    (defun my-toggle-highlight ()
      (interactive)
      (if my-bmkp-toggle
          (bmkp-unlight-bookmarks-this-buffer) (bmkp-light-bookmarks-this-buffer))
      (setq my-bmkp-toggle (not my-bmkp-toggle)))

    (bmkp-turn-on-auto-idle-bookmark-mode)))

; * fm-bookmarks
;; in fm-bookmarks buffer, g to refresh
(setup (:pkg fm-bookmarks)
   (:option fm-bookmarks-enabled-file-managers '(gnome3)
            fm-bookmarks-custom-bookmarks
            '(("Root" #'"/") ("Tmp" #'"/tmp/"))
            fm-bookmarks-hide-by-name-pattern '("Bluetooth" "Images")
            fm-bookmarks-hide-by-path-pattern '()
            fm-bookmarks-enable-mounted-media t
            fm-bookmarks-enable-cache t)
   (:with-map dired-mode-map
     (:bind "'" #'fm-bookmarks))
   (:bind "g" #'my-fm-bookmarks-refresh)

   ;; (("C-'" #'#'fm-bookmarks)
   (defvar fm-bookmarks-hide-duplicated t)
   (defvar fm-bookmarks-enable-mounted-media t)

   (defun my-fm-bookmarks-refresh ()
     (interactive)
     (call-process-shell-command "envsubst < ~/.dotfiles/home/bookmarks | tee $HOME/.config/gtk-3.0/bookmarks > /dev/null" nil 0)
     (fm-bookmarks-refresh)))

(require 'init-pinyin)

;; ace-jump-mode is Dead, Long Live Avy
;; 1. put eye focus on the point to jump to, 2. execute avy
(setup (:pkg avy)
  (:option avy-escape-chars '(?\e ?\C-g)
           avy-background t)
  ;; (defhydra hydra-avy (global-map "M-g" :color blue :hint nil)
  (:autoload hydra-avy/body)
  (:when-loaded
    (defhydra hydra-avy (:color blue :hint nil)
      "
 Line^^       Region^^        Goto
 [_y_] yank   [_Y_] yank      [_c_] timed char  [_C_] char
 [_m_] move   [_M_] move      [_w_] word        [_W_] any word
 [_k_] kill   [_K_] kill      [_g_] line        [_L_] end of line"
      ;; ("x" ace-jump-quick-exchange "quick exchange word and char" :color blue)
      ("y" avy-copy-line)
      ("m" avy-move-line)
      ("k" avy-kill-whole-line)
      ("Y" avy-copy-region)
      ("M" avy-move-region)
      ("K" avy-kill-region)
      ("c" avy-goto-char-timer)
      ("C" avy-goto-char)
      ("w" avy-goto-word-1)
      ("W" avy-goto-word-0)
      ("g" avy-goto-line)               ; in consistent with M-g g
      ("L" avy-goto-end-of-line))

    (defun avy-action-goto (pt)
      "Goto PT."
      (when pt
        (let ((frame (window-frame (selected-window))))
          (unless (equal frame (selected-frame))
            (select-frame-set-input-focus frame)
            (raise-frame frame))
          (goto-char pt))))))

;; (with-eval-after-load 'evil
;;   (define-key evil-normal-state-map (kbd "s") 'evil-avy-goto-line-below)
;;   (define-key evil-normal-state-map (kbd "S") 'avy-goto-line-above)
;;   (define-key evil-normal-state-map (kbd "t") 'avy-goto-char-in-line)
;;   (define-key evil-normal-state-map (kbd "T") 'ace-pinyin-jump-word)

;;   (define-key evil-visual-state-map (kbd "s") 'evil-avy-goto-line-below)
;;   (define-key evil-visual-state-map (kbd "S") 'avy-goto-line-above)
;;   (define-key evil-visual-state-map (kbd "t") 'avy-goto-char-in-line)
;;   (define-key evil-visual-state-map (kbd "T") 'ace-pinyin-jump-word)
;;   ;; dp deletes to wherever avy jumps to.
;;   (define-key evil-motion-state-map (kbd "p") #'avy-goto-word-1)
;;   (define-key evil-motion-state-map (kbd "P") #'avy-goto-line))


;; f t F T 跳到第一个，然后按 ;/, 前进／后退
;; /search_string<return> 之后 n/N 也很方便
;; https://github.com/vyp/evil-quick-scope

;; evil-quickscope
;;    (with-eval-after-load 'evil
;;      (require 'evil-quickscope)
;;      (require 'avy)
;;      (global-evil-quickscope-mode +1)
;;      (define-key evil-normal-state-map (kbd "s") 'avy-goto-line-below)
;;      (define-key evil-normal-state-map (kbd "S") 'avy-goto-line-above))

;; evil-avy
;;    (with-eval-after-load 'evil
;;      (evil-avy-mode +1))
;;    (evil-define-key 'normal evil-avy-mode-map
;;      "f" 'evil-avy-goto-char-in-line
;;      "F" 'evil-avy-find-char-backward
;;      "t" 'evil-avy-goto-char-in-line-to
;;      "T" 'evil-avy-find-char-to-backward)
;;    (evil-define-key 'operator evil-avy-mode-map
;;      "f" 'evil-avy-goto-char-in-line
;;      "F" 'evil-avy-find-char-backward
;;      "t" 'evil-avy-goto-char-in-line-to
;;      "T" 'evil-avy-find-char-to-backward)
;;    (evil-define-key 'visual evil-avy-mode-map
;;      "f" 'evil-avy-goto-char-in-line
;;      "F" 'evil-avy-find-char-backward
;;      "t" 'evil-avy-goto-char-in-line-to
;;      "T" 'evil-avy-find-char-to-backward)
;;    (evil-define-key 'motion evil-avy-mode-map
;;      "f" 'evil-avy-goto-char-in-line
;;      "F" 'evil-avy-find-char-backward
;;      "t" 'evil-avy-goto-char-in-line-to
;;      "T" 'evil-avy-find-char-to-backward)

;; evil-easymotion
;;    (with-eval-after-load 'evil
;;      (require 'evil-easymotion)
;;      (let ((prefix "s"))
;;        (define-key evil-normal-state-map (kbd prefix) evilem-map)
;;        (define-key evil-motion-state-map (kbd prefix) evilem-map)))


(setup (:pkg mwim)
  (:global "C-a" #'mwim-beginning
           "C-e" #'mwim-end))

(setup (:pkg goto-chg)
  (:global "C-x C-/" #'goto-last-change))

(provide 'init-bookmark)
