; -*- coding:utf-8 -*-

;; Support pinyin in Ivy
;; Input prefix ':' to match pinyin
;; Refer to  https://github.com/abo-abo/swiper/issues/919 and
;; https://github.com/pengpengxp/swiper/wiki/ivy-support-chinese-pinyin
(setup (:pkg pinyinlib)
   (:load-after ivy)
   (:when-loaded
     ;; C-o f to toggle case sensitive, @see https://github.com/abo-abo/swiper/issues/1104
     ;; this is used in swiper in-buffer searching
     (setq ivy-re-builders-alist '((t . re-builder-extended-pattern)))

     (defun re-builder-extended-pattern (str)
       (let ((english (ivy--regex-plus str))
             (chinese (let ((case-fold-search nil))
                        (pinyinlib-build-regexp-string str nil))))
         (if (and english (stringp english))
             (concat english "\\|" chinese)
             english)))

     (defun my-pinyinlib-build-regexp-string (str)
       "Build pinyin regexp from STR."
       (let* (rlt (i 0) ch)
         (while (< i (length str))
           (setq ch (elt str i))
           (setq rlt (concat rlt
                             (cond
                               ((and (<= ?a ch) (<= ch ?z))
                                (pinyinlib-build-regexp-char ch))
                               (t
                                (char-to-string ch)))))
           (setq i (1+ i)))
         rlt))))

;; https://github.com/cute-jumper/ace-pinyin
(setup (:pkg ace-pinyin)
   (:delay)
   (:when-loaded
     (setq ace-pinyin--jump-word-timeout 1.5)
     (ace-pinyin-global-mode 1)))

;; evil-find-char-pinyin
;;     (with-eval-after-load 'evil
;;       (evil-find-char-pinyin-mode +1))
;;     (setq evil-find-char-pinyin-only-simplified nil
;;           evil-find-char-pinyin-enable-punctuation-translation t)

(provide 'init-pinyin)
