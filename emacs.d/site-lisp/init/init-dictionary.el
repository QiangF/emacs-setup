; -*- coding:utf-8 -*-

; * youdao dictionary
;; p  youdao-dictionary-play-voice-of-current-word
;; q  quit-window
;; y  youdao-dictionary-play-voice-at-point
(setup youdao-dictionary
  (:global* "C-c y" #'youdao-dictionary-search-at-point))

; * flyspell
;;On-the-fly spell checker
(setup flyspell
  (:option flyspell-auto-correct-binding [nil]
           ispell-dictionary "english"))

; * bing dict google translate
(setup bing-dict
  (:global "C-c b" #'bing-dict-brief)
  (:option bing-dict-show-thesaurus 'both
           bing-dict-add-to-kill-ring t
           bing-dict-show-thesaurus 'both
           bing-dict-pronunciation-style 'uk))

;; google-translate-listen-program default mplayer
(setup go-translate
  (:global* "C-c g" #'google-translate-smooth-translate)
  ;; use c-n and c-p to switch
  (:option go-translate-extra-directions '(("ru" . "en")
                                           ("zh-CN" . "en")
                                           ("en" . "zh-CN")
                                           ("fr" . "en")
                                           ("de" . "en")
                                           ("ja" . "en"))))

(setup web-search
  (:option web-search-default-provider "Bing"))

(provide 'init-dictionary)
