; -*- coding:utf-8 -*-

(setup (:pkg multiple-cursors)
   (:global "C-+" #'mc/mark-next-like-this
            "C-c C-<" #'mc/mark-all-like-this)
   (:autoload hydra-multiple-cursors/body)
   (:when-loaded
     (defhydra hydra-multiple-cursors (:hint nil)
       "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
 [_|_] Align with input CHAR       [Click] Cursor at point"
       ("l" mc/edit-lines :exit t)
       ("a" mc/mark-all-like-this :exit t)
       ("n" mc/mark-next-like-this)
       ("N" mc/skip-to-next-like-this)
       ("M-n" mc/unmark-next-like-this)
       ("p" mc/mark-previous-like-this)
       ("P" mc/skip-to-previous-like-this)
       ("M-p" mc/unmark-previous-like-this)
       ("|" mc/vertical-align)
       ("s" mc/mark-all-in-region-regexp :exit t)
       ("0" mc/insert-numbers :exit t)
       ("A" mc/insert-letters :exit t)
       ("<mouse-1>" mc/add-cursor-on-click)
       ;; Help with click recognition in this hydra
       ("<down-mouse-1>" ignore)
       ("<drag-mouse-1>" ignore)
       ("q" nil))))

(setup (:pkg smart-region)
   (smart-region-on))

(setup (:pkg phi-rectangle))

(provide 'init-multiple-cursors)
