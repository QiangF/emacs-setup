; -*- coding:utf-8 -*-

;; resolve conflicts under version control
;; M-x smerge-ediff 

; * git
;; p Visit previous historic version
;; n Visit next historic version
;; w Copy the abbreviated hash of the current historic version
;; W Copy the full hash of the current historic version
;; q Exit the time machine.
;; C-x # save config and exit config buffer
(setq vc-follow-symlinks t)

(setup git-timemachine
  (:autoload git-timemachine)
  ;; (with-eval-after-load 'evil
  ;;   ;; force update evil keymaps after git-timemachine-mode loaded
  ;;   (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)
  ;;   (evil-make-overriding-map git-timemachine-mode-map 'normal))
  )

;; font-lock-studio

; * magit 
;; S stages all files
;; c c initiates a commit, then press C-c C-c to actually create the commit.
;; P u pushes to the upstream branch.

(setup magit
   (:global "C-x g" #'magit-status
            "C-x G" #'magit-status-with-prefix)
   (:bind "U" #'magit-unstage-all)
   (:with-map magit-file-section-map (:bind "<C-return>" nil))
   (:with-map magit-hunk-section-map (:bind "<C-return>" nil))
   (:hook hl-line-mode)
   (:autoload hydra-magit/body)
   (:when-loaded
     (defhydra hydra-magit (:color blue)
       ("<escape>" nil "quit")
       ("h" magit-view-file-history "file history")
       ("c" magit-status "status")
       ("C" magit-checkout "checkout")
       ("v" magit-branch-manager "branch manager")
       ("m" magit-merge "merge")
       ("l" magit-log "log")
       ;; press e at the unmerged file in magit status buffer (magit-status)
       ("r" vc-resolve-conflicts "resolve conflicts in file")
       ("!" magit-git-command "command")
       ("$" magit-process "process"))
     ;; History can be viewed with:
     ;; git log refs/snapshots/$(git symbolic-ref HEAD)
     (defun magit-monitor (&optional no-display)
       "Start git-monitor in the current directory."
       (interactive)
       (let* ((path (file-truename
                     (directory-file-name
                      (expand-file-name default-directory))))
              (name (format "*git-monitor: %s*"
                            (file-name-nondirectory path))))
         (unless (and (get-buffer name)
                      (with-current-buffer (get-buffer name)
                        (string= path (directory-file-name default-directory))))
           (with-current-buffer (get-buffer-create name)
             (cd path)
             (ignore-errors
               (start-process "*git-monitor*" (current-buffer)
                              "git-monitor" "-d" path))))))

     (defun magit-status-with-prefix ()
       (interactive)
       (let ((current-prefix-arg '(4)))
         (call-interactively 'magit-status)))

     (add-hook 'magit-status-mode-hook #'(lambda () (magit-monitor t)))

     (eval-after-load 'magit-remote
       '(progn
         (magit-define-popup-action 'magit-fetch-popup
          ?f 'magit-get-remote #'magit-fetch-from-upstream ?u t)
         (magit-define-popup-action 'magit-pull-popup
          ?F 'magit-get-upstream-branch #'magit-pull-from-upstream ?u t)
         (magit-define-popup-action 'magit-push-popup
          ?P 'magit--push-current-to-upstream-desc
          #'magit-push-current-to-upstream ?u t)))))

;; (setup magit-commit
;; :after magit
;; )
;; (setup git-commit)

;; (setup magit-files
;; :config
;; (global-magit-file-mode))

;; (setup magit-popup
;;   :ensure t
;;   :defer t)

; * diff-hl
;; (add-hook 'diff-hl-mode-hook
;;           (lambda () (add-hook 'find-file-hook 'my-maybe-recover-this-file)))

(setup (:pkg diff-hl)
  (:delay 5)
  (:hook-into prog-mode text-mode)
  ;; (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (:when-loaded
    (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
    (unless (display-graphic-p) (diff-hl-margin-mode))
    ;; Highlight changes to the current file in the fringe
    (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)))

(provide 'init-git)
