; -*- coding:utf-8 -*-

; * company

;; You have a list of backends (completion engines) company should try to provide completion at
;; point. Company tries each of this backend one after the another and the first one that returns
;; any candidates is used for completion and the rest of the backends are ignored. So by default
;; only one backend is used at a time.

;; Company provides a way to merge completions from multiple source using what it calls grouped
;; backends. Usually the members of company-backends are individual backends, but it can also be
;; a list of backends in which case the completion from the backends are merged provided they
;; return same prefix or the text to be completed. So if you want to merge completions from
;; company-elisp (on recent emacsen company-capf is used) and company-dabbrev simply do this

;; (add-to-list 'company-backends '(company-capf company-dabbrev))

;; (setq company-backends (cons '(company-capf company-dabbrev)
;;                              (remove 'company-capf company-backends)))

;; Additionally you can merge different backends using the :with keyword

;; (add-to-list 'company-backends '(company-capf :with company-dabbrev))

;; This is different from the example a list of backends without :with since company will use
;; only the backends before :with for determining the prefix (the text to be completed). This
;; implies that the candidates from backends after :with will be ignored by company,irrespective
;; of whether the backends return a prefix or no, if the none of the backends before :with return
;; a prefix.

;; You might want to set this locally in emacs-lisp buffer (I prefer doing so)

;; :with, for example: (company-lsp :with company-dabbrev-code), and set company-transformers to
;; be company-sort-by-backend-importance, like so: (setq company-transformers
;; '(company-sort-by-backend-importance)) then company-lsp will always appear before
;; company-dabbrev-code.

;; '(company-elisp company-dabbrev company-keywords company-abbrev company-yasnippet
;; company-dabbrev-code company-files company-capf))

;; adding `comapny-dabbrev` to company-backends hasn't been worked, because of company-capf.
;; https://github.com/company-mode/company-mode/issues/342

;; (let ((basic-backends '(company-capf
;;                          :with company-dabbrev)))
;;   (setq company-backends basic-backends)
;;   (defun local-push-company-backend (extra-backends)
;;     "Add BACKEND to a buffer-local version of `company-backends'."
;;     ;; (make-local-variable 'company-backends)
;;     ;; (push `(,extra-backends ,@basic-backends) company-backends)
;;     (setq company-backends `(,extra-backends ,@basic-backends))))

;; lsp completion:
;; Make sure company-capf is at the front of company-backends.
;; (setq company-backends
;;       (cons 'company-capf
;;             (remove 'company-capf company-backends)))

(setup (:pkg company)
   (:delay 5)
   (:option company-tooltip-align-annotations t
            company-tooltip-flip-when-above t
            ;; Easy navigation to candidates with M-<n>
            company-show-numbers t
            company-dabbrev-other-buffers nil
            company-begin-commands '(self-insert-command)
            company-complete-number t
            company-require-match nil ;; this can be overiden by backends
            company-minimum-prefix-length 3
            company-selection-wrap-around t
            company-echo-delay 0
            company-idle-delay nil      ;nil no 0 immeadiate
            company-auto-complete nil
            ;; the two must use together:
            company-dabbrev-downcase nil
            company-dabbrev-ignore-case nil)
   (:with-map company-active-map
     (:bind "C-n" #'company-select-next
            "C-p" #'company-select-previous
            "<escape>" #'company-abort
            "C-]" #'company-abort))
   (:with-map company-search-map
     (:bind "C-n" #'company-select-next
            "C-p" #'company-select-previous))
   (:autoload my-set-company-backends)
   (:when-loaded
     (defun my-set-company-backends (mode-hook extra-backends)
       (add-hook mode-hook `(lambda ()
                              (local-push-company-backend ',extra-backends))))

     (let ((map company-active-map))
       (mapc (lambda (x) (define-key map (format "%d" x) 'ora-company-number))
             (number-sequence 0 9))

       ;; use numbers 0-9 to select company completion candidates
       ;; (let ((map company-active-map))
       ;; (mapc (lambda (x) (define-key map (format "%d" x)
       ;;                 `(lambda () (interactive) (company-complete-number ,x))))
       ;;         (number-sequence 0 9)))

       ;; un-binds RET and binds SPC to close the company popup.
       (define-key map " " (lambda ()
                             (interactive)
                             (company-abort)
                             (self-insert-command 1)))
       (define-key map (kbd "<return>") nil))

     (defun local-push-company-backend (extra-backends)
       "Add BACKEND to a buffer-local version of `company-backends'."
       (make-local-variable 'company-backends)
       (push `,extra-backends company-backends))

     (defun ora-company-number ()
       "Forward to `company-complete-number'.
    Unless the number is potentially part of the candidate.
    In that case, insert the number."
       (interactive)
       (let* ((k (this-command-keys))
              (re (concat "^" company-prefix k)))
         (if (or (cl-find-if (lambda (s) (string-match re s))
                             company-candidates)
                 (> (string-to-number k)
                    (length company-candidates)))
             (self-insert-command 1)
             (company-complete-number
              (if (equal k "0")
                  10
                  (string-to-number k))))))

     ;; When there are too many candidates for `company-mode`, press "C-M-i" to
     ;; call `complete-symbol`, which uses ivy in turn for fuzzy matching.

     ;; I want to use `company-dabbrev-code' with `company-capf'.  This
     ;; means that, for example, I can get completion on functions I'm
     ;; currently writing, before I've evaluated them, in Emacs Lisp
     ;; buffers.
     ;;
     ;; However, you will get duplicates from `company-dabbrev-code'
     ;; if/when `company-capf' returns completions with annotations like
     ;; "<f>" on functions in Emacs Lisp buffers.  Company does not
     ;; consider two candidates with identical strings but different
     ;; annotations to be duplicates. However, I can't imagine
     ;; `company-dabbrev-code' ever adds annotations, so if you've got a
     ;; completion from it that is a duplicate of one from a smarter
     ;; backend, we always drop the duplicate `company-dabbrev-code'
     ;; completion.  To accomplish this requires us to add a new function
     ;; to `company-transformers'.
     ;;
     ;; See also:
     ;; https://github.com/company-mode/company-mode/issues/432
     ;; https://github.com/company-mode/company-mode/pull/509
     ;; https://github.com/company-mode/company-mode/issues/528
     ;; Company commit 7779820493, and its revert in 395f846b05f

     (defun my:company-remove-duplicates-ignoring-annotations (candidates)
       "Reorder company candidates, removing any duplicates.
cand-1 is a duplicate of cand-2 if (string= cand-1 cand-2).  Note
that this ignores text properties, such as the company-backend
text property as well as any annotation-related properties.  This
is desirable to, for example, remove duplicate candidates when
using `company-dabbrev-code' grouped with other, (presumably) more
intelligent backends.
In fact, this function will also replace a candidate from
`company-dabbrev-code' with any other `string=' candidate.
Order of candidates is preserved (which is usually
important/desirable, particularly when using something like
company-prescient).  If a `company-dabbrev-code' candidate has a
duplicate later in the of candidates, the `company-dabbrev-code'
candidate will be replaced by the candidate that appears later in
the list."
       (let* ((default-backend (if (listp company-backend)
                                   (car company-backend)
                                   company-backend))
              (best-cands (make-hash-table :test #'equal))
              has-duplicates)
         ;; First pass: Put the best candidate in hash table best-cands.
         ;; Candidates from `company-dabbrev-code' backend are worse than
         ;; all other candidates.  Aside from that rule, first candidate ==
         ;; best candidate.
         (dolist (cand candidates)
           (pcase-let* ((cand-backend (or (get-text-property 0 'company-backend cand)
                                          default-backend))
                        (cand-prio (cond
                                     ((eq cand-backend 'company-dabbrev-code) -10)
                                     (t 0)))
                        (`(,best-cand #',best-prio) (gethash cand best-cands)))
             (when best-cand
               (setq has-duplicates t)
               (when (> cand-prio best-prio)
                 (puthash cand (cons cand cand-prio) best-cands)))))
         (if has-duplicates
             ;; Second pass: Remove duplicates.  Replace the first instance
             ;; of a given candidate with its best candidate, e.g. replace
             ;; a `company-dabbrev-code' candidate with a duplicate
             ;; candidate from any other backend.
             (cl-loop
              for cand in candidates
              for (best-cand #'best-prio) = (gethash cand best-cands)
              if best-cand
              collect best-cand
              and do (remhash cand best-cands))
             ;; There were no duplicates (maybe a common case), we can just
             ;; return the original list.
             candidates)))

     (setq company-transformers '(company-sort-by-occurrence))

     ;; Add our transformer.
     (add-to-list 'company-transformers
                  #'my:company-remove-duplicates-ignoring-annotations)

     (global-company-mode)))

;; company-flx
;; only works with the company-capf backend

(provide 'init-company)
