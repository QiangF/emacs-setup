;; Clojure-style list comprehensions for Emacs Lisp

(setq lexical-binding t)

;; (for ((x '(1 2 3 4 5))
;;       (:let ((y (* x x))))
;;       (:while (< y 10)))
;;   y)
;; => (1 4 9)

(defmacro for (bindings body)
  (declare (indent defun))
  (let ((bindings (compile-whiles bindings)))
    (if (null bindings)
        `(list ,body)
      (case (caar bindings)
        (:when (let ((pred (cadar bindings)))
                 `(if ,pred
                      (for ,(cdr bindings)
                        ,body))))
        (:let (let ((b (cdar bindings)))
                `(let ,b
                   (for ,(cdr bindings) ,body))))
        (t (let ((var (caar bindings))
                 (vals (cadar bindings)))
             `(mapcan (lambda (,var)
                        (for ,(cdr bindings) ,body))
                      ,vals)))))))

;; Getting :while conditions to work is tricky. If you can think of a nicer way,
;; let me know!
(defun compile-whiles (bindings)
  (compile-whiles-helper (reverse bindings) nil nil))

;; Convert a list of bindings (in reverse order) into an equivalent list
;; of bindings (in reverse order) without any :whiles.
(defun compile-whiles-helper (rev-bindings acc-while acc-bindings)
  (cond
   ((null rev-bindings)
    (if (null ack-while)
        acc-bindings
      (cons `(:while ,acc-while) acc-bindings)))

   ((eq :while (caar rev-bindings))
    (if (null acc-while)
        (compile-whiles-helper (cdr rev-bindings)
                        (cadar rev-bindings)
                        acc-bindings)
      (compile-whiles-helper (cdr rev-bindings)
                      `(and ,(cadar rev-bindings) ,acc-while)
                      acc-bindings)))

   ((eq :let (caar rev-bindings))
    (if (null acc-while)
        (compile-whiles-helper (cdr rev-bindings)
                        nil
                        (cons (car rev-bindings) acc-bindings))
      (compile-whiles-helper (cdr rev-bindings)
                      `(let ,(cdar rev-bindings)
                         ,acc-while)
                      (cons (car rev-bindings) acc-bindings))))

   ((eq :when (caar rev-bindings))
    (compile-whiles-helper (cdr rev-bindings)
                           acc-while
                           (cons (car rev-bindings) acc-bindings)))

   (t (if (null acc-while)
          (compile-whiles-helper (cdr rev-bindings)
                          nil
                          (cons (car rev-bindings) acc-bindings))
        (let* ((var (caar rev-bindings))
               (vals (cadar rev-bindings))
               (new-binding `(,var (take-while (lambda (,var) ,acc-while) ,vals))))
          (compile-whiles-helper (cdr rev-bindings)
                          nil
                          (cons new-binding acc-bindings)))))))

(defun take-while (p xs)
  (if (null xs)
      nil
    (if (funcall p (car xs))
        (cons (car xs) (take-while p (cdr xs)))
      nil)))

;; Examples of :while compilation.

;; Basic :while compilation
(for ((x '(1 2 3))
      (:while (< x 2)))
  x)
;; (1)

;; Remove the :while by applying it to the xs
(for ((x (take-while (lambda (x)
                       (< x 2))
                     '(1 2 3))))
  x)
;; (1)

;; :let bindings may need to apply to subsequent :whiles
(for ((x '(1 2 3))
      (:let ((y (* x x))))
      (:while (< y 9)))
  y)
;; => (1 4)

;; Propagate the :let into the :while
(for ((x '(1 2 3))
      (:let ((y (* x x))))
      (:while (let ((y (* x x)))
                (< y 9))))
  y)
;; => (1 4)

;; Remove the :while by applying it to the xs
(for ((x (take-while (lambda (x)
                       (let ((y (* x x)))
                         (< y 9)))
                     '(1 2 3)))
      (:let (y (* x x))))
  y)
;; => (1 4)


;; While compilations always apply to the nearest previous binding (non-:let/:while)

(for ((x '(1 2 3))
      (:let ((y (* x x))))
      (y '(4 5 6))
      (:while (< y 9)))
  y)
;; => (4 5 6 4 5 6 4 5 6)

;; Remove :while by applying it to the ys
(for ((x '(1 2 3))
      (:let ((y (* x x))))
      (y (take-while (lambda (y)
                       (< y 9))
                     '(4 5 6))))
  y)
;; => (4 5 6 4 5 6 4 5 6)

;; More complicated example.

(for ((x '(1 2 3))
      (:let ((y (* x x))))
      (:let ((z (+ y 1))))
      (:while (< y 9)))
  z)
;; => (2 5)

;; Vacuum up the z :let binding
(for ((x '(1 2 3))
      (:let ((y (* x x))))
      (:let ((z (+ y 1))))
      (:while (let ((z (+ y 1)))
                (< y 9))))
  z)
;; => (2 5)

;; Vacuum up the y :let binding
(for ((x '(1 2 3))
      (:let ((y (* x x))))
      (:let ((z (+ y 1))))
      (:while (let ((y (* x x)))
                (let ((z (+ y 1)))
                  (< y 9)))))
  z)
;; => (2 5)

;; Remove the :while by applying it to the xs
(for ((x (take-while (lambda (x)
                       (let ((y (* x x)))
                         (let ((z (+ y 1)))
                           (< y 9))))
                     '(1 2 3)))
      (:let ((y (* x x))))
      (:let ((z (+ y 1)))))
  z)
;; => (2 5)
