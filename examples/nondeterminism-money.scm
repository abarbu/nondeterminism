;;; Adapted from the amb egg

(require-extension srfi-1 nondeterminism)

;; Assign different numerals to different symbols
;;    '(s e n d m o r y)
;; so the following is true:
;;
;; s e n d + m o r e = m o n e y
;;
;; Symbols s and m are not zero.
;;
;; The code below significantly prunes the solution space,
;; so the result is found in few thousands trials, rather than
;; in tens of millions of trials when no pruning is applied:
;; (* 2 2 2 10 10 10 10 10 10 9 1) = 72,000,000
;;
;; The pruning is based on the following ideas:
;; 1. The order of assignments is important, so the variables are
;;    ordered in some specific way within the 'let* clause,
;;    rather than the 'let one.
;; 2. Later variables use the info from the previous ones to reduce their
;;    domains. This is a constraint propagation mechanism for this puzzle.
;; 3. Some assignments are deterministic, which significantly reduces the
;;    original domain space of a variable from 10 (or 9) to 1 (or to 2 if we
;;    take into account the effect of 'modulo application)

(define (solve-money-puzzle)
 (define (distinct? xs)
  (let loop ((xs xs))
   (or (null? xs)
      (and (not (member (car xs) (cdr xs) equal?))
         (loop (cdr xs))))))
 (one-value
  (let ((trial 0)
        (m 1))
   (let ((p1 (either 0 1))
         (p2 (either 0 1))
         (p3 (either 0 1))
         (d (a-member-of '(0 1 2 3 4 5 6 7 8 9))))
    (let* ((e (a-member-of (lset-difference equal? '(0 1 2 3 4 5 6 7 8 9) (list d))))
           (y (modulo (+ d e (* -10 p1)) 10))
           (n (a-member-of (lset-difference equal? '(0 1 2 3 4 5 6 7 8 9) (list d e y))))
           (r (modulo (+ (* 10 p2) e (- p1) (- n)) 10))
           (o (modulo (+ (* 10 p3) n (- p2) (- e)) 10))
           (s (modulo (+ (* 9 m) o (- p3)) 10)))
     (set! trial (add1 trial))
     (unless (distinct? (list s e n d m o r y)) (fail))
     (unless (= (+ d e)    (+ (* 10 p1) y)) (fail))
     (unless (= (+ p1 n r) (+ (* 10 p2) e)) (fail))
     (unless (= (+ p2 e o) (+ (* 10 p3) n)) (fail))
     (unless (= (+ p3 s m) (+ (* 10 m)  o)) (fail))
     ;; Result, including a number of recorded trials
     `((,trial trials)
       (s e n d + m o r e = m o n e y)
       (,s ,e ,n ,d + ,m ,o ,r ,e = ,m ,o ,n ,e ,y)))))))
