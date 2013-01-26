(module nondeterminism * 
(import chicken scheme srfi-1)
(begin-for-syntax (require-extension srfi-1))
(declare (type (*fail?* boolean) (fail procedure)))

(define *fail?* #t)

(define (top-level-fail)
 (when *fail?* 
  (abort (make-composite-condition
          (make-property-condition 'exn
                                   'location 'fail
                                   'message "Top-level fail")
          (make-property-condition 'nondeterminism))))
 (set! *fail?* #t))

(define fail top-level-fail)
(define (set-fail! procedure) (set! fail procedure))

(define (unwind-trail) (set! *fail?* #f) (fail))

(define (unwedge-trail)
 (set! *fail?* #t)
 (set-fail! top-level-fail))

(define-syntax for-effects
 (syntax-rules ()
  ((_ body ...)
   (call/cc
    (lambda (return)
     (let ((old-fail fail))
      (set-fail! (lambda () (set-fail! old-fail) (return #f)))
      body ...
      (fail)))))))

(define-syntax all-values
 (syntax-rules ()
  ((_ body ...)
   (let ((values '()))
    (for-effects (set! values (cons (begin body ...) values)))
    (reverse values)))))

(define-syntax upon-failure
 (syntax-rules ()
  ((_ body ...)
   (let ((old-fail fail))
    (set-fail! (lambda () (set-fail! old-fail) body ... (fail)))))))

(define (local-set-car! x y)
 (let ((p (car x))) (upon-failure (set-car! x p)))
 (set-car! x y))

(define (local-set-cdr! x y)
 (let ((p (cdr x))) (upon-failure (set-cdr! x p)))
 (set-cdr! x y))

(define (local-string-set! s i x)
 (let ((p (string-ref s i))) (upon-failure (string-set! s i p)))
 (string-set! s i x))

(define (local-vector-set! v i x)
 (let ((p (vector-ref v i))) (upon-failure (vector-set! v i p)))
 (vector-set! v i x))

(define (a-boolean)
 (call-with-current-continuation
  (lambda (c)
   (let ((old-fail fail))
    (set-fail! (lambda () (set-fail! old-fail) (if *fail?* (c #f) (fail)))))
   #t)))

(define-syntax either
 (syntax-rules ()
  ((_) (fail))
  ((_ a) a)
  ((_ a b ...) (if (a-boolean) a (either b ...)))))

(define-syntax one-value
 (syntax-rules ()
  ((_ a) (one-value a (fail)))
  ((_ a b)
   (call/cc
    (lambda (return)
     (let ((old-fail fail))
      (set-fail! (lambda () (set-fail! old-fail) (return b)))
      (let ((v a))
       (set-fail! old-fail)
       v)))))))

(define-syntax local-one-value
 ;; TODO Unify these two rules
 (syntax-rules ()
  ((_ a) (local-one-value a (fail)))
  ((_ a b)
   (call/cc
    (lambda (return)
     (let ((v #f) (old-fail fail))
      (set-fail!
       (lambda () (set-fail! old-fail)
          (return (cond (*fail?* b) (else (set! *fail?* #t) v)))))
      (set! v a)
      (set! *fail?* #f)
      (fail)))))))

(define-syntax possibly?
 (syntax-rules ()
  ((_ body ...)
   (call/cc
    (lambda (return)
     (let ((old-fail fail))
      (set-fail! (lambda () (set-fail! old-fail) (return #f)))
      (let ((v (begin body ...)))
       (set-fail! old-fail)
       v)))))))

(define-syntax necessarily?
 (syntax-rules ()
  ((_ body ...)
   (call/cc
    (lambda (return)
     (let ((old-fail fail) (u #t))
      (set-fail! (lambda () (set-fail! old-fail) (return u)))
      (let ((v (begin body ...)))
       (when v (set! u v) (fail))
       (set-fail! old-fail)
       #f)))))))

(define-syntax local-set!
 (syntax-rules ()
  ((_ obj val)
   (begin
    (let ((p obj)) (upon-failure (set! obj p)))
    (set! obj val)))))

(define (an-integer-above i) (either i (an-integer-above (+ i 1))))

(define (an-integer-below i) (either i (an-integer-below (- i 1))))

(define (an-integer)
 (either 0 (let ((i (an-integer-above 1))) (either i (- i)))))

(define (an-integer-between i j)
 (when (> i j) (fail))
 (either i (an-integer-between (+ i 1) j)))

(define (a-member-of s)
 (if (vector? s)
     (vector-ref s (an-integer-between 0 (- (vector-length s) 1)))
     (let loop ((l s))
      (when (null? l) (fail))
      (either (first l) (loop (cdr l))))))

(define (a-subset-of l)
 (if (null? l)
     '()
     (let ((y (a-subset-of (cdr l)))) (either (cons (first l) y) y))))

(define (a-split-of l)
 (let loop ((x '()) (y l))
  (if (null? y)
      (list x y)
      (either (list x y) (loop (append x (list (first y))) (cdr y))))))

(define (a-permutation-of l)
 (if (null? l)
     l
     (let ((split (a-split-of (a-permutation-of (cdr l)))))
      (append (first split) (cons (first l) (second split))))))

(define (a-partition-of x)
 (if (null? x)
     x
     (let ((y (a-partition-of (cdr x))))
      (either (cons (list (first x)) y)
              (let ((z (a-member-of y)))
               (cons (cons (first x) z) (remove (lambda (a) (eq? z a)) y)))))))

(define (a-partition-of-size k x)
 (when (< (length x) k) (fail))
 (let loop ((x x))
  (if (= (length x) k)
      (map list x)
      (let* ((y (loop (cdr x)))
             (z (a-member-of y)))
       (cons (cons (first x) z) (remove (lambda (a) (eq? z a)) y))))))

(define (nondeterministic-map f l)
 ;; workaround for map breaking in non-deterministic code
 (let loop ((c '()) (l l))
  (if (null? l) (reverse c) (loop (cons (f (first l)) c) (cdr l)))))
)
