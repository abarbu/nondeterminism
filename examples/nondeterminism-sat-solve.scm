;;; From "Continuations by example" ..." by Matt Might
;;; Adapted from the amb egg

(require-extension nondeterminism)

(define (implies a b) (or (not a) b))

;; The is not the most efficient implementation,
;; because a continuation is captured for each
;; occurrence of the same variable, instead of
;; one for each variable.

(define-syntax sat-solve
  (syntax-rules (and or implies not)
    ((_ ?formula ?body)
     (sat-solve ?formula ?body ?formula))
    ((_ (not ?phi) ?body ?assertion)
     (sat-solve ?phi ?body ?assertion))
    ((_ (and ?phi) ?body ?assertion)
     (sat-solve ?phi ?body ?assertion))
    ((_ (and ?phi1 ?phi2 ...) ?body ?assertion)
     (sat-solve ?phi1 (sat-solve (and ?phi2 ...) ?body ?assertion)))
    ((_ (or ?phi) ?body ?assertion)
     (sat-solve ?phi ?body ?assertion))
    ((_ (or ?phi1 ?phi2 ...) ?body ?assertion)
     (sat-solve ?phi1 (sat-solve (or ?phi2 ...) ?body ?assertion)))
    ((_ (implies ?phi1 ?phi2) ?body ?assertion)
     (sat-solve ?phi1 (sat-solve ?phi2 ?body ?assertion)))
    ((_ #t ?body ?assertion)
     ?body)
    ((_ #f ?body ?assertion)
     (fail))
    ((_ v ?body ?assertion)
     (let ((v (either #t #f)))
       (unless ?assertion (fail))
       ?body))))

; The following prints (#f #f #t)
(possibly?
 (sat-solve (and (implies a (not b)) (not a) c)
            (print "a = " a ", b = " b ", c = " c)))
