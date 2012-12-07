;;; Adapted from the amb egg

(require-extension nondeterminism)

;; Baker, Cooper, Fletcher, Miller, and Smith live on different
;; floors of an apartment house that contains only five floors. Baker
;; does not live on the top floor. Cooper does not live on the bottom
;; floor. Fletcher does not live on either the top or the bottom
;; floor. Miller lives on a higher floor than does Cooper. Smith does not
;; live on a floor adjacent to Fletcher's. Fletcher does not live on a
;; floor adjacent to Cooper's.
;;
;; Where does everyone live?

(define (solve-dwelling-puzzle)
 (define (distinct? xs)
  (let loop ((xs xs))
   (or (null? xs)
      (and (not (member (car xs) (cdr xs) equal?))
         (loop (cdr xs))))))
 (one-value
  (let ((baker (a-member-of '(1 2 3 4 5)))
        (cooper (a-member-of '(1 2 3 4 5)))
        (fletcher (a-member-of '(1 2 3 4 5)))
        (miller (a-member-of '(1 2 3 4 5)))
        (smith (a-member-of '(1 2 3 4 5))))
   ;; They live on different floors.
   (unless (distinct? (list baker cooper fletcher miller smith)) (fail))
   ;; Baker does not live on the top floor.
   (unless (not (= baker 5)) (fail))
   ;; Cooper does not live on the bottom floor.
   (unless (not (= cooper 1)) (fail))
   ;; Fletcher does not live on either the top or the bottom floor.
   (unless (not (= fletcher 5)) (fail))
   (unless (not (= fletcher 1)) (fail))
   ;; Miller lives on a higher floor than does Cooper.
   (unless (> miller cooper) (fail))
   ;; Smith does not live on a floor adjacent to Fletcher's. 
   (unless (not (= (abs (- smith fletcher)) 1)) (fail))
   ;; Fletcher does not live on a floor adjacent to Cooper's.
   (unless (not (= (abs (- fletcher cooper)) 1)) (fail))
   `((baker ,baker) (cooper ,cooper) (fletcher ,fletcher) (miller ,miller) (smith ,smith)))))
