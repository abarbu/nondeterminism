;;; From "Continuations by example" ..." by Matt Might
;;; Adapted from the amb egg

(require-extension nondeterminism)

(possibly?
  (let ((a (a-member-of '(1 2 3 4 5 6 7)))
        (b (a-member-of '(1 2 3 4 5 6 7)))
        (c (a-member-of '(1 2 3 4 5 6 7))))
   ;; We're looking for dimensions of a legal right
   ;; triangle using the Pythagorean theorem:
   (unless (= (* c c) (+ (* a a) (* b b))) (fail))
   ;; And, we want the second side to be the shorter one:
   (unless (< b a) (fail))
   ;; Print out the answer:
   (print " a = " a ", b = " b ", c = " c)))
