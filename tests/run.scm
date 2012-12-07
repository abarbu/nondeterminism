;;;; amb test

(include "../examples/nondeterminism-kalotan")
(assert (equal? '(female male female) (solve-kalotan-puzzle)))

(include "../examples/nondeterminism-money")
(assert (equal? '(9 5 6 7 + 1 0 8 5 = 1 0 6 5 2) (caddr (solve-money-puzzle))))

(include "../examples/nondeterminism-dwelling")
(assert (equal? '((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1)) (solve-dwelling-puzzle)))
