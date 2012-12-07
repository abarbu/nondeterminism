;;; Adapted from the amb egg

(require-extension nondeterminism)

;; The following code is a rewrite of an example from the book "Teach Yourself
;; Scheme in Fixnum Days" by Dorai Sitaram. The book gives the following problem
;; setting:
;;
;; The Kalotans are a tribe with a peculiar quirk. Their males always tell the
;; truth. Their females never make two consecutive true statements, or two
;; consecutive untrue statements.
;;
;; An anthropologist (let's call him Worf) has begun to study them. Worf does not
;; yet know the Kalotan language. One day, he meets a Kalotan (heterosexual)
;; couple and their child Kibi. Worf asks Kibi: "Are you a boy?" Kibi answers in
;; Kalotan, which of course Worf doesn't understand.
;;
;; Worf turns to the parents (who know English) for explanation. One of them says:
;; "Kibi said: 'I am a boy.'" The other adds: "Kibi is a girl. Kibi lied.
;;
;; Solve for the sex of the parents and Kibi.

(define (solve-kalotan-puzzle)
 (define (xor a? b?) (if (and a? b?) #f (or a? b?)))
 (one-value
  (let ((parent1 (either 'male 'female))
        (parent2 (either 'male 'female))
        (kibi (either 'male 'female))
        (kibi-self-desc (either 'male 'female))
        (kibi-lied? (a-boolean)))
   (unless (not (eq? parent1 parent2)) (fail))
   (when kibi-lied?
    (unless (xor (and (eq? kibi-self-desc 'male)
                    (eq? kibi 'female))
                 (and (eq? kibi-self-desc 'female)
                    (eq? kibi 'male)))
     (fail)))
   (unless kibi-lied?
    (unless (xor (and (eq? kibi-self-desc 'male)
                    (eq? kibi 'male))
                 (and (eq? kibi-self-desc 'female)
                    (eq? kibi 'female)))
     (fail)))
   (when (eq? parent1 'male)
    (unless (and (eq? kibi-self-desc 'male)
               (xor (and (eq? kibi 'female)
                       (not kibi-lied?))
                    (and (eq? kibi 'male)
                       kibi-lied?)))
     (fail)))
   (when (eq? parent1 'female)
    (unless (and (eq? kibi 'female) 
               kibi-lied?)
     (fail)))
   (list parent1 parent2 kibi))))
