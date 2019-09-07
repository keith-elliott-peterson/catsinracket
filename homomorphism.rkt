#lang racket
(require (file "renamers.rkt"))

(struct homomorphism (procedure source target)
  #:transparent
  #:guard
  (λ (procedure source target hom-name)
    (unless (equal? (procedure source)
                    target)
      (error "error"))
    (values procedure source target)))

(define (homomorphism-compose hom1 hom2)
  (when (and
         (equal? (homomorphism-target hom1)
                 (homomorphism-source hom2))
         (not
          (or
           (equal?
            identity
            (homomorphism-procedure hom1))
           (equal?
            identity
            (homomorphism-procedure hom2))
           )))
    (homomorphism
     (compose-and-rename
      (homomorphism-procedure hom2)
      (homomorphism-procedure hom1))
     (homomorphism-source hom1)
     (homomorphism-target hom2))))

(define empty-homomorphism
  (homomorphism (λ(z)(set)) (set) (set)))

(define (empty-homomorphism? hom)
  (and (set? (homomorphism-source hom))
       (set? (homomorphism-target hom))
       (set-empty?
        (homomorphism-source hom))
       (set-empty?
        (homomorphism-target hom))))

(define (homomorphism-product hom1 hom2)
  (if (or (empty-homomorphism? hom1)
          (empty-homomorphism? hom2))
      empty-homomorphism
      (homomorphism
       (product-proc-and-rename
        (homomorphism-procedure hom1)
        (homomorphism-procedure hom2))
       (cons (homomorphism-source hom1)
             (homomorphism-source hom2))
       (cons (homomorphism-target hom1)
             (homomorphism-target hom2))
       )))
(provide (all-defined-out))