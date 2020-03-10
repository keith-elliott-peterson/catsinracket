#lang racket

(require (file "categories.rkt"))

(struct functor (map source target)
  #:guard
  (λ (map source target this)
    (unless (category? source)
      (error this "expected category, given: ~e" source))
    (unless (category? target)
      (error this "expected category, given: ~e" target))
    (for [(o (in-set (category-objects source)))]
      (unless (set-member? (category-objects target) (map o))
        (error this "expected ~e to be member of ~e" (map o)
               (category-objects target))))
    (for [(m (in-set (category-morphisms source)))]
      (unless (set-member? (category-morphisms target) (map m))
        (error this "expected ~e to be member of ~e" (map m)
               (category-morphisms target))))
    (for* ([m (in-set (category-morphisms source))]
           [n (in-set (category-morphisms source))])
      (when (equal? ((category-target source) m)
                    ((category-source source) n))
        (unless (equal?
                 ((category-composition target) (map n) (map m))
                 (map ((category-composition source) n m)))
          (error "~e doesn't satisfy functoriality." map))))
    (values map source target)
    ))

(define (identityfunct cat)
  (functor (λ (x)
             (cond
               [(set-member? (category-objects cat) x)
                x]
               [(set-member? (category-morphisms cat) x)
                x]))
           cat cat))

(define (!-funct cat)
  (functor (λ (x)
             (cond
               [(set-member? (category-objects cat) x)
                1]
               [(set-member? (category-morphisms cat) x)
                (list 1 1 identity)]))
           cat Unit))


(provide (all-defined-out))