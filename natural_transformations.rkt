#lang racket

(require (file "categories.rkt")
         (file "functors.rkt"))

(struct natural-transformation
  (map source target)
  #:guard
  (λ (map source target this)
    (unless (equal? (functor-source source)
                    (functor-source target))
      (error this "expected ~e and ~e to match on source categories."
             source target))
    (unless (equal? (functor-target source)
                    (functor-target target))
      (error this "expected ~e and ~e to match on target categories."
             source target))
    (for ([o (in-set (category-objects (functor-source source)))])
      (unless  (set-member? (category-morphisms
                             (functor-target source))
                            (map o))
        (error this "expected ~e to be member of ~e"
               (map o) (category-morphisms (functor-target source))))
      (unless (equal? ((category-source (functor-target source)) (map o))
                      ((functor-map source) o))
        (error this "expected ~e to equal ~e"
               ((category-source (functor-target source)) (map o))
                      ((functor-map source) o)))
      (unless (equal? ((category-target (functor-target source)) (map o))
                      ((functor-map target) o))
        (error this "expected ~e to equal ~e"
               ((category-target (functor-target source)) (map o))
                      ((functor-map target) o))))
    (for ([m (in-set (category-morphisms (functor-source source)))])
      (unless (equal?
               ((category-composition (functor-target source))
                (map ((category-target (functor-source source)) m))
                ((functor-map target) m))
               ((category-composition (functor-target source))
                ((functor-map source) m)
                (map ((category-source (functor-source source)) m))
                )
               )
        (error this "expected to behave on morphisms"))
      )
    (values map source target))
  )
  
(define (IdentityNatTrans funct)
  (natural-transformation
   (λ (x)
     (cond [(set-member? (category-objects (functor-source funct)) x)
            ((category-identities (functor-target funct))
             ((functor-map funct) x))]
           [(set-member? (category-morphisms (functor-source funct)) x)
            ((functor-map funct) x)])
     )
   funct funct))
