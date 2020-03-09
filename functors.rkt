#lang racket

(require (file "categories.rkt"))

(struct functor (map source target)
  #:guard
  (λ (map source target this)
    (unless (category? source)
      (error "~e expected category, given: ~e" this source))
    (unless (category? target)
      (error "~e expected category, given: ~e" this target))
    (for [(o (in-set (category-objects source)))]
      (unless (set-member? (category-objects target) (map o))
        (error "~e expected ~e to be member of ~e" this (map o)
               (category-objects target))))
    (for [(m (in-set (category-morphisms source)))]
      (unless (set-member? (category-objects target) (map m))
        (error "~e expected ~e to be member of ~e" this (map m)
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