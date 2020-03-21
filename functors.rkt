#lang racket

(require (file "categories.rkt"))

(struct Functor (map source target)
  #:transparent
  #:guard
  (λ (map source target this)
    (unless (category? source)
      (error this "expected source to be category, given: ~e" source))
    (unless (category? target)
      (error this "expected target to be category, given: ~e" target))
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

(define (=Functor? Funct1 Funct2)
  (and
   (=category? (Functor-source Funct1) (Functor-source Funct2))
   (=category? (Functor-target Funct1) (Functor-target Funct2))
   (for/and ([o (in-set (category-objects (Functor-source Funct1)))])
     (equal? ((Functor-map Funct1) o) ((Functor-map Funct2) o)))
   (for/and ([m (in-set (category-morphisms (Functor-source Funct1)))])
     (equal? ((Functor-map Funct1) m)((Functor-map Funct2) m)))
   ))

(define (IdentityFunct cat)
  (Functor (λ (x)
             (cond
               [(set-member? (category-objects cat) x)
                x]
               [(set-member? (category-morphisms cat) x)
                x]))
           cat cat))

(define (IdentityFunct? Funct)
  (and
   (=category? (Functor-source Funct) (Functor-target Funct))
   (for/and ([o (in-set (category-objects (Functor-source Funct)))])
     (equal? o ((Functor-map Funct) o)))
   (for/and ([m (in-set (category-morphisms (Functor-source Funct)))])
     (equal? m ((Functor-map Funct) m)))
   ))

(define (OppositeFunct cat)
  (Functor
   (λ (x)
     (cond
       [(set-member? (category-objects cat) x)
        x]
       [(set-member? (category-morphisms cat) x)
        (list (second x) (first x) (third x))]))
   cat
   (opposite cat)))

(define (!-Funct cat)
  (Functor (λ (x)
             (cond
               [(set-member? (category-objects cat) x)
                1]
               [(set-member? (category-morphisms cat) x)
                (list 1 1 identity)]))
           cat Unit))

(define (FunctorCompose Funct1 Funct2)
  (unless (=category? (Functor-source Funct1)
                      (Functor-target Funct2))
    (error "expected source of ~e to match target of ~e." Funct1 Funct2))
  (cond
    [(IdentityFunct? Funct1) Funct2]
    [(IdentityFunct? Funct2) Funct1]
    [else (Functor (λ (x) ((Functor-map Funct1)((Functor-map Funct2) x)))
                   (Functor-source Funct2)
                   (Functor-target Funct1))]
    ))

(define (FunctorApply Funct cat)
  (unless (=category? (Functor-source Funct) cat)
    (error "expected source of ~e to match category ~e." Funct cat))
  (if (IdentityFunct? Funct)
      cat
      (category
       (for/set ([o (in-set (category-objects cat))])
         ((Functor-map Funct) o))
       (for/set ([m (in-set (category-morphisms cat))])
         ((Functor-map Funct) m))
       (category-identities (Functor-target Funct))
       (category-composition (Functor-target Funct))
       (category-source (Functor-target Funct))
       (category-target (Functor-target Funct)))
      ))

(define (full? Funct)
  (>= (set-count (category-morphisms (Functor-source Funct)))
      (set-count (category-morphisms (FunctorApply Funct (Functor-source Funct))))
      ))

(define (faithfull? Funct)
  (<= (set-count (category-morphisms (Functor-source Funct)))
      (set-count (category-morphisms (FunctorApply Funct (Functor-source Funct))))
      ))

(define (fully-faithfull? Funct)
  (and (full? Funct)
       (faithfull? Funct)))

(provide (all-defined-out))