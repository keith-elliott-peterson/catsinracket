#lang racket

(struct category
  (objects morphisms identities composition source target)
  #:guard
  (λ (objects morphisms identities composition source target this)
    (unless
        (set? objects)
      (error this "expected a set. Given: ~e" objects))
    (unless (set? morphisms)
      (error this "expected a set. Given: ~e" morphisms))
    (for* ([o (in-set objects)]
           [m (in-set morphisms)])
      (unless (set-member? morphisms (identities o))
        (error this "expected the identity morphism of ~e, ~e, to be member of ~e."
               o (identities o) morphisms))
      (unless (set-member? objects (source m))
        (error this "expected the source of ~e, ~e, to be member of ~e."
               m (source m) objects))
      (unless (set-member? objects (target m))
        (error this "expected the target of ~e, ~e, to be member of ~e."
               m (target m) objects))
      (unless (equal? o (source (identities o)))
        (error this "expected ~e to equal source of identity, given: ~e."
               o (source (identities o))))
      (unless (equal? o (target (identities o)))
        (error this "expected ~e to equal target of identity, given: ~e."
               o (target (identities o))))
      (unless (equal? m (composition m (identities (source m))))
        (error this "expected composition of ~e and ~e to equal ~e"
               m (identities (source m)) m))
      (unless (equal? m (composition (identities (target m)) m))
        (error this "expected composition of ~e and ~e to equal ~e"
               (identities (target m)) m))
      )
    (for* ([m (in-set morphisms)]
           [n (in-set morphisms)])
      (when (equal? (target m) (source n))
        (unless (set-member? morphisms (composition n m))
          (error this "expected the composition of ~e and ~e, ~e, to be a member of ~e"
                 m n (composition n m) morphisms))
        (unless (equal? (source m) (source (composition n m)))
          (error this "expected source of ~e to equal the source of ~e."
                 m (composition n m)))
        (unless (equal? (target n) (target (composition n m)))
          (error this "expected target of ~e to equal the target of ~e."
                 n (composition n m)))
        )
      )
    (for* ([m (in-set morphisms)]
           [n (in-set morphisms)]
           [o (in-set morphisms)])
      (when (and (equal? (target m) (source n))
                 (equal? (target n) (source o)))
        (unless (equal? (composition o (composition n m))
                        (composition (composition o n) m))
          (error this "expected ~e to equal ~e, ~e doesn't satisfy associative law."
                 (composition o (composition n m))
                 (composition (composition o n) m)
                 composition))
        ))
    (values objects morphisms identities composition source target))
  )

(define (opposite cat)
  (category (category-objects cat)
            (for/set ([m (in-set (category-morphisms cat))])
              (list (second m) (first m) (third m)))
            (category-identities cat)
            (λ (x y) ((category-composition cat) y x))
            (category-target cat) (category-source cat)))

(define (composer x y)
  (list (first y) (second x)
        (cond
          [(equal? identity (third x)) (third y)]
          [(equal? identity (third y)) (third x)]
          [else (flatten (list (third x) (third y)))]
          )))
  
(define Empty
  (category (set) (set)
            (λ (x) (list x x identity))
            composer
            first second))

(define Unit
  (category (set 1) (set (list 1 1 identity))
            (λ (x) (list x x identity))
            composer
            first second))

(define Arrow
  (category (set 1 2)
            (set (list 1 1 identity)
                 (list 1 2 add1)
                 (list 2 2 identity))
            (λ (x) (list x x identity))
            composer
            first second))

(define Triangle
  (category (set 1 2 3)
            (set (list 1 1 identity)
                 (list 2 2 identity)
                 (list 3 3 identity)
                 (list 1 2 add1)
                 (list 2 3 add1)
                 (list 1 3 (list add1 add1)))
            (λ (x) (list x x identity))
            composer
            first second))

(define Tetrahedron
  (category (set 1 2 3 4)
            (set (list 1 1 identity)
                 (list 2 2 identity)
                 (list 3 3 identity)
                 (list 4 4 identity)
                 (list 1 2 add1)
                 (list 2 3 add1)
                 (list 3 4 add1)
                 (list 1 3 (list add1 add1))
                 (list 1 4 (list add1 add1 add1))
                 (list 2 4 (list add1 add1))
                 )
            (λ (x) (list x x identity))
            composer
            first second))
