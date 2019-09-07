#lang racket

(define (compose-and-rename proc1 proc2)
  (cond [(equal? identity proc1) proc2]
        [(equal? identity proc2) proc1]
        [else
         (procedure-rename
          (compose proc1 proc2)
          (string->symbol
           (string-join 
            (list (symbol->string (object-name proc1))
                  (symbol->string (object-name proc2)))
            "∘")))]))

(define (product-proc-and-rename proc1 proc2)
  (if (and (equal? identity proc1)
           (equal? identity proc2))
      identity
      (procedure-rename
       (λ (x) (cons (proc1 (car x))
                    (proc2 (cdr x))))
       (string->symbol
        (string-join 
         (list (symbol->string (object-name proc1))
               (symbol->string (object-name proc2)))
         "×")))))

(provide (all-defined-out))