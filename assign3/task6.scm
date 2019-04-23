(include "streams.scm")

(define (quad square-coef linear-coef constant step)
    (define (return-stream curr)
        (define x (* (real curr) (real step)))
        (cons-stream
            (+ (* square-coef (* x x)) (* linear-coef x) (real constant))
            (return-stream (+ (real curr) 1.0))
        )
    )
    (return-stream 0.0)
)

(define (integrate str step)
    (define (return-stream i curr next-str total)
        (cons-stream
            (+ total (trapezoid-area curr (real (scar next-str)) step))
            (return-stream (+ i 1.0) (scar next-str) (scdr next-str) (+ total (trapezoid-area curr (scar next-str) step)))
        )
    )
    (cons-stream 0.0 (return-stream 1.0 (real (scar str)) (scdr str) 0.0))
)

(define (derivate str step constant)
    (define (return-stream prev-height prev-area area-str)
        (cons-stream
            (reverse-trap (real (scar area-str)) prev-area (real step) prev-height)
            (return-stream 
                (reverse-trap (real (scar area-str)) prev-area (real step) prev-height) 
                (real (scar area-str)) 
                (scdr area-str)
            )
        )
    )
    (cons-stream (real constant) (return-stream (real constant) 0.0 (scdr str)))
)

(define (same-stream? s1 s2 n t)
    (define (helper source store1 store2)
        (cond 
            ((= source 0) #t)
            ((>= (abs (- (scar store1) (scar store2))) t) #f)
            (else 
                (helper (- source 1) (scdr store1) (scdr store2))
            )
        )
    )
    (helper n s1 s2)
)
(define (trapezoid-area a b height) (* (/ (+ a b) 2.0) height))
(define (reverse-trap curr-area prev-area step prev-height)
    (-
        (/
            (*
                2.0
                (- (real curr-area) (real prev-area))
            )
            (real step)
        )
        (real prev-height)
    )
)
(define (abs n) (if (< n 0) (- n) n))

(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define env this)
    (define (iter expr)
        (if (not (eof?)) 
            (begin (eval expr env) (iter (readExpr)))
        )
    )
    (iter (readExpr))    
)