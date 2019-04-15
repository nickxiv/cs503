(include "streams.scm")

(define (quad square-coef linear-coef constant step)
    (define (return-stream curr)
        (define x (* curr step))
        (cons-stream
            (+ (* square-coef (* x x)) (* linear-coef x) constant)
            (return-stream (+ curr 1))
        )
    )
    (return-stream 0.0)
)

(define (integrate str step)
    (define (return-stream i curr next-str total)
        (cons-stream
            (+ total (trapezoid-area curr (scar next-str) step))
            (return-stream (+ i 1) (scar next-str) (scdr next-str) (+ total (trapezoid-area curr (scar next-str) step)))
        )
    )
    (cons-stream 0.0 (return-stream 1 (scar str) (scdr str) 0.0))
)

(define (derivate str step constant)
    (define (return-stream)
        (println "str =")
        (sdisplay 5 str) (println)
    )
    (return-stream)
    str
    
)

    ; (define (return-stream prev-height next-str)
    ;     (cons-stream
    ;         (- 
    ;             (reverse-trap (scar next-str) step prev-height) 
    ;         prev-height)
    ;         (return-stream (- (reverse-trap (scar next-str) step prev-height) prev-height) (scdr next-str))
    ;     )
    ; )
    ; (cons-stream constant (return-stream constant (scdr str)))

(define (same-stream? s1 s2 n t)
    (define (helper source store1 store2)
        (cond 
            ((= source 0) #t)
            ((>= (abs (- (scar s1) (scar s2))) t) #f)
            (else 
                (helper (- source 1) (scdr store1) (scdr store2))
            )
        )
    )
    (helper n s1 s2)
)
(define (trapezoid-area a b height) (* (/ (+ a b) 2) height))
(define (reverse-trap y h b)
    (- (/ (* 2 y) h) b)
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