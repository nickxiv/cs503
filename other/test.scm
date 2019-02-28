(define (fact n)                                        ;not tail recursive, takes linear space
    (if (< n 2)
    1
    (* n (fact ( - n 1)))
    )
)

(define (fact2 n)                                       ;tail recursive, takes constant space
    (define (iter store source)
        (if (= source 0)
            store
            (iter (* store source) (- source 1))
        )
    )
(iter 1 n)
)

(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1 )) (fib (- n 2)))
    )
)

(define (fib2 n)
    (define (iter prevprev prev src)
        (if (= src 0)
            prevprev
            (iter prev (+ prevprev prev) (- src 1)))
    )
    (iter 0 1 n)
)

; (println (fact 5))
; (println (fact2 5))
(println (fib2 1))
(println (fib2 2))
(println (fib2 3))
(println (fib2 4))
(println (fib2 5))
(println (fib2 6))
(println (fib2 30))

; single line comment
;{
    multi line comment
;}

;$
comment to end of file (only in scam)