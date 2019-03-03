(define (mandelbrot n)
    (lambda (x y) 
        (define (helper r s i)
            (cond 
                ((>
                    (+
                        (* r r)
                        (* s s)
                    )
                    4) i)
                ((> i n) 0)
                (else (helper 
                        (+ (- (* r r) (* s s)) x) 
                        (+ (* 2 r s) y) 
                        (+ i 1)))
            )
        )
        (helper 0.0 0.0 0)
    )
)

(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define arg1 (readExpr))
    (define arg2 (readExpr))
    (define arg3 (readExpr))

    (println "((mandelbrot " arg1 ") " arg2 " " arg3 ") is " ((mandelbrot arg1) arg2 arg3))
)