(define (ecfr n)
    (define (helper i)
        (cond 
            ((= n i) 0)
            (else (/ 1.0
                    (+ 1.0
                        (/ 1.0
                            (+
                                (* 2.0 (+ i 1))
                                    (/  1.0
                                        (+ 1.0 (helper (+ i 1)))
                                    )
                            )
                        )
                    )
                ) 
            )
            (else

            )
        )
    )
    (+ 2.0 (helper 0))
)

(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define arg1 (readExpr))
    (println "(ecfr " arg1 ") is " (fmt "%.25f" (ecfr arg1)))
)
