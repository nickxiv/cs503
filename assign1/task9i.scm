(define (ramanujani depth)
    (define (helper source i store)
        (cond 
            ((= source 0) (sqrt store))
            (else
                (sqrt (+ 
                            store
                            (* 
                                (+ i 1) 
                                (helper (- source 1) (+ i 1) (+ store 1))
                            )
                        )
                )
            )
        )
    )
    (helper depth 1 6)
)



(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define arg1 (readExpr))
    (println "(ramanujani " arg1 ") is " (fmt "%.25f" (ramanujani arg1)))
)