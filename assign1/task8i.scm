(define (ecfi num)
    (define (helper source store)
        (cond
            ((= source 0) (+ 2 store))
            (else
                (helper (- source 1) (/ 1
                                        (+ 1
                                            (/ 1
                                                (+
                                                    (* 2 source)
                                                    (+
                                                        (/  1
                                                            (+ 1 store)
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                     )
                )
            )
        )
    )
    (helper num 0.0)

)

(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define arg1 (readExpr))
    (println "(ecfi " arg1 ") is " (fmt "%.25f" (ecfi arg1)))
)
