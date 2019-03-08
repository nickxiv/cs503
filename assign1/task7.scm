(define (double n)
    (+ n n)
)

(define (halve n)
    (define (helper source store)
        (cond
            ((= source 0) store)
            (else 
                (helper 
                    (- source
                        (pow-2 (glp source))
                    )
                    (+ store
                        (pow-2 (- (glp source) 1))
                    )
                )
            )
        )
    )
    (helper n 0)    
)

(define (glp n)         ;get largest power
    (define (helper i)
        (cond 
            ((> (pow-2 i) n) (- i 1))
            (else 
                (helper (+ i 1))
            )
        )
    )
    (helper 0)
)

(define (div2? n)
    (define (helper source store) 
        (cond 
            ((= source 0) #t)
            ((= source 1) #f)
            (else 
                (helper 
                    (- source
                        (pow-2 (glp source))
                    )
                    (+ store
                        (pow-2 (- (glp source) 1))
                    )
                )
            )        
        )
    )
    (helper n 0)
)

(define (pow-2 x)
    (cond ((= x 0) 1)
        ((< x 0) 0)
        (else
            (define (helper i store)
                (cond 
                    ((= i (- x 1)) store)
                    (else (helper (+ i 1) (+ store store))))
            )
            (helper 0 2)
        )
    )
)

(define (ethiop a b)
    (define (helper source1 source2 store)
        (cond
            ((= source2 1) (+ store source1))
            (else (helper (double source1) (halve source2) 
                    (if (not (div2? source2))
                        (+ source1 store)
                        store
                    )
                  )
            )
        )
    )
    (helper a b 0)
)

(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define arg1 (readExpr))
    (define arg2 (readExpr))
    (println "(halve " arg1 ") is " (halve arg1))
    (println "(double " arg1 ") is " (double arg1))
    (println "(div2? " arg1 ") is " (div2? arg1))
    (println "(ethiop " arg1 " "arg2 ") is " (ethiop arg1 arg2))
    (println)
)