(define (ramanujani depth)
    (define (helper source store)
        (cond 
            ((= source 0) store)
            (else (helper (- source 1) (* 
                                    source 
                                    (sqrt (+ (+ source 5) store))
                                 )
                  )
            )
        )
    )
    (helper (+ depth 1) 0)
)

(define (print-raman depth)
    (define (helper source i string store)
        (cond 
            ((= source 0) 
                (append-braces (string+ string i " \\cdot \\sqrt{" store) depth)
            )
            (else
                (helper (- source 1) (+ i 1) (string+ string i " \\cdot \\sqrt{" store " + ") (+ store 1))
            )
        )
    )
    (helper depth 1 "$" 6)
)

(define (append-braces string num)
    (define (helper source store)
        (cond 
            ((= source 0) (string+ store "$"))
            (else (helper (- source 1) (string+ store "}")))
        )
    )
    (helper (+ num 1) string)
)

(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define arg1 (readExpr))
    (println "(ramanujani " arg1 ") is " (fmt "%.25f" (ramanujani arg1)))
    (println "$4$")
)