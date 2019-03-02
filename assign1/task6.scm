(define (w f i)
    (cond 
        ((= i 0) (f i))
        (else (/ 
                (-                          ;BEGIN NUMER S(f, i + 1) * S(f, i -1) - S(f, i)^2
                    (*                      ; S(f, i + 1) * S(f, i - 1)
                        (S f (+ i 1)) 
                        (S f (- i 1))
                    )
                    (square (S f i))        ; S(f, i)^2
                )                           ;END NUMER

                (+                          ;BEGIN DENOM S(f, i + 1) - 2S(f, i) + S(f, i -1)
                    (-                      ; S(f, i + 1) - 2S(f, i)
                        (S f (+ i 1))
                        (*
                            2
                            (S f i)
                        )
                    )
                    (S f (- i 1))
                )                           ;END DENOM
              )
        )
    )
)

(define (square n)
    (* n n)
)

(define (S f n)
    (define (iter i store)
        (cond 
            ((< n i) store)
            (else (iter (+ i 1) (+ store (f i))))
        )
    )
    (iter 0 0)
)


(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define f (eval (readExpr) this))
    (define arg2 (readExpr))
    (println "(S " f " " arg2 ") is " (S f 0))
    (println "(w " f " " arg2 ") is " (w f 0))

)
