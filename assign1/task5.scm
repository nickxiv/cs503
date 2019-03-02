(define (curry f)
    (lambda (arg1)
        (lambda (arg2) 
            (lambda (arg3) 
                (lambda (arg4) 
                    (f arg1 arg2 arg3 arg4)
                )
            )
        )
    )
)



(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define f (eval (readExpr) this))
    (define arg1 (readExpr))
    (define arg2 (readExpr))
    (define arg3 (readExpr))
    (define arg4 (readExpr))
    (println "(((((curry " f ") " arg1 ") " arg2 ") " arg3 ") " arg4 ") is " (((((curry f) arg1) arg2) arg3) arg4))
    ;(((((curry <function f(a b c d)>) 1) 2) 3) 4) is 10
)