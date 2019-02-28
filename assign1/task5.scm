(define (curry f)
    f(curry (+ f 1))
)


(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define f (eval (readExpr) this))
    (define arg1 (eval (readExpr) this))
    (define arg2 (eval (readExpr) this))
    (define arg3 (eval (readExpr) this))
    (define arg4 (eval (readExpr) this))

    (inspect (f arg1 arg2 arg3 arg4))    
    (inspect (curry f))
)