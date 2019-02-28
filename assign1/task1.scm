(define pi 3.14159265358979323846)

(define (scale num)
    (* 255 (/ (real num) 100))
)

(define (cyan num)
    (* (sin (scale num)) pi)
)

(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define arg1 (readExpr))

    (inspect arg1)
    (inspect (cyan arg1))
    (inspect (scale arg1))
)


;d for magenta is 127.5