(define (curry f @)
    (define nparams (length (get 'parameters f))
    (inspect nparams)
)
(define (f a b c) (+ a b c))
(inspect (curry f 1)