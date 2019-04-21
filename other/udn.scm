
(define (udn x)
    (define (up y)
        (set! x (+ x y))
    )
    (define (down y)
        (set! x (- x y))
    )
    (define (get) 
        x    
    )
    (return this)
    (define (dispatch op)
        (cond
            ((eq? op 'up) up)
            ((eq? op 'down) down)
            ((eq? op 'get) get)
            (else (throw 'BAD_OP (string+ "op " op " not implemented")))
        )
    )
    dispatch
)

(define z (udn 10))

(inspect z)
(inspect (z 'get))
(inspect ((z 'get)))
(inspect (z 'up))
(inspect ((z 'up ) 1))
(inspect ((z 'get)))
