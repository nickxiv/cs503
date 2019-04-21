(define old+ +)

(define (optype a)
    (if (eq? (type a) 'CONS)
        (car a)
        (type a)
    )
)

(define (sx+ a b)
    (string+ a (string b))
)

(define (is+ a b)
    (+ a (integer b))
)

(define (rs+ a b)
    (+ a (real b))
)

(define (rat n d)
    (list 'RATIONAL n d)
)

(define (numerator r) (cadr r))
(define (denominator r) (caddr r))

(define (rat+ a b)
    (rat
        (+ 
            (* (numerator a) (denominator b))
            (* (numerator b) (denominator a))
        )
        (* (denominator a) (denominator b))
    )
)

(define (coerceInteger x) (rat x 1))
(define (coerceRational x) (/ (numerator x) (real (denominator x))))

(define opTable
    (list
        (list 'INTEGER coerceInteger)
        (list 'RATIONAL coerceRational)
        (list '(INTEGER INTEGER) old+)
        (list '(REAL REAL) old+)
        (list '(RATIONAL RATIONAL) rat+)
        (list '(STRING STRING) string+)
        (list '(INTEGER STRING) is+)
        (list '(STRING INTEGER) sx+)
        (list '(REAL STRING) rs+)
        (list '(STRING REAL) sx+)
    )
)

(define (tower x)
    (define types '(STRING REAL RATIONAL INTEGER))
    (length (member x types))
)

(define (get typeA typeB)
    (cadr (assoc (list typeA typeB) opTable))
)

(define (get-coersion t)
    (cadr (assoc t opTable))
)

(define (+ x y)
    (define t1 (optype x))
    (define t2 (optype y))
    (cond 
        ((or (eq? t1 'STRING) (eq? t2 'STRING) (eq? t1 t2)) 
            ((get (optype x) (optype y)) x y)
        )
        ((< (tower t1) (tower t2))
            (+ ((get-coersion t1) x) y)
        )
        (else
            (+ x ((get-coersion t2) y))
        )
    )
)

(inspect (+ 3 4.4))
(inspect (+ "hello " (rat 2 3)))