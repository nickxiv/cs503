(include "gates.scm")
(include "queue.scm")
(include "nand.scm")

(define the-agenda (make-agenda))

(define (inverter a out)
    (nand-gate a a out)
    'ok
)

(define (and-gate a b out)
    (let ((ab (make-wire)))
        (nand-gate a b ab)
        (nand-gate ab ab out)
        'ok
    )
)
(define (or-gate a b out)
    (let ((a1 (make-wire)) (b1 (make-wire)))
        (nand-gate a a a1)
        (nand-gate b b b1)
        (nand-gate a1 b1 out)
        'ok
    )
)

(define (nor-gate a b out)
    (let (
        (not-a (make-wire))
        (not-b (make-wire))
        (not-a-not-b (make-wire))
    )
    (nand-gate a a not-a)
    (nand-gate b b not-b)
    (nand-gate not-a not-b not-a-not-b)
    (nand-gate not-a-not-b not-a-not-b out)
    'ok
    )
)

(define (xor-gate a b out)
    (let (
        (c (make-wire))
        (d (make-wire))
        (e (make-wire))
    )
    (nand-gate a b c)
    (nand-gate a c d)
    (nand-gate b c e)
    (nand-gate d e out)
    
    'ok
    )
)

(define (xnor-gate a b out)
    (let (
        (not-a (make-wire))
        (not-b (make-wire))
        (c (make-wire))
        (d (make-wire))
    )
    (nand-gate a a not-a)
    (nand-gate b b not-b)
    (nand-gate not-a not-b c)
    (nand-gate a b d)
    (nand-gate c d out)
    'ok
    )
)


(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define env this)
    (define (iter expr)
        (if (not (eof?)) 
            (begin (eval expr env) (iter (readExpr)))
        )
    )
    (iter (readExpr))    
)