;all functions are sourced from or based upon functions and exercises found within SICP

(define (accumulate op initial sequence)
    (if (nil? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))
        )
    )
)

(define (accumulate-n op init seqs)
    (if (nil? (car seqs))
        nil
        (cons 
            (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))
    )
)

(define (dot-product v w)
    (accumulate + 0 (map * v w))
)

(define (matrix-*-vector m v)
    (map (lambda (x) (dot-product v x)) m)
)

(define (matrix-*-matrix m n)
    (transpose (map (lambda(x) (matrix-*-vector n x)) (transpose m)))
)

(define (transpose mat) (accumulate-n cons nil mat))


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