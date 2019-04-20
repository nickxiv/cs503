(define (replace f replacements)
    (define parameters (get 'parameters f))
    (define body (cadr (get 'code f)))
    (set 'parameters (build-new-params parameters replacements) f)
    (set 'code (cons 'begin (list (build-new-body body replacements))) f)
)

(define (build-new-params old-params replacements)
    (define (helper source store)
        (cond 
            ((nil? source) (reverse store))
            (else
                (helper 
                    (cdr source) 
                    (cons (compare-all (car source) replacements) store)
                )
            )
        )
    )
    (helper old-params nil)
)

(define (build-new-body old-body replacements)
    (cond 
        ((equal? (car old-body) 'quote) old-body)
        (else (define (helper source store)
            (cond 
                ((nil? source)(reverse store))
                ((pair? (car source))
                    (helper 
                        (cdr source) 
                        (cons (build-new-body (car source) replacements) store)
                    ) 
                )
                (else
                    (helper 
                        (cdr source)
                        (cons (compare-all (car source) replacements) store)
                    )
                )
                
            )
        )
        (helper old-body nil))
    )
)

(define (compare-all x the-list)
    (define (helper source)
        (cond 
            ((nil? source) x)
            ((equal? (car source) x) (cadr source))
            (else (helper (cddr source)))
        )
    )
    (helper the-list)
)

(define (reverse n)
    (define (helper a b)
        (cond 
            ((nil? a) b)
            (else
                (helper (cdr a) (cons (car a) b))
            )
        )
    )
    (helper n nil)
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