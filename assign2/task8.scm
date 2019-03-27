(define (node value left right)
    (define (display) (print value))
    this
)

(define (newBST value)
    (node value nil nil)
)

(define (displayBST root)
(define (iter root indent)
    (if (valid? root)
        (begin
            (iter (root'right) (string+ indent "    "))
            (print indent)
            ((root'display))
            (println)
            (iter (root'left) (string+ indent "    "))
            )
        )
    )
(iter root "")
)

(define (insertBST root val)
    (println "INSERT " val)
    (cond
        ((nil? root)                       ;insert into empty tree
            (println "BASE CASE")
            (newBST val)
        )
        ((< val (root'value))              ;insert val < root
            (println "INSERT LEFT")
            (set (root'left) (insertBST (root'left) val))
        )
        (else                                   ;insert val > root
            (println "INSERT RIGHT")
            (set (root'right) (insertBST (root'right) val))
        )
    )
    ; root
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