(define ones (cons-stream 1 ones))
(define wholes (cons-stream 0 (combine-streams + ones wholes)))
(define g (cons-stream 1 (cons-stream 2 (cons-stream 3 g))))
(define (combine-streams op s t)
    (cons-stream 
        (op (stream-car s) (stream-car t))
        (combine-streams op (stream-cdr s) (stream-cdr t))
    )
)

(define (stream-map f s)
    (cons-stream
        (f (stream-car s))
        (stream-map f (stream-cdr s)))
)


(define (pairs s t)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (merge 
            (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
            (pairs (stream-cdr s) (stream-cdr t))    
        )
    )
)

(define (shuffle s t)
    (cons-stream 
        (stream-car s)
        (shuffle t (stream-cdr s))
    )
)

(define (shuffle2 s t)
    (define a (stream-car s))
    (define b (stream-car t))
    (cond 
        ((< (cadr a) (cadr b)) 
            (cons-stream a (shuffle2 (stream-cdr s) t))    
        )
        ((< (cadr b) (cadr a)) 
            (cons-stream b (shuffle2 s (stream-cdr t)))    
        )
        ((< (car a) (car b))
            (cons-stream a (shuffle2 (stream-cdr s) t))
        )
        (else
            (cons-stream b (shuffle2 s (stream-cdr t)))   
        )
    )
)

(define merge shuffle2)

(define (sdisplay z n)
    (cond
        ((= n 0) (println "..."))
        (else 
            (print (stream-car z) " ")
            (sdisplay (stream-cdr z) (- n 1))
        )
    )
)


(sdisplay g 12)