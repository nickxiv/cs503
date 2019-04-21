(define ones (cons-stream 1 ones))
(define twos (cons-stream 2 twos))
(define wholes (cons-stream 0 (combine-streams + ones wholes)))
(define fibs (cons-stream 0 (cons-stream 1 (combine-streams + fibs (stream-cdr fibs)))))
(define alt-ones (cons-stream 1 (cons-stream -1 alt-ones)))
(define odds (cons-stream 1 (combine-streams + twos odds)))
(define (// a b) (/ a (real b)))

(define (sref z n)
    (cond 
        ((= n 0 ) (stream-car z))
        (else (sref (stream-cdr z) (- n 1))))
)         ;goes to ith index of stream s

(define (scar z) (car z))
(define (scdr z) ((cdr z)))

(define (sdisplay z n)
    (cond
        ((= n 0) (println "..."))
        (else 
            (print (stream-car z) " ")
            (sdisplay (stream-cdr z) (- n 1))
        )
    )
)

(define (st s)
    (define s0 (sref s 0))
    (define s1 (sref s 1))
    (define s2 (sref s 2))
    (cons-stream 
        (/ 
            (- (* s2 s0) (* s1 s1))
            (+ s2 (- (* 2 s1)) s)
        )
        (st (stream-cdr s))
    )
)

(define (stream-map f s)
    (cons-stream (f (stream-car s)) (stream-map f (stream-cdr s)))
)

(define (tt xform s)
    (cons-stream s (tt xform (xform s)))
)

(define (combine-streams op s t)
    (cons-stream 
        (op (stream-car s) (stream-car t))
        (combine-streams op (stream-cdr s) (stream-cdr t))
    )
)

(define pi-stream (combine-streams // alt-ones odds))


(define (ps s)
    (cons-stream
        (stream-car s)
        (combine-streams + (stream-cdr s) (ps s))    
    )
)
(define pi/4 (ps pi-stream))
(define super-acc-pi/4 (stream-map stream-car (tt st pi/4)))

(sdisplay fibs 10)
(sdisplay odds 10)
(sdisplay wholes 10)
(sdisplay pi-stream 10)
(sdisplay super-acc-pi/4 10)
