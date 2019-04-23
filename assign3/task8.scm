(include "streams.scm")

(define (cube x) (^ x 3))

(define (merge-weighted s1 s2 weight) 
    (cond 
        ((stream-null? s1) s2) 
        ((stream-null? s2) s1) 
        (else 
            (cond 
                ((< (weight (scar s1)) (weight (scar s2))) 
                    (cons-stream (scar s1) 
                        (merge-weighted (scdr s1) s2 weight)
                    )
                ) 
                ((> (weight (scar s1)) (weight (scar s2))) 
                    (cons-stream (scar s2) 
                        (merge-weighted s1 (scdr s2) weight)
                    )
                ) 
                (else 
                    (cons-stream 
                        (scar s1)
                            (cons-stream
                                (scar s2)
                                (merge-weighted (scdr s1) (scdr s2) weight)
                            )
                    )
                )
            )
        )
    )
)


(define (weighted-pairs s t weight) 
    (cons-stream 
        (list (scar s) (scar t)) 
        (merge-weighted
            (stream-map (lambda (x) (list (scar s) x))
                (scdr t)
            )
            (weighted-pairs (scdr s) (scdr t) weight)
            weight
        )
        ; (merge-weighted 
        ;     (merge-weighted 
        ;         (stream-map 
        ;             (lambda (x) (list x (scar s))) 
        ;             (scdr t)
        ;         ) 
        ;         (stream-map 
        ;             (lambda (x) (list (scar t) x)) 
        ;             (scdr s)
        ;         ) 
        ;         weight
        ;     ) 
        ;     (weighted-pairs (scdr s) (scdr t) weight) 
        ;     weight
        ; )
    )
)

; (define (weighted-pairs s t weight)
;     (cons-stream
;         (list (scar s) (scar t))
;         (merge-weighted
;             (stream-map (lambda (x) (list (scar s) x)) (scdr t))
;             (weighted-pairs (scdr s) (scdr t) weight)
;             weight
;         )
;     )
; )
    
(define (Ramanujan s)
    ; (cond 
    ;     ((add-it? s cube-sum)
    ;         (cons-stream (cube-sum (scar s))
    ;             (Ramanujan (scdr (scdr (scdr (scdr s)))))
    ;         ) 
    ;     )
    ;     (else (Ramanujan (scdr s)))
    ; )
    ; (println)
    ; (inspect (cube-sum (scar s)))
    ; (inspect (cube-sum (scar (scdr s))))
    ; (println)
    (cond 
        ((= (cube-sum (scar s)) (cube-sum (scar (scdr s))))  
            (cons-stream 
                (cube-sum (scar s)) 
                (Ramanujan (scdr (scdr s)))
            )
        ) 
        (else 
            (Ramanujan (scdr s))
        )
    )
)

; (define (change-stream str)
;     (cond 
;         ((add? str)
;             (cons-stream 
;                 (scar str)
;                 (change-stream (scdr (scdr (scdr (scdr str)))))    
;             )
;         )
;         (else (change-stream (scdr (scdr str))))
;     )
; )
; (define (add? str)
;     (= 
;         (scar str)
;         (scar (scdr str))
;         (scar (scdr (scdr str)))
;         (scar (scdr (scdr (scdr str))))
;         (scar (scdr (scdr (scdr (scdr str)))))
;     )
    
; )
; (define (test-stream str weight)
;     (cons-stream 
;         (weight (scar str))
;         (test-stream (scdr str) weight)
;     )
; )

; (define (add-it? str weight)
;     (= 
;         (weight (scar str))
;         (weight (scar (scdr str)))
;         (weight (scar (scdr (scdr str))))
;         (weight (scar (scdr (scdr (scdr str)))))
;     )
; )

(define (cube-sum x)
    (+ (cube (car x)) (cube (cadr x)))
)   

(define (ramanujan) 
    (Ramanujan (weighted-pairs integers integers cube-sum))
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