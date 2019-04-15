(include "streams.scm")

(define (smush stream op)
    (define ones (cons-stream 1 ones))
    (define indices (cons-stream 0 (combine-streams + ones indices)))
    (define (return-stream index str total) 
        (cons-stream 
            index               ;index first
            (cons-stream
                (scar str)      ;stream at index second
                (cons-stream
                    (cond 
                        ((nil? total) (scar str))       ;if no total, just put stream[0] third
                        (else (op total (scar str))))   ; else, accumulate third
                    (cond 
                        ((nil? total) (return-stream (+ index 1) (scdr str) (scar str)))    ;if no total, init total to stream[0] rest
                        (else
                            (return-stream (+ index 1) (scdr str) (op total (scar str)))    ;else, set to accumulation rest
                        )
                    )
                )
            )
        )
    )
    (return-stream 0 stream nil)
)

(define (old-smush stream op)
    (define ones (cons-stream 1 ones))
    (define indices (cons-stream 0 (combine-streams + ones indices)))
    (define (return-stream index str total) 
        (cons-stream 
            (list 
                index
                (scar str)
                (cond 
                    ((nil? total) (scar str))
                    (else (op (scar str) total)))
            )
            (cond 
                ((nil? total) (return-stream (+ index 1) (scdr str) (scar str)))
                (else
                    (return-stream (+ index 1) (scdr str) (op total (scar str)))
                )
            )
            
        )
    )
    (return-stream 0 stream nil)
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