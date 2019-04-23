
(define (deque)
    (define front nil)
    (define back nil)
    (define size 0)

    (define (enqueueFront val)
        (define temp (node val nil nil))
        (cond 
            ((nil? front)           ;empty queue 
                (set! back temp)
            )
            (else
                ((front'set-prev!) temp)
                ((temp'set-next!) front)
            )
        )
        (set! front temp)
        (set! size (+ size 1))
    )
    (define (enqueueBack val)
        (define temp (node val nil nil))
        (cond 
            ((nil? back)           ;empty queue 
                (set! front temp)
            )
            (else
                ((back'set-next!) temp)
                ((temp'set-prev!) back)
                
            )
        )
        (set! back temp)
        (set! size (+ size 1))

    )

    (define (enqueueIndex index val)
        (cond 
            ((= 0 index) (enqueueFront val))
            ((= size index) (enqueueBack val))
            (else 
                (define (helper source nodeBefore)
                    (cond 
                        ((= index source) 
                            (define temp (node val (nodeBefore'next) nodeBefore))
                            (cond 
                                ((not (nil? (nodeBefore'next))) 
                                    (((nodeBefore'next)'set-prev!) temp)
                                )
                            )
                            ((nodeBefore'set-next!) temp)
                            (set! size (+ size 1))
                        )
                        (else 
                            (helper (+ source 1) (nodeBefore'next))
                        )
                    )
                )
                (helper 1 front)
            )
        )
    )

    (define (dequeueIndex index)
        (cond
            ((= 0 index) (dequeueFront))
            ((= (- size 1) index) (dequeueBack))
            (else 
                (define (helper source curr)
                    (cond 
                        ((= index source)
                            (((curr'next)'set-prev!) (curr'prev))
                            (((curr'prev)'set-next!) (curr'next))
                            (set! size (- size 1))
                            (curr'value)
                        )
                        (else
                            (helper (- source 1) (curr'prev))
                        )
                    )
                )
                (helper (- size 1) back)
            )
        )
    )

    (define (dequeueFront)
        (cond 
            ((= size 0) (println "Dequeue from empty list"))
            (else
                (define returnVal (front'value))
                (cond 
                    ((not (nil? (front'next))) (((front'next)'set-prev!) nil))
                )
                (set! front (front'next))
                (set! size (- size 1))
                returnVal
            )
        )
    )

    (define (dequeueBack)
    (cond 
        ((= size 0) (println "Dequeue from empty list"))
        (else
            (define returnVal (back'value))
            (cond
                ((not (nil? (back'prev))) (((back'prev)'set-next!) nil))
            )
            (set! back (back'prev))

            (set! size (- size 1))
            returnVal
            )
    )
)


    (define (display)
        (define (helper source)
            (cond 
                ((nil? source) (print "]"))
                (else 
                    (print (source'value))
                    (if (not (nil? (source'next))) (print ","))
                    (helper (source'next))
                )
            )
        )
        (print "[")
        (helper front)
    )
    this
)

(define (node value next prev)
    (define (set-next! val)
        (set! next val)
    )
    (define (set-prev! val)
        (set! prev val)
    )
    (define (set-value! val)
        (set! value val)
    )
    this
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