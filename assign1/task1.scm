(define pi 3.14159265358979323846)

(define (cyan num)
    (int
        (+
            (*
                255
                (sin
                    (+
                        (/
                            (*
                                num
                                pi    
                            )
                            200
                        )
                        (/
                            pi
                            2
                        )
                    )
                )
            )
            0.00000001
        )
    )
)

(define (yellow num)
    (int
        (+
            (+
                (*
                    -255
                    (sin
                        (/
                            (* pi num)
                            100
                        )    
                    )    
                )
              255  
            )
            0.00000001
        )    
    )
)

(define (magenta num)
    (int
        (+
            (+
                (*
                    127.5    
                    (sin
                        (+
                            (/
                                (* 3 num pi)
                                200
                            )    
                            (/ pi 2)
                        )
                    )
                )
                127.5    
            )
            0.00000001
        )
    )
)

(define (cym num)
    (define c (cyan num))
    (define y (yellow num))
    (define m (magenta num))
    (string+ 
        "#" 
        (numToHexString (/ c 16)) (numToHexString (% c 16)) 
        (numToHexString (/ y 16)) (numToHexString (% y 16)) 
        (numToHexString (/ m 16)) (numToHexString (% m 16))
    )
)

(define (numToHexString num)
    (cond 
        ((= num 0) "0")
        ((= num 1) "1")
        ((= num 2) "2")
        ((= num 3) "3")
        ((= num 4) "4")
        ((= num 5) "5")
        ((= num 6) "6")
        ((= num 7) "7")
        ((= num 8) "8")
        ((= num 9) "9")
        ((= num 10) "A")
        ((= num 11) "B")
        ((= num 12) "C")
        ((= num 13) "D")
        ((= num 14) "E")
        ((= num 15) "F")
    )

)

(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define arg1 (readExpr))

    (println "(cyan " arg1 ") is " (cyan arg1))
    (println "(yellow " arg1 ") is " (yellow arg1))
    (println "(magenta " arg1 ") is " (magenta arg1))
    (println "(cym " arg1 ") is " (cym arg1))

)
