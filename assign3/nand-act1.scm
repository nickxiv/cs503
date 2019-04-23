(include "gates.scm")

(define (nand-gate a1 a2 out)
    (define (nand-action-procedure)
        (let ((new-value (logical-nand (get-signal a1) (get-signal a2))))
            
            (after-delay nand-gate-delay 
                (lambda ()
                    (print "\nNAND active!")
                    (set-signal! out new-value)
                )
            )
        )
    )
    (add-action! a1 nand-action-procedure)
    (add-action! a2 nand-action-procedure)
    'ok
)

(define nand-gate-delay 6)
(define (logical-nand a b) (logical-not (logical-and a b)))
