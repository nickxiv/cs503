(define zero
  (lambda (f)
    (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define hey (lambda (x) (print x)))
(((add-1 zero) hey) "HEY\n")