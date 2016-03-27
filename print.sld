(define-library (print)
  (import (scheme base)
          (scheme write))
  (export
    print
    println
    prints
    printsln)
  (begin
    (define (print . args)
      (for-each display args))

    (define (prints . args)
      (display (car args))
      (for-each (lambda (s)
                  (display " ")
                  (display s)) (cdr args)))

    (define (println . args)
      (apply print args)
      (newline))

    (define (printsln . args)
      (apply prints args)
      (newline))))
