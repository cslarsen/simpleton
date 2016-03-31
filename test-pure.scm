(import (scheme base)
        (print)
        (compiler))

(define square-code
  '(define (square x)
      (* x x)))

(define adder-code
  '(define (adder x)
      (+ x y)))

(define-syntax test
  (syntax-rules ()
    ((_ expected-result code)
     (let ((actual-result code))
        (println 
          (if (eq? actual-result expected-result)
              "OK: " "FAIL: ")
          (quote code) " ==> " actual-result)))))

(test #t (pure-lambda? square-code))
(test #f (pure-lambda? adder-code))
