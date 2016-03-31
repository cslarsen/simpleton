(import (scheme base)
        (print)
        (compiler))

(define square-code
  '(define (square x)
      (* x x)))

(define adder-code
  '(define (adder x)
      (+ x y)))

(define let-code-pure
  '(define (foo)
      (let ((x 10)
            (y 20))
       (+ x y (* x y 30)))))

(define let-code-impure
  '(define (foo)
      (let ((x 10)
            (y 20))
       (+ x y (* x y z 30)))))

(define-syntax check
  (syntax-rules ()
    ((_ expected-result code)
     (let* ((actual-result code)
            (ok? (equal? actual-result expected-result)))
        (println
          (if ok? "OK " "FAIL ")
          (quote code) " ==> " actual-result
          (if (not ok?) " != " "")
          (if (not ok?) expected-result ""))))))

(check #t (pure-lambda? square-code))
(check #f (pure-lambda? adder-code))
(check #t (pure-lambda? let-code-pure))
(check #f (pure-lambda? let-code-impure))

(let ((code '(let loop ((n 1) (x 0))
               (loop (+ n 1) x))))
  (check 'loop (let-name code))
  (check '((n 1) (x 0)) (let-parameters code))
  (check '(loop (+ n 1) x) (let-body code))
  (check '(n x) (let-parameter-names code)))

(check #t
  (pure-lambda?
    '(define (begin-test x y)
        (define bar (+ x y))
        (* bar bar))))

(check #f
  (pure-lambda?
    '(define (begin-test x y)
        (define bar (+ x y))
        (* bar baar))))
