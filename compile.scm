;; Compiler based on SICP chapter 4 and 5

(import
  (scheme base)
  (scheme write)
  (scheme cxr)
  (scheme read)
  (print))

(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; formal parameters
                   (cddr exp)))) ; body

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp)
  (cdr exp))

(define (last-exp? seq)
  (null? (cdr seq)))

(define (first-exp seq)
  (car seq))

(define (rest-exps seq)
  (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
   ((last-exp? seq) (first-exp seq))
   (else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))

(define (application? exp)
  (pair? exp))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (no-operands? ops)
  (null? ops))

(define (first-operand ops)
  (car ops))

(define (rest-operands ops)
  (cdr ops))

(define (compile-application exp target linkage)
  (printsln "application: " exp target linkage))

(define (preserving reg . rest)
  (println "preserve: " reg))

(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
    (compile (first-exp seq) target linkage)
    (preserving '(env continue)
      (compile (first-exp seq) target 'next)
      (compile-sequence (rest-exps seq) target linkage))))

(define (compile-definition exp target linkage)
  (println "compile definition: " exp)
  (println "  target: " target " linkage: " linkage))

(define (compile exp target linkage)
  (cond
    ((self-evaluating? exp) (compile-self-evaluating exp target linkage))
    ((quoted? exp) (compile-quoted exp target linkage))
    ((variable? exp) (compile-variable exp target linkage))
    ((assignment? exp) (compile-assignment exp target linkage))
    ((definition? exp) (compile-definition exp target linkage))
    ((if? exp) (compile-if exp target linkage))
    ((lambda? exp) (compile-lambda exp target linkage))
    ((begin? exp) (compile-sequence (begin-actions exp) target linkage))
;    ((cond? exp) (compile (cond->if exp) target linkage))
    ((application? exp) (compile-application exp target linkage))
    (else
      (error "Unknown expression type: " exp))))

(define (read-all)
  (let loop ((exp (read))
             (body '()))
    (if (eof-object? exp) body
      (loop (read) (append body (list exp))))))

(let ((source (cons 'begin (read-all))))
  (println source)
  (println "rest: " (compile source 'main-return 'return)))
