;; Most of this was taken from SICP chapter 4
;; https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_sec_4.1

(define-library (compiler)
  (import (scheme base)
          (scheme cxr)
          (scheme write))
  (export
    application?
    assignment-value
    assignment-variable
    assignment?
    begin?
    cond?
    definition-value
    definition-variable
    definition?
    if-alternative
    if-consequent
    if-predicate
    if?
    import?
    lambda-body
    lambda-parameters
    lambda?
    library-definition?
    make-lambda
    quoted?
    tagged-list?
    variable?)

  (begin
    (define (tagged-list? symbol exp)
      (and (pair? exp)
           (eq? symbol (car exp))))

    ;; Unexported helper macro
    (define-syntax define-predicates
      (syntax-rules ()
        ((_) #f)
        ((_ (symbol procedure) rest ...)
         (begin
           (define (procedure exp)
              (tagged-list? (quote symbol) exp))
           (define-predicates rest ...)))))

    (define-predicates
       (begin begin?)
       (cond cond?)
       (define definition?)
       (define-library library-definition?)
       (if if?)
       (import import?)
       (lambda lambda?)
       (quote quoted?)
       (set! assignment?))

    (define (assignment-variable exp)
      (cadr exp))

    (define (assignment-value exp)
      (caddr exp))

    (define (application? exp)
      (and (pair? exp)
           (symbol? (car exp))))

    (define (self-evaluating? exp)
      (or (number? exp)
          (string? exp)))

    (define (variable? exp)
      (symbol? exp))

    (define (definition-variable exp)
      (if (symbol? (cadr exp))
          (cadr exp)
          (caadr exp)))

    (define (make-lambda parameters body)
      (cons 'lambda (cons parameters body)))

    (define (definition-value exp)
      (if (symbol? (cadr exp))
          (caddr exp)
          (make-lambda (cdadr exp)   ; formal parameters
                       (cddr exp)))) ; body

    (define (lambda-parameters exp)
      (cadr exp))

    (define (lambda-body exp)
      (cddr exp))

    (define (if-predicate exp)
      (cadr exp))

    (define if-consequent exp)
      (caddr exp)

    (define (if-alternative exp)
      (if (not (null? (cdddr exp)))
          (caddr exp)
          #f))))
