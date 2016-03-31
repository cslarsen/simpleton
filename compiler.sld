;; Most of this was taken from SICP chapter 4
;; https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_sec_4.1

(define-library (compiler)
  (import (scheme base)
          (scheme cxr)
          (scheme write)
          (rename (only (print) println)
                  (println debug)))
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
    let-body
    let-name
    let-parameter-names
    let-parameters
    let?
    library-definition?
    make-lambda
    named-let?
    pure-lambda?
    quoted?
    self-evaluating?
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
       (let let?)
       (quote quoted?)
       (set! assignment?))

    (define (assignment-variable exp)
      (cadr exp))

    (define (assignment-value exp)
      (caddr exp))

    (define (named-let? exp)
      (and (let? exp) (symbol? (cadr exp))))

    (define (let-name exp)
      (if (named-let? exp) (cadr exp) #f))

    (define (let-parameters exp)
      (if (named-let? exp)
          (caddr exp)
          (cadr exp)))

    (define (let-parameter-names exp)
      (map car (let-parameters exp)))

    (define (let-body exp)
      (if (named-let? exp)
          (cadddr exp)
          (caddr exp)))

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

    (define (if-consequent exp)
      (caddr exp))

    (define (make-sequence exp)
      (if (begin? exp) exp
          (cons 'begin exp)))

    (define (pure-lambda? exp)
      (pure-lambda?-helper (lambda-parameters exp)
                           (make-sequence (lambda-body exp))))

    (define (true? exp)
     ;; TODO: This blows up the stack
      (if (not (pair? exp))
          (not (not exp))
          (and (not (not (car exp))) (true? (cdr exp)))))

    (define (pure-begin? variables exp)
      ;(debug "  pure-begin? vars " variables " exp " exp)
      (cond
        ((null? exp) #t)
        ((definition? (car exp))
         (let ((variables (cons (definition-variable (car exp))
                                variables)))
            (and (pure-lambda?-helper variables (definition-value (car exp)))
                 (pure-begin? variables (cdr exp)))))
        (else
          (if (pure-lambda?-helper variables (car exp))
              (pure-begin? variables (cdr exp))
              #f))))

    (define (pure-lambda?-helper variables exp)
      ;(debug "  pure-lambda? " variables " for " exp)
      ;; TODO: Must handle all internal bindings (all let-blocks, etc.)
      (cond
        ((eq? exp '()) #t)
        ((self-evaluating? exp) #t)
        ((variable? exp) (true? (memq exp variables)))
        ((quoted? exp) #t)

        ((if? exp)
         (true? (pure-lambda?-helper variables (if-predicate exp))
                (pure-lambda?-helper variables (if-consequent exp))))

        ((cond? exp) (error "unimplemented"))

        ((lambda? exp)
         (pure-lambda?-helper (append (lambda-parameters exp) variables)
                              (lambda-body exp)))

        ((let? exp)
         (pure-lambda?-helper (append (let-parameter-names exp) variables)
                              (let-body exp)))

        ((assignment? exp)
         (true? (pure-lambda?-helper variables (assignment-variable exp))
                (pure-lambda?-helper variables (assignment-value exp))))

        ((begin? exp)
         (pure-begin? variables (cdr exp)))

        ((definition? exp)
         (true? (pure-lambda?-helper variables (definition-variable exp))
                (pure-lambda?-helper variables (definition-value exp))))

        ((application? exp)
         ; We don't check that the first symbol actually exists. Strictly
         ; speaking, all Scheme procedures are impure, because they use
         ; procedures defined elsewhere. So in (lambda (x) (* x x)) the *
         ; operator is globally defined. So we can't trust this function one
         ; hundred percent. A better name for pure? would be "referentially
         ; transparent".
         (true? (map (lambda (x)
                       (pure-lambda?-helper variables x))
                     (cdr exp))))

        ; For completeness, add these as well, although they are only allowed
        ; top-level, and at the beginning of the file
        ((import? exp) #t)
        ((library-definition? exp) #t)

        (else
          (error "Unknown expression: " exp))))

    (define (if-alternative exp)
      (if (not (null? (cdddr exp)))
          (caddr exp)
          #f))))
