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
    library-definition?
    make-lambda
    pure-lambda?
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

    (define (pure-lambda?-helper variables exp)
      (debug "  pure-lambda? " variables " for " exp)

      ;; TODO: Must transform let blocks so newly introduced variables are
      ;; accounted for as well. Additionally, internal definitions are not
      ;; accounted for here; in a begin-sequence, we must add to the variables
      ;; whenever we encounter an internal definition.
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

        ((assignment? exp)
         (true? (pure-lambda?-helper variables (assignment-variable exp))
                (pure-lambda?-helper variables (assignment-value exp))))

        ((begin? exp)
         (true? (map (lambda (x)
                       (pure-lambda?-helper variables x))
                     (cdr exp))))

        ((definition? exp)
         (true? (pure-lambda?-helper variables (definition-variable exp))
                (pure-lambda?-helper variables (definition-value exp))))

        ((application? exp)
         (true? (map (lambda (x)
                       (pure-lambda?-helper variables x))
                     (cdr exp)))) ; theoretically, all funcs are non-pure because
                                  ; they may use functions defined elsewhere

        ; for completeness, add these as well, although they are only allowed
        ; top-level, and at the beginning of the file
        ((import? exp) #t)
        ((library-definition? exp) #t)

        (else
          (error "Unknown expression: " exp))))

    (define (if-alternative exp)
      (if (not (null? (cdddr exp)))
          (caddr exp)
          #f))))
