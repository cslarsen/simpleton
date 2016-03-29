(import
  (scheme base)
  (scheme write)
  (scheme read)
  (scheme cxr)
  (print))

(define (compile exp target linkage)
  (cond
    ((quote? exp) (compile-quote exp target linkage))
    ((if? exp) (compile-if exp target linkage))
    ((cond? exp) (compile (transform-cond exp) target linkage))
    ((lambda? exp) (compile-lambda exp target linkage))
    ((set? exp) (compile-set exp target linkage))
    ((begin? exp) (compile-begin exp target linkage))
    ((import? exp) (compile-import exp target linkage))
    ((define? exp) (compile-define exp target linkage))
    ((application? exp) (compile-application exp target linkage))
    ((define-library? exp) (compile-library exp target linkage))
    (else
      (error "Unknown expression: " exp))))

(define (tee . args)
  (apply println args)
  args)

(define (pprint x)
  (print "(" (car x))
  (for-each (lambda (x) (prints "\n  " x)) (cdr x))
  (println ")"))

(define (tagged-list? symbol exp)
  (and (pair? exp)
       (eq? symbol (car exp))))

(define (define-library? exp)
  (tagged-list? 'define-library exp))

(define (quote? exp)
  (tagged-list? 'quote exp))

(define (if? exp)
  (tagged-list? 'if exp))

(define (cond? exp)
  (tagged-list? 'cond exp))

(define (lambda? exp)
  (tagged-list? 'lambda exp))

(define (set? exp)
  (tagged-list? 'set! exp))

(define (application? exp)
  (and (pair? exp)
       (symbol? (car exp))))

(define (define? exp)
  (tagged-list? 'define exp))

(define (begin? exp)
  (tagged-list? 'begin exp))

(define (import? exp)
  (tagged-list? 'import exp))

(define (compile-begin exp target linkage)
  (tee
    (map (lambda (x)
           (compile x target linkage)) (cdr exp))))

(define (compile-quote exp target linkage)
  (println "compile-quote: " exp)
  exp)

(define (compile-library exp target linkage)
  (error "define-library not supported"))

(define (compile-define exp target linkage)
  (let* ((name (caadr exp))
         (args (cdadr exp))
         (body (cons 'begin (cddr exp))))
     (tee `(define (name: ,name)
                   (args: ,args)
                   (body: ,(compile body target linkage))))))

(define (compile-if exp target linkage)
  (tee `(if-statement ,exp)))

(define (compile-lambda exp target linkage)
  (error "compile-lambda"))

(define (compile-application exp target linkage)
  (let ((name (car exp))
        (args (cdr exp)))
   (tee
    `(apply (name ,name)
            (args ,(map (lambda (x)
                          (compile x target linkage)) args))))))

(define (compile-set exp target linkage)
  (println "compile-set: " exp))

(define (compile-import exp target linkage)
  (map
    (lambda (import-name)
      (tee `(import ,import-name))) (cdr exp)))

(define (named-let? exp)
  (and (tagged-list? 'let exp)
       (symbol? (cadr exp))))

(define (transform-named-let exp)
  (error "Unimplemented"))

(define (transform-let exp)
  (if (named-let? exp)
      (transform-named-let exp)
      (let*
        ((args (cadr exp))
         (arg-names (map car args))
         (arg-values (map cadr args))
         (body (cddr exp)))
        `((lambda ,arg-names
            (begin ,@body)) ,@arg-values))))

(define (transform-cond exp)
  (transform-cond-clauses (cdr exp)))

(define (sequence->exp exp)
  (if (and (pair? exp) (pair? (cdr exp)))
      `(begin ,@exp)
      (car exp)))

(define (transform-cond-clauses exp)
  ;; ((<test1> <cons1>)) ==> (if <test1> (begin <cons1>) ...)
  (if (null? exp) '()
    (if (eq? 'else (caar exp)) (cadar exp)
        `(if ,(caar exp)
             ,(sequence->exp (cdar exp))
             ,(transform-cond-clauses (cdr exp))))))

(define (main)
  (let ((exp (read)))
    (if (not (eof-object? exp))
      (begin
        (compile exp 'main 'next)
        (main)))))

(main)
