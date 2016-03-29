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
    ((lambda? exp) (compile-lambda exp target linkage))
    ((set? exp) (compile-set exp target linkage))
    ((begin? exp) (compile-begin exp target linkage))
    ((import? exp) (compile-import exp target linkage))
    ((define? exp) (compile-define exp target linkage))
    ((application? exp) (compile-application exp target linkage))
    (else
      (error exp))))

(define (tagged-list? symbol exp)
  (and (pair? exp)
       (eq? symbol (car exp))))

(define (quote? exp)
  (tagged-list? 'quote exp))

(define (if? exp)
  (tagged-list? 'if exp))

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
  (println "compile-begin: " exp)
  (for-each compile (cdr exp)))

(define (compile-quote exp target linkage)
  (println "compile-quote: " exp)
  exp)

(define (compile-define exp target linkage)
  (let ((name (caadr exp))
        (args (cdadr exp))
        (body (cddr exp)))
    (println "define: " name)
    `(compile-define
        (name ,name)
        (args ,args)
        (body ,body))))

(define (compile-if exp target linkage)
  (println "compile-if: " exp))

(define (compile-lambda exp target linkage)
  (println "compile-lambda: " exp))

(define (compile-application exp target linkage)
  (let ((name (car exp))
        (args (cdr exp)))
    (println "application: " name)
    `(apply (name ,name)
            (args ,(map compile args)))))

(define (compile-set exp target linkage)
  (println "compile-set: " exp))

(define (compile-import exp target linkage)
  (println "compile-import: " exp))

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

(define (main)
  (let ((exp (read)))
    (if (not (eof-object? exp))
      (begin
        (compile exp 'main 'next)
        (main)))))

(main)
