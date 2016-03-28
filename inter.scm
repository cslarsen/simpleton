;; A simple interpreter for Scheme (well, not all of it yet)

(import
  (scheme base)
  (scheme write)
  (scheme read)
  (scheme cxr)
  (print))

(define (self-evaluating? exp)
  (cond
    ((number? exp) #t)
    ((string? exp) #t)
    (else #f)))

(define (evlis args env)
  (map (lambda (x) (eval x env)) args))

(define (eval-application op args env)
  (apply-op op (evlis args env)))

(define (apply-op op args)
  (cond
   ((eq? op '+) 
    (begin
      (println "execing + w/args" args)
      (apply + args)))
   (error "unsupported op: " op)))

(define (application? exp)
  (and (pair? exp)
       (symbol? (car exp))))

(define (eval-self exp)
  (println "eval self: " exp)
  exp)

(define (eval exp env)
  (cond
    ((self-evaluating? exp) (eval-self exp))
    ((application? exp) (eval-application (car exp) (cdr exp) env))
    (else
      (error "unknown: " exp))))

(define (empty-environment)
  '())

(define (repl)
  (display "> ")
  (let ((code (read)))
    (if (not (eof-object? code))
      (begin
        (println (eval code (empty-environment)))
        (repl)))))

(repl)
