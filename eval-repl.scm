;; A simple small Scheme-like interpreter

(import
  (scheme base)
  (scheme write)
  (scheme read)
  (scheme cxr)
  (print)
  (compiler))

(define (evlis args env)
  (map (lambda (exp)
          (eval exp env))
       args))

(define (eval-application op args env)
  (apply-op op (evlis args env)))

(define procedures
  `((+ ,+)
    (* ,*)
    (display ,(lambda args (apply display args) '()))
    (newline ,(lambda () (newline) '()))
    (println ,(lambda args (apply println args) '()))))

(define (apply-op op args)
  (let ((pair (assq op procedures)))
    (if pair (apply (cadr pair) args)
             (raise-continuable (list "Unknown procedure: " op)))))

(define (eval-self exp)
  exp)

(define (eval exp env)
  (cond
    ;; Observe that some entries here are order dependent (e.g. application?
    ;; must come before definition?, etc.)
    ((self-evaluating? exp) (eval-self exp))
    ((variable? exp) (env 'lookup exp))
    ((definition? exp)
     (let ((name (definition-variable exp))
           (value (eval (definition-value exp) env)))
        (env 'add name value)
        '()))
    ((application? exp) (eval-application (car exp) (cdr exp) env))
    (else
      (raise-continuable (list "Unknown type of expression: " exp)))))

(define (environment)
  (let ((definitions '()))
    (define (add name value)
      (set! definitions (append definitions `((,name ,value)))))

    (define (lookup name)
      (let ((result (assq name definitions)))
        (if result (cadr result) #f)))

    (define (names)
      (map car definitions))

    (lambda (action . args)
      (cond
        ((eq? action 'add) (apply add args))
        ((eq? action 'list) definitions)
        ((eq? action 'names) (names))
        ((and (eq? action 'lookup) (pair? args)) (lookup (car args)))
        (else (raise "Environment: Unknown action"))))))

(define (repl env)
  (println "Environment:"
    (apply string-append
      (map (lambda (s)
             (string-append " " (symbol->string s)))
           (env 'names))))
  (print "> ")
  (let ((code (read)))
    (with-exception-handler
      (lambda (con)
        (if (list? con)
            (begin
              (apply println "Error: " con)
              '())
            (raise con)))
      (lambda ()
        (if (not (eof-object? code))
          (let ((result (eval code env)))
            (if (not (null? result))
                (println result))
            (repl env)))))))

(define (main)
  (println "A simple Scheme interpreter written in Scheme")
  (println "Supported procedures:"
    (apply string-append (map (lambda (s)
                                (string-append " " (symbol->string (car s))))
                              procedures)))
  (repl (environment)))

(main)
