(import
  (scheme base)
  (scheme cxr)
  (scheme read)
  (scheme write))

(define indent
  (make-parameter 0
    (lambda (count)
      (make-string (* 2 count) #\ ))))

(define (println . args)
  (display (indent))
  (for-each display args)
  (newline))

(define (translate-define-function code)
  (println "function " code)
  (parameterize ((indent 1))
    (println "name: " (caadr code))
    (println "args: " (cdadr code))
    (println "args count: " (length (cdadr code)))
    (println "body: " (translate (cddr code)))))

(define (function? code)
  (and (equal? (car code) 'define)
    (list? (cadr code))))

(define (translate code)
  (let ((head (car code)))
    (cond
     ((function? code) (translate-define-function code))
     (else code))))

(define (translate-all)
  (let loop
    ((body '())
     (code (read)))
    (if (eof-object? code)
      body
      (loop (append body (translate code))
        (read)))))

(let
  ((body (translate-all)))
  (println "body: " body))
