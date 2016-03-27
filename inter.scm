(import
  (scheme base)
  (scheme write)
  (scheme read)
  (scheme cxr)
  (scheme eval)
  (print))

(define (repl)
  (display "> ")
  (let ((code (read)))
    (if (not (eof-object? code))
      (begin
        (println (eval code))
        (repl)))))

(repl)
