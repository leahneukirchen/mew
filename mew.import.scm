(module mew (dec def div esc fin inc loc mod op prn puts rep str)
  (import scheme
          (rename (chicken base)
             (print puts))
          (chicken module)
          (chicken port))

  (reexport
    (only (chicken base)
      unless
      when
      rec))

  (import
    (rename (r7rs)
      (floor-quotient div)
      (floor-remainder mod)))

  (reexport
    (rename (scheme)
;     (define def)
      (set! set)
      (begin seq)   ; XXX return #f for (seq)
      (lambda fun)
      (apply app)
      ))

  (define (inc i)
    (+ i 1))
  
  (define (dec i)
    (- i 1))

  (define (str . args)
    (with-output-to-string
      (lambda ()
        (for-each display args))))

  (define (prn . args)
    (if (null? args)
        (newline)
        (begin
          (write (car args))
          (unless (null? (cdr args))
            (display " "))
          (apply prn (cdr args)))))
  
  (define-syntax def                           
    (syntax-rules ()
      ((_ . rest)
       (define . rest))))

  (define-syntax esc
    (syntax-rules ()
      ((_ return body ...)
       (call-with-current-continuation
         (lambda (return)
           body ...)))))

  (define-syntax fin
    (syntax-rules ()
      ((_ form rest ...)
       (dynamic-wind
         (lambda () (begin))
         (lambda () form)
         (lambda () rest ...)))))

  (define-syntax loc
    (syntax-rules ()
      ((_ () . rest)
       (let () . rest))
      ((_ (x y . brest) . rest)
       (let ((x y)) (loc brest . rest)))))

  ;; possible additions: multiple _, allow ... for rest
  (define-syntax op
    (er-macro-transformer
      (lambda (expr rename compare)
        `(lambda (_)
           ,@(cond ((= 1 (length expr)) '(_))
                   ((= 2 (length expr)) (cdr expr))
                   (#t (list (cdr expr))))))))

  (define-syntax rep
    (syntax-rules ()
      ((_ name ((var val) ...) body ...)
       (let name ((var val) ...) body ...))))
)
