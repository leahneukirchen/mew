(module mew (at dec def div esc fin get inc loc mod nth op prn puts rep str tbl while until)
  (import scheme
          (rename (chicken base)
             (print puts))
          (chicken module)
          (chicken port)
          srfi-17
          srfi-69
          matchable)

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
      (set! set)
      (begin seq)
      (lambda fun)
      (apply app)
      (equal? =?)
      ))

  (define (inc i)
    (+ i 1))
  
  (define (dec i)
    (- i 1))

  (define (nth n lst)
    (list-ref lst n))

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

  (define-syntax while
    (syntax-rules ()
      ((_ cond body ...)
       (let loop ((c cond))
         (if c
           (begin
             body ...
             (loop cond)))))))

  (define-syntax until
    (syntax-rules ()
      ((_ cond body ...)
       (while (not cond) body ...))))

  (define (list-ref-default l i default)
    (if (zero? i)
        (if (null? l)
            default
            (car l))
        (list-ref-default (cdr l) (- i 1) default)))

  (define (vector-ref-default v i default)
    (if (< i (vector-length v))
        (vector-ref v i)
        default))

  (define (string-ref-default s i default)
    (if (< i (string-length s))
        (string-ref s i)
        default))

  (define (get o k . default)
    (if (null? default)
        (cond ((list? o) (list-ref o k))
              ((vector? o) (vector-ref o k))
              ((hash-table? o) (hash-table-ref o k))
              ((string? o) (string-ref o k))
              (#t (error "no at defined")))
        (cond ((list? o) (list-ref-default o k (car default)))
              ((vector? o) (vector-ref-default o k (car default)))
              ((hash-table? o) (hash-table-ref/default o k (car default)))
              ((string? o) (string-ref-default o k (car default)))
              (#t (error "no at defined")))))

  (define (get-setter o k v)
    (cond ; no list-set!
          ((vector? o) (vector-set! o k v))
          ((hash-table? o) (hash-table-set! o k v))
          ((string? o) (string-set! o k v))
          (#t (error "not set for at defined"))))

  (define at (getter-with-setter get get-setter))

  (define (kvs->alist kvs)
    (let loop ((kvs kvs))
      (match kvs
        ((k v . kvs2) (cons (list k v) (loop kvs2)))
        (()           '())
        (_            (error "odd key value list")))))

  (define (tbl . kvs)
    (alist->hash-table (kvs->alist kvs)))
)
