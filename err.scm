; Influenced by Rust, Haskell, Racket rebellion/results, JS promises

(module err (err unerr err? ok? ok=> err=> ok/if ok ok/or ok/and guard-err)

(import
  (scheme)
  (chicken base)
  (chicken format)
  (chicken condition))

(define-record-type <err>
  (%make-err data)
  err?
  (data %err-data))

(set-record-printer! <err>
  (lambda (x out)
    (fprintf out "#<err ~s>" (%err-data x))))

(define (err x)
  (if (err? x)
    x
    (%make-err x)))

(define (ok? e)
  (not (err? e)))

(define (unerr e)
  (if (err? e)
    (%err-data e)
    (if #f #f)))

(define ok/if
  (case-lambda
    ((x when-ok)
     (if (err? x)
       x
       (when-ok x)))
    ((x when-ok when-err)
     (if (err? x)
       (when-err (unerr x))
       (when-ok x)))))

(define (ok=> x . fs)
  (if (null? fs)
    x
    (if (ok? x)
      (apply ok=> ((car fs) x) (cdr fs))
      x)))

(define (err=> x . fs)
  (if (ok? x)
    x
    (let loop ((x (unerr x))
               (fs fs))
      (if (null? fs)
        x
        (loop ((car fs) x) (cdr fs))))))

(define (ok x)
  (err=> x (lambda (e)
             (if (condition? e)
               (abort e)
               (error e)))))

(define-syntax ok/or
  (syntax-rules ()
    ((_)            (err (if #f #f)))
    ((_ a)          a)
    ((_ a b)        (let ((va a))
                      (if (err? va)
                        b
                        va)))
    ((_ a b c ...)  (ok/or (ok/or a b) c ...))))

(define-syntax ok/and
  (syntax-rules ()
    ((_)            #t)
    ((_ a)          a)
    ((_ a b)        (let ((va a))
                      (if (err? va)
                        va
                        b)))
    ((_ a b c ...)  (ok/and (ok/and a b) c ...))))

(define-syntax guard-err
  (syntax-rules ()
    ((_ expr)
     (guard-err expr ()))
    ((_ expr conditions ...)
     (condition-case expr
       (exn conditions (err exn)) ...))))

)
