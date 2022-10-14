(module mew
  (export
     at
     dec def div
     empty? eof esc
     fin final for generic-for-each
     get gfix giterate gmatch gsplit gwindow
     inc
     keys keyvals
     len loc
     mod
     nth
     op
     prn puts
     rep
     str slurp
     tbl time
     while
     until
     vals
     -> ->> fun-> fun->> set-> set->>
     ~?)

  (import-for-syntax matchable)

  (import scheme
          (rename (chicken base)
             (print puts))
          (chicken module)
          (chicken syntax)
          (chicken port)
          srfi-17
          (rename (srfi-69)
             (hash-table-keys keys)
             (hash-table-values vals))
          srfi-158
          matchable)

  (reexport srfi-1)
  (reexport srfi-158)
  (reexport
    (rename (srfi-158)
      (make-range-generator range)
      (circular-generator cycle)))
  (reexport matchable)
  (reexport
    (rename (matchable)
      (match-lambda match-fun)
      (match-lambda* match-fun*)))
  (reexport (chicken irregex))

  (reexport
    (only (chicken base)
      unless
      when
      rec))

  (reexport
    (only (chicken time)
      time))

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
       (match-let ((x y)) (loc brest . rest)))))

  ;; possible additions: multiple _, allow ... for rest
  (define-syntax op
    (er-macro-transformer
      (lambda (expr rename compare)
        `(,(rename 'lambda) (_)
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
        ((k v . kvs2) (cons (cons k v) (loop kvs2)))
        (()           '())
        (_            (error "odd key value list")))))

  (define (tbl . kvs)
    (alist->hash-table (kvs->alist kvs)))

  (define (keyvals h)
    (reverse (hash-table-fold h (lambda (k v l) (cons v (cons k l))) '())))

  (define (empty? o)
    (or (null? o)
        (equal? o "")
        (equal? o #())
        (and (hash-table? o)
             (zero? (hash-table-size o)))))

  (define (len o)
    (cond ((list? o) (length o))
          ((string? o) (string-length o))
          ((vector? o) (vector-length o))
          ((hash-table? o) (hash-table-size o))
          ((procedure? o) (generator-count (op #t) o))
          (#t (error "no len defined"))))

  (define (generic-for-each obj)
    (cond ((list? obj) for-each)
          ((vector? obj) vector-for-each)
          ((hash-table? obj) (lambda (f h) (hash-table-for-each h f)))
          ((procedure? obj) generator-for-each)
          (#t (error "no generic-for-each defined"))))

  (define-syntax for
    (syntax-rules ()
      ((_ ((i j) obj) body ...)
       (let ((o obj))
         ((generic-for-each o) (lambda (i j) body ...) o)))
      ((_ (i obj) body ...)
       (let ((o obj))
         ((generic-for-each o) (lambda (i) body ...) o)))))

  (define (eof) #!eof)

  (define (gwindow gen n)
    (let ((window #f))
      (lambda ()
        (if (not window)
          (begin
            (set! window (generator->list gen n))
            (if (= (len window) n)
              window
              (eof)))
          (let ((next (gen)))
            (if (eof-object? next)
              (eof)
              (begin
                (set! window (append (cdr window) (list next)))
                window)))))))

  (define (giterate f x)
    (make-unfold-generator (op #f) (op) f x))

  (define (gfix g)
    (let ((prev (if #f #f)))
      (gmap (lambda (x)
              (if (equal? prev x)
                (eof)
                (begin
                  (set! prev x)
                  x)))
            g)))

  (define (final g)
    (generator-fold (lambda (x a) x) (if #f #f) g))

  (define-syntax ->
    (er-macro-transformer
     (lambda (expr rename compare)

       (define (->? sym)
         (compare sym (rename '->)))

       (define (->>? sym)
         (compare sym (rename '->>)))

       (define (pass1 a b v)
         (match v
           ('()
            (reverse (cons (reverse b) a)))
           (((and (or (? ->?) (? ->>?)) arr) . rest)
            (pass1 (cons (reverse b) a) `(,arr) rest))
           ((other . rest)
            (pass1 a (cons other b) rest))))

       (define (pass2 a v)
         (match v
           ('()
            a)
           ((((? ->?) h . t) . rest)
            (pass2 `(,h ,a ,@t) rest))
           ((((? ->>?) . t) . rest)
            (pass2 `(,@t ,a) rest))))

       (let ((r (pass1 '() '() (cdr expr))))
         (pass2 (if (= (length (car r)) 1)
                  (caar r)
                  (car r))
                (cdr r))))))

  (define-syntax ->>
    (syntax-rules ()
      ((_ . rest)
       (-> . rest))))

  (define-syntax fun->
    (syntax-rules ()
      ((_ rest ...)
       (lambda (x) (-> x -> rest ...)))))

  (define-syntax fun->>
    (syntax-rules ()
      ((_ rest ...)
       (lambda (x) (->> x ->> rest ...)))))

  (define-syntax set->
    (syntax-rules (-> ->>)
      ((_ location -> rest ...)
       (set! location (-> location -> rest ...)))
      ((_ location ->> rest ...)
       (set! location (-> location ->> rest ...)))
      ((_ location rest ...)
       (set! location (-> location -> rest ...)))))

  (define-syntax set->>
    (syntax-rules (-> ->>)
      ((_ location -> rest ...)
       (set! location (->> location -> rest ...)))
      ((_ location ->> rest ...)
       (set! location (->> location ->> rest ...)))
      ((_ location rest ...)
       (set! location (->> location ->> rest ...)))))

  (def (~? str pat)
    (let ((data (irregex-search pat str)))
      (if data
        (map (op irregex-match-substring data _)
             (iota (inc (irregex-match-num-submatches data))))
        #f)))

  (def (gmatch pat str)
    (let ((start 0))
      (lambda ()
        (if (<= 0 start (string-length str))
          (let ((data (irregex-search pat str start)))
            (if data
              (begin
                (set! start (irregex-match-end-index data 0))
                (when (= (irregex-match-start-index data 0)
                         (irregex-match-end-index data 0))
                  (set! start (inc start)))
                (if (> (irregex-match-num-submatches data) 0)
                  (map (op irregex-match-substring data _)
                       (iota (inc (irregex-match-num-submatches data))))
                  (irregex-match-substring data 0)))
              (begin
                (set! start -1)
                (eof))))
          (eof)))))

  (def (gsplit pat str . max)
    (let ((start 0)
          (n 1)
          (max (if (null? max) -1 (car max))))
      (lambda ()
        (if (or (>= start 0)
                (and (> max 0) (< n max)))
          (let ((data (irregex-search pat str start)))
            (if (and data (or (< max 0) (< n max)))
              (let ((s (substring str start (irregex-match-start-index data 0))))
                (set! n (inc n))
                (set! start (irregex-match-end-index data 0))
                (when (= start (len str))
                  (set! start -1))
                s)
              (let ((s (substring str start)))
                (set! n (inc n))
                (set! start -1)
                s)))
          (eof)))))

  (def (slurp io)
    (cond ((not io)          (read-string #f (current-input-port)))
          ((input-port? io)  (read-string #f io))
          ((string? io)      (with-input-from-file io
                               (lambda ()
                                 (read-string #f (current-input-port)))))
          (else              (error "no slurp defined"))))

)
