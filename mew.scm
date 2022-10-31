(module mew
  (export
     accumulate at
     comp
     dec def div
     empty? eof esc
     fin final for generic-for-each
     get gen genumerate gfix given giterate gmatch group-by-accumulator
     gsplit gslice-when gwindow
     inc into
     keys
     len loc
     mod
     negate nth
     one-of op
     per prn puts
     rep
     set str slurp
     tally-accumulator tbl time
     while
     until
     vals
     -> ->> fun-> fun->> set-> set->>
     <>?
     ~?)

  (import-for-syntax matchable)

  (import scheme
          (rename (chicken base)
             (print puts)
             (complement negate)
             (compose comp))
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
  (reexport srfi-69)
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
  (reexport (chicken io))
  (reexport (chicken irregex))
  (reexport (chicken pretty-print))

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

  (define <>?
    (case-lambda
      ((a b) (not (equal? a b)))
      ((a b c) (and (not (equal? a b))
                    (not (equal? b c))
                    (not (equal? c a))))
      ((a b c d) (and (not (equal? a b))
                      (not (equal? a c))
                      (not (equal? a d))
                      (not (equal? b c))
                      (not (equal? b d))
                      (not (equal? c d))))
      ((a b c d . rest)
       (call-with-current-continuation
        (lambda (return)
          (let ((seen (tbl)))
            (for-each (lambda (o)
                        (when (hash-table-update!/default seen o not #t)
                          (return #f)))
                      (apply list a b c d rest)))
          #t)))
      ))

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

  (define-syntax set
    (syntax-rules ()
      ((_ id expr)
       (let ((val expr))
         (set! id val)
         val))))

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

  (define-syntax op
    (er-macro-transformer
      (lambda (expr rename compare)
        (match expr
          ((_) (rename 'values))
          ((_ x) `(,(rename 'lambda) ... ,x))
          ((_ . rest) `(,(rename 'lambda) (_) ,rest))))))

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

  (define get
    (case-lambda
      ((o k)
       (cond ((list? o) (list-ref o k))
             ((vector? o) (vector-ref o k))
             ((hash-table? o) (hash-table-ref o k))
             ((string? o) (string-ref o k))
             (#t (error "no at defined"))))
      ((o k default)
       (cond ((list? o) (list-ref-default o k default))
             ((vector? o) (vector-ref-default o k default))
             ((hash-table? o) (hash-table-ref/default o k default))
             ((string? o) (string-ref-default o k default))
             (#t (error "no at defined"))))))

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
          ((hash-table? obj) (lambda (f h)
                               (hash-table-for-each h (lambda (k v)
                                                        (f (cons k v))))))
          ((procedure? obj) generator-for-each)
          (#t (error "no generic-for-each defined"))))

  (define-syntax for
    (syntax-rules ()
      ((_ (i obj) body ...)
       (let ((o obj))
         ((generic-for-each o) (match-lambda (i body ...)) o)))))

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

  (define (gslice-when pred gen)
    (let ((slice #f)
          (prev #f)
          (this #f))
      (lambda ()
        (unless slice
          (set! prev (gen))
          (when (eof-object? prev)
            (set! this (eof)))
          (set! slice (list prev)))
        (if (eof-object? this)
            this
            (let loop ()
              (set! this (gen))
              (if (eof-object? this)
                  (reverse slice)
                  (if (pred prev this)
                      (let ((finished-slice (reverse slice)))
                        (set! slice (list this))
                        (set! prev this)
                        finished-slice)
                      (begin
                        (set! slice (cons this slice))
                        (set! prev this)
                        (loop)))))))))

  (define (genumerate gen)
    (let ((n -1))
      (lambda ()
        (let ((val (gen)))
          (if (eof-object? val)
            val
            (begin
              (set! n (inc n))
              (cons n val)))))))

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
    (syntax-rules ()
      ((_ rest ...)
       (->chunk () () (rest ...)))))

  (define-syntax ->>
    (syntax-rules ()
      ((_ rest ...)
       (->chunk () () (rest ...)))))

  (define-syntax ->chunk
    (syntax-rules (-> ->>)
      ((_ (result ...) (current ...) (-> rest ...))
       (->chunk (result ... (current ...)) (->) (rest ...)))
      ((_ (result ...) (current ...) (->> rest ...))
       (->chunk (result ... (current ...)) (->>) (rest ...)))
      ((_ (result ...) (current ...) (a rest ...))
       (->chunk (result ...) (current ... a) (rest ...)))
      ((_ ((x) result ...) (current ...) ())
       (->thread x (result ... (current ...))))
      ((_ (x result ...) (current ...) ())
       (->thread x (result ... (current ...))))))

  (define-syntax ->thread
    (syntax-rules (-> ->>)
      ((_ result ((-> f args ...) rest ...))
       (->thread (f result args ...) (rest ...)))
      ((_ result ((->> f args ...) rest ...))
       (->thread (f args ... result) (rest ...)))
      ((_ result ())
       result)))

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

  (define-syntax given
    (syntax-rules ()
      ((_ expr bool (then . then-rest) (else . else-rest))
       (let ((val expr))
         (if bool
           (then val . then-rest)
           (else val . else-rest))))
      ((_ expr bool (then . then-rest) else)
       (given expr bool (then . then-rest) (else)))
      ((_ expr bool then (else . else-rest))
       (given expr bool (then) (else . else-rest)))
      ((_ expr bool then else)
       (given expr bool (then) (else)))
      ((_ expr bool then)
       (given expr bool then ((op))))
      ))

  (define (~? str pat)
    (let ((data (irregex-search pat str)))
      (if data
        (map (op irregex-match-substring data _)
             (iota (inc (irregex-match-num-submatches data))))
        #f)))

  (define (gmatch pat str)
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

  (define (gsplit pat str . max)
    (let ((start 0)
          (n 1)
          (max (optional max -1)))
      (lambda ()
        (cond ((< start 0)   (eof))
              ((<= 0 max n)  (let ((s (substring str start)))
                               (set! start -1)
                               s))
              (else          (let ((data (irregex-search pat str start)))
                               (if data
                                 (let ((s (substring str start (irregex-match-start-index data 0))))
                                   (set! n (inc n))
                                   (if (equal? s "")
                                     (begin
                                       (set! s (substring str start (inc start)))
                                       (set! start (inc start)))
                                     (set! start (irregex-match-end-index data 0)))
                                   (when (= start (len str))
                                     (set! start -1))
                                   s)
                                 (begin
                                   (let ((s (substring str start)))
                                     (set! start -1)
                                     s)))))))))

  (define (slurp io)
    (cond ((not io)          (read-string #f (current-input-port)))
          ((input-port? io)  (read-string #f io))
          ((string? io)      (with-input-from-file io
                               (lambda ()
                                 (read-string #f (current-input-port)))))
          (else              (error "no slurp defined"))))

  (define (hash-table->generator h)
    (make-for-each-generator (lambda (f t)
                               (hash-table-for-each t (lambda (k v)
                                                        (f (cons k v)))))
                             h))

  (define (gen o . rest)
    (cond ((list? o)       (apply list->generator o rest))
          ((vector? o)     (apply vector->generator o rest))
          ((string? o)     (apply string->generator o rest))
          ((hash-table? o) (apply hash-table->generator o rest))
          ((procedure? o)  o)
          (else            (error "no gen defined"))))

  (define (generic-make-accumulator a)
    (cond ((procedure? a) a)
          ((list? a)      (make-accumulator cons (reverse a) reverse))
          ((vector? a)    (make-accumulator cons (reverse (vector->list a))
                                            (lambda (x)
                                              (list->vector (reverse x)))))
          ((hash-table? a) (make-accumulator (lambda (kv h)
                                               (hash-table-set! h (car kv) (cdr kv))
                                               h)
                                             a
                                             (op)))
          ((string? a)     (make-accumulator cons (reverse (string->list a))
                                             (lambda (lst) (list->string (reverse lst)))))
          (else            (error "no make-accumulator defined"))))

  (def (into acc . generators)
    (let ((acc (generic-make-accumulator acc))
          (gen (apply gappend (map gen generators))))
      (let loop ((val (gen)))
        (acc val)
        (if (not (eof-object? val))
          (loop (gen))
          (acc val)))))

  (define-syntax accumulate
    (syntax-rules ()
      ((_ (var init) body ...)
       (let ((var (generic-make-accumulator init)))
         body ...
         (var (eof))))))

  (define (tally-accumulator)
    (let ((tally (tbl)))
      (lambda (x)
        (if (eof-object? x)
            tally
            (hash-table-update!/default tally x inc 0)))))

  (define (group-by-accumulator f)
    (let ((groups (tbl)))
      (lambda (x)
        (if (eof-object? x)
            groups
            (hash-table-update!/default groups (f x) (op cons x _) '())))))

  (define-syntax one-of
    (er-macro-transformer
     (lambda (expr rename compare)
       `(,(rename 'lambda) (x)
         (,(rename 'or) ,@(map (lambda (v)
                                 `(,(rename 'equal?) x ,v))
                               (cdr expr)))))))

  (define (per . args)
    (apply comp (reverse args)))
)
