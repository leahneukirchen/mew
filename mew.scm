(module mew
  (export
     act accumulate at
     comp
     dec def del-at div
     empty? eof esc
     fin final for fun*
     gconcatenate gen generic-for-each genumerate get gfix giterate gmatch
     group-by-accumulator gslice-when gsplit gwindow
     inc inject into
     juxt
     keys
     len loc
     mod
     negate
     one-of op op*
     per prn puts
     rep
     sing? set set-at str slurp
     tally-accumulator tbl time
     while
     uniq-accumulator unlist until
     vals
     -> fun-> fun->> set->
     <>?
     ~?

     generic-make-accumulator)

  (import-for-syntax srfi-1)
  (import-for-syntax matchable)

  (import scheme
          (rename (chicken base)
             (print puts)
             (complement negate)
             (compose comp))
          (chicken module)
          (chicken port)
          (chicken repl)
          (chicken syntax)
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
  (reexport (chicken sort))

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
      (ceiling ceil)
      ))

  (define (inc i)
    (+ i 1))

  (define (dec i)
    (- i 1))

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

  (define-syntax fun*
    (syntax-rules ()
      ((_ args body rest ...)
       (match-lambda* (args body rest ...)))))

  (define-syntax op
    (er-macro-transformer
      (lambda (expr rename compare)
        (match expr
          ((_) (rename 'values))
          ((_ x) `(,(rename 'lambda) ... ,x))
          ((_ . rest) `(,(rename 'lambda) (_) ,rest))))))

  (define-syntax op*
    (er-macro-transformer
     (lambda (expr rename compare)
       (when (= 1 (length expr))
         (syntax-error "op* needs at least one argument"))
       `(,(rename 'lambda) ...
         ,(receive (pre post) (break (lambda (x) (compare '... x)) (cdr expr))
            (match post
              (()      `(,(rename apply) ,@pre ...))
              ((x)     `(,(rename apply) ,@pre ...))
              ((x . y) `(,(rename apply) ,@pre
                         (,(rename append) ... (,(rename list) ,@y))))
              ))))))

  (define-syntax rep-internal
    (syntax-rules ()
      ((_ (bindings ...) name () (body ...))
       (let name (bindings ...) body ...))
      ((_ (bindings ...) name (x y brest ...) body)
       (rep-internal (bindings ... (x y)) name (brest ...) body))))

  (define-syntax rep
    (syntax-rules ()
      ((_ name (bindings ...) body ...)
       (rep-internal () name (bindings ...) (body ...)))))

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
    (if (null? l)
      default
      (if (zero? i)
        (car l)
        (list-ref-default (cdr l) (- i 1) default))))

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
          (#t (error "no set for at defined"))))

  (define at (getter-with-setter get get-setter))

  (define (kvs->alist kvs)
    (let loop ((kvs kvs))
      (match kvs
        ((k v . kvs2) (cons (cons k v) (loop kvs2)))
        (()           '())
        (_            (error "odd key value list")))))

  (define (tbl . kvs)
    (alist->hash-table (kvs->alist kvs)))

  (define (set-at o . rest)
    (cond ((hash-table? o) (for-each (lambda (kv)
                                       (hash-table-set! o (car kv) (cdr kv)))
                                     (kvs->alist rest)))
          ((vector? o)     (for-each (lambda (kv)
                                       (vector-set! o (car kv) (cdr kv)))
                                     (kvs->alist rest)))
          ((string? o)     (for-each (lambda (kv)
                                       (string-set! o (car kv) (cdr kv)))
                                     (kvs->alist rest)))
          (else            (error "no set-at defined")))
    o)

  (define (del-at o . rest)
    (cond ((hash-table? o) (for-each (lambda (k)
                                       (hash-table-delete! o k))
                                     rest))
          (else            (error "no del-at defined")))
    o)

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

  (define (gconcatenate gen)
    (let ((gen2 #f))
      (lambda ()
        (unless gen2
          (set! gen2 (gen)))
        (let loop ()
          (if (eof-object? gen2)
            gen2
            (let ((v (gen2)))
              (if (eof-object? v)
                (begin
                  (set! gen2 (gen))
                  (loop))
                v)))))))

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

  (define-syntax and-apply
    (syntax-rules ()
      ((_ x f args ...)
       (let ((v x))
         (if v
           (f v args ...)
           #f)))))

  (define-syntax and-apply-last
    (syntax-rules ()
      ((_ x f args ...)
       (let ((v x))
         (if v
           (f args ... v)
           #f)))))

  (define-syntax if-apply
    (syntax-rules ()
      ((_ expr bool (then . then-rest) (else . else-rest))
       (let ((val expr))
         (if bool
           (then val . then-rest)
           (else val . else-rest))))
      ((_ expr bool (then . then-rest) else)
       (if-apply expr bool (then . then-rest) (else)))
      ((_ expr bool then (else . else-rest))
       (if-apply expr bool (then) (else . else-rest)))
      ((_ expr bool then else)
       (if-apply expr bool (then) (else)))
      ((_ expr bool then)
       (if-apply expr bool then ((op))))
      ))

  (define-syntax if-apply-last
    (syntax-rules ()
      ((_ expr bool (then then-rest ...) (else else-rest ...))
       (let ((val expr))
         (if bool
           (then then-rest ... val)
           (else else-rest ... val))))
      ((_ expr bool (then . then-rest) else)
       (if-apply-last expr bool (then . then-rest) (else)))
      ((_ expr bool then (else . else-rest))
       (if-apply-last expr bool (then) (else . else-rest)))
      ((_ expr bool then else)
       (if-apply-last expr bool (then) (else)))
      ((_ expr bool then)
       (if-apply-last expr bool then ((op))))
      ))

  (define-syntax ->
    (syntax-rules ()
      ((_ rest ...)
       (->chunk () () (rest ...)))))

  (define-syntax ->chunk
    (syntax-rules (-> ->> and-> and->> if-> if->>)
      ((_ (result ...) (current ...) (-> rest ...))
       (->chunk (result ... (current ...)) (->) (rest ...)))
      ((_ (result ...) (current ...) (->> rest ...))
       (->chunk (result ... (current ...)) (->>) (rest ...)))
      ((_ (result ...) (current ...) (and-> rest ...))
       (->chunk (result ... (current ...)) (-> and-apply) (rest ...)))
      ((_ (result ...) (current ...) (and->> rest ...))
       (->chunk (result ... (current ...)) (-> and-apply-last) (rest ...)))
      ((_ (result ...) (current ...) (if-> rest ...))
       (->chunk (result ... (current ...)) (-> if-apply) (rest ...)))
      ((_ (result ...) (current ...) (if->> rest ...))
       (->chunk (result ... (current ...)) (-> if-apply-last) (rest ...)))
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
       (lambda (x) (-> x ->> rest ...)))))

  (define-syntax set->
    (syntax-rules (-> ->> and-> and->> if-> if->>)
      ((_ location -> rest ...)
       (set! location (-> location -> rest ...)))
      ((_ location ->> rest ...)
       (set! location (-> location ->> rest ...)))
      ((_ location and-> rest ...)
       (set! location (-> location and-> rest ...)))
      ((_ location and->> rest ...)
       (set! location (-> location and->> rest ...)))
      ((_ location if-> rest ...)
       (set! location (-> location if-> rest ...)))
      ((_ location if->> rest ...)
       (set! location (-> location if->> rest ...)))
      ((_ location rest ...)            ; default to ->
       (set! location (-> location -> rest ...)))))

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

  (define uniq-accumulator
    (case-lambda
      (() (uniq-accumulator (op)))
      ((f) (let ((items (tbl)))
             (lambda (x)
               (if (eof-object? x)
                 (hash-table-values items)
                 (hash-table-update!/default items (f x) identity x)))))))

  (define-syntax one-of
    (er-macro-transformer
     (lambda (expr rename compare)
       `(,(rename 'lambda) (x)
         (,(rename 'or) ,@(map (lambda (v)
                                 `(,(rename 'equal?) x ,v))
                               (cdr expr)))))))

  (define (per . args)
    (apply comp (reverse args)))

  (define inject
    (case-lambda
      ((f) (lambda (o)
             (let* ((g (gen o))
                    (v (g)))
               (if (eof-object? v)
                 (f)
                 (generator-fold f v g)))))
      ((f v) (lambda (o)
               (generator-fold f v (gen o))))))

  (define (sing? l)
    (match l
      ((_) #t)
      (_   #f)))

  (define (unlist args)
    (apply values args))

  (define (juxt . fs)
    (lambda args
      (unlist (map (lambda (f) (apply f args)) fs))))

  (define (act x . fs)
    ((apply per fs) x))

  (let ((old-repl-prompt (repl-prompt)))
    (repl-prompt (lambda ()
                   (let ((old-prompt (old-repl-prompt)))
                     (str (substring old-prompt 0 2)
                          "^_^;"
                          (substring old-prompt 2))))))
)
