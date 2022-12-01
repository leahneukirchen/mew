(module mew
  (export
     act accumulate andloc at
     boolean
     comp cross-product
     dec def del-at div
     empty? eof esc
     fail fin final for fun*
     gconcatenate gen generator-xfold generic-for-each genumerate get
     gfix giterate gmatch gpick group-by-accumulator gslice-when
     gsplit gsplit-on gwindow
     imp inc inject into
     juxt
     keys
     len lines loc
     mod
     negate
     odometer one-of op op*
     per prn proj puts
     rand range rep
     sample scan scan-right sing? search seq set set-at
     shuffle shuffle! str slurp
     tally-accumulator tbl time
     while
     uniq-accumulator unlist until
     vals void?
     xcond xfold xfold-right xreduce xreduce-right xscan xscan-right
     -> fun-> fun->> set->
     =? <>?
     ~?
     => =>* and=> fun=> op=> set=>

     generic-make-accumulator)

  (import-for-syntax srfi-1)
  (import-for-syntax matchable)

  (import scheme
          (rename (chicken base)
             (print puts)
             (complement negate)
             (compose comp))
          (chicken condition)
          (chicken module)
          (chicken port)
          (chicken random)
          (chicken repl)
          (chicken syntax)
          srfi-17
          (rename (srfi-69)
             (hash-table-keys keys)
             (hash-table-values vals))
          srfi-158
          err)

  (reexport srfi-69)
  (reexport srfi-158)
  (reexport
    (rename (srfi-158)
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
    (only (r7rs)
      list->vector
      vector-map))
  (reexport
    (only (chicken format)
      format))
  (reexport utf8-srfi-13)

  (reexport
    (only (chicken base)
      rec
      unless
      void
      when))

  (reexport
    (only (chicken time)
      time))

  (import
    (rename (r7rs)
      (floor-quotient div)
      (floor-remainder mod)))

  (reexport
    (rename (except (scheme)
               assoc
               member)
      (lambda fun)
      (apply app)
      (ceiling ceil)
      (truncate trunc)
      ))

  (reexport srfi-1)

  (reexport err)


  (define (inc i)
    (+ i 1))

  (define (dec i)
    (- i 1))

  (define (boolean x)
    (not (not x)))

  (define (tbl=? a b)
    (and (hash-table? a)
         (hash-table? b)
         (= (hash-table-size a) (hash-table-size b))
         (call-with-current-continuation
          (lambda (ret)
            (hash-table-for-each
             a
             (lambda (k v)
               ;; n.b. nan is never equal to anything
               (unless (=? (hash-table-ref/default b k +nan.0) v)
                 (ret #f))))
            #t))))

  (define =?
    (case-lambda
      (()  #t)
      ((_) #t)
      ((x y) (or (equal? x y)
                 (tbl=? x y)))
      ((x . rest) (every (lambda (y) (=? x y)) rest))))

  (define <>?
    (case-lambda
      ((a b) (not (=? a b)))
      ((a b c) (and (not (=? a b))
                    (not (=? b c))
                    (not (=? c a))))
      ((a b c d) (and (not (=? a b))
                      (not (=? a c))
                      (not (=? a d))
                      (not (=? b c))
                      (not (=? b d))
                      (not (=? c d))))
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
    (let loop ((args args)
               (ret (void)))
      (if (null? args)
        (begin
          (newline)
          ret)
        (begin
          (write (car args))
          (unless (null? (cdr args))
            (display " "))
          (loop (cdr args) (car args))))))

  (define-syntax seq
    (syntax-rules ()
      ((_)
       (void))
      ((_ . rest)
       (begin . rest))))

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
       (receive vals (call-with-current-continuation
                      (lambda (return)
                        (seq body ...)))
         (if (null? vals)
           (void)
           (car vals))))))

  (define-syntax fin
    (syntax-rules ()
      ((_ form rest ...)
       (dynamic-wind
         (lambda () (begin))
         (lambda () form)
         (lambda () (seq rest ...))))))

  (define-syntax loc
    (syntax-rules ()
      ((_ () rest ...)
       (let () (seq rest ...)))
      ((_ (x y . brest) . rest)
       (match-let ((x y)) (loc brest . rest)))))

  (define-syntax andloc
    (syntax-rules (_)
      ((_ () rest ...)
       (let () (seq rest ...)))
      ((_ (_ y . brest) . rest)
       (let ((unused y)) (and unused (andloc brest . rest))))
      ((_ (x y . brest) . rest)
       (let ((x y)) (and x (andloc brest . rest))))))

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
       (let name (bindings ...) (seq body ...)))
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
           (let ()
             body ...
             (loop cond)))))))

  (define-syntax until
    (syntax-rules ()
      ((_ cond body ...)
       (while (not cond) body ...))))

  (define-syntax xcond
    (syntax-rules ()
      ((_ rest ...)
       (cond rest ... (else (error "missing case in xcond"))))))

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

  (define (plist-generator l)
    (lambda ()
      (cond ((null? l)        (eof))
            ((pair? (cdr l))  (let ((p (cons (car l) (cadr l))))
                                (set! l (cddr l))
                                p))
            (else
             (error "odd number of elements in key value list")))))

  (define tbl
    (case-lambda
      (()   (make-hash-table))
      (kvs  (into (tbl) (plist-generator kvs)))))

  (define (set-at o . rest)
    (cond ((hash-table? o) (generator-for-each
                            (lambda (kv) (hash-table-set! o (car kv) (cdr kv)))
                            (plist-generator rest)))
          ((vector? o)     (generator-for-each
                            (lambda (kv) (vector-set! o (car kv) (cdr kv)))
                            (plist-generator rest)))
          ((string? o)     (generator-for-each
                            (lambda (kv) (string-set! o (car kv) (cdr kv)))
                            (plist-generator rest)))
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

  (define (empty o)
    (cond ((list? o) '())
          ((string? o) "")
          ((vector? o) #())
          ((hash-table? o) (tbl))
          (else "no empty defined")))

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

  (define (void? x)
    (eq? x (void)))

  (define rand
    (case-lambda
      (()    (pseudo-random-real))
      ((n)   (pseudo-random-integer n))
      ((n m) (+ n (pseudo-random-integer (- m n))))))

  (define (shuffle! v)
    (let loop ((i (- (vector-length v) 1)))
      (when (positive? i)
        (let* ((j (rand (+ i 1)))
               (vi (vector-ref v i))
               (vj (vector-ref v j)))
          (vector-set! v i vj)
          (vector-set! v j vi)
          (loop (dec i)))))
    v)

  (define (shuffle v)
    (let ((l (vector-length v)))
      (do ((res (make-vector l))
           (i 0 (+ i 1)))
          ((= i l) res)
        (let ((j (rand (+ i 1))))
          (unless (= j i)
            (vector-set! res i (vector-ref res j)))
          (vector-set! res j (vector-ref v i))))))

  (define sample
    (let ((gen-get (lambda (o)
                     (if (hash-table? o)
                       (lambda (n)
                         (esc ret
                           (let ((i 0))
                             (hash-table-for-each o
                                                  (lambda (k v)
                                                    (if (= i n)
                                                      (ret (cons k v))
                                                      (set! i (inc i))))))))
                       (lambda (n)
                         (get o n))))))
      (case-lambda
        ((o)
         ((gen-get o) (rand (len o))))
        ((o k)
         (cond
          ((= k 0)        (empty o))
          ((<= (len o) k) (let ((r (into #() o)))
                            (shuffle! r)
                            (if (vector? o)
                              r
                              (into (empty o) r))))
          (else
           ;; Algorithm L with additional shuffle at the end.
           ;; https://dl.acm.org/doi/pdf/10.1145/198429.198435
           (let ((geto (gen-get o))
                 (r (make-vector k))
                 (w (exp (/ (log (rand)) k)))
                 (n (len o))
                 (i 0))
             (while (< i k)
               (vector-set! r i (geto i))
               (set! i (inc i)))
             (while (< i n)
               (set! i (+ i 1 (inexact->exact (floor (/ (log (rand))
                                                        (log (- 1 w)))))))
               (when (< i n)
                 (vector-set! r (rand k) (geto i))
                 (set! w (* w (exp (/ (log (rand)) k))))))
             (shuffle! r)
             (if (vector? o)
               r
               (into (empty o) r)))))
         ))))

  (define range
    (case-lambda
      (()               (make-range-generator 0 +inf.0 1))
      ((start)          (make-range-generator start +inf.0 1))
      ((start end)      (make-range-generator start end 1))
      ((start end step) (set! start (- (+ start step) step))
                        (let ((cmp (if (>= step 0) < >)))
                          (lambda () (if (cmp start end)
                                       (let ((v start))
                                         (set! start (+ start step))
                                         v)
                                       (eof)))))))

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

  (define (gpick f gen)
    (lambda ()
      (let loop ((item (gen)))
        (if (eof-object? item)
          item
          (let ((v (f item)))
            (if (eof-object? v)
              (loop (gen))
              v))))))

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

  (define (gsplit-on pred gen)
    (let ((slice '())
          (this #f))
      (lambda ()
        (if (eof-object? this)
          this
          (let loop ()
            (set! this (gen))
            (if (eof-object? this)
              (if (null? slice)
                (eof)
                (reverse slice))
              (if (pred this)
                (let ((finished-slice (reverse slice)))
                  (set! slice '())
                  finished-slice)
                (begin
                  (set! slice (cons this slice))
                  (loop)))))))))

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
    (let ((prev (void)))
      (gmap (lambda (x)
              (if (equal? prev x)
                (eof)
                (begin
                  (set! prev x)
                  x)))
            g)))

  (define (final g)
    (generator-fold (lambda (x a) x) (void) g))

  (define (odometer . wheels)
    (define (grepeat rep gen)
      (gconcatenate (gmap (lambda (item)
                            (gtake (cycle item) rep))
                          gen)))
    (if (null? wheels)
      (generator)
      (match-let (((total . parts) (scan-right * 1 wheels)))
        (gtake (apply gmap list (map (lambda (r i)
                                       (grepeat r (gmap (op mod _ i) (range 0))))
                                     parts wheels))
               total))))

  (define (cross-product . xs)
    (gmap (op map get xs _) (apply odometer (map len xs))))

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

  (define-syntax ok-apply
    (syntax-rules ()
      ((_ x f args ...)
       (let ((v x))
         (if (ok? v)
           (f v args ...)
           v)))))

  (define-syntax ok-apply-last
    (syntax-rules ()
      ((_ x f args ...)
       (let ((v x))
         (if (ok? v)
           (f args ... v)
           v)))))

  (define-syntax err-apply
    (syntax-rules ()
      ((_ x f args ...)
       (let ((v x))
         (if (err? v)
           (f (unerr v) args ...)
           v)))))

  (define-syntax err-apply-last
    (syntax-rules ()
      ((_ x f args ...)
       (let ((v x))
         (if (err? v)
           (f args ... (unerr v))
           v)))))

  (define-syntax ->
    (syntax-rules ()
      ((_ rest ...)
       (->chunk () () (rest ...)))))

  (define-syntax ->chunk
    (syntax-rules (-> ->> and-> and->> if-> if->> ok-> ok->> err-> err->>)
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
      ((_ (result ...) (current ...) (ok-> rest ...))
       (->chunk (result ... (current ...)) (-> ok-apply) (rest ...)))
      ((_ (result ...) (current ...) (ok->> rest ...))
       (->chunk (result ... (current ...)) (-> ok-apply-last) (rest ...)))
      ((_ (result ...) (current ...) (err-> rest ...))
       (->chunk (result ... (current ...)) (-> err-apply) (rest ...)))
      ((_ (result ...) (current ...) (err->> rest ...))
       (->chunk (result ... (current ...)) (-> err-apply-last) (rest ...)))
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
    (syntax-rules (-> ->> and-> and->> if-> if->> ok ok->> err-> err->>)
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
      ((_ location ok-> rest ...)
       (set! location (-> location ok-> rest ...)))
      ((_ location ok->> rest ...)
       (set! location (-> location ok->> rest ...)))
      ((_ location err-> rest ...)
       (set! location (-> location err-> rest ...)))
      ((_ location err->> rest ...)
       (set! location (-> location err->> rest ...)))
      ((_ location rest ...)            ; default to ->
       (set! location (-> location -> rest ...)))))

  (define (~? str pat)
    (let ((data (irregex-search pat str)))
      (if data
        (list-tabulate (inc (irregex-match-num-submatches data))
                       (op irregex-match-substring data _))
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
                  (list-tabulate (inc (irregex-match-num-submatches data))
                                 (op irregex-match-substring data _))
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

  (define lines
    (case-lambda
      (() read-line)
      ((x) (cond ((input-port? x)
                  (lambda ()
                    (read-line x)))
                 ((string? x)
                  (let ((file (open-input-file x)))
                    (lambda ()
                      (let ((line (read-line file)))
                        (when (eof-object? line)
                          (close-input-port file))
                        line))))
                 (else
                  (error "can't read lines"))))))

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

  (define (generator-xfold f seed . gs)
    (define (inner-xfold seed)
      (let ((vs (map (lambda (g) (g)) gs)))
        (if (any eof-object? vs)
          seed
          (inner-xfold (apply f seed vs)))))
    (inner-xfold seed))

  (define-syntax one-of
    (er-macro-transformer
     (lambda (expr rename compare)
       `(,(rename 'lambda) (x)
         (,(rename 'or) ,@(map (lambda (v)
                                 `(,(rename 'equal?) x ,v))
                               (cdr expr)))))))

  (define inject
    (case-lambda
      ((f) (lambda (o)
             (let* ((g (gen o))
                    (v (g)))
               (if (eof-object? v)
                 (f)
                 (generator-xfold f v g)))))
      ((f v) (lambda (o)
               (generator-xfold f v (gen o))))
      ((f v g)
       ((inject f v) (gen g)))))

  (define (sing? l)
    (match l
      ((_) #t)
      (_   #f)))

  (define (unlist args)
    (apply values args))

  (define (juxt . fs)
    (lambda args
      (unlist (map (lambda (f) (apply f args)) fs))))

  (define (per . args)
    (apply comp (reverse args)))

  (define (act x . fs)
    ((apply per fs) x))

  (define => act)

  (define-syntax =>*
    (syntax-rules ()
      ((_ expr . fs)
       (receive args expr
         (apply (per . fs) args)))))

  (define-syntax set=>
    (syntax-rules ()
      ((_ location . fs)
       (set location (=> location . fs)))))

  (define (and=> x . fs)
    (and x
         (if (null? fs)
           x
           (let ((result ((car fs) x)))
             (and result
                  (apply and=> result (cdr fs)))))))

  (define-syntax fun=>-inner
    (syntax-rules ()
      ((_ (acc ...))
       (compose acc ...))
      ((_ (acc ...) ,arg args ...)
       (fun=>-inner (arg acc ...) args ...))
      ((_ (acc ...) (arg ...) args ...)
       (fun=>-inner ((op arg ...) acc ...) args ...))
      ((_ (acc ...) arg args ...)
       (fun=>-inner (arg acc ...) args ...))
      ))

  (define-syntax fun=>
    (syntax-rules ()
      ((fun=> . args)
       (fun=>-inner () . args))))

  (define-syntax op=>
    (syntax-rules (_)
      ((op=> init . args)
       ((fun=> . args) init))))

  (define-syntax proj
    (er-macro-transformer
      (lambda (expr rename compare)
        (let* ((items (cdr expr))
               (max-n (if (null? items) 0 (+ 1 (apply max items))))
               (args (list-tabulate max-n
                                    (lambda (n)
                                      (string->symbol
                                       (string-append "x"
                                                      (number->string n)))))))
          `(,(rename 'lambda) (,@args . rest)
            (,(rename 'values) ,@(map (lambda (n) (list-ref args n)) items)))))))

  (define (fail exn . args)
    (if (list? exn)
      (signal (apply condition
                     (list (car exn) 'message (apply format args))
                     (map list (cdr exn))))
      (apply fail '(exn) exn args)))

  ;; The QuickSearch algorithm, for generic sequences
  ;; http://www-igm.univ-mlv.fr/~lecroq/string/node19.html#SECTION00190
  (define search
    (case-lambda
      ((needle haystack) (search needle haystack 0))
      ((needle haystack start)
       (let* ((lh (len haystack))
              (ln (len needle))
              (t  (into (tbl) (gmap (lambda (c i)
                                      (cons c (- ln i)))
                                    (gen needle)
                                    (range 0)))))
         (def (match? offset)
           (let loop ((i (dec ln)))
             (and (=? (at haystack (+ i offset)) (at needle i))
                  (or (zero? i)
                      (loop (dec i))))))

         (if (zero? ln)
           start
           (let loop ((i start))
             (if (<= i (- lh ln))
               (if (match? i)
                 i
                 (loop (+ i (get t (get haystack (+ i ln) #f) (inc ln)))))
               #f)))))))

  (define-syntax imp
    (syntax-rules ()
      ((_ a b)
       (or (not a) b))
      ((_ a b c ...)
       (or (not a) (imp b c ...)))))

  ;; adapted from https://code.call-cc.org/svn/chicken-eggs/release/5/srfi-1/trunk/srfi-1.scm ------------------------------
  (define (##srfi1#cars+cdrs lists)
    (##sys#call-with-current-continuation
     (lambda (abort)
       (let recur ((lists lists))
         (if (pair? lists)
	   (receive (list other-lists) (car+cdr lists)
	     (if (null-list? list)
               (abort '() '()) ; LIST is empty -- bail out
	       (receive (a d) (car+cdr list)
		 (receive (cars cdrs) (recur other-lists)
		   (values (cons a cars) (cons d cdrs))))))
	   (values '() '()))))))

  (define (##srfi1#cdrs lists)
    (##sys#call-with-current-continuation
     (lambda (abort)
       (let recur ((lists lists))
	 (if (pair? lists)
	   (let ((lis (car lists)))
	     (if (null-list? lis)
               (abort '())
	       (cons (cdr lis) (recur (cdr lists)))))
	   '())))))

  (define (xfold kons knil lis1 . lists)
    (if (pair? lists)
      (let lp ((lists (cons lis1 lists)) (ans knil))	; N-ary case
	(receive (cars cdrs) (##srfi1#cars+cdrs lists)
	  (if (null? cars)
            ans ; Done.
	    (lp cdrs (apply kons ans cars)))))

      (let lp ((lis lis1) (ans knil))			; Fast path
	(if (null-list? lis)
          ans
	  (lp (cdr lis) (kons ans (car lis)))))))

  (define (xfold-right kons knil lis1 . lists)
    (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))		; N-ary case
        (let ((cdrs (##srfi1#cdrs lists)))
	  (if (null? cdrs)
            knil
	    (apply kons (recur cdrs) (map car lists)))))

      (let recur ((lis lis1))				; Fast path
	(if (null-list? lis)
          knil
	  (let ((head (car lis)))
	    (kons (recur (cdr lis)) head))))))

  (define (xreduce f ridentity lis)
    (if (null-list? lis)
      ridentity
      (xfold f (car lis) (cdr lis))))

  (define (xreduce-right f ridentity lis)
    (if (null-list? lis)
      ridentity
      (let recur ((head (car lis)) (lis (cdr lis)))
	(if (pair? lis)
	  (f (recur (car lis) (cdr lis)) head)
	  head))))
  ;; end of code lifted from srfi-1.scm ------------------------------

  (define (scan kons knil . lists)
    (reverse (apply xfold (lambda (acc . elts)
                            (cons (apply kons (append elts (list (car acc)))) acc))
                    (list knil)
                    lists)))

  (define (scan-right kons knil . lists)
    (apply xfold-right (lambda (acc . elts)
                         (cons (apply kons (append elts (list (car acc)))) acc))
           (list knil)
           lists))

  (define (xscan kons knil . lists)
    (reverse (apply xfold (lambda (acc . elts)
                           (cons (apply kons (car acc) elts) acc))
                    (list knil)
                    lists)))

  (define (xscan-right kons knil . lists)
    (apply xfold-right (lambda (acc . elts)
                         (cons (apply kons (car acc) elts) acc))
           (list knil)
           lists))

  (let ((old-repl-prompt (repl-prompt)))
    (repl-prompt (lambda ()
                   (let ((old-prompt (old-repl-prompt)))
                     (str (substring old-prompt 0 2)
                          "^_^;"
                          (substring old-prompt 2))))))

  (set-pseudo-random-seed! (random-bytes))
)
