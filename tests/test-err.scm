(import test
        mew
        err
        (chicken condition))

(test-group "basics"
  (test #t (err? (err 42)))
  (test #t (ok? 42))
  (test #t (ok? #f))
  (test #t (ok? (if #f #f)))
  (test #t (ok? '()))
  (test 42 (unerr (err 42)))
  (test #t (void? (unerr 42))))

(test-group "record-printer"
  (test "#<err 42>" (format "~a" (err 42))))

(test-group "ok/if"
  (test 5 (ok/if 4 inc))
  (test 5 (ok/if 4 inc dec))
  (test 3 (ok/if (err 4) inc dec)))

(test-group "ok=>"
  (test 5 (ok=> 5))
  (test 7 (ok=> 5 inc inc))
  (test (err 5) (ok=> (err 5)))
  (test (err 7) (ok=> 5 inc inc err inc inc)))

(test-group "err=>"
  (test 7 (err=> 7))
  (test 7 (err=> (err 7)))
  (test 7 (err=> 7 inc))
  (test 9 (err=> (err 7) inc inc))
  (test-error (err=> (err 7) err inc)))

(test-group "ok"
  (test 7 (ok 7))
  (test-error (ok (err 7)))
  (test #t (condition? (unerr (guard-err (ok (guard-err (/ 1 0))))))))

(test-group "ok/or"
  (test #t (err? (ok/or)))
  (test 7 (ok/or 7))
  (test #f (ok/or #f 7))
  (test 7 (ok/or (err 6) 7))
  (test 7 (ok/or (err 6) (err 8) 7))
  (test 7 (ok/or (err 6) (err 8) 7 8))
  (test 7 (ok/or 7 (error "not reached")))
  (test-error (ok/or (err 7) (error "reached"))))

(test-group "ok/and"
  (test #t (ok/and))
  (test 8 (ok/and 6 7 8))
  (test 8 (ok/and #f 7 8))
  (test (err 6) (ok/and (err 6) 7 8))
  (test (err 7) (ok/and 6 (err 7) 8)))

(test-group "guard-err"
  (test #t (err? (guard-err (/ 1 0))))
  (test 1/2 (guard-err (/ 1 2)))
  (test-error (err? (guard-err (/ 1 0) (exn bounds)))))
