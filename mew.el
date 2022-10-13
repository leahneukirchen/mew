(put 'def 'scheme-indent-function 'defun)
(put 'esc 'scheme-indent-function 1)
(put 'fin 'scheme-indent-function 0)
(put 'fun 'scheme-indent-function 1)
(put 'loc 'scheme-indent-function 1)
(put 'rec 'scheme-indent-function 1)
(put 'rep 'scheme-indent-function 'scheme-let-indent)
(put 'seq 'scheme-indent-function 0)

(put 'if 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)

(put '-> 'scheme-indent-function #'(lambda (_ _ _) 1))
(put '->> 'scheme-indent-function #'(lambda (_ _ _) 1))
