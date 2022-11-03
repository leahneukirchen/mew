(put 'def 'scheme-indent-function 'defun)
(put 'esc 'scheme-indent-function 1)
(put 'fin 'scheme-indent-function 0)
(put 'fun 'scheme-indent-function 1)
(put 'loc 'scheme-indent-function 1)
(put 'andloc 'scheme-indent-function 1)
(put 'rec 'scheme-indent-function 1)
(put 'rep 'scheme-indent-function 2)
(put 'seq 'scheme-indent-function 0)

(put 'if 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)
(put 'accumulate 'scheme-indent-function 1)

(setq auto-mode-alist
      (cons '("\\.mew\\'" . scheme-mode)
            auto-mode-alist))
