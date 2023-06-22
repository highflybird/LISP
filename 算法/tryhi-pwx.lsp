;; Matrix Inverse  -  gile & Lee Mac
;; Uses Gauss-Jordan Elimination to return the inverse of a non-singular nxn matrix.
;; Args: m - nxn matrix
;(invm '((2 0 0)(2 2 0)(0 0 2)))
(defun mat_inv ( m / c f p r )
        (defun f ( p m ) (mapcar (function (lambda ( x ) (mapcar (function (lambda ( a b ) (- a (* (car x) b)))) (cdr x) p))) m))
        (setq  m (mapcar (function append) m (mat_o (length m))))
        (while m
                (setq c (mapcar (function (lambda ( x ) (abs (car x)))) m))
                (repeat (vl-position (apply (function max) c) c)
                        (setq m (append (cdr m) (list (car m))))
                )
                (if (equal 0.0 (caar m) 1e-14)
                        (setq m nil r nil)
                        (setq
                                p (mapcar (function (lambda ( x ) (/ (float x) (caar m)))) (cdar m))
                                m (f p (cdr m))
                                r (cons p (f p r))
                        )
                )
        )
        (reverse r)
)

;; Identity Matrix  -  Lee Mac
;; Args: n - matrix dimension

(defun mat_o ( n / i j l m )
        (repeat (setq i n)
                (repeat (setq j n)
                        (setq
                                l (cons (if (= i j) 1.0 0.0) l)
                                j (1- j)
                        )
                )
                (setq
                        m (cons l m)
                        l nil
                        i (1- i)
                )
        )
        m
)

;;已知三点求抛物线的a b c常数
(defun try-y=ax^2+bx+c (p1 p2 p3 / x1 x2 x3 y1 y2 y3)
        (mapcar 'set '(x1 y1 ) p1)
  (mapcar 'set '(x2 y2 ) p2)
        (mapcar 'set '(x3 y3 ) p3)
        (mxv
                (mat_inv
                        (list
                                (list (* x1 x1) x1 1)
                                (list (* x2 x2) x2 1)
                                (list (* x3 x3) x3 1)
                        )
                )
                (list y1 y2 y3)
        )
)

(setq
        p1 '(10000. 9999.650)
        p2 '(20000. 19999.4790)
        p3 '(0. 0.)
)


(mapcar '(lambda(x)(rtos x 2 16))(try-y=ax^2+bx+c p1 p2 p3))