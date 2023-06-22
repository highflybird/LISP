; Take elements 0,2,4... and 1,3,5... of a list
(defun evens (f)
  (if f
    (cons (car f) (evens (cddr f)))
  )
)

(defun odds (f)
  (if f
    (cons (cadr f) (odds (cddr f)))
  )
)

; Length of a list
(defun len (f)
  (if f
     (1+ (len (cdr f)))
    0
  )
)

; Multiply by exp(2 pi i k/N)
(defun phase (s k N / x)
  ;(exp (/ (* 0+2i s k Pi) N))
  (setq x (/ (* 2 s k pi) n))
  (list (cos x) (sin x))
)


(defun rotate (s k N f)
  (if f
    (cons
      (mapcar (function (lambda ( e ) (* e (car f)))) (phase s k N))
      ;(lambda  '* (phase s k N) (car f))
      (rotate s (1+ k) N (cdr f))
    )
  )
)

; With these preliminaries FFT is simple
(defun four (s f)
  (if (= 1 (length f))
    f
    (combine s (four s (evens f)) (four s (odds f)))
  )
)

(defun combine (s ev od)
  (plusminus ev (rotate s 0 (len od) od))
)

(defun plusminus (a b)
  (if (= 1 (length a))
    (setq a (list (car a) 0))
  )
  (append (mapcar  '+ a b) (mapcar '- a b))
)

(defun FFT1 (s f / e o)
  (if (= 2 (length f))
    (list
      (list (apply '+ f ) 0)
      (list (apply '- f ) 0)
    )
    (progn
      (while (cadr f)
	(setq e (cons (car f) e))
	(setq f (cdr f))
	(setq o (cons (car f) o))
	(setq f (cdr f))
      )
      (setq e (reverse e))
      (setq o (reverse o))
      (combine s (FFT1 s e) (FFT1 s o))
    )
  )
)


(defun rotate1 (s k N f / l)
  (if f
    (foreach x f
      (setq l (cons (CMP:MUL (phase s k N) x) l))
      (setq k (1- k))
    )
  )
  (reverse l)
)



; A little test
(four -1 (four 1 '(1 -6)))
;(four -1 (four 1 '(1 -6 2 4)))