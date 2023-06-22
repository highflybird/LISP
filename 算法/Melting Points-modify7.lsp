;;; the main routine
;;; 主程序
(defun C:test (/ TOL ss Pts T0 pairs)
  ;;set or input Tolerance
  ;;输入容差
  (if (not (setq TOL (getdist "\nPlease input the tolerance:")))
    (setq TOL 70)
  )
  ;;也可以用其他方式取得点集
  ;;Get the points
  (setq ss (ssget '((0 . "POINT"))))
  (setq Pts (getpt ss))
  ;;按照X值排序
  ;;sort by X value
  (setq t0 (getvar "TDUSRTIMER"))
  (setq Pts (sortx Pts tol))
  (princ "\nThe time for sorting these points:")
  (princ (* (- (getvar "TDUSRTIMER") t0) 86400))
  (princ "s.")
  ;;函数用时估算，以了解函数性能
  ;;how long it will take
  (setq t0 (getvar "TDUSRTIMER"))
  (setq pairs (f2 Pts TOL))
  (princ "\nThe time for finding the pairs of points:")
  (princ (* (- (getvar "TDUSRTIMER") t0) 86400))
  (princ "s.")
  (if pairs
    ;;画最短距离的点对集的连线，可能有多条
    ;;draw the lines between these pairs.
    (repeat (/ (length pairs) 2)
      (entmake
	(list
	  '(0 . "LINE")
	  (cons 10 (car pairs))
	  (cons 11 (cadr pairs))
	  (cons 62 1)
	)
      )
      (setq pairs (cddr pairs))
    )
  )
  (princ)
)
;;; 取点函数，其中i为点的编号
;;; Get points.
(defun getpt (ss / i l a b c)
  (setq i 0)
  (if ss
    (repeat (sslength ss)
      (setq a (ssname ss i))
      (setq b (entget a))
      (setq c (cdr (assoc 10 b)))
      (setq l (cons c l))
      (setq i (1+ i))
    )
  )
  (reverse l)
)
;;; 从0到k的点集
;;; the elements of points from 0 to k 
(defun Array (Pts k / PtsR PtsL)
  (setq PtsR Pts)
  (repeat k
    (setq PtsL (cons (car PtsR) PtsL))
    (setq PtsR (cdr PtsR))
  )
  (list PtsL PtsR)
)
;;; 对X排序
;;; sort points by X value.
(defun sortX (pts tol)
  (vl-sort
    pts
    (function
      (lambda (e1 e2)
	(< (car e1) (car e2))
      )
    )
  )
)
(defun sortY (pts tol)
  (vl-sort
    pts
    (function
      (lambda (e1 e2)
	(< (cadar e1) (cadar e2))
      )
    )
  )
)
;;; 用递归解决
;;; use recursion to solve this problem (divide-and-conquer)

(defun f2 (Pts TOL / l m n pt1 pt2 Pts1 Pts2 Pts3 Pts4 Mx x1 x2)
  (setq l (length Pts))
  (cond
    ((= l 1) nil)
    ((= l 2)
     ;; 两点还用说
     ;; judge directly if 2 points
     (setq pt1 (car Pts))
     (setq pt2 (cadr Pts))
     (if (and (equal (car pt1) (car pt2) TOL)
	      (equal (cadr pt1) (cadr pt2) TOL)
	      (<= (distance pt1 pt2) TOL)
	 )
       (setq pairs (cons pt1 pairs)				;add the start point into pairs;
	     pairs (cons pt2 pairs)				;add the end point into pairs;
       )
     )
    )
    ((> l 2)
     (setq n (/ l 2)
	   m (- l n)
     )
     (setq Pts1 (Array Pts n))					;Divide
     (setq Pts2 (cadr Pts1))					;R points
     (setq Pts1 (car Pts1))					;L points
     (f2 (reverse Pts1) TOL)					;Recurse Left
     (f2 Pts2 TOL)						;Recurse Right
     (setq Mx (caar Pts2))					;Midpoint
     (setq x1 (- Mx tol))					;L points in the TOL
     (setq x2 (+ Mx tol))					;R points in the TOL
     (while (and pts2 (<= (caar pts2) x2))			;gather the points in the Right
       (setq Pts3 (cons (cons (car pts2) 1) Pts3))		;1 means in the Right
       (setq pts2 (cdr pts2))					
     )
     (setq pts3 (reverse pts3))
     (while (and pts1 (>= (caar pts1) x1))			;gather the points in the Left
       (setq Pts3 (cons (cons (car pts1) 0) Pts3))		;0 means in the Left
       (setq pts1 (cdr pts1))
     )
     (setq Pts3 (sorty pts3 tol))				;sort these points by Y value
     (setq pts4 (cdr pts3))					;consider these points one by one
     (if pts4
       (while pts3
	 (setq pt1 (caar pts3))
	 (setq pt2 (caar pts4))
	 (while	(and pts4 (<= (- (cadr pt2) (cadr pt1)) tol))
	   (if (and (/= (cdar pts3) (cdar pts4))		;different side;
		    (equal (car pt1) (car pt2) tol)		;x value in tol;
		    (<= (distance pt1 pt2) tol)			;distance <= tol;
	       )
	     (setq pairs (cons pt1 pairs)			;add the start point into pairs;
		   pairs (cons pt2 pairs)			;add the end point into pairs;
	     )
	   )
	   (setq pts4 (cdr pts4))				
	   (setq pt2 (caar pts4))
	 )
	 (setq pts3 (cdr pts3))
	 (setq pts4 (cdr pts3))
       )
     )
     Pairs
    )
  )
)