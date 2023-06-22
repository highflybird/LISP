;;; the main routine
;;; 主程序
(defun C:test (/ TOL sl to ss ptlist T0 pp)
  ;;set or input Tolerance
  ;; 输入容差
  (setq TOL 70)
  ;;也可以用其他方式取得点集
  ;;Get the points
  (setq sl '((0 . "POINT")))
  (setq t0 (getvar "TDUSRTIMER"))
  (setq ss (ssget sl))
  (setq ptlist (getpt ss))
  ;;按照X值排序
  ;;sort by X value
  (setq t0 (getvar "TDUSRTIMER"))
  (setq ptlist (sortx ptlist))
  (princ "\n函数排序用时")
  (princ (* (- (getvar "TDUSRTIMER") t0) 86400))
  (princ "秒")
  ;;函数用时估算，以了解函数性能
  ;;how long it will take
  (setq t0 (getvar "TDUSRTIMER"))
  (setq pp (f2 ptlist TOL))
  (princ "\n函数查找用时")
  (princ (* (- (getvar "TDUSRTIMER") t0) 86400))
  (princ "秒")
  (if pp
    ;;画最短距离的点对集的连线，可能有多条
    ;;draw the line between these pairs.
    (foreach n pp
      (entmake
	(list
	  '(0 . "LINE")
	  (cons 10 (car n))
	  (cons 11 (cadr n))
	  (cons 62 1)
	)
      )
    )
  )
)
;;; 取点函数，其中i为点的编号
;;; get points.
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
;;; the elments of points from 0 to k 
(defun Array (ptlist k / l1 l2)
  (setq l1 ptlist)
  (repeat k
    (setq l2 (cons (car l1) l2))
    (setq l1 (cdr l1))
  )
  (list (reverse l2) l1)
)
;;; 对X排序
;;; sort points by X value.
(defun sortX (ptlist)
  (vl-sort ptlist '(lambda (e1 e2) (< (car e1) (car e2))))
)
;;; 在带形区域查找
;;; search these points in the Tolerance
(defun searchX (Pts1 x1 x2 / pp)
  (foreach n Pts1
    (if	(and (>= (car n) x1)
	     (<= (car n) x2)
	)
      (setq pp (cons n pp))
    )
  )
  (reverse pp)
)

;;; 用递归解决
;;; use recursion to solve this problem (divide-and-conquer)

(defun f2 (ptlist TOL /	 l	m      n      a	     b	    pt1
	   pt2	  ppp	 Pts0	Pts1   Pts2   Pts3   Pts4   midptx
	   mind1  mind2
	  )
  (setq l (length ptlist))
  (cond
    ((= l 2)
     ;; 两点还用说
     ;; judge directly if two points
     (setq pt1 (car ptlist)
	   pt2 (cadr ptlist)
     )
     (if (and (equal (car pt1) (car pt2) TOL)
	      (equal (cadr pt1) (cadr pt2) TOL)
	      (< (distance (car ptlist) (cadr ptlist)) TOL)
	 )
       (list ptlist)
     )
    )
    ((> l 2)
     (setq n (/ l 2)
	   m (- l n)
     )
     ;; 分治
     ;; divide
     (setq Pts0 (Array ptlist n))
     (setq Pts1 (car Pts0))
     (setq Pts2 (cadr Pts0))
     ;; 递归左边
     ;; recurse left
     (setq mind1 (f2 Pts1 TOL))
     ;;递归右边
     ;; recurse right
     (setq mind2 (f2 Pts2 TOL))
     ;; 合并左右集合
     ;; merge them
     (setq ppp (append mind1 mind2))
     ;; 对带形区域的合并
     ;; consider the points in the Tolerance
     (setq midptx (caar Pts2))
     (setq a (- midptx TOL)
	   b (+ midptx TOL)
     )
     (if (setq Pts3 (searchX Pts1 a midptx))
       (if (setq Pts4 (searchx Pts2 midptx b))
	 (foreach n Pts3
	   (foreach e Pts4
	     (if (equal (cadr n) (cadr e) TOL)
	       (if (<= (distance n e) TOL)
		 (setq ppp (cons (list n e) ppp))
	       )
	     )
	   )
	 )
       )
     )
     ppp
    )
  )
)