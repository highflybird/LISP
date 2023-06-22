;;;************************************************************************
;;;一个求点集合的凸包的lisp程序--------------------------------------------
;;;------采用的算法为礼品包扎法--------------------------------------------
;;;方法为最右端的点开始处理，将该点作为凸包边界的第一个点P1，从最初的垂直线
;;;方向绕P1顺时针旋转，直到碰到另一个P2这就是凸包边界的第二个点P2，依此类推
;;;p2求得p3......直到又重新回到p1，已经考虑了各种退化情况和浮点运算，其算法
;;;时间不超过O(n.h),其中h是凸包的复杂度，时间还是很快的。大家不妨验证。    
;;;参考文献<<计算几何-算法及其应用>>(第二版),以及参考了其他网站的一些源代码
;;;------------------------------------------------------------------------
;;;其中程序主段是核心算法，其他的附加程序为取得点集，画凸包边界线，测试大量
;;;点集函数处理所花费的时间。----------------------------------------------
;;;用法: 加载lisp运行test选取点，直线段，或多义线(全是直线段组成)即可。----
;;;************************************************************************
(defun C:test (/ olderr en errmsg oldmode oce sl ss t0 ptlist pp)
  ;;定义错误函数和预处理--------------------
  (setvar "errno" 0)
  (setq olderr *error*)
  (defun *error* (msg)
    (setq en (getvar "errno"))
    (setq errmsg (strcat "errno=" (itoa en) "\nError:" msg))
    (alert errmsg)
    (setq *error* olderr)
  )
  (graphscr)
  (setq oldmode (getvar "osmode"))
  (setq oce (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (command ".ucs" "W")
  ;;也可以用其他方式取得点集----------------
  ;;取点，画线，并对函数用时计算------------
  (setq	sl '((-4 . "<OR")
	     (0 . "POINT")
	     (0 . "LINE")
	     (0 . "POLYLINE")
	     (0 . "LWPOLYLINE")
	     (-4 . "OR>")
	    )
  )
  (setq ss (ssget sl))
  (setq ptlist (getpt ss))
  (setq t0 (getvar "TDUSRTIMER"))
  (setq pp (hull ptlist))
  (princ "\n用时")
  (princ (* (- (getvar "TDUSRTIMER") t0) 86400))
  (princ "秒")
  (if (= nil pp)
    (progn
      (alert "点的有效数目太小，请重新输入!")
      (command ".ucs" "p")
      (setvar "osmode" oldmode)
      (setvar "cmdecho" oce)
      (princ)
    )
    (progn
      ;;画凸包边界线------------------------
      (setvar "osmode" 0)
      (entmake
	(append
	  '((0 . "lwpolyline")
	    (100 . "AcDbEntity")
	    (100 . "AcDbPolyline")
	   )
	  (list (cons 90 (length pp)))
	  (mapcar '(lambda (x) (cons 10 (list (car x) (cadr x)))) pp)
	  (list (cons 70 1))
	  (list (cons 62 1))
	)
      )
      (command ".ucs" "P")
      (setvar "osmode" oldmode)
      (setvar "cmdecho" oce)
      (princ)
    )
  )
)
;;;*****************************************
;;;*****************************************
;;;程序主段，可以单独成为函数---------------
(defun hull (ptlist / pfirst p0 p1 p2 pp)
  (cond
    ( (= (length ptlist) 0)
      nil
    )
    ( (or nil (= (length ptlist) 1) (= (length ptlist) 2))
      (progn
        (alert "你输入的点为两点或一点!")
        ptlist
      )
    )
    ( t
      (progn
        ;;计算--------------------------------
        (setq pfirst (maxium ptlist))
        (setq p1 pfirst
	      p0 (list (car pfirst) (+ 1.0 (cadr pfirst)))
        )
        (setq p2 (angmax ptlist p0 p1))
        (setq pp (cons p2 (list p1)))
        (while (not (equal pfirst p2 1e-8))
	  (setq pp (cons p2 pp))
	  (setq p0 p1
	        p1 p2
	        p2 (angmax ptlist p0 p1)
	  )
        )pp     
      )
    )
  )
)
;;;程序主段结束-----------------------------
;;;*****************************************
;;;*****************************************
;;依据晓东网站的代码改写而成的取点函数------
(defun getpt (ss / i listpp a b c d)
  (setq	i 0
	listpp nil
  )
  (if ss
    (repeat (sslength ss)
      (setq a (ssname ss i))
      (setq b (entget a))
      (setq ename (cdr (assoc 0 b)))
      (cond
	((or nil (= ename "POLYLINE") (= ename "LWPOLYLINE"))
	 (progn
	   (setq c (xdl-pl-vertexs a))
	   (setq listpp (append c listpp))
	 )
	)
	((= ename "LINE")
	 (progn
	   (setq c (cdr (assoc 10 b)))
	   (setq d (cdr (assoc 11 b)))
	   (setq listpp (cons c listpp))
	   (setq listpp (cons d listpp))
	 )
	)
	((= ename "POINT")
	 (progn
	   (setq c (cdr (assoc 10 b)))
	   (setq listpp (cons c listpp))
	 )
	)
      )
      (setq i (1+ i))
    )
  )
  listpp
)
;;定义顺时针方向的夹角为正值，反之为负
(defun ang (p1 p0 p2 / x)
  (setq x (- (angle p1 p2) (angle p1 p0)))
  (cond
    ((equal p1 p2 1e-8) 0)
    ((<= (abs (- x 1e-8)) Pi) x)
    (t (- x (* (/ x (abs x)) 2 Pi)))
  )
)
;;求点集中顺时针方向的夹角的最大值的点
(defun angmax (ptlist p0 p1 / ppp)
  (setq ppp (mapcar '(lambda (x) (ang p1 p0 x)) ptlist))
  (nth (vl-position (apply 'max ppp) ppp) ptlist)
)
;;排序函数----------------------------
(defun maxium (ptlist)
  (car
    (vl-sort ptlist
	     '(lambda (e1 e2)
		(if (equal (car e1) (car e2) 1e-8)
		  (> (cadr e1) (cadr e2))
		  (> (car e1) (car e2))
		)
	      )
    )
  )
)
;;取得多边形顶点------------------感谢eachy!
(defun xdl-pl-vertexs (e / n lst)
  (if (= e nil)
    nil
    (progn
      (setq lst
	     (repeat (setq n (fix (1+ (vlax-curve-getendparam e))))
	       (setq lst
		      (cons (vlax-curve-getpointatparam e (setq n (1- n))) lst)
	       )
	     )
      )
      (if (= 0 (cdr (assoc 70 (entget e))))
	lst
	(cdr lst)
      )
    )
  )
)