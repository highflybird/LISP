;|*************************************************************;
软件作者: Highflybird                                          ;
软件用途: 抛物线生成程序                                       ;
日期地点: 2020.10.25 深圳                                      ;
程序语言: AutoLISP,Visual LISP                                 ;
版本号:   Ver. 0.20.1025                                       ;
===============================================================;
================================================================
本软件为开源软件: 以下是开源申明:                               
----------------------------------------------------------------
本页面的软件遵照 GPL协议开放源代码，您可以自由传播和修改，在遵照
下面的约束条件的前提下:                                         
                                                                
一. 只要你在本开源软件的每一副本上明显和恰当地出版版权声明，保持
    此许可证的声明和没有担保的声明完整无损，并和程序一起给每个其
    他的程序接受者一份许可证的副本，你就可用任何媒体复制和发布你
    收到的原始程序的源代码。你也可以为转让副本的实际行动收取一定
    费用，但必须事先得到的同意。                                
二. 你可以修改本开源软件的一个或几个副本或程序的任何部分，以此形
    成基于程序的作品。只要你同时满足下面的所有条件，你就可以按前
    面第一款的要求复制和发布这一经过修改的程序或作品。          
  1.你必须在修改的文件中附有明确说明：你修改了这一文件及具体的修
    改日期。                                                    
  2.你必须使你发布或出版的作品（它包含程序的全部或一部分，或包含
    由程序的全部或部分衍生的作品）允许第三方作为整体按许可证条款
    免费使用。                                                  
  3.如果修改的程序在运行时以交互方式读取命令，你必须使它在开始进
    入常规的交互使用方式时打印或显示声明: 包括适当的版权声明和没
    有担保的声明（或者你提供担保的声明）；用户可以按此许可证条款
    重新发布程序的说明；并告诉用户如何看到这一许可证的副本。（例
    外的情况: 如果原始程序以交互方式工作，它并不打印这样的声明，
    你的基于程序的作品也就不用打印声明。                        
三. 只要你遵循一、二条款规定，您就可以自由使用并传播本源代码，但
    必须原封不动地保留原作者信息。                              
================================================================
**************************************************************|;
(defun c:pwx1 (/ a b c m n p s)
  (initget 3)
  (setq a (getreal "\n请输入抛物线方程系数a:"))
  (initget 1)
  (setq b (getreal "\n请输入抛物线方程系数b:"))
  (initget 1)
  (setq c (getreal "\n请输入抛物线方程系数c:"))
  (initget 1)
  (setq m (getdist "\n请输入下届:"))
  (initget 1)
  (setq n (getdist "\n请输入上届:"))
  (if (equal m n 1e-8)
    (alert "上下届不能相等！请重新输入。")
    (progn
      (initget 9)
      (setq p (getpoint "\n请输入插入点（代表坐标原点）:"))
      (setq p (trans p 1 0))
      (ent:make_xline p '(1 0 0))
      (ent:make_xline p '(0 1 0))
      (setq S (MATH:GetArgumentsByEquation a b c m n p))
      (apply 'ent:SplineBy3p s)
    )
  )
  (princ)
)

;;;=============================================================
;;; 功能: 根据抛物线三点绘制抛物线                              
;;; 输入: 抛物线的起点和终点以及极值点                          
;;; 输出: 所求的抛物线                                          
;;;=============================================================
;;; a = RootOf(y1^2 - 2*y1*y2 + y2^2 + (-2*y1 - 2*y2 + 4*y3)*_Z 
;;;     + _Z^2)/(x1 - x2)^2                                     
;;; b = -(x1*RootOf(y1^2 - 2*y1*y2 + y2^2 +(-2*y1 - 2*y2 + 4*y3)
;;;     *_Z + _Z^2) + x2*RootOf(y1^2 - 2*y1*y2 + y2^2 + (-2*y1 -
;;;     2*y2 + 4*y3)*_Z + _Z^2) - x1*y1 + x1*y2 + x2*y1 - x2*y2)
;;;     /(x1 - x2)^2                                            
;;; c = (RootOf(y1^2 - 2*y1*y2 + y2^2 + (-2*y1 - 2*y2 + 4*y3)*_Z
;;;     + _Z^2)*x1*x2 + x1^2*y2 - x1*x2*y1 - x1*x2*y2 + x2^2*y1)
;;;     /(x1 - x2)^2                                            
;;;-------------------------------------------------------------
(defun c:pwx2 (/ A A1 A2 B d K K1 K2 M N P1 P2 P3 
	         PT Q1 Q2 X1 X2 X3 Y1 Y2 Y3 Z3)
  (initget 9)
  (setq p1 (getpoint "\n请输入起始点:"))
  (setq x1 (car p1))
  (setq y1 (cadr p1))
  
  (initget 9)
  (setq p2 (getpoint "\n请输入终止点:"))
  (setq x2 (car p2))
  (setq y2 (cadr p2))
  
  (initget 9)
  (setq p3 (getpoint "\n请输入最低（高）点:"))
  (setq y3 (cadr p3))
  (setq z3 (caddr p3))
  (if (or
	(equal (cos (angle p1 p2)) 0 1e-8)
	(and (equal (sin (angle p1 p3)) 0  1e-8)
	     (equal (sin (angle p2 p3)) 0  1e-8)
	)
	(< y1 y3 y2)
	(< y2 y3 y1)
      )
    (alert
      "输入的三点不符合要求!请满足如下要求后重新输入:
      \n1. 输入的抛物线的三点不在同一直线上。
      \n2. 最低点或者最高点的在两端点之上或之下。
      \n3. 起始点和终止点的X值不相同。"
    )
    (progn
      (setq m (- (+ y1 y2) y3 y3))
      (setq n (* 2 (sqrt (* (- y1 y3) (- y2 y3)))))
      (setq k (- x1 x2))
      (setq d (/ (- y1 y2) k))
      (setq k (* k k))
      ;;根据方程求出抛物线方程的两个系数a,b
      (if (and (>= y3 y1) (>= y3 y2))
	(setq a (/ (- m n) k))
	(setq a (/ (+ m n) k))
      )
      (setq b (- d (* a (+ x1 x2))))
      ;;求出端点切线
      (setq k1 (+ b (* 2 a x1)))
      (setq a1 (atan k1 1))
      (setq q1 (polar p1 a1 1000))
      ;;求出另一个端点切线
      (setq k2 (+ b (* 2 a x2)))
      (setq a2 (atan k2 1))
      (setq q2 (polar p2 a2 1000))
      ;;两切线相交，则为其中间的控制点
      (setq pt (inters p1 q1 p2 q2 nil))
      (setq x3 (/ b -2 a))
      ;;根据三点方式画抛物线
      (ENT:SplineBy3P
	(trans p1 1 0)
	(trans pt 1 0)
	(trans p2 1 0)
	1
      )
    )
  )
  (princ)
)

;;;=============================================================
;;; 功能: 根据抛物线三点绘制抛物线                              
;;; 输入: 抛物线的起点和终点以及极值点                          
;;; 输出: 所求的抛物线                                          
;;;=============================================================
;;;-------------------------------------------------------------
(defun c:pwx3 (/ A A1 A2 B d K K1 K2 M N P1 P2 P3 PT Q1 Q2 X1 X2 X3 Y1 Y2 Y3 Z3)	         
  (initget 9)
  (setq p1 (getpoint "\n请输入起始点:"))
  (initget 9)
  (setq p2 (getpoint "\n请输入终止点:"))
  (initget 9)
  (setq p3 (getpoint "\n请输入第三点:"))
  (setq p1 (trans p1 1 0))
  (setq p2 (trans p2 1 0))
  (setq p3 (trans p3 1 0))
  (mapcar 'set  '(p1 p2 p3) (ALG:Sort3PbyX p1 p2 p3))
  (mapcar 'set  '(x1 x2 x3) (mapcar 'car (list p1 p2 p3)))
  (if (or
	(equal x1 x2 1e-8)
	(equal x2 x3 1e-8)
	(equal x3 x1 1e-8)
      )
    (alert
      "输入的三点不符合要求!请满足如下要求后重新输入:
      \n1. 输入的抛物线的三点不在同一直线上。
      \n2. 最低点或者最高点的在两端点之上或之下。
      \n3. 起始点和终止点的X值不相同。"
    )
    (progn
      (mapcar 'set '(A B C) (MATH:GetArgumentsBy3P p1 p2 p3))
      (setq S (MATH:GetArgumentsByEquation a b c x1 x3 '(0 0)))
      (apply 'ent:SplineBy3p s)
    )
  )
  (princ)
)

;;;=============================================================
;;; 功能: 根据三点的X值排序三点                                 
;;; 输入: 三点p1,p2,p3（点表）                                  
;;; 输出: 按照X值从小到大的排序三点                             
;;;=============================================================
(defun ALG:Sort3PbyX (p1 p2 p3 / x1 x2 x3)
  (setq x1 (car p1)
	x2 (car p2)
	x3 (car p3)
  )
  (cond
    ( (<= x1 x2 x3)
      (list p1 p2 p3)
    )
    ( (<= x1 x3 x2)
      (list p1 p3 p2)
    )
    ( (<= x2 x3 x1)
      (list p2 p3 p1)
    )
    ( (<= x2 x1 x3)
      (list p2 p1 p3)
    )
    ( (<= x3 x1 x2)
      (list p3 p1 p2)
    )
    ( t
      (list p3 p2 p1)
    ) 
  )
)

;;;=============================================================
;;; 功能: 根据抛物线上三点获取抛物系数                          
;;; 输入: 抛物线上三点p1,p2,p3 （点表，至少二维）               
;;; 输出: 抛物线方程y=Ax^2+Bx+C的三个系数A,B,C                  
;;;=============================================================
(defun MATH:GetArgumentsBy3P (p1 p2 p3 / A B C D DX1 DX2 DX3 DY1 DY2 DY3 X1 X2 X3 Y1 Y2 Y3)
  (mapcar 'set  '(x1 x2 x3) (mapcar 'car (list p1 p2 p3)))
  (mapcar 'set  '(y1 y2 y3) (mapcar 'cadr (list p1 p2 p3)))
  (if (not (or (equal x1 x2 1e-8) (equal x2 x3 1e-8) (equal x3 x1 1e-8)))
    (setq dx1 (- x1 x2)
	  dx2 (- x2 x3)
	  dx3 (- x3 x1)
	  dy1 (- y1 y2)
	  dy2 (- y2 y3)
	  dy3 (- y3 y1)
	  A (/ (- (* dx3 dy1) (* dx1 dy3)) (float (* dx1 dx2 dx3)))
	  B (- (/ dy1 (float dx1)) (* A (+ x1 x2)))
          C (- y1 (* A x1 x1) (* B x1))
          D (list A B C)
    )
  )
)

;;;=============================================================
;;; 功能: 根据抛物线系数获取圆锥曲线参数                        
;;; 输入: 抛物线系数a,b,c和上下界m,n以及相对点（插入点）        
;;; 输出: 抛物线上的三点                                        
;;;=============================================================
(defun MATH:GetArgumentsByEquation (a b c m n p / A1 A2 P1 P2 P3 Q1 Q3)
  (setq p1 (list m (+ (* m m a) (* b m) c) 0.0))
  (setq p3 (list n (+ (* n n a) (* b n) c) 0.0))
  (setq a1 (atan (+ (* 2 a m) b)))
  (setq a2 (atan (+ (* 2 a n) b)))
  (setq q1 (polar p1 a1 10000))
  (setq q3 (polar p3 a2 10000))
  (setq p2 (inters p1 q1 p3 q3 nil))
  (setq p1 (mapcar '+ p1 p))
  (setq p2 (mapcar '+ p2 p))
  (setq p3 (mapcar '+ p3 p))
  (list p1 p2 p3 1)
)

;;;-------------------------------------------------------------
;;;创建一条直线段                                       	
;;;输入: 两个三维或者二维的点                           	
;;;输出: 线段实体的图元名                               	
;;;-------------------------------------------------------------
(defun Ent:Make_Line (p q)
  (entmakeX (list '(0 . "LINE") (cons 10 p) (cons 11 q)))
)

;;;-------------------------------------------------------------
;;; 创建一条射线                                         	
;;; 输入: 射线通过的基点和方向矢量                              
;;; 输出: 创建后的射线图元名                               	
;;;-------------------------------------------------------------
(defun Ent:make_XLine (p v)
  (entmakex
    (list
      '(0 . "XLINE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbXline")
      (cons 10 p)
      (cons 11 v)
    )
  )
)

;;;=============================================================
;;; 功能: 用Spline绘制圆锥曲线                                  
;;; 输入: 圆锥曲线参数，三点以及权重                            
;;; 输出: 满足条件的圆锥曲线                                    
;;;=============================================================
(defun ENT:SplineBy3P (p1 p2 p3 w)
  (entmakex
    (list
      '(0 . "SPLINE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbSpline")
      '(70 . 12)
      '(71 . 2)
      '(72 . 6)
      '(73 . 3)
      '(74 . 0)
      '(42 . 1.0e-010)
      '(43 . 1.0e-010)
      '(40 . 0.0)
      '(40 . 0.0)
      '(40 . 0.0)
      '(40 . 1.0)
      '(40 . 1.0)
      '(40 . 1.0)
      (cons 10 p1)
      (cons 41 1.0)
      (cons 10 p2)
      (cons 41 w)
      (cons 10 p3)
      (cons 41 1.0)
    )
  )
)

;;;-------------------------------------------------------------
;;;创建轻多段线                                         	
;;;输入: 二维的点集                                     	
;;;输出: 轻多段线实体名                                 	
;;;-------------------------------------------------------------
(defun Ent:Make_LWPoly (pts closed /)
  (entmakeX                                              
    (VL-LIST*
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbPolyline")
      (cons 90 (length pts))                      	        ;顶点个数
      (cons 70 (if closed 1 0))                          	;闭合的
      (mapcar (function (lambda (x) (cons 10 x))) pts)  	;多段线顶点
    )
  )
)


(princ "\nPWX1 是根据抛物线方程的三个系数绘制。\nPWX2 是根据两端点和最低（高）点绘制。")
(princ)

;|;
;(1-t)^2*P0+2*t*(1-t)*P1+t^2*P2

(defun getpts (p0 p1 p2 w)
  (setq p0x (car p0))
  (setq p1x (car p1))
  (setq p2x (car p2))
  (setq p0y (cadr p0))
  (setq p1y (cadr p1))
  (setq p2y (cadr p2))
  (setq x 0)
  (setq l nil)
  (repeat 21
    (setq y (1- x))
    (setq ptx (+ (* y y p0x) (* -2 w x y p1x) (* x x p2x)))
    (setq pty (+ (* y y p0y) (* -2 w x y p1y) (* x x p2y)))
    (setq l (cons (list ptx pty) l))
    (setq x (+ x 0.05))
  )
  (setq l (reverse l))
)

(defun c:tt2()
  (initget 9)
  (setq p0 (getpoint "\n第一点:"))
  (initget 9)
  (setq p1 (getpoint "\n第一点:"))
  (initget 9)
  (setq p2 (getpoint "\n第一点:"))
  (setq p0 (trans p0 1 0))
  (setq p1 (trans p1 1 0))
  (setq p2 (trans p2 1 0))
  (setq ps (getpts p0 p1 p2 1))
  (Ent:Make_LWPoly ps nil)
  (princ)
)

;|;