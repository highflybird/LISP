;|*************************************************************;
软件作者: Highflybird                                          ;
软件用途: 抛物线生成程序                                       ;
创建日期: 2020.10.25 深圳                                      ;
更新日期: 2021.09.08 深圳                                      ;
程序语言: AutoLISP,Visual LISP                                 ;
版本号:   Ver. 1.21.0904                                       ;
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

;;;=============================================================
;;; 功能: 根据圆锥曲线上四点绘制此曲线（平行于X轴）             
;;; 输入: 交互输入四个点                                        
;;; 输出: 绘制此圆锥曲线（如果是在UCS,则此抛物线平行于此UCS）   
;;;-------------------------------------------------------------
(defun c:yzx (/ p1 p2 p3 p4 ret a b c)
  (initget 9)
  (setq p1 (getpoint "\n请输入第一点:"))
  (initget 9)
  (setq p2 (getpoint "\n请输入第二点:"))
  (initget 9)
  (setq p3 (getpoint "\n请输入第三点:"))
  (initget 9)
  (setq p4 (getpoint "\n请输入第四点:"))
  (ent:make_point1 p1)
  (ent:make_point1 p2)
  (ent:make_point1 p3)
  (ent:make_point1 p4)
  (if (setq ret (Geo:ConicBy4P p1 p2 p3 p4))
    (if (caddr ret)
      (apply 'ent:make_ellipse1 ret)
      (progn
	(setq c (caar ret))
	(setq a (cadar ret))
	(setq b (caddar ret))
	(ent:make_xline1 c (list a b))
	(ent:make_xline1 c (list a (- b)))
	(foreach n (cadr ret)
	  (apply 'ent:SplineBy3p1 n)
	)
      )
    )
  )
  (princ)
)

;;;=============================================================
;;; 功能: 根据双曲线的中心点和两个半轴长绘制双曲线              
;;; 输入: 交互输入插入点和两个半轴长                            
;;; 输出: 绘制此圆锥曲线（如果是在UCS,则此抛物线平行于此UCS）   
;;;-------------------------------------------------------------
(defun c:sqx (/ a b c)
  (initget 9)
  (setq c (getpoint "\n插入点[设置(S)]<默认>:"))
  (initget 7)
  (setq a (getdist c "\n半轴长a:"))
  (initget 7)
  (setq b (getdist c "\n半轴长b:"))

  (ent:make_xline1 c (list a b))
  (ent:make_xline1 c (list a (- b)))
  
  (foreach n (GEO:ConicByAxis a b c (+ a a) nil)
    (apply 'ent:SplineBy3p1 n)
  )
  (princ)
)

;;;=============================================================
;;; 功能: 根据双曲线的中心点和两个半轴长获取SPLINE的三点参数    
;;; 参数: 两个半轴长a,b和中心C，x取值范围，isY是否以Y轴对称     
;;; 返回: 两条SPLINE的参数（三个点和权重）                      
;;;-------------------------------------------------------------
(defun GEO:ConicByAxis (a b c x isY / cx cy mx nx px py qx qy wt)
  (if (> x a)
    (progn
      (setq wt (/ x a))
      (setq py (* b (sqrt (1- (* wt wt)))))                     ;起点Y值-->此处要检查能否开平方?
      (setq mx (/ a wt))			                ;控制点坐标
      (setq cx (car c))
      (setq cy (cadr c))
      (setq Qx (- cx x))
      (setq Px (+ cx x))
      (setq Nx (- cx mx))
      (setq Mx (+ cx mx))
      (setq Qy (- cy py))
      (setq Py (+ cy py))
      (if isY
	(list
	  (list (list py px) (list cy mx) (list qy px) wt)
	  (list (list py qx) (list cy nx) (list qy qx) wt)
	)
	(list
	  (list (list px py) (list mx cy) (list px qy) wt)
	  (list (list qx py) (list nx cy) (list qx qy) wt)
	)
      )
    )
  )
)

;;;=============================================================
;;; 功能: 根据圆锥曲线上四点绘制此曲线(平行于X轴线)             
;;; 参数: 圆锥曲线上四点p1,p2,p3,p4                             
;;; 返回: 圆锥曲线的参数                                        
;;;-------------------------------------------------------------
(defun GEO:ConicBy4P (p1 p2 p3 p4 / A B C D CX CY LA LB MAT RET
		                    X1 X2 X3 X4 Y1 Y2 Y3 Y4)
  ;;为了简化计算，设置P1为原点
  (setq p2 (mapcar '- p2 p1))					
  (setq p3 (mapcar '- p3 p1))
  (setq p4 (mapcar '- p4 p1))
  (mapcar 'set '(x1 x2 x3 x4) (mapcar 'car (list p1 p2 p3 p4)))
  (mapcar 'set '(y1 y2 y3 y4) (mapcar 'cadr (list p1 p2 p3 p4)))
  ;;代入坐标，获取方程组系数
  (setq mat (list
	      (list (* x2 x2) x2 y2 (- (* y2 y2))) 
              (list (* x3 x3) x3 y3 (- (* y3 y3))) 
	      (list (* x4 x4) x4 y4 (- (* y4 y4)))
	    )
  )
  (setq ret (Mat:Gauss_Equations mat))				;高斯消元法解方程组
  (mapcar 'set '(A B C) ret)					;二次曲线方程系数
  (if (and A (not (Mat:Zerop A)))				;此方程有解且不为抛物线
    (setq D  (* 0.25 (+ (* C C) (/ (* B B) A))))
  )
  (if (and D (not (Mat:Zerop D)))                               ;不应轴长为0
    (progn    
      (setq la (sqrt (abs (/ D A))))                            ;半长轴值
      (setq lb (sqrt (abs D)))		                        ;半短轴值
      (setq cx (+ x1 (/ b -2 A)))		                ;中心X坐标
      (setq cy (+ y1 (* -0.5 c)))                               ;中心Y坐标
      (setq x2 (+ x1 x2) x3 (+ x1 x3) x4 (+ x1 x4))             ;把各点坐标归位
      (setq y2 (+ y1 y2) y3 (+ y1 y3) y4 (+ y1 y4))		;把各点坐标归位
      (if (> A 0)
	(if (> D 0)
	  (list (list cx cy) la lb)                             ;返回一个椭圆
	)
	(if (> D 0)                                             
	  (list 
	    (list (list cx cy) la lb)                           ;中心和两个半轴长
	    (Ent:GetSplineOfConic lb la cy cx y1 y2 y3 y4 T)    ;关于Y轴对称的双曲线
	  )
	  (list 
	    (list (list cx cy) la lb)                           ;中心和两个半轴长
	    (Ent:GetSplineOfConic la lb cx cy x1 x2 x3 x4 nil)  ;关于X轴对称的双曲线
	  )
	)
      )
    )
  )
)

;;;=============================================================
;;; 功能: 获取圆锥曲线参数的子函数                              
;;; 参数: 两个半轴长度la,lb和中心坐标cx,cy以及四点X值，是否Y对称
;;; 返回: 两条SPLINE的参数（三个点和权重）                      
;;;-------------------------------------------------------------
(defun Ent:GetSplineOfConic (la lb cx cy x1 x2 x3 x4 isY /
			     d1 d2 d3 d4 px py qx qy mx nx wt) 
  (setq d1 (abs (- x1 cx)))
  (setq d2 (abs (- x2 cx)))
  (setq d3 (abs (- x3 cx)))
  (setq d4 (abs (- x4 cx)))
  (setq px (max d1 d2 d3 d4))					;起点X值
  (setq wt (/ px la))			                        ;第二点权重
  (setq py (* lb (sqrt (1- (* wt wt)))))                        ;起点Y值-->此处要检查能否开平方?
  (setq mx (/ la wt))			                        ;控制点坐标
  (setq Qy (- cy py))
  (setq Py (+ cy py))
  (setq Qx (- cx px))
  (setq Px (+ cx px))
  (setq Nx (- cx mx))
  (setq Mx (+ cx mx))
  (if isY
    (list
      (list (list py px) (list cy mx) (list qy px) wt)
      (list (list py qx) (list cy nx) (list qy qx) wt)
    )
    (list
      (list (list px py) (list mx cy) (list px qy) wt)
      (list (list qx py) (list nx cy) (list qx qy) wt)
    )
  )
)

;;;=============================================================
;;; 功能: 根据抛物线三点绘制抛物线                              
;;; 输入: 交互输入抛物线上的三个点                              
;;; 输出: 绘制抛物线段（如果是在UCS,则此抛物线平行于此UCS）     
;;;-------------------------------------------------------------
(defun c:pwx1 (/ P1 P2 P3 S)
  (initget 9)
  (setq p1 (getpoint "\n请输入第一点:"))
  (initget 9)
  (setq p2 (getpoint "\n请输入第二点:"))
  (initget 9)
  (setq p3 (getpoint "\n请输入第三点:"))
  (if (setq S (MATH:GetArgumentsBy3P p1 p2 p3))
    (apply 'ent:SplineBy3p1 s)
    (alert "请输入的不共线三点!")
  )
  (princ)
)

;;;=============================================================
;;; 功能: 根据抛物线的三个系数绘制抛物线，原点为指定点或(0,0)   
;;; 输入: 交互输入抛物线方程的三个系数a,b,c                     
;;; 输出: 绘制抛物线段（如果是在UCS,则此抛物线平行于此UCS）     
;;;-------------------------------------------------------------
(defun c:pwx2 (/ a b c m n p s)
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
      (setq p (getpoint "\n请输入插入点以代表坐标原点<默认(0,0)>:"))
      (if (null p)
	(setq p '(0 0 0))
      )
      (setq S (MATH:GetArgumentsByEquation a b c m n p))
      (setq p (trans p 1 0))
      (ent:make_xline p (trans '(1 0 0) 1 0 T))
      (ent:make_xline p (trans '(0 1 0) 1 0 T))
      (apply 'ent:SplineBy3p1 S)
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
(defun c:pwx3 (/ A A1 A2 B d K K1 K2 M N P1 P2 P3 PT Q1 Q2 X1 X2 X3 Y1 Y2 Y3 Z3)
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
      \n1. 输入的抛物线的三点不共线。
      \n2. 最低点或者最高点的Y值在两端点之间。
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
      (setq q1 (polar p1 (atan k1) 1000))
      ;;求出另一个端点切线
      (setq k2 (+ b (* 2 a x2)))
      (setq q2 (polar p2 (atan k2) 1000))
      ;;两切线相交，则为其中间的控制点
      (setq pt (inters p1 q1 p2 q2 nil))
      (setq x3 (/ b -2 a))
      ;;根据三点方式画抛物线
      (ENT:SplineBy3P1 p1 pt p2 1)
    )
  )
  (princ)
)

;;;=============================================================
;;; 功能: 根据抛物线上三点获取抛物线方程系数                    
;;; 输入: 抛物线上三点p1,p2,p3 （点表，至少二维）               
;;; 输出: 抛物线方程y=Ax^2+Bx+C的三个系数A,B,C                  
;;;=============================================================
(defun MATH:GetEquationBy3P (p1 p2 p3 / A B C D DX1 DX2 DX3 DY1
			     DY2 DY3 X1 X2 X3 Y1 Y2 Y3)
  (mapcar 'set '(x1 x2 x3) (mapcar 'car (list p1 p2 p3)))
  (mapcar 'set '(y1 y2 y3) (mapcar 'cadr (list p1 p2 p3)))
  (if (not (GEO:Colinearity p1 p2 p3))
    (setq dx1 (- x1 x2)
	  dx2 (- x2 x3)
	  dx3 (- x3 x1)
	  dy1 (- y1 y2)
	  dy2 (- y2 y3)
	  dy3 (- y3 y1)
	  dx1 (float dx1)
	  A (/ (- (* dx1 dy2) (* dx2 dy1)) (* dx1 dx2 dx3))
	  B (- (/ dy1 dx1) (* A (+ x1 x2)))
          C (- y1 (* A x1 x1) (* B x1))
          D (list A B C)
    )
  )
)

;;;=============================================================
;;; 功能: 根据抛物线上三点获取获取圆锥曲线参数                  
;;; 输入: 抛物线上三点p1,p2,p3 （点表，至少二维）               
;;; 输出: CAD中的抛物线的三点　　　　　　　　　　　　　　　   　
;;;=============================================================
(defun MATH:GetArgumentsBy3P (p1 p2 p3 / A B C DX1 DX2 DX3 DY1
			      DY2 DY3 Q1 Q2 Q3 X1 X2 X3 Y1 Y2 Y3)
  (mapcar 'set '(p1 p2 p3) (ALG:Sort3PbyX p1 p2 p3))
  (mapcar 'set '(x1 x2 x3) (mapcar 'car (list p1 p2 p3)))
  (mapcar 'set '(y1 y2 y3) (mapcar 'cadr (list p1 p2 p3)))
  (if (not (GEO:Colinearity p1 p2 p3))
    (setq dx1 (- x1 x2)
	  dx2 (- x2 x3)
	  dx3 (- x3 x1)
	  dy1 (- y1 y2)
	  dy2 (- y2 y3)
	  dy3 (- y3 y1)
	  dx1 (float dx1)
	  A   (/ (- (* dx1 dy2) (* dx2 dy1)) (* dx1 dx2 dx3))
	  B   (- (/ dy1 dx1) (* A (+ x1 x2)))
          C   (- y1 (* A x1 x1) (* B x1))
          q1  (polar p1 (atan (+ (* 2 A x1) B)) 666)
          q3  (polar p3 (atan (+ (* 2 A x3) B)) 666)
          q2  (list p1 (inters p1 q1 p3 q3 nil) p3 1)
    )
  )
)

;;;=============================================================
;;; 功能: 根据抛物线系数获取圆锥曲线参数                        
;;; 输入: 抛物线系数a,b,c和上下界m,n以及相对点（插入点）        
;;; 输出: 抛物线的两端点及其切线交点                            
;;;=============================================================
(defun MATH:GetArgumentsByEquation (a b c m n p / P1 P2 P3 Q1 Q3)
  (setq p1 (list m (+ (* m m a) (* b m) c) 0.0))
  (setq p3 (list n (+ (* n n a) (* b n) c) 0.0))
  (setq q1 (polar p1 (atan (+ (* 2 a m) b)) 666))
  (setq q3 (polar p3 (atan (+ (* 2 a n) b)) 666))
  (if (setq p2 (inters p1 q1 p3 q3 nil))
    (list (mapcar '+ p p1) (mapcar '+ p p2) (mapcar '+ p p3) 1)
  )
)

;;;=============================================================
;;; 判断一个数在误差范围内是否为0                               
;;;=============================================================
(defun Mat:Zerop (x)
  (equal x 0 1e-14)
)

;;;=============================================================
;;; 判断某一行是否要加入高斯消元法的递归表中                    
;;;=============================================================
(defun Mat:Join_Gauss (n l / y)
  (setq n (cdr n))
  (setq y (cdr (reverse n)))
  (if (or (null y) (vl-every 'Mat:Zerop y))			;系数如果全为0，则不做改变
    l
    (cons n l)						        ;系数如果不全为0
  )
)

;;;=============================================================
;;; 高斯消元法之矩阵分解                                        
;;; 根据网上程序做了修改                                        
;;;=============================================================
(defun Mat:Gauss (m / A B L R x y)
  (if (car m)
    (progn
      (setq r (car m))
      (setq x (abs (car r)))
      (foreach n (cdr m)
        (if (> (setq y (abs (car n))) x)
          (setq r n x y)					;获取其首元素绝对值取最大值所在项
        )
      )				
      (setq a (float (car r)))					;需转化为浮点数，防止整除
      (if (equal a 0 1e-14)
	(Mat:Gauss (mapcar 'cdr m))                 		;去掉全部为零的列
	(progn 
	  (setq r (mat:vxs r (/ 1.0 a)))			;归一化
	  (foreach n m
	    (if (equal (setq b (car n)) 0 1e-14)                ;是否首元素为0
	      (setq l (Mat:Join_Gauss n l))
	      (setq n (mapcar '- n (mat:vxs r b))               ;消元法
	            l (Mat:Join_Gauss n l)        		;不相同则加入进去
              )
	    )
	  )
	  (cons r (Mat:Gauss l))
	)
      )
    )
  )
)

;;;=============================================================
;;;从高斯消元法得到的三角形矩阵回代解方程                       
;;;=============================================================
(defun Mat:TriangularForm (m / l)
  (if (and m (= (- (length (car m)) (length m)) 1))
    (progn
      (setq m (reverse m))
      (setq l (cons (cadar m) l))
      (foreach n (cdr m)
	(setq l (cons (- (last n) (mat:dot (cdr n) l)) l))
      )
      l
    )
  )
)

;;;=============================================================
;;;解一次方程组                                                 
;;;=============================================================
(defun Mat:Gauss_Equations (mat)
  (Mat:TriangularForm (Mat:Gauss mat))
)

;;;=============================================================
;;; 向量乘标量(系数)                                            
;;; Vector x Scalar - Lee Mac                                   
;;; Args: v - vector in R^n, s - real scalar                    
;;;=============================================================
(defun MAT:vxs ( v s )
  (mapcar (function (lambda ( n ) (* n s))) v)
)

;;;=============================================================
;;; 两向量的点积                                                
;;; Vector Dot Product                                          
;;; Input: v1,v2 -vectors in R^n                                
;;;=============================================================
(defun MAT:Dot (v1 v2)
  (apply '+ (mapcar '* v1 v2))
)

;;;-------------------------------------------------------------
;;; 功能: 判断平面上的三点是否共线                       	
;;; 输入: 三点 P1,P2,P3                                  	
;;; 输出: T 说明三点共线，否则不共线                     	
;;;-------------------------------------------------------------
(defun GEO:Colinearity	(p1 p2 p3 /)
  ( (lambda (a b c)
      (or
        (equal (+ a b) c 1e-8)
        (equal (+ b c) a 1e-8)
        (equal (+ c a) b 1e-8)
      )
    )
    (distance p1 p2)
    (distance p2 p3)
    (distance p3 p1)
  )
)

;;;=============================================================
;;; 功能: 根据三点的X值排序三点                                 
;;; 输入: 三点p1,p2,p3（点表）                                  
;;; 输出: 按照X值从小到大的排序三点                             
;;;=============================================================
;;; (vl-sort (list p1 p2 p3) '(lambda (x y)(< (car x) (car y))))
(defun ALG:Sort3PbyX (p1 p2 p3 / x1 x2 x3)
  (setq	x1 (car p1)
	x2 (car p2)
	x3 (car p3)
  )
  (if (< x1 x2)
    (if	(< x2 x3)
      (list p1 p2 p3)
      (if (< x3 x1)
	(list p3 p1 p2)
	(list p1 p3 p2)
      )
    )
    (if	(< x3 x2)
      (list p3 p2 p1)
      (if (< x1 x3)
	(list p2 p1 p3)
	(list p2 p3 p1)
      )
    )
  )
)

;;;-------------------------------------------------------------
;;;创建一个点(WCS)                                           	
;;;输入: 一个三维或者二维的点                           	
;;;输出: 点实体的图元名                                 	
;;;-------------------------------------------------------------
(defun Ent:Make_Point (p)
  (entmakex (list '(0 . "POINT") (cons 10 p)))
)

;;;-------------------------------------------------------------
;;;创建一个点(UCS)                                           	
;;;输入: 一个三维或者二维的点                           	
;;;输出: 点实体的图元名                                 	
;;;-------------------------------------------------------------
(defun Ent:Make_Point1 (p)
  (entmakex (list '(0 . "POINT") (cons 10 (trans p 1 0))))
)

;;;-------------------------------------------------------------
;;;创建一条直线段(WCS)                                       	
;;;输入: 两个三维或者二维的点                           	
;;;输出: 线段实体的图元名                               	
;;;-------------------------------------------------------------
(defun Ent:Make_Line (p q)
  (entmakeX (list '(0 . "LINE") (cons 10 p) (cons 11 q)))
)

;;;-------------------------------------------------------------
;;;创建一条直线段(UCS)                                       	
;;;输入: 两个三维或者二维的点                           	
;;;输出: 线段实体的图元名                               	
;;;-------------------------------------------------------------
(defun Ent:Make_Line1 (p q)
  (Ent:Make_Line (trans p 1 0) (trans q 1 0))
)

;;;-------------------------------------------------------------
;;; 创建一条射线(WCS)                                         	
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

;;;-------------------------------------------------------------
;;; 创建一条射线(UCS)                                         	
;;; 输入: 射线通过的基点和方向矢量                              
;;; 输出: 创建后的射线图元名                               	
;;;-------------------------------------------------------------
(defun Ent:make_XLine1 (p v)
  (Ent:make_XLine (trans p 1 0) (trans v 1 0 T))
)

;;;-------------------------------------------------------------
;;; 创建椭圆(WCS)                                               
;;; 输入: 椭圆的中心center,主轴major,比率ratio                  
;;; 输出: 成功返回椭圆的图元名，否则返回nil                     
;;;-------------------------------------------------------------
(defun Ent:Make_Ellipse	(center major ratio)
  (entmakeX
    (list
      '(0 . "ELLIPSE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbEllipse")
      (cons 10 center)
      (cons 11 major)
      (cons 40 ratio)
    )
  )
)

;;;-------------------------------------------------------------
;;; 创建椭圆(UCS)                                               
;;; 输入: 椭圆的中心center,半长轴la,半短轴lb                    
;;; 输出: 成功返回椭圆的图元名，否则返回nil                     
;;;-------------------------------------------------------------
(defun Ent:Make_Ellipse1 (center la lb)
  (if (> la lb)
    (Ent:Make_Ellipse
      (trans center 1 0)
      (trans (list la 0 0) 1 0 T)
      (/ lb la)
    )
    (Ent:Make_Ellipse
      (trans center 1 0)
      (trans (list 0 lb 0) 1 0 T)
      (/ la lb)
    )
  )
)

;;;=============================================================
;;; 功能: 用Spline绘制圆锥曲线(WCS)                             
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

;;;=============================================================
;;; 功能: 用Spline绘制圆锥曲线(UCS)                             
;;; 输入: 圆锥曲线参数，三点以及权重                            
;;; 输出: 满足条件的圆锥曲线                                    
;;;=============================================================
(defun ENT:SplineBy3P1 (p1 p2 p3 w)
  (ENT:SplineBy3P (trans p1 1 0) (trans p2 1 0) (trans p3 1 0) w)
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

(defun ALG:LengthOfParabola (a x / b c)
  (if (< a 0) (setq a (- a)))
  (setq b (/ 1.0 (+ a a)))
  (setq b (* b b))
  (setq c (sqrt (+ (* x x) b)))
  (* a (+ (* x c) (* b (log (* 2 a (+ x c))))))
)

(defun c:tt()
  (initget 3)
  (setq a (getreal "\n请输入抛物线方程系数a:"))
  (initget 1)
  (setq x (getdist "\n请输入范围:"))
  (setq p (getpoint "\n请输入插入点以代表坐标原点<默认(0,0)>:"))
  (if (null p)
    (setq p '(0 0 0))
  )
  (setq S (MATH:GetArgumentsByEquation a 0 0 0 x p))
  ;;(ent:make_xline1 p '(1 0 0))
  ;;(ent:make_xline1 p '(0 1 0))
  (apply 'ent:SplineBy3p1 S)
  (setq l (ALG:LengthOfParabola a x))
  (setq e (entlast))
  (setq d (vlax-curve-getDistAtParam e (vlax-curve-getendparam e)))
  (setq y (rtos (- d l) 2 20))
  (setq z (rtos (/ (- d l) l) 2 20))
  (princ (strcat "\n两者相差:" y " 百分比:" z))
  (princ)
)
   
(vl-load-com)
(princ "\nYZX 是根据圆锥曲线上四个点绘制。")
(princ "\nSQX 是根据双曲线的中心和两个半轴长绘制。")
(princ "\nPWX1 是根据抛物线上三个点绘制。")
(princ "\nPWX2 是根据抛物线方程的三个系数绘制。")
(princ "\nPWX3 是根据两端点和最低（高）点绘制。")
(princ)