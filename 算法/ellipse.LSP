;|*************************************************************;
软件作者: Highflybird                                          ;
软件用途: 为AutoCAD 的LISP定制的一些算法和函数(椭圆部分)       ;
日期地点: 2012.12.12 深圳                                      ;
修改时间: 2019.05.01 深圳                                      ;
程序语言: AutoLISP,Visual LISP                                 ;
版本号:   Ver. 1.0.19.0501                                     ;
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

;;;*************************************************************
;;; 一、椭圆的基本要素与其创建和更新                            
;;;*************************************************************
                                                                        
;|                                                              
一 椭圆的几个基本参数：                                         
DXF 组码中的几个数值                                            
 10  椭圆中心                                                   
 11  长轴矢量                                                   
 40  长短比率                                                   
 41  起点角度                                                   
 42  终点角度                                                   
210  法线矢量                                                   
这些参数可以由entget得到，也可以由activeX方法得到。             
注: 起点角度和终点角度的计算，如果是不是椭圆弧，是全椭圆，则这个
    数值为：O 和2Pi，如果是椭圆弧，则如图1；                    
    需要注意的是vla-get-StartAngle 和vla-get-StartParameter 在一
    般情况下，是两个不同数值，它们所代表的角度的意义如上图。同样
    去理解:vla-get-EngAngle 和 vla-get-EndParameter. 但是:      
    vla-get-StartParameter = vlax-curve-getStartParam           
    vla-get-EndParameter = vlax-curve-getEndParam               
    vlax-curve-getParamAtPoint 其参数可按照上述方式去理解。     
|;

;;;=============================================================
;;; 功能: 画一个椭圆（平面）                                    
;;; 输入: 中心，长轴，短轴和旋转角度                            
;;; 输出: 成功返回椭圆的图元名，否则返回nil                     
;;;=============================================================
(defun Ent:Make_Ellipse	(cen a b ang) 
  (if (> b a)
    (Ent:Make_Ellipse_1
      cen
      (polar '(0 0 0) (+ ang (* pi 0.5)) b)
      (/ a (float b))
    )
    (Ent:Make_Ellipse_1
      cen
      (polar '(0 0 0) ang a)
      (/ b (float a))
    )
  )
)

;;;=============================================================
;;; 功能: 画一个椭圆（平面）                                    
;;; 输入: 中心，长轴矢量，长短比率                              
;;; 输出: 成功返回椭圆的图元名，否则返回nil                     
;;;=============================================================
(defun Ent:Make_Ellipse_1 (cen maj rat)
  (entmakeX
    (list
      '(0 . "ELLIPSE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbEllipse")
      (cons 10 cen)
      (cons 11 maj)
      (cons 40 rat)
    )
  )
)

;;;=============================================================
;;; 功能: 画一个空三维空间的椭圆                                
;;; 输入: 中心，长轴，短轴、旋转角度和法线矢量                  
;;; 输出: 成功返回椭圆的图元名，否则返回nil                     
;;;=============================================================
(defun Ent:Make_Ellipse_3d (cen a b ang an1 an2 Normal / maj rat)
  (if (> b a)
    (setq maj (polar '(0 0 0) (+ ang (* pi 0.5)) b)
          rat (/ a b 1.0)
    )
    (setq maj (polar '(0 0 0) ang a)
          rat (/ b a 1.0)
    )
  )
  (setq Normal (mat:unit Normal))
  (setq maj (trans maj Normal 0 T))
  (entmakeX
    (list
      '(0 . "ELLIPSE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbEllipse")
      (cons 10 cen)
      (cons 11 maj)
      (cons 40 rat)
      (cons 41 an1)
      (cons 42 an2)
      (cons 210 Normal)
    )
  )
)

;;;=============================================================
;;; 功能: 画一个椭圆弧                                          
;;; 输入: 中心，长轴，短轴和旋转角度，起始角度，终点角度        
;;; 输出: 成功返回椭圆弧的图元名，否则返回nil                   
;;;=============================================================
(defun Ent:Make_EllipseArc (cen a b ang an1 an2 / maj rat)
  (if (> b a)
    (setq maj (polar '(0 0 0) (+ ang (* pi 0.5)) b)
          rat (/ a b 1.0)
    )
    (setq maj (polar '(0 0 0) ang a)
          rat (/ b a 1.0)
    )
  )
  (entmakeX
    (list
      '(0 . "ELLIPSE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbEllipse")
      (cons 10 cen)
      (cons 11 maj)
      (cons 40 rat)
      (cons 41 an1)
      (cons 42 an2)
    )
  )
)

;;;=============================================================
;;; 功能: 从某一点获取椭圆的参数(纯数学法)                      
;;; 输入: 椭圆上的一点和椭圆的中心，半长轴，半短轴和旋转角      
;;; 输出: 此点的参数值                                          
;;;=============================================================
(defun ELL:GetParam (pt Cen a b ang / x y p)            
  (setq p (mapcar '- pt cen))
  (setq x (car p))
  (setq y (cadr p))
  (atan
    (* a (- (* y (cos ang))(* x (sin ang))))
    (* b (+ (* x (cos ang))(* y (sin ang))))
  )
)

;;;=============================================================
;;; 功能: 更新平面椭圆                                          
;;; 输入: 要更新的实体名，更新后的中心，半长轴，半短轴和旋转角  
;;; 输出: 成功返回该实体名，否则返回nil                         
;;;=============================================================
(defun ELL:Update (ent cen a b ang / D C X R m n an)
  (if (> b a)
    (setq m b n a an (+ ang (/ pi 2)))
    (setq m a n b an ang)
  )
  (setq D (entget ent))
  (setq C (cons 10 cen))
  (setq X (cons 11 (polar '(0. 0. 0.) an m)))
  (setq R (cons 40 (/ n m 1.0)))
  (setq D (subst C (assoc 10 D) D))
  (setq D (subst X (assoc 11 D) D))
  (setq D (subst R (assoc 40 D) D))
  (entmod D)
  (entupd ent)
)

;;;=============================================================
;;; 六个参数的椭圆方程                                          
;;; 功能: 已知椭圆的二次方程的六系数，求此椭圆                  
;;; 输入: 二次方程的六个系数a,b,c,d,f,g                         
;;; 输出: 所求椭圆，一解或者无解。                              
;;; 参考：http://mathworld.wolfram.com/Ellipse.html             
;;;=============================================================
(defun ELL:FromEquation (a b c d f g / E I J K M N O R S X Y)
  (setq b (* 0.5 b))
  (setq d (* 0.5 d))
  (setq f (* 0.5 f))
  (setq S (mat:det3 a b d b c f d f g))
  (setq J (- (* a c) (* b b)))
  (setq I (float (+ a c)))
  (if (and (not (equal I 0 1e-14)) (> j 0) (MINUSP (/ S I)))    ;此处可能会产生浮点误差
    (progn
      (setq O (list
		(/ (- (* b f) (* c d)) j)
		(/ (- (* b d) (* a f)) j)
              )
	    k (- (+ (* a f f) (* c d d) (* g b b))
		 (* 2 b d f)
		 (* a c g)
	      )
	    k (+ k k)
	    e (sqrt (+ (* 4 b b) (* (- a c) (- a c)))))
      (setq m (sqrt (/ k (* J (- I e)))))
      (setq n (sqrt (/ k (* J (+ I e)))))
      (setq r (* 0.5 (atan (+ b b) (- a c))))
      (list O m n (+ r (* pi 0.5)))
    )
  )
)

;;;=============================================================
;;; 六个参数的圆锥曲线方程                                      
;;; 功能: 已知圆锥曲线的二次方程的六系数，求此圆锥曲线          
;;; 输入: 二次方程的六个系数a,b,c,d,f,g                         
;;; 输出: 所求圆锥曲线，一解或者无解。                          
;;; 参考：https://mathworld.wolfram.com/QuadraticCurve.html     
;;;=============================================================
(defun CUR:FromEquation (a b c d f g / E I J K M N O R S X Y)
  (setq b (* 0.5 b))
  (setq d (* 0.5 d))
  (setq f (* 0.5 f))
  (setq S (mat:det3 a b d b c f d f g))
  (setq J (- (* a c) (* b b)))
  (setq I (float (+ a c)))
  (setq K (+ (- (* a g) (* d d)) (- (* c g) (* f f))))
  
  (cond
    ( (equal S 0 1e-14)
      (cond
	( (and (equal J 0 1e-14) (equal K 0 1e-14))
          (princ "\nCoincident lines")
	)
	( (and (equal J 0 1e-14) (> K 0))
	  (princ "\nParallel lines (imaginary).")
	)
	( (and (equal J 0 1e-14) (< K 0))
	  (princ "\nParallel lines (real).")
	)
	( (> J 0)
	  (princ "\nIntersecting lines (imaginary).")
	)
	( (< J 0)
	  (princ "\nIntersecting lines (real).")
	)
      )
    )
    ;;Parabola
    ( (equal j 0 1e-14)
      (princ "\nParabola:")
      (setq O (list
		(/ (- (* b f) (* c d)) j)
		(/ (- (* b d) (* a f)) j)
	      )
      
      	    k (- (+ (* a f f) (* c d d) (* g b b))
		 (* 2 b d f)
		 (* a c g)
	      )
	    k (+ k k)
	    e (sqrt (+ (* 4 b b) (* (- a c) (- a c))))
	    m (sqrt (/ k (* J (- I e))))
	    n (sqrt (/ k (* J (+ I e))))
	    r (* 0.5 (atan (+ b b) (- a c)))
      )
      (list O m n (+ r (* pi 0.5)))
    )
    ;;Hyperbola
    ( (< j 0)
      (princ "\nHyperbola:")
      (setq O (list
		(/ (- (* b f) (* c d)) j)
		(/ (- (* b d) (* a f)) j)
	      )
      )
      (setq k (- (* a c g) (* a f f) (* c d d) (* g b b) (* -2 b d f)))
      (setq k (+ k k))
      (setq e (sqrt (+ (* 4 b b) (* (- a c) (- a c)))))
      (setq m (sqrt (abs (/ k (* J (- e I))))))
      (setq n (sqrt (abs (/ k (* J (+ e I))))))
      (setq r (* 0.5 (atan (+ b b) (- a c))))
      (list O n m r)
    )
    ;;Ellipse
    ( (> j 0)
      (setq O (list
		(/ (- (* b f) (* c d)) j)
		(/ (- (* b d) (* a f)) j)
              )
      )
      (setq k (- (+ (* a f f) (* c d d) (* g b b)) (* 2 b d f) (* a c g)))
      (setq k (+ k k))
      (setq e (sqrt (+ (* 4 b b) (* (- a c) (- a c)))))
      (if (MINUSP (/ S I))
	(setq m (sqrt (/ k (* J (- I e))))
	      n (sqrt (/ k (* J (+ I e))))
	      r (* 0.5 (atan (+ b b) (- a c)))
	      X (list O n m r)
        )
	(setq m (sqrt (/ (- k) (* J (- I e))))
	      n (sqrt (/ (- k) (* J (+ I e))))
	      r (* 0.5 (atan (+ b b) (- a c)))
	      X (list O m n r)
        )
      ) 
    )
  )
)

;;;=============================================================
;;; 获取椭圆的二次方程的六个系数                                
;;; 功能: 已知一椭圆，求椭圆的二次方程的六系数                  
;;; 输入: 椭圆的长轴，短轴，中心，旋转角                        
;;; 输出: 二次方程的六个系数                                    
;;;=============================================================
(defun ELL:GetFactors (x0 y0 m n an / A0 A1 B0 B1 C1 D1 E1 F0 F1 K1 K2)
  (setq an (- an))
  (setq A0 (/ n m))
  (setq B0 (/ m n))
  (setq F0 (- (* m n)))
  (setq k1 (cos an))
  (setq k2 (* (- A0 B0) k1 k1))
  (setq A1 (+ B0 k2))
  (setq C1 (- A0 k2))
  (setq B1 (* (sin (+ an an)) (- B0 A0)))
  (setq D1 (- (* -2 A1 X0) (* B1 Y0)))
  (setq E1 (- (* -2 C1 Y0) (* B1 X0)))
  (setq F1 (+ (* A1 X0 X0) (* B1 X0 Y0) (* C1 Y0 Y0) F0))
  (list A1 B1 C1 D1 E1 F1)
)

;;;=============================================================
;;; 功能: 校验某点是否在用二次方程的表达的椭圆上。              
;;; 输入: 某坐标(x, y)和椭圆的二次方程的六个系数。              
;;; 输出: 在此椭圆上则返回T，否则返回nil                        
;;;=============================================================
(defun ELL:VeryFunctions (x y A B C D E F)
  (equal
    (+ (* A x x) (* B x y) (* C y y) (* D x) (* E y) F)
    0
    1e-4
  )
)

;;;=============================================================
;;; 功能: 获取椭圆的属性                                        
;;; 输入: 椭圆图元                                              
;;; 输出: 椭圆中心的X坐标，Y坐标，半长轴长，半短轴长，旋转角度。
;;;=============================================================
(defun ELL:GetParameters (e / d c m r x y a m n)
  (setq d (entget e))
  (setq c (cdr (assoc 10 d)))
  (setq m (cdr (assoc 11 d)))
  (setq r (cdr (assoc 40 d)))
  (setq x (car c))
  (setq y (cadr c))
  (setq a (angle '(0 0 0) m))
  (setq m (distance '(0 0 0) m))
  (setq n (* r m))
  (list (list x y) m n a)
)

;;;*************************************************************
;;; 二、椭圆的曲率、弧长、面积和离心率                          
;;;*************************************************************

;;;=============================================================
;;;功能: 获取椭圆上一点处的曲率和转弯半径                       
;;;参数: 椭圆实体和椭圆上的一点                                 
;;;返回: 此处离心圆圆心、转弯半径及其曲率（离心率）             
;;;说明: 如果要在CAD中几何作图，可以参考此贴：                  
;;;      http://bbs.mjtd.com/thread-62980-1-1.html              
;;;=============================================================
(defun ELL:GetCurvature (en pt / obj a b px x y v1 v2 rad cen)
  (setq obj (vlax-ename->vla-object en))
  (setq a   (vla-get-MajorRadius obj))                          ;椭圆的半长轴
  (setq b   (vla-get-MinorRadius obj))				;椭圆的半短轴
  (setq pt  (vlax-curve-getclosestpointto en pt))		;保证此点在椭圆上
  (setq px  (vlax-curve-getParamAtPoint en pt))			;此点的椭圆参数
  (setq v1  (vlax-curve-getFirstDeriv en px))			;此点的一阶矢量
  (setq v2  (list (- (cadr v1)) (car v1) (caddr v1)))		;此点的切线矢量
  (setq x   (* a (sin px)))
  (setq y   (* b (cos px)))
  (setq rad (/ (expt (+ (* x x) (* y y)) 1.5) (* a b)))		;得到转弯半径
  (setq cen (polar pt (angle '(0 0 0) v2) rad))			;圆心
  (list cen rad)						;圆心及半径
)

;;;=============================================================
;;; 椭圆的离心率由椭圆的长短比率得出。算式：(sqrt(1-ratio^2))   
;;; 离心率越大圆就越扁，越小则越接近于圆                        
;;;-------------------------------------------------------------
;;; 功能: 获取椭圆的离心率，值越大形状越扁，越小越接近圆        
;;; 输入: 椭圆的图元名                                          
;;; 输出: 椭圆的离心率                                          
;;;=============================================================
(defun ELL:Eccentricity (e / ratio)
  (setq ratio (cdr (assoc 40 (entget e))))
  (sqrt (- 1 (* ratio ratio)))
)

;;;=============================================================
;;; 说明: 此函数为纯数学计算，可不用图元作参，采用龙贝塔积分法。
;;;       适用于某些特殊情况。一般可用vlax-curve函数求。见样例。
;;; 功能: 获取椭圆弧的长度。                                    
;;; 输入: 椭圆的长半轴，短半轴，参数1，参数2和精度              
;;; 输出: 椭圆弧的长度                                          
;;;=============================================================
(defun ELL:Length (la lb p1 p2 eps / Func ratio 0.5Pi)
  (defun Func (x / cx ee)
    (setq cx (cos x))
    (setq ee (* (1+ ratio) (1- ratio))) 
    (sqrt (1+ (* ee cx cx)))
  )
  
  (if (<= lb la)
    (progn 
      (setq ratio (/ lb (float la)))
      (* la (Math:Romberg p1 p2 eps))
    )
    (progn 
      (setq ratio (/ la (float lb)))
      (setq 0.5Pi (* pi 0.5))
      (* lb (Math:Romberg (- p1 0.5pi) (- p2 0.5pi) eps))
    )
  )
)

;;;=============================================================
;;; 说明: 此函数为纯数学计算，意味可不用图元参与，速度居然不慢。
;;;       适用于某些特殊情况。一般可用vlax-curve-getarea函数求，
;;;       也可以用vla-get-area获得。测试见样例。                
;;; 功能: 获取椭圆弧的面积。                                    
;;; 输入: 椭圆的长半轴，短半轴，参数1，参数2                    
;;; 输出: 椭圆弧的面积                                          
;;;=============================================================
(defun ELL:Area (la lb A1 A2 / a k)
  (if (> A1 A2)
    (setq A (- (+ pi pi A2) A1))
    (setq A (- A2 A1))
  )
  (setq k (sin (* 0.5 A)))
  (setq k (* 1.333333333333333333333 la k k k))
  (* 0.5 la lb (- A (sin A)))
)

;;;*************************************************************
;;; 三、椭圆的变换矩阵及其法线和平面投影                        
;;;*************************************************************

;;;=============================================================
;;; 功能: 椭圆的简易变换                                        
;;; 输入: 要变换的点，角度余弦，正弦，水平平移和垂直平移        
;;; 输出: 变换后的点坐标。                                      
;;;=============================================================
(defun ELL:SimpleTrans (p cx sx vx vy)
  (list 
    (+ vx (- (* (car p) cx) (* (cadr p) sx)))
    (+ vy (+ (* (car p) sx) (* (cadr p) cx)))
  )
)

;;;=============================================================
;;; 功能: 获取椭圆的变换矩阵                                    
;;; 输入: 椭圆的图元名                                          
;;; 输出: 椭圆的变换矩阵及逆矩阵                                
;;; 说明: 由椭圆的长轴矢量，和短轴矢量以及法线矢量可以构成椭圆自
;;;       身的变换矩阵。如果椭圆位于图块内或者嵌套块内，关于其变
;;;       换矩阵可以参考这个帖子的讨论：                        
;;;       http://bbs.mjtd.com/thread-93828-1-1.html.            
;;;       此帖中对椭圆的变换讨论的比较深入。                    
;;;=============================================================
(defun ELL:TransMatrix (e / Obj Cen DX DY DZ mat)
  (setq obj (vlax-ename->vla-object e))
  (setq Cen (vlax-get obj 'Center))                             ; 中心点
  (setq DX  (Mat:unit (vlax-get obj 'MajorAxis)))               ; OCS的X轴方向 长轴方向
  (setq DY  (Mat:unit (vlax-get obj 'MinorAxis)))               ; OCS的Y轴方向 短轴方向
  (setq DZ  (Mat:unit (vlax-get obj 'Normal)))                  ; OCS的Z轴方向 法线方向 
  (setq mat (list DX DY DZ))                                    ; 椭圆的旋转矩阵
  (list
    (Mat:DispToMatrix (Mat:trp mat) cen)                        ; 椭圆的变换矩阵 = (trans Pt En 0)
    (Mat:DispToMatrix mat (mapcar '- (Mat:mxv mat cen)))        ; 椭圆的逆变换矩阵 = (trans Pt 0 En)
  )
)

;;;=============================================================
;;; 功能: 获取椭圆的旋转矩阵                                    
;;; 输入: 椭圆的长轴矢量和椭圆的法线                            
;;; 输出: 椭圆的旋转变换矩阵及逆矩阵                            
;;;=============================================================
(defun ELL:RotationMatrix (Maj Nrm / DX DY DZ mat)      
  (setq DX (Mat:unit Maj))                                      ; OCS的X轴方向
  (setq DZ (Mat:unit Nrm))                                      ; OCS的Z轴方向
  (setq DY (Mat:Unit (Mat:vxv DZ DX)))                          ; OCS的Y轴方向
  (setq mat (list DX DY DZ))                                    ; 椭圆的旋转矩阵的逆矩阵
  (list (Mat:trp mat) mat)                                      ; 椭圆的旋转矩阵和逆矩阵
)

;;;=============================================================
;;; 功能: 修改一个椭圆的法线方向(不能直接用ActiveX方法)         
;;; 输入: 椭圆的图元名，目标法线方向                            
;;; 输出: 成功返回椭圆的DXF表，否则返回nil                      
;;;=============================================================
(defun ELL:Put_Normal (ent Nrm / DXF Maj)
  (setq Nrm (mat:unit Nrm))
  (setq dxf (entget ent))
  (setq maj (cdr (assoc 11 dxf)))                               ; 首先取得主轴方向
  (setq maj (trans maj 0 (cdr (assoc 210 dxf)) T))              ; 主轴方向在OCS中的方向
  (setq maj (trans Maj Nrm 0 T))                                ; 把OCS中的方向转化为目标方向
  (setq dxf (subst (cons 11 maj) (assoc 11 dxf) dxf))           ; 椭圆的主轴方向
  (setq dxf (subst (cons 210 Nrm) (assoc 210 dxf) dxf))         ; 椭圆的法线方向
  (entmod dxf)                                                  ; 更新图元
)

;;;=============================================================
;;; highflybird  2012.11.1 第一版  2013.5.25 修改于深圳         
;;; 功能: 给定一个椭圆或者圆(弧)，求在平面上的投影（一般为椭圆）
;;; 输入: 椭圆或者圆(弧)的图元名E,平面的法线Normal和其一点Origin
;;; 输出: 投影椭圆(创建3d椭圆的几个参数)                        
;;; 参考: http://bbs.mjtd.com/forum.php?mod=viewthread&tid=84527
;;;       http://www.theswamp.org/index.php?topic=43031.0       
;;;=============================================================
(defun ELL:Projection (E Normal Origin /
                       A1  AN1 AN2 ANG B1  C0  C1  C2  ENT OBJ PA0
                       PA1 PB0 PB1 PE0 PE1 PR1 PR2 PS0 PS1 RET)
  (setq C0  (cdr (assoc 10 (entget E))))                        ; 中心点(用vla-get-center可能出错？)
  (setq C0  (trans C0 e 0))                                     ; 此处很重要!!!
  (setq PS0 (vlax-curve-getStartPoint E))                       ; 这个可以通过角度计算出来StartAngle
  (setq PE0 (vlax-curve-getEndPoint E))                         ; 这个可以通过角度计算出来EndAngle
  (setq Pr1 (vlax-curve-getStartParam E))
  (setq Pr2 (vlax-curve-getEndParam E))
  (if (LINE:Colinearity C0 PS0 PE0)                             ; 防止中心，起点，终点共线情况出现
    (setq PA0 (vlax-curve-getPointAtParam E (1+ pr1))           ; 起点参数加一
          pB0 (vlax-curve-getpointatparam E (1- pr2))           ; 终点参数加一
    )
    (setq pA0 (vlax-curve-getpointatparam E (* 0.5 (+ pr1 pr2))); 取中点为第二点
          pB0 PE0                                               ; 终点为第三点
    )
  )
   
  (mapcar
    (function (lambda (x y) (set x (trans y 0 Normal))))        ; 把这些点投影到平面上
    (quote (PA1 PB1 PS1 PE1 C1))
    (list PA0 PB0 PS0 PE0 C0)                                   ; 椭圆上取的三点、起始点和端点
  )
  (if (setq RET (ELL:C3P C1 PS1 PA1 PB1))                       ; 中心和三点画椭圆法
    (progn 
      (setq A1  (cadr ret))                                     ; 投影椭圆的长轴
      (setq B1  (caddr ret))                                    ; 投影椭圆的短轴
      (setq ang (cadddr ret))                                   ; 投影椭圆的旋转角
      (setq C2  (PLANE:Perpendicular_Foot c0 Origin Normal))    ; 椭圆的实际中心
      (if (vlax-curve-isClosed E)
        (list C2 A1 B1 ang 0 (+ pi pi) Normal)                  ; 返回创建3d椭圆的几个参数
        (progn 
          (setq an1 (ELL:GetParam PS1 C1 A1 B1 ang))            ; 投影椭圆的起始参数
          (setq an2 (ELL:GetParam PE1 C1 A1 B1 ang))            ; 投影椭圆的终点参数
          (if (> (TRI:Det3P pS1 pA1 pB1) 0)                     ; 此处的确要作判断
            (list C2 a1 b1 ang an1 an2 Normal)                  ; 此时是逆时针
            (list C2 a1 b1 ang an2 an1 Normal)                  ; 顺时针要交换起始角和终止角
          )
        )
      )
    )
  )
)

;;;*************************************************************
;;; 四、椭圆的展点和椭圆锥的展开曲线                            
;;;*************************************************************

;;;=============================================================
;;; 功能: 椭圆展点。                                            
;;; 输入: 无                                                    
;;; 输出: 无                                                    
;;;=============================================================
(defun C:ExpandEllipse(/ A B C D E F I L N P X Y)
  (initget 15)
  (setq a (getdist "\n请输入椭圆的长轴:"))      
  (initget 15)
  (setq b (getdist "\n请输入椭圆的短轴:"))
  (initget 7)
  (setq E (ENT:MAKE_ELLIPSE '(0 0 0) a b 0))			;构造一个椭圆
  ;(setq E (car (entsel "\n请选择一个椭圆:")))			;或者选择一个椭圆
  (setq n (getint "\n请输入等分数目:"))
  (setq C (vlax-curve-getendparam e))
  (setq C (vlax-curve-getdistatparam e C))			;椭圆的周长
  (setq D (/ C N))						;每等分段长度
  (setq i 0)
  (repeat n
    (setq p (vlax-curve-getPointAtDist e (* i D)))		;等分点
    (Ent:Make_Point p)
    (setq L (cons p L))
    (setq i (1+ i))
  )
  (setq L (reverse L))
  (entdel e)							;删除此椭圆
  (if (setq f (getfiled "输入坐标文件名" "C:/TEMP" "TXT" 1))	;输出坐标到文件
    (progn
      (setq f (open f "W"))
      (foreach p L
	(setq x (rtos (car p) 2 8))
	(setq y (rtos (cadr p) 2 8))
        (write-line (strcat X "\t" Y) f)
      )
      (close f)
    )
  ) 
)

;;;=============================================================
;;; 功能: 展开椭圆锥体。                                        
;;; 输入: 椭圆锥底椭圆的长轴a，短轴b，椭圆锥的高h和分弧精度n    
;;; 输出: 展开图的曲线的坐标点(可据此形成展开面)                
;;;=============================================================
(defun ELL:ExpandCone (a b h n / Pts Ang D Param DivAng P0 P1 R0 R1 X Y)
  (setq DivAng (/ pi n 0.5))                                    ;等分角度
  (setq Param 0)                                                ;开始参数为0
  (setq p0 (list a 0))                                          ;从椭圆最右边的点开始
  (setq r0 (distance '(0 0 0) (list a 0 h)))                    ;开始的锥顶最右边点的距离
  (setq Pts (list (list r0 0) '(0 0)))                          ;把最开始的两点加入到数据表
  (setq Ang 0)                                                  ;开始角度也为0
  (repeat n
    (setq Param  (+ Param DivAng))
    (setq x   (* a (cos Param)))                                ;椭圆上的x坐标
    (setq y   (* b (sin Param)))                                ;椭圆上的y坐标
    (setq p1  (list x y))                                       ;点的坐标
    (setq r1  (distance '(0 0 0) (list x y h)))                 ;展开面的锥线长度
    (setq d   (distance p0 p1))                                 ;椭圆上的上一点到这点的距离
    (setq Ang (+ Ang (car (TRI:CosinesLaw d r0 r1))))           ;坐标角度
    (setq Pts (cons (polar '(0 0) Ang r1) Pts))                 ;得到了坐标，并把它加入到表中
    (setq p0 p1)                                                ;用新的坐标位置替换旧位置
    (setq r0 r1)                                                ;用新的锥线长度替换旧长度
  )
  (reverse Pts)                                                 ;逆转表，得到正序的数据
)

;;;*************************************************************
;;; 五、椭圆的相切和相交                                        
;;;*************************************************************

;;;=============================================================
;;; 功能: 数学法求椭圆上一点的切线矢量(vlax-curve-GetFirstDeriv)
;;; 输入: 这点的参数Param,椭圆中心C,长轴方向M，比率R和其法线N   
;;; 输出: 这点的切线矢量                                        
;;;=============================================================
(defun ELL:PointTangentOn (Param C M R N / a b v)
  (setq a (distance '(0 0 0) M))
  (setq b (* R a))
  (setq v (list (* (- a) (sin Param)) (* b (cos Param)) 0))
  (Mat:mxv (car (ELL:RotationMatrix M N)) v)
)

;;;=============================================================
;;; 功能: 求椭圆(平面)外一点到椭圆的切线                        
;;; 输入: 椭圆外一点P,中心C,半长轴a,半短轴b,和椭圆的旋转角ang   
;;; 输出: 此点到与椭圆相切的切点(一个，两个或者nil)             
;;; 说明: 程序流程，首先把P 转为椭圆的坐标，然后非均匀缩放椭圆为
;;;       圆，同时也缩放此点，求出点到圆的切线后再把坐标转回去。
;;;=============================================================
(defun ELL:PointTangentTo (P C m n ang / A B D L P1 X1 Y1 x y)
  (setq p1 (mat:w2o p c ang))					;世界坐标相对于椭圆的坐标
  (setq x1 (car p1))
  (setq y1 (* (/ m n) (cadr p1)))                               ;把Y坐标值缩放
  (setq d (sqrt (+ (* x1 x1) (* y1 y1))))
  (if (equal D m 1e-8)                                          ;如果在椭圆上
    (list P)                                                    ;则为此点
    (if	(> D m)
      (progn
	(setq l (sqrt (* (+ d m) (- d m))))                     ;求出到切点距离
	(setq b (angle '(0 0) (list x1 y1)))                    ;点到中心的角度
	(setq a (atan l m))                                     ;与切点夹角
	(setq x (+ b a))                                        ;正向角度的切点1
	(setq y (- b a))					;负向角度的切点2
	(list
	  (mat:o2w (list (* m (cos x)) (* n (sin x))) C ang)    ;转换切点1 坐标
	  (mat:o2w (list (* m (cos y)) (* n (sin y))) C ang)	;转换切点2 坐标
	)
      )
    )
  )
)

;;;=============================================================
;;; 上面程序的另一种方法，速度慢，代码不精练                    
;;;=============================================================
(defun ELL:PointTangentTo1 (P C a b ang / AA BB K1 K2 KK X Y Z XX YY)
  (defun GetTan (A B C bb K1 X Y Z KK / d1 k4 k5 s1 x1 y1 p1)
    (setq d1 (+ (* bb x) kk))
    (setq k4 (/ (* d1 x) k1))
    (setq k5 (/ (* d1 a) k1))
    (setq s1 (atan (/ (* b (- 1 k4)) y) k5))
    (setq x1 (* a (cos s1)))
    (setq y1 (* b (sin s1)))
    (setq P1 (trans (list y1 0 x1) z 0 T))
    (setq p1 (mapcar '+ P1 C)) 
  )
  (setq P (mapcar '- P C))
  (setq z (polar '(0 0 0) ang a)) 
  (setq P (trans P 0 z T))
  (setq x (caddr p))
  (setq y (car p))
  (if (equal y 0 1e-8)
    (if (> (abs x) a)
      (progn 
        (setq kk (/ a x))
        (setq xx (* a kk))
        (setq yy (* b (sqrt (- 1 (* kk kk)))))
        (list
          (mapcar '+ C (trans (list yy 0 xx) z 0 T))
          (mapcar '+ C (trans (list (- yy) 0 xx) z 0 T))
        )
      )
    )
    (progn 
      (setq aa (* a a))
      (setq bb (* b b))
      (setq xx (* x x))
      (setq yy (* y y))
      (setq k1 (+ (* aa yy) (* bb xx)))
      (setq k2 (- k1 (* aa bb)))
      (if (> k2 0)
        (progn
          (setq kk (* (sqrt k2) (abs y)))
          (list
	    (GetTan A B C bb K1 X Y Z kk)
            (GetTan A B C bb K1 X Y Z (- kk))
          )
        )
      )
    )
  )
)

;;;=============================================================
;;; 关于椭圆的公切线问题请参考：                                
;;; http://bbs.mjtd.com/forum.php?mod=viewthread&tid=82900      
;;; 另外可参考我的附件: ellipse-tan-solve1.LSP                  
;;;=============================================================

;;;=============================================================
;;; 功能: 已知一圆一椭圆，求其公切线（精确解，非迭代解）子程序。
;;; 输入: 椭圆中心在原点，主轴在X轴，圆中心为(x0,y0)，半径为R。 
;;; 输出: 如有公切线，则输出公切线集（用点对表示），否则返回nil 
;;;=============================================================
(defun ELL:TangentToCircle (m n C0 R / ret A B C D E J L MM MN NN Q U1 U2 V W1 W2 X Y)
  (setq mm (* m m))
  (setq nn (* n n))
  (setq mn (* m n))
  (setq u1 (/ (car c0) mm))
  (setq w1 (/ (cadr c0) nn))
  (setq u2 (/ r mm))
  (setq w2 (/ r nn))
  (setq A  (* (+ u1 u2) (- u1 u2)))
  (setq C  (* (+ w1 w2) (- w1 w2)))
  (setq B  (* 2 u1 w1))
  (setq D  (- (+ U1 u1)))
  (setq E  (- (+ w1 w1)))
  (setq A  (* A mn))
  (setq B  (* B mn))
  (setq C  (* C mn))
  (setq D  (* D mn))
  (setq E  (* E mn))
  (foreach p (ELL:GetInters1 m n A B C D E mn)
    (setq x (car p))
    (setq y (cadr p))
    (setq v (list (- (/ (* m y) n)) (/ (* n x) m)))
    (setq d (distance p C0))
    (setq l (sqrt (* (+ d r) (- d r))))
    (setq j (angle '(0 0) v))
    (setq q (polar p j l))
    (if (> (distance q c0) d)
      (setq q (polar p j (- l)))
    )
    (setq ret (cons (list p q) ret))
  )
  (reverse ret)
)

;;;=============================================================
;;; 功能: 已知两个椭圆，求其公切线（精确解，非迭代解）。        
;;; 输入: 两个椭圆，每个椭圆的列表的元素中心的X坐标，中心的Y坐标
;;;       半长轴长，半短轴长，椭圆的旋转角度。                  
;;; 输出: 如有公切线，则输出公切线集（用点对表示），否则返回nil 
;;;=============================================================
(defun ELL:GetTangents (S1 S2 / rt A1 A2 A3 A4 C1 C2 C3 C4 C5 M1 M2 M4 N1 N2 N4 S S4 U3 U4 W3 W4)
  ;;第一个椭圆参数
  (setq C1 (car s1))
  (setq m1 (cadr s1))
  (setq n1 (caddr s1))
  (setq a1 (last s1))
  ;;第二个椭圆参数
  (setq C2 (car s2))
  (setq m2 (cadr s2))
  (setq n2 (caddr s2))
  (setq a2 (last s2))
  ;;相对于第一个椭圆，第二个椭圆的坐标
  (setq c3 (Mat:w2o c2 c1 a1))
  (setq a3 (- a2 a1))
  (setq u3 (polar c3 a3 m2))
  (setq w3 (polar c3 (+ a3 (* 0.5 pi)) n2))
  ;;以第一个椭圆长短轴非等比缩放到单位圆变换第二个椭圆
  (setq c4 (list (/ (car c3) m1) (/ (cadr c3) n1)))
  (setq u4 (list (/ (car u3) m1) (/ (cadr u3) n1)))
  (setq w4 (list (/ (car w3) m1) (/ (cadr w3) n1)))
  (setq S4 (ELL:2J C4 u4 w4))
  (setq m4 (cadr s4))
  (setq n4 (caddr s4))
  (setq a4 (cadddr s4))
  ;;单位圆圆心相对于变换椭圆的坐标
  (setq C5 (Mat:w2o '(0 0) c4 a4))
  ;;根据每个子程序求出切点并变换
  (foreach pair (ELL:TangentToCircle m4 n4 C5 1)
    (setq s nil)
    (foreach p pair
      (setq p (Mat:o2w p c4 a4))
      (setq p (list (* m1 (car p)) (* n1 (cadr p))))
      (setq p (Mat:o2w p c1 a1))
      (setq s (cons p s))
    )
    (setq rt (cons s rt))
  )	
)

;;;=============================================================
;;; 功能: 已知两个椭圆，求其公切线精确解。此算法较前算法慢。    
;;; 输入: 两个椭圆，每个椭圆的列表的元素中心的X坐标，中心的Y坐标
;;;       半长轴长，半短轴长，椭圆的旋转角度。                  
;;; 输出: 如有公切线，则输出公切线集（用点对表示），否则返回nil 
;;;=============================================================
(defun ELL:GetTangents2 (S1 S2 / rt A1 A2 A3 A4 C1 C2 C3 C4 CX L0
			M1 M2 M3 MR MX N1 N2 N3 S Q S3 SX U1 U2
			U3 W1 W2 W3 X3 X4 Y3 Y4)
  ;;第一个椭圆参数
  (setq C1 (car s1))
  (setq m1 (cadr s1))
  (setq n1 (caddr s1))
  (setq a1 (last s1))
  (setq u1 (polar c1 a1 m1))
  (setq w1 (polar c1 (+ a1 (* 0.5 pi)) n1))
  ;;第二个椭圆参数
  (setq C2 (car s2))
  (setq m2 (cadr s2))
  (setq n2 (caddr s2))
  (setq a2 (last s2))
  (setq u2 (polar c2 a2 m2))
  (setq w2 (polar c2 (+ a2 (* 0.5 pi)) n2))
  ;;获得第二个椭圆到单位圆的仿射变换
  (setq mx (Mat:AffineTrans c2 '(0 0) u2 '(1 0) w2 '(0 1)))
  (setq mr (cadr mx))
  (setq mx (car mx))
  ;;变换椭圆一
  (setq c3 (mat:mxv mx (append c1 '(1))))
  (setq u3 (mat:mxv mx (append u1 '(1))))
  (setq w3 (mat:mxv mx (append w1 '(1))))
  ;;获得变换椭圆的参数
  (setq S3 (ELL:2J C3 u3 w3))
  (setq x3 (car c3))
  (setq y3 (cadr c3))
  (setq m3 (cadr s3))
  (setq n3 (caddr s3))
  (setq a3 (last s3))
  (setq cx (cos a3))
  (setq sx (sin a3))
  ;;获取原点相对于变换椭圆的坐标
  (setq C4 (Mat:w2o '(0 0) C3 A3))
  ;;根据每个子程序求出切点并变换
  (foreach pair (ELL:TangentToCircle m3 n3 C4 1)
    (setq S nil)
    (foreach p pair
      (setq p (ELL:SimpleTrans p cx sx x3 y3))
      (setq p (mat:mxv mr (append p '(1))))
      (setq S (cons p S))
    )
    (setq rt (cons S rt))
  )	
)

;;;=============================================================
;;; 功能: 已知两椭圆，求其交点（精确解，非迭代解）。            
;;; 输入: 两个椭圆，每个椭圆的列表的元素中心的X坐标，中心的Y坐标
;;;       半长轴长，半短轴长，椭圆的旋转角度。                  
;;; 输出: 如果有交点，则输出交点集，否则返回nil                 
;;;=============================================================
(defun ELL:IntersWithEllipse (S1 S2 / A1 A2 C1 C2 C3 CX SX M1 M2 N1 N2 S ff x1 y1)
  ;;椭圆一参数
  (setq C1 (car s1))
  (setq m1 (cadr s1))
  (setq n1 (caddr s1))
  (setq a1 (last s1))
  (setq x1 (car c1))
  (setq y1 (cadr c1))
  ;;椭圆二参数
  (setq C2 (car s2))
  (setq m2 (cadr s2))
  (setq n2 (caddr s2))
  (setq a2 (last s2))
  ;;椭圆二中心相对于椭圆一的坐标
  (setq c3 (Mat:w2o C2 C1 a1))
  ;;获取椭圆二的二次方程系数
  (setq ff (ELL:GetFactors (car c3) (cadr c3) m2 n2 (- a2 a1)))
  (setq ff (cons m1 (cons n1 ff)))
  (setq cx (cos a1))
  (setq sx (sin a1))

  (foreach p (apply 'ELL:GetInters1 ff)
    (setq p (ELL:SimpleTrans p cx sx x1 y1))
    (setq S (cons p S))
  )
)

;;;=============================================================
;;; 功能: 已知两椭圆，求其交点（精确解，非迭代解）的子程序      
;;; 输入: 两个椭圆，其中一个椭圆中心在原点，主轴在X 轴。两半轴长
;;;       分别为m,n，另一椭圆为的二次方程的六个系数A,B,C,D,E,F。
;;; 输出: 如果有交点，则输出交点集，否则返回nil                 
;;;=============================================================
(defun ELL:GetInters1 (m n A B C D E F / mm nn G A1  A2 B2 C2 D2 E2 F1 x y ret h)
  (setq nn (* n n))
  (setq mm (* m m))
  (setq G  (- (/ nn mm)))
  (setq A1 (+ A (* C G)))
  (setq F1 (+ F (* C nn)))
  (setq A2 (- (* A1 A1) (* G B B)))
  (setq B2 (- (* A1 D) (* G B E)))
  (setq B2 (+ B2 B2))
  (setq C2 (- (* D D) (* -2 A1 F1) (* E E G) (* nn B B)))
  (setq D2 (- (* F1 D) (* B E nn)))
  (setq D2 (+ D2 D2))
  (setq E2 (- (* F1 F1)(* E E nn)))
  (foreach p (Math:Quartic_Equation A2 B2 C2 D2 E2)
    (setq x (car p))
    (setq h (+ (* n n) (* G x x)))
    (if (and (equal (cadr p) 0 1e-4) (> h 0))
      (foreach y (list (setq h (sqrt h)) (- h))
	(if (ELL:VeryFunctions x y A B C D E F)
	  (setq ret (cons (list x y) ret))
	)
      )
    )
  )
  (reverse ret)
)

;;;=============================================================
;;; 功能: 已知两个椭圆，求两个椭圆的交点的另一种解法(已废)      
;;; 输入: 两个椭圆，其中一个椭圆中心在原点，主轴在X轴。         
;;; 输出: 如果有交点，则输出交点集，否则返回nil                 
;;;=============================================================
(defun ELL:GetInters2 (m1 n1 x0 y0 m2 n2 an / A1 A2 B1 B2 B3 C0 C1 MM NN)
  (setq mm (* m1 m1))
  (setq nn (* n1 n1))
  (setq A1 (/ (* 2 m2 x0) mm))
  (setq A2 (/ (* 2 n2 y0) nn))
  (setq B1 (/ (* m2 m2) mm))
  (setq B2 (/ (* n2 n2) nn))  
  (setq C0 (+ (/ (* x0 x0) mm) (/ (* y0 y0) nn) -1))
  (setq B3 (- B1 B2))
  (setq C1 (+ B2 C0))
  (Math:Quartic_Equation
    (* B3 B3)
    (* 2 A1 B3)
    (+ (* 2 B3 C1) (* A1 A1) (* A2 A2))
    (* 2 A1 C1)
    (* (- C1 A2) (+ C1 A2))
  ) 
)

;;;=============================================================
;;; 功能: 求椭圆与直线的交点(计算几何方式1)                     
;;; 输入: 椭圆的长轴a,短轴b，两点P1,P2.                         
;;; 输出: 直线P1P2与椭圆的交点。                                
;;; 参考: http://bbs.mjtd.com/thread-62003-2-1.html             
;;; RootOf((M^2*a^2+N^2*b^2)*_Z^2-M^2*a^2+L^2+2*L*N*b*_Z)*b     
;;;=============================================================
(Defun ELL:IntersWithLine (a b P1 P2 / D E K L M N P PA PB X1 X2 Y1 Y2)
  (setq k  (/ b (float a)))
  (setq X1 (car P1))
  (setq X2 (car P2))
  (setq Y1 (/ (cadr P1) k))
  (setq Y2 (/ (cadr P2) k)) 
  (Setq M  (- Y1 Y2))                                           ;直线方程系数M
  (Setq N  (- X2 X1))                                           ;直线方程系数N
  (setq D  (sqrt (+ (* M M) (* N N)))) 
  (setq D  (/ (- (* Y2 X1) (* Y1 X2)) D))      			;垂距
  (setq e  (angle (list x1 y1) (list x2 y2)))                   ;直线的与X轴线的交角
  (setq p  (polar '(0 0) (- e (* pi 0.5)) d))                   ;圆心到直线的垂足
  (setq d  (abs d))
  (if (equal d a 1e-8)                                          ;如果垂距等于半径
    (list (list (car p) (* k (cadr p))))                        ;相切
    (if (< d a)                                                 ;如果垂距小于半径
      (progn 
        (setq L  (sqrt (* (+ a d)(- a d))))                     ;半弦长
        (setq Pa (polar p e (- L)))
        (setq Pb (polar p e L))
        (setq pa (list (car Pa) (* k (cadr Pa))))
        (setq pb (list (car Pb) (* k (cadr Pb))))
        (list pa pb)                                            ;有两个交点
      )
    )
  )
)

;;;=============================================================
;;; 功能: 求椭圆与直线的交点(计算几何方式2,似乎比方式1慢)       
;;; 输入: 椭圆的长轴a,短轴b，两点P1,P2.                         
;;; 输出: 直线P1P2与椭圆的交点。                                
;;;=============================================================
(Defun ELL:IntersWithLine_1 (a b P1 P2 / K1 K2 K3 M N L MA NB PS X X1 X2 Y Y1 Y2)
  (Setq X1 (Car  P1))
  (Setq Y1 (Cadr P1))
  (Setq X2 (Car  P2))
  (Setq Y2 (Cadr P2))
  (Setq M  (float (- Y1 Y2)))                                   ;直线方程系数M
  (Setq N  (float (- X2 X1)))                                   ;直线方程系数N
  (Setq L  (float (- (* Y2 X1) (* Y1 X2))))                     ;直线方程系数L
  (setq Nb (* N b))
  (if (equal M 0 1e-8)                                          ;水平直线
    (cond
      ( (equal (setq K1 (* (+ Nb L) (- Nb L))) 0 1e-8)
        (list (list 0 (- (/ L N))))
      )
      ( (> K1 0)
        (setq x (/ (* a (sqrt k1)) Nb))
        (setq y (- (/ L N)))
        (list (list x y) (list (- x) y))
      )
    )
    (progn
      (setq Ma (* M a))
      (setq k1 (+ (* Ma Ma) (* Nb Nb)))
      (setq k2 (* 2 L Nb))
      (setq k3 (- (* L L) (* Ma Ma)))
      (foreach e (Math:Quadratic_Equation_1 k1 k2 k3)
        (setq x (/ (+ L (* N e b)) (- M)))
        (setq y (* e b))
        (setq ps (cons (list x y) ps))
      )
    )
  )
)

;;;=============================================================
;;; 功能: 求平面椭圆与直线的交点                                
;;; 输入: 椭圆的中心Center,长轴方向major,椭率ratio，两点P1,P2.  
;;; 输出: 直线P1P2与椭圆的交点。                                
;;;=============================================================
(Defun ELL:IntersWithLine2D (CEN MAJ RAT P1 P2 / A B D an K L M P Q)
  (if (not (equal p1 p2 1e-8))					;直线不应该两端点重合
    (progn
      (setq an (angle '(0 0) MAJ))				;椭圆的X 倾角
      (setq m  (distance '(0 0) MAJ))				;椭圆的半长轴
      (setq p1 (mat:w2o p1 CEN an))				;直线端点相对于椭圆的坐标
      (setq p2 (mat:w2o p2 CEN an))				;直线端点相对于椭圆的坐标
      (setq p1 (list (car p1) (/ (cadr p1) RAT)))               ;不等比压缩成圆后的直线端点坐标
      (setq p2 (list (car p2) (/ (cadr p2) RAT)))		;不等比压缩成圆后的直线端点坐标
      (setq D  (/ (MAT:Det2V p1 p2) (distance p1 p2)))		;中心到直线的有向距离
      (setq K  (polar '(0 0) (- (angle p1 p2) (* pi 0.5)) D))	;中心到直线的垂足
      (setq D  (abs D))						;垂距
      (if (equal D m 1e-8)					
	(list (mat:o2w (list (car k) (* RAT (cadr k))) CEN an)) ;此时直线与椭圆相切
	(if (< D m)
	  (progn
	    (setq a (angle '(0 0) K))				;变换后垂足的X 倾角
	    (setq L (sqrt (* (+ m D) (- m D))))			;半弦长
	    (setq b (atan L D))                                 ;夹角
	    (setq p (polar '(0 0) (+ a b) m))                   ;变换后的交点
	    (setq q (polar '(0 0) (- a b) m))			;变换后的交点
	    (list
	      (mat:o2w (list (car p) (* RAT (cadr p))) CEN an)  ;把交点变换回去
	      (mat:o2w (list (car q) (* RAT (cadr q))) CEN an)	;把交点变换回去
	    )
	  )
	)
      )
    )
  )
)


;;;=============================================================
;;; 另一方法，速度稍慢                                          
;;;=============================================================
(Defun ELL:IntersWithLine2D_1 (Center major Ratio P1 P2 / v1 v2 a b s)
  (setq v1 (mapcar '- p1 center))
  (setq v2 (mapcar '- p2 center))
  (setq v1 (trans v1 0 major T))
  (setq v2 (trans v2 0 major T))
  (setq v1 (list (caddr v1) (car v1)))
  (setq v2 (list (caddr v2) (car v2)))
  (setq a  (distance '(0 0 0) major))
  (setq b  (* ratio a))
  (foreach p (ELL:IntersWithLine a b v1 v2)
    (setq p (trans (list (cadr p) 0 (car p)) major 0 T))
    (setq p (mapcar '+ center p))
    (setq s (cons p s))
  )
)

;;;=============================================================
;;; 功能: 求空间椭圆与直线的交点（此交点为在椭圆平面上的交点）  
;;; 输入: 椭圆的中心CEN,长轴MAJ,椭率RAT,法线Normal,两点P1,P2.   
;;; 输出: 直线P1P2与椭圆的交点。                                
;;;=============================================================
(Defun ELL:IntersWithLine3D (CEN MAJ RAT Normal P1 P2 / A B M1 M2 S V1 V2)
  (setq v1 (mapcar '- p1 CEN))
  (setq v2 (mapcar '- p2 CEN))
  (setq m1 (ELL:RotationMatrix MAJ Normal))
  (setq m2 (cadr m1))
  (setq m1 (car m1))
  (setq v1 (mat:mxv m2 v1))
  (setq v2 (mat:mxv m2 v2))
  (setq a  (distance '(0 0 0) MAJ))
  (setq b  (* RAT a))
  (foreach p (ELL:IntersWithLine_1 a b v1 v2)
    (setq p (mat:mxv m1 (append p '(0))))
    (setq p (mapcar '+ CEN p))
    (setq s (cons p s))
  )
)

;;;*************************************************************
;;; 六、与椭圆的最近点和最远点                                  
;;;*************************************************************

;;;=============================================================
;;; 功能: 求平面上一点到椭圆的距离的极值(含最小值和最大值)方式1 
;;; 输入: 椭圆的长轴a，短轴b和平面的一点P                       
;;; 输出: 椭圆的跟平面的P点距离为极值的点                       
;;;=============================================================
(defun ELL:GetExtremumDist (a b P / am cc K1 K2 K3 m n v x y ps)
  (setq m (car p))
  (setq n (cadr p))
  (setq cc (* (+ a b) (- a b)))
  (setq k1 (* n b))
  (setq am (* a m))
  (setq k2 (+ am cc))
  (setq k2 (+ k2 k2))
  (setq k3 (- am cc))
  (setq k3 (+ k3 k3))
  (foreach s (Math:Quartic_Equation k1 k2 0 k3 (- k1))
    (if (equal (cadr s) 0 1e-8)
      (progn
        (setq s (car s))
        (setq v (* s s))
        (setq x (/ (* a (- 1 v)) (1+ v)))
        (setq y (/ (* b (+ s s)) (1+ v)))
        (setq ps (cons (list x y) ps))
      )
    )
  )
  (if (equal k1 0 1e-8)
    (setq ps (cons (list (- a) 0) ps))
    (reverse ps)
  )
)

;;;=============================================================
;;; 功能: 求平面上一点到椭圆的距离的极值(含最小值和最大值)方式2 
;;; 输入: 椭圆的长轴a，短轴b和平面的一点P                       
;;; 输出: 椭圆的跟平面的P点距离为极值的点                       
;;;=============================================================
(defun ELL:GetExtremumDist1 (a b P / c cc e ee f K1 K2 K3 K4 K5 m n x y ps)
  (setq m (car p))
  (setq n (cadr p))
  (if (equal m 0 1e-10)
    (list (list 0 (- b)) (list 0 b))
    (progn
      (setq cc (* (+ a b) (- a b)))
      (setq c  (sqrt cc))                               
      (setq e  (/ c a))
      (setq f  (/ b a))
      (setq ee (* e e))
      (setq k1 (* ee ee))
      (setq k2 (* -2 m ee))
      (setq k3 (- (+ (* n n f f) (* m m)) (* ee cc)))
      (setq k4 (* 2 m cc))
      (setq k5 (- (* a a m m)))
      (foreach s (Math:Quartic_Equation k1 k2 k3 k4 k5)
        (if (equal (cadr s) 0 1e-6)
          (progn
            (setq x (car s))
            (setq y (/ (* n b b) (- (/ (* m a a) x) cc)))
            (setq ps (cons (list x y) ps))
          )
        )
      )
      ps
    )
  )
)

;;;=============================================================
;;; 功能: 求空间一点到椭圆的距离的极值（包括最小值和最大值）    
;;; 输入: 椭圆的中心CEN,长轴矢量MAJ,椭率RAT,法线Normal,3D点P.   
;;; 输出: 椭圆的跟空间的P点距离为极值的点                       
;;;=============================================================
(defun ELL:GetExtremumDist_3D (CEN MAJ RAT Normal P / A B M1 M2 S V1)
  (setq v1 (mapcar '- p CEN))
  (setq m1 (ELL:RotationMatrix MAJ Normal))
  (setq m2 (cadr m1))
  (setq m1 (car m1))
  (setq v1 (mat:mxv m2 v1))
  (setq a  (distance '(0 0 0) MAJ))
  (setq b  (* RAT a))
  (foreach n (ELL:GetExtremumDist a b v1)
    (setq n (mat:mxv m1 (append n '(0))))
    (setq n (mapcar '+ CEN n))
    (setq s (cons n s))
  )
)

;;;*************************************************************
;;; 七 椭圆的包围盒和最小包围盒                                 
;;;*************************************************************

;;;=============================================================
;;; 功能: 获取椭圆的最小包围盒(未完整测试，可能对变形椭圆无效)  
;;; 参数: 椭圆实体                                              
;;; 返回: 包围这个椭圆的最小矩形的四个角点                      
;;;=============================================================
(defun ELL:GetMinBox (ent / D M P Q)
  (setq D (entget ent))
  (setq M (cdr (assoc 11 D)))
  (setq P (vlax-curve-getPointAtParam ent (* pi 0.5)))
  (setq Q (vlax-curve-getPointAtParam ent (* pi 1.5)))
  (list
    (mapcar '- Q M)
    (mapcar '+ Q M)
    (mapcar '+ P M)
    (mapcar '- P M)
  )
)

(defun ELL:GetMinBox1 (ent / O C X Y V)
  (setq O (vlax-ename->vla-object ent))
  (setq C (vlax-get O 'Center))
  (setq X (vlax-get O 'MajorAxis))
  (setq Y (vlax-get O 'MinorAxis))
  (setq V (mapcar '- X Y))
  (list
    (mapcar '- C X Y)
    (mapcar '+ C V)
    (mapcar '+ C X Y)
    (mapcar '- C V)
  )
)

;;;=============================================================
;;; 功能: 获取椭圆OCS的包围盒                                   
;;; 参数: 椭圆的中心，半长轴，半短轴，旋转角                    
;;; 返回: 包围这个椭圆的OCS方向的四个角点                       
;;;=============================================================
(defun ELL:GetOCSBox (C a b ang / l x p 90D LL LR UL UR)
  (setq 90d (* pi 0.5))
  (foreach n (list 0 90d)
    (setq x (- n ang))
    (setq x (atan (* a (sin x)) (* b (cos x))))
    (setq x (+ x 90d))
    (setq p (list (* a (cos x)) (* b (sin x))))
    (setq p (GEO:Rot2D p '(0 0) ang))
    (setq l (cons (mapcar '+ C p) l))
    (setq l (cons (mapcar '- C p) l))
  )
  (setq ll (apply 'mapcar (cons 'min l)))
  (setq ur (apply 'mapcar (cons 'max l)))
  (setq lr (list (car ur) (cadr ll)))
  (setq ul (list (car ll) (cadr ur)))
  (list ll lr ur ul)
)

;;;=============================================================
;;; 功能: 获取椭圆的包围盒（此包围盒方向平行于当前坐标系）      
;;; 参数: 椭圆实体                                              
;;; 返回: 包围这个椭圆的平行当前坐标系方向的四个角点            
;;;=============================================================
(defun ELL:GetBoundingBox (ent / lst obj pta ptb C Cen Maj Mnr Ret)
  (setq obj (vlax-ename->vla-object ent))
  (if (= 1 (getvar 'WORLDUCS))
    (progn
      (setq lst (vla-getboundingbox obj 'pta 'ptb))
      (setq pta (vlax-safearray->list pta))
      (setq ptb (vlax-safearray->list ptb))
      (list
        (list (car pta) (cadr pta) 0)
        (list (car ptb) (cadr pta) 0)
        (list (car ptb) (cadr ptb) 0)
        (list (car pta) (cadr ptb) 0)
      )
    )
    (progn
      (setq cen (vlax-get obj 'center))
      (setq maj (vlax-get obj 'MajorAxis))
      (setq mnr (vlax-get obj 'MinorAxis))
      (setq C   (trans cen 0 1))
      (setq pta (mapcar '+ C (trans maj 0 1 T)))
      (setq ptb (mapcar '+ C (trans mnr 0 1 T)))
      (if (setq ret (ELL:2J C pta ptb)) 
        (mat:translist (apply 'ELL:GetOCSBox ret) 1 0 nil)
      )
    )
  )
)
   
;;;*************************************************************
;;; 八、椭圆作图和求解问题                                      
;;; 以下字母代表的含义:                                         
;;; C----Center, The center of Ellipse, 椭圆的中心              
;;; F----Focus, The focus(foci) of Ellipse, 椭圆的焦点          
;;; X----Axis, An axis of Ellipse, 椭圆的轴                     
;;; P----Point, A Point on Ellipse, 椭圆上的一点                
;;; T----Tangent, A tangent of Ellipse, 椭圆的切线              
;;; J----conjugate diameter, 共轭轴                             
;;; D----Direction, 已知某个共轭轴的方向                        
;;; M----Major Direction, 已知主轴的方向                        
;;;*************************************************************

;;;=============================================================
;;; Given a pair of conjugate diameters, construct an ellipse   
;;; 功能: 已知两共轭半轴作椭圆                                  
;;; 输入: 共轭轴的交点C(即椭圆圆心)、共轭半轴的两端点P,Q        
;;; 输出: 成功返回椭圆的基本要素，否则返回nil                   
;;;=============================================================
(defun ELL:2J (C P Q / K M R A)                         
  (if (not (LINE:Colinearity C P Q))                            ;如果三点不共线
    (progn
      (setq K (GEO:Rot90 C C Q))				;Q点绕C旋转90度得到K
      (setq M (GEO:Midpoint K P))				;M为KP的中点
      (if (equal (cos (- (angle C P) (angle C Q))) 0 1e-8)      ;如果CP和CQ垂直
	(list C (distance C P) (distance C Q) (angle C P))	;则它们就是椭圆的两轴	
	(progn 
          (setq R (distance M C))			        ;以M为圆心，MC为半径画圆，
          (setq A (polar M (angle M P) R))                      ;交KP于A,B
          (list C (distance K A) (distance P A) (angle C A))    ;则CA,CB为椭圆的两轴线方向，半轴长分别为KA，PA.
	)
      )
    )
  )
)

;;;=============================================================
;;; Another way                                                 
;;; To descirbe an ellipse having given conjugate diameters.    
;;; Ref: Page 111, Problem 63                                   
;;;=============================================================
(defun ELL:2J_1 (C P Q / D E F G M a b x)
  (if (not (LINE:Colinearity C P Q))
    (progn
      (setq E (GEO:Rot90 Q C P))
      (setq F (mapcar '+ Q (mapcar '- Q E)))
      (setq D (distance C F))
      (setq X (angle E C))
      (setq G (polar C X D))
      (setq M (polar c X (- D)))
      (setq a (* (distance E G) 0.5))
      (setq b (* (distance E M) 0.5))
      (list C a b (angle C (geo:midpoint M F)))
    )
  )
)

;;;=============================================================
;;; Construct an ellipse,one axis and a point on it being given.
;;; Note: Page 114, Problem 64                                  
;;; 功能: 已知椭圆的一个长轴，和椭圆上的一点                    
;;; 输入: 长轴的两个端点M、N和椭圆上的一点P                     
;;; 输出: 成功返回椭圆的基本要素，否则返回nil                   
;;;=============================================================
(defun ELL:XP (M N P / C a b Maj Vec x y d)
  (setq C (Geo:Midpoint M N))
  (setq A (distance C M))
  (setq Maj (mapcar '- M C))
  (setq vec (trans (mapcar '- P C) 0 Maj T))                            
  (setq x (caddr vec))
  (setq y (car vec))
  (if (not (>= (abs x) a))
    (progn
      (setq d (sqrt (* (+ a x) (- a x))))
      (setq b (abs (* (/ y d) a)))
      (list C a b (angle C M))
    )
  )
)

;;;=============================================================
;;; Construct an ellipse with a given axis to touch a given line
;;; Note: Page 115, Problem 65                                  
;;; 功能: 已知椭圆的一个长轴，和椭圆外的一条切线                
;;; 输入: 长轴的两个端点M、N和椭圆外的一条切线PQ                
;;; 输出: 成功返回椭圆的基本要素，否则返回nil                   
;;;=============================================================
(defun ELL:XT (M N P Q / C A Ax an v1 v2 y1 x1 y2 x2 d1 d2 d3)
  (setq C  (Geo:Midpoint M N))                                  ;椭圆的中心
  (setq A  (distance C M))                                      ;半长轴长
  (setq Ax (mapcar '- M C))                                     ;CM矢量
  (setq an (angle C M))                                         ;CM角 
  (setq v1 (trans (mapcar '- P C) 0 Ax T))                      ;P到MN的距离和CP的投影
  (setq v2 (trans (mapcar '- Q C) 0 Ax T))                      ;Q到MN的距离和CQ的投影
  (setq y1 (car   v1))						;P到MN的距离
  (setq x1 (caddr v1))						;CP的投影
  (setq y2 (car   v2))						;Q到MN的距离
  (setq x2 (caddr v2))						;CQ的投影
  (setq d1 (- (* x1 y2) (* x2 y1)))
  (setq d2 (- y1 y2))
  (setq d3 (- (* D1 D1) (* d2 d2 a a)))                         
  (if (and (not (equal x1 x2 1e-14)) (> d3 0)) 
    (list C a (abs (/ (sqrt d3) (- x1 x2))) an)			;计算出半短轴长
  )
)
;;;=============================================================
;;; Another way                                                 
;;;=============================================================
(defun ELL:XT_1 (M N P Q / C A Maj I L x Ang G H J)
  (setq C (Geo:Midpoint M N))
  (setq A (distance C M))
  (setq Maj (mapcar '- M C))
  (if (setq I (inters M N P Q nil))
    (progn 
      (setq L (distance C I))
      (setq x (/ (* a a) L))
      (setq ang (angle C I))
      (setq G (polar C ang (abs x)))
      (setq H (polar G (+ ang (* pi 0.5)) a))
      (if (setq J (inters G H P Q nil))
        (ELL:XP M N J)
      )
    )
    (progn
      (setq ang (angle C M))
      (setq G (polar C (+ ang (* pi 0.5)) A))
      (if (setq H (inters C G P Q nil))
        (list C a (distance C H) ang)
      )
    )
  )
)

;;;=============================================================
;;; To decribe an ellipse, the directions of a pair of conjugate
;;; diameters, a tangent and its point of contact being given   
;;; Note: Page 116, Problem 66                                  
;;; 功能: 已知椭圆的两个共轭轴方向，一条切线和这个切点          
;;; 输入: 两条共轭轴JK和MN,切线PQ并切于P点                      
;;; 输出: 成功返回椭圆的基本要素，否则返回nil                   
;;;=============================================================
(defun ELL:TP2D (J K M N P Q / C G H A B E F U W r s x y)
  (if (and (setq C (inters J K M N nil))
           (setq G (inters P Q J K nil))
           (setq H (inters P Q M N nil))
      )
    (progn
      (setq A (angle C G))
      (setq B (angle C H))
      (setq U (inters P (polar P B 1) J K nil))
      (setq W (inters P (polar P A 1) M N nil))
      (setq r (distance C G))
      (setq s (distance C H))
      (setq x (distance C U))
      (setq y (distance C W))
      (if (and (< x r) (< y s))
        (progn 
          (setq x (sqrt (* x r)))
          (setq y (sqrt (* y s)))
          (setq E (polar C A X))
          (setq F (polar C B Y))
          (ELL:2J C E F)
        )
      )
    )
  )
)

;;;=============================================================
;;; To describe an ellipse, the center, two points on the curve 
;;; and directions of a pair of conjugate diameters being given.
;;; Note: Page 116, Problem 67                                  
;;; 功能: 已知椭圆的中心，上面两点和一对共轭轴的方向，求此椭圆  
;;; 输入: 中心C,共轭轴CM和CN, 两点P,Q.                          
;;; 输出: 成功返回椭圆的基本要素，否则返回nil                   
;;;=============================================================
(defun ELL:C2D2P (C M N P Q / A B D E F G I P1 Q1 R S U W X Y Z)
  (if (and  (inters P Q C M nil) (inters P Q C N nil))
    (progn
      (setq x (angle C M))
      (setq y (angle C N))
      (setq D (inters P (polar P x 1) C N nil))
      (setq S (inters Q (polar Q y 1) C M nil))
      (setq E (inters P D Q S nil))
      (setq P1 (polar D (angle P D) (distance P D)))
      (setq Q1 (polar S (angle Q S) (distance Q S)))
      (setq F (polar S (angle P Q) 1))
      (setq F (inters S F D E nil))
      (setq G (polar S (angle P1 Q1) 1))
      (setq G (inters S G D E nil))
      (setq z (sqrt (* (distance E F) (distance E G))))
      (setq i (angle E P))
      (setq r (distance P D))
      (setq a (sqrt (+ (* r r) (* z z))))
      (setq U (polar C I a))
      (setq b (/ (* (distance C D) a) z))
      (setq W (polar C (angle C D) b))
      (ELL:2J C U W)
    )
  )
)

;;;=============================================================
;;; To describe an ellipse, the center, direction of the major  
;;; axis and two tangents being given.                          
;;; Note: Page 118, Problem 68                                  
;;; 功能: 已知椭圆的中心，主轴的方向，和两切线，求此椭圆        
;;; 输入: 中心C,主轴方向CM，和两切线PD，PE（须相交）。          
;;; 输出: 成功返回椭圆的基本要素，否则返回nil                   
;;;=============================================================
(defun ELL:CM2T (C M P D E / A A0 A1 A2 A3 A4 A5 B F F1 F2 F3 G H L O R S T1)
  (setq a0 (angle C M))                                         ;长轴的方向角
  (setq a1 (angle p D))                                         ;直线一的方向角
  (setq a2 (angle p E))                                         ;直线二的方向角
  (setq a3 (* (+ a1 a2) 0.5))                                   ;两直线内角平分线
  (setq a4 (+ a3 (* pi 0.5)))                                   ;两直线外角平分线
  (setq G (inters P (polar P a3 1) C M nil))                    ;内角平分线与长轴交于G
  (setq H (inters P (polar p a4 1) C M nil))                    ;外角平分线与长轴交于H
  (if (and G H)                                                 ;如果交点都存在（否则无解或无穷解）
    (progn 
      (setq O (Geo:MidPoint G H))                               ;GH的中点为圆心，GH为直径做作圆
      (setq R (distance O G))                                   ;OG为这个圆的半径
      (setq L (distance O C))                                   
      (if (>= L R)                                              ;如果可以作切线
        (progn 
          (setq F  (sqrt (* (+ L R) (- L R))))                  ;切线长即为焦距
          (setq F1 (polar C a0 (- F)))                          ;左焦点F1
          (setq F2 (polar C a0 F))                              ;右焦点F2
	  (setq F3 (Geo:Mirror2D F1 P a1))			;F1关于PD的对称点F3
          (setq T1 (inters F2 F3 P D nil))                      ;F2F3与PD的交点为切点
          (setq a5 (* (+ (angle T1 F1) (angle T1 F2)) 0.5))     ;角F1T1F2的平分线
          (if (equal (rem (- a1 a5) pi) 0 1e-6)			;如果平分线与PD重合
            nil                                                 ;无解
            (setq a (/ (+ (distance F1 T1) (distance F2 T1)) 2) ;否则得到椭圆的长轴长
                  b (sqrt (* (- a f) (+ a f)))                  ;由焦距和长轴得到短轴
                  s (list C a b a0)                             ;因而得解
            )
          )
        )
      )
    )
  )
)

;;;=============================================================
;;; Construct an ellipse,the center,the directions of a pair of 
;;; conjugate diameters,a tangent and a point on it being given.
;;; Note: Page 119, Problem 69                                  
;;; 功能: 已知椭圆中心，两共轭轴方向，经过一点和一切线。求此椭圆
;;; 输入: 中心C,共轭轴方向CD,CE,点P,切线P1P2。                  
;;; 输出: 返回椭圆的解集（可能无解或者多解）。                  
;;;=============================================================
(defun ELL:CPT2D (C D E P P1 P2 / A1 A2 F G H J K L1 L2 L3 L4 M N O R S)
  (if (or (LINE:Colinearity C D E)
          (LINE:Colinearity C P1 P2)
          (equal P C 1e-8)
      )
    nil
    (progn
      (setq M (inters P1 P2 C D nil))
      (setq N (inters P1 P2 C E nil))
      (setq L1 (distance C P))
      (if (and M N)
        (if (setq F (inters C P M N nil))
          (if (>= (setq L2 (distance C F)) L1)
            (progn
              (setq L3 (sqrt (* (+ L2 L1) (- L2 L1))))
              (setq O (Geo:MidPoint M N))
              (setq R (distance O M))
              (setq a1 (angle F O))
              (setq a2 (+ a1 (* pi 0.5)))
              (setq J (polar F a1 L3))
              (setq K (polar J a2 L1))
              (setq S nil)
              (foreach I (CIR:Inters_Circle_Line O R F K)
                (setq H (inters I (polar I a2 1) M N nil))
                (setq G (polar C a1 (distance H I)))
                (setq S (cons (ELL:2J C G H) S))
              )
            )
          )
          (cond
            ( (equal L1 (setq R (* 0.5 (distance M N))) 1e-8)
              (List (ELL:2J C P (Geo:MidPoint M N)))
            )
            ( (< L1 R)
              (setq O (Geo:MidPoint M N))
              (setq L2 (sqrt (* (+ R L1) (- R L1))))
              (setq a1 (angle O M))
              (setq J  (polar O a1 L2))
              (setq K  (polar O a1 (- L2)))
              (setq S  nil)
              (foreach I (list J K)
                (setq S (cons (ELL:2J C I P) S))
              )
            )
          )
        )
        (progn
          (and N (setq M N N D D E E N))
          (if (setq F (inters C P P1 P2 nil))
            (if (< L1 (setq L2 (distance C F)))
              (progn
                (setq L3 (sqrt (* (+ L2 L1) (- L2 L1))))
                (setq L4 (/ (* L1 (distance F M)) L3))
                (setq E  (polar C (angle C E) L4))
                (list (ELL:2J C E M))
              )
            )
            (list (ELL:2J C P M))
          )
        )
      )
    )
  )
)


;;;=============================================================
;;; Construct an ellipse,the center,two tangents and a point on 
;;; the curve being given                                       
;;; Note: Page 121, Problem 70                                  
;;; 功能: 给定椭圆的中心，两切线和其上一点，求此椭圆。          
;;; 参数: 中心C, 椭圆上的一点P, 两切线IM, IN（须相交）          
;;; 返回: 所求椭圆的解集。                                      
;;;=============================================================
(defun ELL:CP2T (C P I M N / A1 A2 A3 D E F G H L1 L2 L3 O R S)
  (if (not (setq E (inters C P I N nil)))
    (setq E N N M M E E (inters C P I N nil))
  )
  (setq L1 (distance C P))
  (setq L2 (distance C E))
  (if (> L2 L1)
    (progn
      (setq L3 (sqrt (* (+ L2 L1) (- L2 L1))))
      (setq a1 (angle I M))
      (setq a2 (angle I N))
      (setq a3 (+ a2 (* pi 0.5)))
      (setq O  (inters C (polar C a1 1) I N nil))
      (setq F  (polar E a2 L3))
      (setq G  (polar F a3 L1))
      (setq R  (distance O I))
      (foreach K (CIR:Inters_Circle_Line O R G E)
        (setq H (inters K (polar K a3 1) I N nil))
        (setq D (polar C a2 (distance K H)))
        (setq s (cons (ELL:2J C H D) s))
      )       
    )
  )
)

;;;=============================================================
;;; Construct an ellipse, the center, two points on curve and a 
;;; tangent being given                                         
;;; Note: Page 123, Problem 72                                  
;;; 功能: 给定椭圆的中心，一切线和椭圆上两点，求此椭圆。        
;;; 参数: 中心C, 椭圆上的两点A,B, 一切线PQ。                    
;;; 返回: 所求椭圆的解集。                                      
;;;=============================================================
(defun ELL:CT2P (C A B P Q / d e)
  (setq D (GEO:MidPoint A B))
  (setq E (inters C (polar C (angle A B) 1) P Q nil))
  (ELL:CPT2D C D E A P Q)
)

;;;=============================================================
;;; Construct an ellipse, the center, three tangents being given
;;; Note: Page 122, Problem 71                                  
;;; 功能: 给定椭圆的中心和三切线，求此椭圆。                    
;;; 参数: 中心C, 三切线DE,EF,FD（应两两相交）。                 
;;; 返回: 所求椭圆。                                            
;;;=============================================================
(defun ELL:C3T (C D E F / A B D1 D2 D3 G H I J K L1 L2 L3 P Q U W X Y)
  (setq d1 (distance C D))
  (setq d2 (distance C E))
  (setq d3 (distance C F))
  (setq l3 (distance D E))
  (setq l2 (distance D F))
  (setq l1 (distance E F))

  (if (and (TRI:IsTriangle l1 l2 l3)
           (TRI:IsTriangle d1 d2 l3)
           (TRI:IsTriangle d2 d3 l1)
           (TRI:IsTriangle d3 d1 l2)
      )
    (progn
      (setq X (inters (GEO:scale2 C D) (GEO:scale2 C F) D E nil))
      (setq Y (GEO:Scale2 C X))
      (setq G (inters Y (polar Y (angle C E) 1) E F nil))
      (setq H (inters X (polar X (angle C F) 1) E F nil))
      (setq I (inters X G Y H nil))
      (setq J (inters I D E F nil))
      (setq K (inters J (polar J (angle X G) 1) D E nil))
      (setq A (angle C X))
      (setq B (angle C D))
      (setq P (inters K (polar K B 1) X Y nil))
      (setq Q (inters K (polar K A 1) C D nil))
      (setq D1 (distance C P))
      (setq D2 (distance C X))
      (setq L1 (distance C Q))
      (setq L2 (distance C D))
      (if (and (< D1 D2) (< L1 L2))
        (progn 
          (setq U (polar C A (sqrt (* D1 D2))))
          (setq W (polar C B (sqrt (* L1 L2))))
          (ELL:2J C U W)
        )
      )
    )
  )
)

;;;=============================================================
;;;highflybird  2012.5.29 创作于深圳 2013.5.5 修改于深圳        
;;;-------------------------------------------------------------
;;;功能：根据中心和椭圆上的三点画这个椭圆                       
;;;参数：中心点，和其他三点                                     
;;;返回：中心点，半长轴值、半短轴、旋转角                       
;;;=============================================================
(defun ELL:C3P (Cen p1 p2 p3 / a b c abc ac bb aX bY an I J PT1 PT2 PT3 SS)
  (setq p1 (mapcar '- p1 cen))
  (setq p2 (mapcar '- p2 cen))
  (setq p3 (mapcar '- p3 cen))
  (setq abc (Mat:3VLE (* (car  p1) (car  p1))
                      (* (car  p1) (cadr p1))
                      (* (cadr p1) (cadr p1))
                      (* (car  p2) (car  p2))
                      (* (car  p2) (cadr p2))
                      (* (cadr p2) (cadr p2))
                      (* (car  p3) (car  p3))
                      (* (car  p3) (cadr p3))
                      (* (cadr p3) (cadr p3))
                      1. 1. 1.
            )
  )
  (if abc
    (progn
      (setq a  (car   abc))
      (setq b  (cadr  abc))
      (setq c  (caddr abc))
      (setq b  (/ b 2))
      (setq bb (* b b))
      (setq ac (* a c))
      (setq I  (+ a c))
      (setq J  (- a c))
      (setq ss (sqrt (+ (* J J) (* 4 bb))))
      (if (> I ss)
        (progn
          (setq aX (sqrt (/ 2 (- I ss))))
          (setq bY (sqrt (/ 2 (+ I ss))))
          (if (equal (/ J I) 0 1e-16)
            (setq an (* (atan b 0) 0.5))
            (setq an (* (atan (/ (+ b b) J)) 0.5))
          )
          (and (> a c) (setq an (+ an (* pi 0.5))))
          (list cen aX bY an)                                   ;返回中心，半长轴，半短轴，旋转角度
        )
      )
    )
  )
)

;;;=============================================================
;;;highflybird  2012.5.29                                       
;;;功能：根据中心和椭圆上的三点画这个椭圆（另一算法）           
;;;参数：中心点，和其他三点                                     
;;;返回：中心点，半长轴值、半短轴、旋转角                       
;;;=============================================================
(defun ELL:C3P_1 (Cen p1 p2 p3 / A11 A12 A13 A21 A22 A23 A31 A32 A33
                                 ANG A B PT1 PT2 PT3 UVW U V W)
  (setq pt1 (mapcar '- p1 cen))
  (setq pt2 (mapcar '- p2 cen))
  (setq pt3 (mapcar '- p3 cen))
  (setq a11 (* (car  pt1) (car  pt1)))
  (setq a12 (* (car  pt1) (cadr pt1)))
  (setq a13 (* (cadr pt1) (cadr pt1)))
  (setq a21 (* (car  pt2) (car  pt2)))
  (setq a22 (* (car  pt2) (cadr pt2)))
  (setq a23 (* (cadr pt2) (cadr pt2)))
  (setq a31 (* (car  pt3) (car  pt3)))
  (setq a32 (* (car  pt3) (cadr pt3)))
  (setq a33 (* (cadr pt3) (cadr pt3)))
  (setq uvw (MAT:3VLE a11 a12 a13 a21 a22 a23 a31 a32 a33 1. 1. 1.))
  (setq u (car uvw))
  (setq v (cadr uvw))
  (setq w (caddr uvw))
  (if uvw
    (if (and (zerop v) (equal u w 1e-16))                       ;这是个圆
      (list cen (sqrt (/ 1 u)))
      (progn
        (if (zerop v)
          (setq ang (/ (atan 0 (- u w)) 2)
                a   (/ (+ u w (/ (- u w) (cos (* 2 ang)))) 2.0)
          )
          (progn
            (if (equal u w 1e-16)
              (setq ang (/ (atan v 0) 2))
              (setq ang (/ (atan (/ v (- u w))) 2))
            )
            (setq a (/ (+ u w (/ v (sin (* 2 ang)))) 2.0))
          )
        )
        (setq b (+ u w (- a)))
        (if (and (> a 0) (> b 0))
          (progn
            (setq a (sqrt (/ 1 a)))
            (setq b (sqrt (/ 1 b)))
            (if (> b a)
              (list cen b a (+ ang (* pi 0.5)))
              (list cen a b ang)
            )
          )
        )
      )
    )
  )
)

;;;=============================================================
;;; To describe an ellipse, the center, 3 points on the curve   
;;; being given. Ref: Page 124, Problem 73                      
;;;=============================================================
(defun ELL:C3P_2 (C D E F / G H I J K M P Q R)
  (setq G (GEO:MidPoint E F))
  (setq H (GEO:MidPoint D F))
  (setq I (GEO:MidPoint D E))
  (if (setq J (inters C G D F nil))
    (setq K (inters C I E J nil)
          M (inters H K E F nil)
    )
    (setq K (inters C G D E nil)
          J (inters C H F K nil)
          M (inters I J E F nil)
    )
  )
  (if (null M )
    (setq M (polar D (angle E F) (distance E F)))
  )
  (setq P (inters C I D M nil))
  (setq Q (inters C H D M nil))
  (if (and P Q (setq R (inters E P F Q nil)))
    (ELL:C3T C P Q R)
  )
)

;;;=============================================================
;;;highflybird  2012.5.29 创作于深圳 2013.5.5 修改于深圳        
;;;-------------------------------------------------------------
;;;功能：根据椭圆上的四点画水平(或者垂直)方向椭圆               
;;;参数：四个给定的二位或者三维点                               
;;;返回：中心点，半长轴值、半短轴、旋转角                       
;;;=============================================================
(defun ELL:4P (p0 p1 p2 p3 / A B C D K M N O X1 X2 X3 Y1 Y2 Y3 U1 U2 U3 W1 W2 W3)
  (setq p1 (mapcar '- p1 p0)
        p2 (mapcar '- p2 p0)
        p3 (mapcar '- p3 p0)
        x1 (car  p1)
        y1 (cadr p1)
        x2 (car  p2)
        y2 (cadr p2)
        x3 (car  p3)
        y3 (cadr p3)
        U1 (* x1 x1)
        W1 (* y1 y1)
        U2 (* x2 x2)
        W2 (* y2 y2)
        U3 (* x3 x3)
        W3 (* y3 y3)
        A  (MAT:Det3 W1 x1 y1 W2 x2 y2 W3 x3 y3)
        B  (MAT:Det3 U1 x1 y1 U2 x2 y2 U3 x3 y3)
        C  (MAT:Det3 U1 W1 y1 U2 W2 y2 U3 W3 y3)
        D  (MAT:Det3 U1 W1 x1 U2 W2 x2 U3 W3 x3)
        B  (- B)
        D  (- D)
  )
  (if (or (and (> A 0) (> B 0)) (and (< A 0) (< B 0)))
    (progn
      (setq K (+ (/ (* C C) 4 A) (/ (* D D) 4 B)))
      (setq m (sqrt (/ K A)))
      (setq n (sqrt (/ K B)))
      (setq O (list (/ C A -2) (/ D B -2) 0))
      (setq O (mapcar '+ O P0))
      (if (/= m 0.0 n 0.0)
        (if (> n m)
          (list O n m (/ pi 2))                                 ;返回中心，半长轴，半短轴，旋转角度90度
          (list O m n 0.0)                                      ;返回中心，半长轴，半短轴，旋转角度0度
        )
      )        
    )
  )
)

;;;=============================================================
;;; To describe an ellipse, the foci and a point on the curve   
;;; being given. Ref: Page 125, Problem 74                      
;;; 功能: 已知椭圆两焦点及其一点，求此椭圆                      
;;; 输入: 两焦点E,F 和经过一点P。                               
;;; 输出: 椭圆的解。                                            
;;;=============================================================
(defun ELL:P2F (E F P / o a b c)
  (setq O (GEO:MidPoint E F))
  (setq c (distance O F))
  (setq a (* 0.5 (+ (distance E P) (distance F P))))
  (setq b (sqrt (* (+ a c) (- a c))))
  (list O a b (angle E F))
)


;;;=============================================================
;;; To describe an ellipse, the foci and a tangent to the curve 
;;; being given. Ref: Page 125, Problem 75                      
;;; 功能: 已知椭圆两焦点及一切线，求此椭圆                      
;;; 输入: 两焦点E,F 和一点切线PQ。                              
;;; 输出: 椭圆的解。                                            
;;;=============================================================
(defun ELL:T2F (E F P Q / G H I)
  (setq I (inters P Q E F nil))
  (if (and I (equal (angle E I) (angle I F) 1e-8))
    nil
    (progn 
      (setq G (GEO:Mirror2D E P (angle P Q)))
      (setq H (inters P Q F G nil))
      (ELL:P2F E F H)
    )
  )
)

;;;=============================================================
;;; To describe an ellipse, a focus, a tangent with its point of
;;; contact and second point on the curve being given           
;;; Note: Page 126, Problem 76                                  
;;; 功能: 已知椭圆一焦点及一切线切点和椭圆上一点，求此椭圆      
;;; 输入: 焦点F,点P 和切线RS（R为切点）。                       
;;; 输出: 椭圆的解。                                            
;;;=============================================================
(defun ELL:FPT (F P R S / G H M J E)
  (if (LINE:IsSameSide F P R S)
    (progn 
      (setq G (GEO:Mirror2D F R (angle R S)))
      (setq H (polar G (angle G R) (distance F P)))
      (setq M (GEO:MidPoint H P))
      (setq J (polar M (+ (angle H P) (* pi 0.5)) 1))
      (setq E (inters M J R G nil))
      (if (LINE:IsSameSide E F R S)
        (ELL:P2F E F P)
      )
    )
  )
)

;;;=============================================================
;;; To describe an ellipse, a focus, a tangent and two points on
;;; the curve being given.                                      
;;; Note: Page 127, Problem 77                                  
;;; 功能: 已知椭圆一焦点及一切线和椭圆上两点，求此椭圆          
;;; 输入: 焦点F,点P,Q 和切线MN。                                
;;; 输出: 椭圆的解集。                                          
;;;=============================================================
(defun ELL:FT2P (F M N P Q / d1 d2 d3 d4 d5 E O S X)
  (if (and (LINE:IsSameSide F P M N) (LINE:IsSameSide F Q M N))
    (progn
      (setq E (GEO:Mirror2D F M (angle M N)))
      (setq d1 (distance F P))
      (setq d2 (distance F Q))
      (if (equal d1 d2 1e-8)
        (setq X (CIR:PPC P Q E d1))
        (if (> d1 d2)
          (setq X (CIR:PCC P Q (- d1 d2) E d1))
          (setq X (CIR:PCC Q P (- d2 d1) E d2))
        )
      )
      (foreach v X
        (setq O  (car v))
        (setq d3 (distance O E))
        (setq d4 (+ d1 (distance O P)))
        (setq d5 (+ d2 (distance O Q)))
        (if (and (equal d3 d4 1e-8) (equal d3 d5 1e-8))
          (setq s (cons (ELL:P2F O F P) s))
        )
      )
      (reverse S)
    )
  ) 
)

;;;=============================================================
;;; Construct an ellipse, a focus, a point on the curve and two 
;;; tangents being given                                        
;;; Note: Page 127, Problem 78                                  
;;; 功能: 已知椭圆一焦点及两条切线和椭圆上一点，求此椭圆        
;;; 输入: 焦点F,点P 和两条切线L1L2,L3L4。                       
;;; 输出: 椭圆的解集。                                          
;;;=============================================================
(defun ELL:FP2T (F P L1 L2 L3 L4 / D1 D2 D3 E G H R s)
  (setq G (GEO:Mirror2D F L1 (angle L1 L2)))
  (setq H (GEO:Mirror2D F L3 (angle L3 L4)))
  (setq S nil)
  (setq R (distance P F))
  (foreach n (CIR:PPC G H P R)
    (setq E (car n))
    (setq d1 (distance E G))
    (setq d2 (distance E H))
    (setq d3 (+ R (distance E P)))
    (if (and (equal d1 d2 1e-6) (equal d2 d3 1e-6))
      (setq s (cons (ELL:P2F E F P) s))
    )
  )
  S
)

;;;=============================================================
;;; Construct an ellipse, a focus and three tangents being given
;;; Note: Page 129, Problem 79                                  
;;; 功能: 已知椭圆一焦点及三条切线，求此椭圆                    
;;; 输入: 焦点F,三切线L1L2,L3L4,L5L6。                          
;;; 输出: 椭圆的解集。                                          
;;;=============================================================
(defun ELL:F3T (F L1 L2 L3 L4 L5 L6 / E G H I)
  (setq G (GEO:Mirror2D F L1 (angle L1 L2)))
  (setq H (GEO:Mirror2D F L3 (angle L3 L4)))
  (setq I (GEO:Mirror2D F L5 (angle L5 L6)))
  (if (setq E (car (TRI:CircumCenter G H I)))
    (if (and (LINE:IsSameSide E F L1 L2)
             (LINE:IsSameSide E F L3 L4)
             (LINE:IsSameSide E F L5 L6)
        )
      (ELL:P2F E F (inters E G L1 L2 nil))
    )
  )
)

;;;=============================================================
;;; Construct an ellipse, a focus and three points being given  
;;; Note: Page 129, Problem 80                                  
;;; 功能: 已知椭圆一焦点及其上三点，求此椭圆                    
;;; 输入: 焦点F,经过三点I,J,K.                                  
;;; 输出: 椭圆的解集。                                          
;;;=============================================================
(defun ELL:F3P (F I J K / a1 a2 a3 b1 b2 b3 p1 p2 p3 M N Pt)
  (setq a1 (angle F I))
  (setq A2 (angle F J))
  (setq A3 (angle F K))
  (setq b1 (* 0.5 (+ a1 a2)))
  (setq b2 (* 0.5 (+ a2 a3)))
  (setq b3 (* 0.5 (+ a3 a1)))
  (setq p1 (polar F b1 pi))
  (setq p2 (polar F b2 pi))
  (setq P3 (polar F b3 pi))
  (setq M  (inters I P3 F P1 nil))
  (setq N  (inters K P3 F P2 nil))
  (setq Pt (inters M N I K nil))
  (and (null pt) (setq Pt (polar J (angle I K) Pi)))
  (setq M  (inters Pt J F P1 nil))
  (setq N  (inters Pt J F P2 nil))
  (ELL:F3T F I M Pt J N K)
)


;;;=============================================================
;;; Construct an ellipse,2 tangents with their points of contact
;;; and a third point being given                               
;;; Note: Page 131, Problem 81                                  
;;; 功能: 已知椭圆两切线及其切点和椭圆上第三点，求此椭圆        
;;; 输入: 两切线VQ,VR(Q,R是其切点)和经过第三点P。               
;;; 输出: 椭圆的解集。                                          
;;;=============================================================
(defun ELL:P2T (V Q R P / s u w x y m n c)
  (setq S (inters Q P V R nil))
  (setq U (inters V Q R P nil))
  (cond
    ( (and S U)
      (setq W (inters U S Q R nil))
    )
    ( U
      (setq W (polar U (angle V R) 1))
      (setq W (inters U W Q R nil))
    )
    ( S
      (setq W (polar S (angle V Q) 1))
      (setq W (inters S W Q R nil))
    )
    (t
      (setq W (polar P (angle Q R) 1))
    )
  )
  (setq X (inters P W V R nil))
  (setq Y (inters P W V Q nil))
  (setq M (Geo:Midpoint Q R))
  (setq N (Geo:MidPoint P R))
  (setq C (inters V M X N nil))
  (ELL:C3T C V X Y)
)


;;;=============================================================
;;; To describe an ellipse,2 tangents and 3 points being given. 
;;; Note: Page 133, Problem 82                                  
;;; 功能: 已知椭圆两切线和椭圆上三点，求此椭圆。                
;;; 输入: 两切线T1, T2和椭圆上三点A,B,C。                       
;;; 输出: 椭圆的解集。                                          
;;;=============================================================
(defun ELL:2T3P (T1 T2 A B C / p1 p2 q1 q2 p q pts x1 x2 res s)
  (setq p1 (car  t1))
  (setq p2 (cadr t1))
  (setq q1 (car  t2))
  (setq q2 (cadr t2))
  (foreach i (list B C)
    (setq P (inters A i P1 P2 nil))
    (setq q (inters A i Q1 Q2 nil))
    (setq pts (cons (cdr (GEO:Cen_Foci A i p q)) pts))
  )

  (foreach p (car pts)
    (foreach q (cadr pts)
      (setq x1 (inters p q p1 p2 nil))
      (setq x2 (inters p q q1 q2 nil))
      (if (and x1 x2)
        (if (setq s (ELL:5P_1 x1 x2 A B C))
          (setq res (cons s res))
        )
      )
    )
  )
  res
)

;;;=============================================================
;;; To describe an ellipse,2 tangents and 3 points being given. 
;;; Note: Page 134, Problem 83                                  
;;; 功能: 已知椭圆三切线及和椭圆上两点，求此椭圆。              
;;; 输入: 三切线T1,T2,T3和椭圆上的两点A,B。                     
;;; 输出: 椭圆的解集。                                          
;;;=============================================================
(defun ELL:2P3T (T1 T2 T3 A B / p1 p2 q1 q2 r1 r2 p q r M N L U V W X Y res s)
  (setq p1 (car  t1))
  (setq p2 (cadr t1))
  (setq q1 (car  t2))
  (setq q2 (cadr t2))
  (setq R1 (car  t3))
  (setq R2 (cadr t3))
  (setq P  (inters q1 q2 r1 r2 nil))
  (setq Q  (inters r1 r2 p1 p2 nil))
  (setq R  (inters p1 p2 q1 q2 nil))
  
  (setq M (inters A B R1 R2 nil))
  (setq N (inters A B P1 P2 nil))
  (setq L (inters A B Q1 Q2 nil))


  (foreach D (cdr (GEO:Cen_Foci A B M L))
    (foreach E (cdr (Geo:Cen_FOci A B M N))
      (setq V (GEO:MeanPoint M D E))
      (setq W (inters R V R1 R2 nil))
      (setq X (inters W D Q1 Q2 nil))
      (setq Y (inters W E P1 P2 nil))
      (if (and W X Y)
        ;;(if (setq S (ELL:P2T Q W X A))
        (if (setq S (ELL:5P_1 A B W X Y))
          (setq Res (cons S Res))
        )
      )
    )
  )
  (reverse Res)
)

;;;=============================================================
;;; To describe an ellipse to touch five given lines            
;;; Note: Page 135, Problem 84                                  
;;; 功能: 已知椭圆五切线，求此椭圆。                            
;;; 输入: 五切线T1,T2,T3。                                      
;;; 输出: 椭圆的解集。                                          
;;;=============================================================
(defun ELL:5T(A B C D E / L S F P)
  (setq L (ALG:Graham (list A B C D E)))
  (if (= (length L) 5)
    (progn
      (setq S nil)
      (setq L (append (cdr L) L))
      (repeat 5
        (setq F (inters (car L) (caddr L) (cadr L) (cadddr L) nil))
        (setq P (inters (cadr L) (caddr L) (nth 4 L) F nil))
        (setq S (cons P S))
        (setq L (cdr L))
      )
      (apply 'ELL:5P_1 S)
    )
  )
)

;;;=============================================================
;;; To describe an ellipse,4 tangents AB,BC,CD,CA and a point E 
;;; on the curve being given                                    
;;; Note: Page 137, Problem 85                                  
;;; 功能: 已知椭圆四切线及和椭圆上一点，求此椭圆。              
;;; 输入: 四切线AB,BC,CD,DA和椭圆上的一点E。                    
;;; 输出: 椭圆的解集。                                          
;;;=============================================================
(defun ELL:P4T (A B C D E / ps B1 C1 P Q X s A0 AN AX D1 D2 F G H I SS)
  (setq Ps (ALG:Graham (list A B C D)))
  (if (and (= (length ps) 4) (ALG:Inside-p E ps))
    (progn
      (setq A  (car ps) B (cadr ps) C (caddr ps) D (cadddr ps))
      (setq I  (inters A C B D))
      (setq an (angle I E))
      (setq A0 (GEO:Angle1 I an A))
      (while (setq ps (cdr ps))
	(setq ax (geo:angle1 I an (car ps)))
	(if (> ax A0)
	  (setq a0 ax X A A B B C C D D X)
	)
      )
      (setq B1 (inters B E A D nil))
      (setq C1 (inters C E A D nil))
      (setq D1 (distance A D))
      (setq D2 (distance C D))
      
      (setq SS (GEO:Cen_Foci A C1 D B1))
      (setq P  (caddr ss))
      (setq F  (inters P E C D nil))
      (if (setq X (ELL:5T A B C F P))
	(setq s (cons X s))
      )
      (setq Q (cadr ss))
      (setq G (inters Q E C D nil))
      (setq H (inters Q E B C nil))
      (if (setq x (Ell:5T A B H G D))
	(setq s (cons X s))
      )
    )
  )
)

;;;=============================================================
;;; To describe an ellipse, four points on the curve A, B, C, D,
;;; and a tangent PQ being given                                
;;; Note: Page 139, Problem 87                                  
;;; 功能: 已知椭圆一切线及和椭圆上四点，求此椭圆。              
;;; 输入: 椭圆上的四点A,B,C,D和其切线PQ。                       
;;; 输出: 椭圆的解集。                                          
;;;=============================================================
(defun ELL:T4P (A B C D P Q / E F G H X S l)
  (setq l (ALG:Graham (list A B C D)))
  (setq A (car l) B (cadr l) C (caddr L) D (cadddr L))
  (setq E (inters A B P Q nil))
  (setq F (inters B C P Q nil))
  (setq G (inters C D P Q nil))
  (setq H (inters D A P Q nil))
 	  
  (foreach n (cdr (GEO:Cen_Foci E G F H))
    (Ent:make_point n)
    (if (setq x (ELL:5P A B C D N))
      (setq S (cons x S))
    )
  )
  (reverse S)
)

;;;=============================================================
;;; To describe an ellipse, five points on the curve being given
;;; Note: Page 138, Problem 86                                  
;;; 功能: 过五点画一个椭圆（几何法）                            
;;; 输入: 椭圆上的五点P1,P2,P3,P4,P5.                           
;;; 输出: 椭圆的解。                                            
;;;=============================================================
(defun ELL:5P_1 (p1 p2 p3 p4 p5 / p12 p23 p34 p45 t2 t3 t4 t23 t34 t42 m23 m34 m42 cen lst)
  (setq lst (ALG:Graham (list p1 p2 p3 p4 p5)))
  (if (= (length lst) 5)
    (progn
      (setq p1 (car lst))
      (setq p4 (cadr lst))
      (setq p2 (caddr lst))
      (setq p5 (cadddr lst))
      (setq p3 (last lst))

      (setq p12 (inters p5 p1 p2 p3 nil))
      (setq p23 (inters p1 p2 p3 p4 nil))
      (setq p34 (inters p2 p3 p4 p5 nil))
      (setq p45 (inters p3 p4 p5 p1 nil))

      (setq t2 (inters p12 p23 p4 p5 nil))
      (setq t3 (inters p23 p34 p5 p1 nil))
      (setq t4 (inters p34 p45 p1 p2 nil))
      
      (if (setq t23 (inters t2 p2 t3 p3 nil))
        (if (setq t34 (inters t3 p3 t4 p4 nil))
          (setq m23 (GEO:Midpoint p2 p3)
                m34 (GEO:Midpoint p3 p4)
                cen (inters t23 m23 t34 m34 nil)
          )
          (setq t42 (inters t4 p4 t2 p2 nil)
                m42 (GEO:Midpoint p4 p2)
                m23 (GEO:Midpoint p2 p3)
                cen (inters t23 m23 t42 m42 nil)
          )
        )
        (setq t42 (inters t4 p4 t2 p2 nil)
              t34 (inters t3 p3 t4 p4 nil)
              m34 (GEO:Midpoint p3 p4)
              m42 (GEO:Midpoint p4 p2)
              cen (inters t42 m42 t34 m34 nil)
        )
      )
      (ELL:C3P cen p2 p3 p4)
    )
  )
)

;;;=============================================================
;;; 功能: 过五点画一个椭圆（解方程法）                          
;;; 输入: 椭圆上的五点P1,P2,P3,P4,P5.                           
;;; 输出: 椭圆的解。                                            
;;;=============================================================
(defun ELL:5P (p1 p2 p3 p4 p5 / ret)
  (setq
    ret
    (Mat:Gauss_Equations
      (mapcar
        (function
          (lambda (p / x y)
            (setq x (car p))
            (setq y (cadr p))
            (list (* x y) (* y y) x y 1 (- (* x x)))
          )
        )
        (list p1 p2 p3 p4 p5)
      )
    )
  )
  (if ret
    (apply 'ELL:FromEquation (cons 1 ret))
  )
)

;;;=============================================================
;;; 功能: 画Steiner椭圆(三角形内具有最大面积的内切椭圆)         
;;; 输入: 平面的三点                                            
;;; 输出: 平面的椭圆的基本要素表                                
;;; 日期: 2012-09-11                                            
;;;-------------------------------------------------------------
;;; 参考: http://bbs.mjtd.com/thread-96417-1-1.html             
;;; www.theswamp.org/index.php?topic=42698.msg480873#msg480873  
;;; en.wikipedia.org/wiki/Steiner_inellipse#cite_note-Scimemi-6 
;;;=============================================================
(defun ELL:SteinerEllipse (pt1 pt2 pt3 / a b D E F G H M N O P R w)
  (setq w (angle pt2 pt3))
  (setq G (GEO:CENTROID (list pt1 pt2 pt3)))
  (setq H (polar pt2 (+ w (/ pi 3)) (distance pt2 pt3)))
  (setq O (GEO:CENTROID (list H pt2 pt3)))
  (setq N (GEO:MIDPOINT pt2 pt3))
  (setq R (distance O N))
  (setq M (GEO:MIDPOINT O G))
  (setq P (polar M (+ (angle O G) (/ pi 2)) (distance O G)))
  (if (setq P (inters pt2 pt3 M P nil))
    (progn
      (setq d (distance P O))
      (setq E (polar P w d))
      (setq F (polar P w (- d)))
      (setq a (* (distance G E) (/ R (distance O E))))
      (setq b (* (distance G F) (/ R (distance O F))))
      (list G a b (angle G E))
    )
    (progn
      (setq a R)
      (setq b (distance G N))
      (list G a b w)
    )
  )
)

;;;=============================================================
;;; 功能: Steiner椭圆(三角形内具有最大面积的内切椭圆)的测试样例 
;;; 输入: 无。                                                  
;;; 输出: 无。                                                  
;;; 日期: 2012-09-11                                            
;;;=============================================================
(defun C:Steiner (/ pt1 pt2 pt3 res ent obj)
  (initget 9)
  (setq pt1 (getpoint "\n1st point:"))
  (initget 9)
  (setq pt2 (getpoint pt1 "\n2nd point:"))
  (initget 9)
  (setq pt3 (getpoint pt2 "\n3rd point:"))
  (setq pt1 (trans pt1 1 0))
  (setq pt2 (trans pt2 1 0))
  (setq pt3 (trans pt3 1 0))
  (mapcar 'ENT:MAKE_LINE (list pt1 pt2 pt3) (list pt2 pt3 pt1))
  (if (setq res (ELL:SteinerEllipse pt1 pt2 pt3))
    (if (setq ent (apply 'ENT:MAKE_ELLIPSE res))
      (progn
        (setq obj (vlax-ename->vla-object ent))
        (vla-copy obj)
        (vla-scaleentity obj (vlax-3d-point (car res)) 2)
      )
    )
    (princ "\nInvalid input!")
  )
  (princ)
)

;;;=============================================================
;;; 功能: 用Gramham扫描法构造凸包                               
;;; 输入: 二维点集                                              
;;; 输出: 点集的凸包                                            
;;;=============================================================
(defun ALG:Graham (pts / Hull pMax P Q)
  (if (< (length pts) 3)                                        ;3点以下
    pts                                                      	;是本集合
    (progn
      (setq pMax (assoc (apply 'max (mapcar 'car pts)) pts))    ;最右边的点
      (setq Pts  (ALG:SortByAngle pts pMax))                    ;分类点集
      (setq Hull (list (cadr Pts) pMax))                        ;开始的两点
      (foreach n (cddr Pts)                                     ;从第3点开始
        (setq Hull (cons n Hull))                               ;把Pi加入到凸集
        (setq P (cadr Hull))                                    ;Pi-1
        (setq Q (caddr Hull))                                   ;Pi-2
        (while (and Q (> (MAT:Det3P n P Q) -1e-6))              ;如果左转
          (setq Hull (cons n (cddr Hull)))                      ;删除Pi-1点
          (setq P (cadr Hull))                                  ;得到新的Pi-1点
          (setq Q (caddr Hull))                                 ;得到新的Pi-2点
        )
      )
      (reverse Hull)                                            ;返回凸集
    )
  )
)

;;;=============================================================
;;; 以某点为基点，按照角度和距离分类点集                        
;;;=============================================================
(defun ALG:SortByAngle (lst p /)
  (vl-sort
    lst
    (function
      (lambda (e1 e2 / a1 a2)
        (if (equal (setq a1 (angle p e1)) (setq a2 (angle p e2)) 1e-8)
          (< (distance p e1) (distance p e2))
          (< a1 a2)
        )
      )
    )
  )
)

;|
1. 四点画水平椭圆。
http://bbs.mjtd.com/thread-91856-1-1.html
2. 已知椭圆的圆心和椭圆上的三点创建一个椭圆。
http://bbs.mjtd.com/thread-91856-1-1.html
3. 已知椭圆的四条切线创建一个水平椭圆。
http://bbs.mjtd.com/forum.php?mod=viewthread&tid=55068
4. 过五点画椭圆，此问题没lisp程序，但有几何解法，参见：
http://bbs.mjtd.com/forum.php?mod=viewthread&tid=77145&page=1#pid410208
|;


;|九 椭圆拟合问题：
http://bbs.mjtd.com/thread-56692-1-1.html
下面的帖子是用圆弧拟合椭圆的讨论：
http://bbs.mjtd.com/thread-86496-3-1.html
|;

;|十一 最似圆椭圆问题
http://bbs.mjtd.com/thread-62903-1-1.html
|;

;;;=============================================================
;;;过四点的最接近圆的椭圆                                       
;;;=============================================================
(defun Ell:NearToCircleEllipse (Pa Pb Pc Pd / CCC InC Pd1 Pe Pe1)
  (setq CCC (Tri:CircumCenter Pa Pb Pc))
  (setq InC (Tri:InCenter Pa Pb Pc))
  (setq Pd1 (Tri:Isogonal-Conjugate-Point-1 Pd Pa Pb InC))
  (setq Pe  (polar Pd1 (+ (angle Pd1 CCC) (/ pi 2)) 1))
  (setq Pe1 (Tri:Isogonal-Conjugate-Point-1 Pe Pa Pb InC))
  (ELL:5P_1 pa pb pc pd pe1)
)

;|十二 椭圆JIG 教程
;;;http://bbs.mjtd.com/forum.php?mod=viewthread&tid=66579
|;

;|十三 椭圆的UCS投影
http://bbs.mjtd.com/forum.php?mod=viewthread&tid=84527
http://www.theswamp.org/index.php?topic=43031.0
|;

;;;=============================================================
;;;把以前用多段线画的椭圆改成真实的椭圆。                       
;;;=============================================================
(defun ELL:GetEllipseFromPoly (poly / pts pt0 pt1 pt2 pt3 cen a b rat maj)
  (defun Get-3dpoly-Coordinates (poly / v e l)
    (setq v (entnext poly))
    (while (/= (cdr (assoc 0 (setq e (entget v)))) "SEQEND")
      (setq l (cons (cdr (assoc 10 e)) l)
            v (entnext v)
      )
    )
    (reverse l)
  )
  (setq pts (Get-3dpoly-Coordinates poly))
  (setq pt0 (car pts))
  (setq pt1 (nth 4 pts))
  (setq pt2 (nth 8 pts))
  (setq pt3 (nth 12 pts))
  (setq cen (mapcar '* (mapcar '+ pt0 pt2) '(0.5 0.5 0.5)))
  (setq a   (distance cen pt0))
  (setq b   (distance cen pt1))
  (setq rat (/ b a))
  (setq maj (mapcar '- pt0 cen))
  (list cen maj rat)
)

;;;=============================================================
;;; Romberg Integration                                         
;;; 龙贝格积分法                                                
;;;=============================================================
(defun Math:Romberg (a b eps / EP H I K M N P Q S X Y Y0)
  (setq h (- b a))
  (setq y nil)
  (setq i 0)
  (repeat 20
    (setq y (cons (cons i 0.0) y))
    (setq i (1+ i))
  )
  (setq y (reverse y))
  (setq y0 (* h (+ (func a) (func b)) 0.5))
  (setq y (cons (cons 0 y0) (cdr y)))
  (setq m  1
        n  1
        ep (1+ eps)
  )
  (while (and (>= ep eps) (<= m 19))
    (setq p 0.0)
    (setq i 0)
    (repeat n
      (setq x (+ a (* (+ i 0.5) h)))
      (setq p (+ p (func x)))
      (setq i (1+ i))
    )
    (setq p (/ (+ (cdar y) (* h p)) 2.0))
    (setq s 1.0)
    (setq k 1)
    (repeat m
      (setq s (+ s s s s))
      (setq q (/ (- (* s p) (cdr (assoc (1- k) y))) (1- s)))
      (setq y (subst (cons (1- k) p) (assoc (1- k) y) y))
      (setq p q)
      (setq k (1+ k))
    )
    (setq ep (abs (- q (cdr (assoc (1- m) y)))))
    (setq m (1+ m))
    (setq y (subst (cons (1- m) q) (assoc (1- m) y) y))
    (setq n (+ n n))
    (setq h (/ h 2.0))
  )
  q
)
;;|
;;;测试椭圆的曲率:
(defun C:GetCurvature (/ sel ent dxf pnt ret)
  (setq sel (nentselp "\n选取椭圆："))
  (if (and (setq ent (car sel))
           (setq dxf (entget ent))
           (= "ELLIPSE" (cdr (assoc 0 dxf)))
      )
    (progn
      (setq pnt (cadr sel))
      (setq ret (ELL:GetCurvature ent (trans pnt 1 0)))
      (princ ret)
      (entmakeX
        (list
          '(0 . "CIRCLE")
          (cons 10 (car ret))
          (cons 40 (cadr ret))
        )
      )
    )
  )
  (princ)
)

;;;测试椭圆弧长和面积样例
(defun C:TestLA (/ e o la lb par1 par2 l1 l2 s1 s2 s3)
  (setq e  (car (entsel "\n请选择椭圆弧: ")))
  (setq o  (vlax-ename->vla-object e))
  (setq la (vla-get-MajorRadius o))
  (setq lb (vla-get-MinorRadius o))
  (setq par1 (vlax-curve-getStartParam e))
  (setq par2 (vlax-curve-getEndParam e))
  (setq l1 (ell:length la lb par1 par2 1e-14))                          ; 用数学计算获得椭圆弧长
  (setq l2 (vlax-curve-getDistAtParam e par2))                          ; 用vlax-curve的函数获得椭圆弧长
  (princ "\n用vlax-curve函数计算的结果和积分法计算相差:")
  (princ (rtos (- l1 l2) 2 20))
  (uti:bench 1001
    (list
      (list 'ell:length la lb par1 par2 1e-8)                           ; 因为椭圆弧长没有数学公式，所以慢了
      (list 'vlax-curve-getDistAtParam e par2)
    )
  )
  (setq s1 (vla-get-area o))
  (setq s2 (ELL:Area la lb par1 par2))
  (setq s3 (vlax-curve-getarea e))
  (princ "\n用vla-get-area函数计算的结果和数学法计算相差:")
  ;(princ (list s1 s2 s3))
  (princ (rtos (- s1 s2) 2 20))
  (uti:bench 1001
    (list
      (list 'ELL:Area la lb par1 par2)                                  ; 居然用这个方法最快??
      (list 'vla-get-area o)
      (list 'vlax-curve-getarea e)
    )
  )
  (princ)
)

;;; 测试椭圆锥的展开
(defun c:ExpandCone(/ h N e d r c m a b l e o)
  (initget 1)
  (setq h (getdist "\n输入椭圆锥高: "))
  (initget 7)
  (setq N (getint "\n等分数:"))
  (prompt "\n选择椭圆:")
  (if (setq s (ssget ":S" '((0 . "ELLIPSE"))))
    (progn
      (setq e (ssname s 0))
      (setq d (entget e))
      (setq r (cdr (assoc 40 d)))
      (setq c (cdr (assoc 10 d)))
      (setq m (cdr (assoc 11 d)))
      (setq a (distance '(0 0 0) m))
      (setq b (* r a))
      (setq l (ELL:ExpandCone a b h n))
      (setq o (vlax-ename->vla-object (Ent:Make_LWPoly l T)))
      (vla-move o (vlax-3d-point '(0 0 0)) (vlax-3d-point c))
      (princ)
    )
  )
)

;;;以下为测试程序
(defun c:testMatrix (/ sel ent mat obj doc)
  (if (setq sel (ssget ":S" '((0 . "ELLIPSE"))))
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (vla-StartUndoMark doc)
      (setq ent (ssname sel 0))
      (setq mat (ELL:TransMatrix ent))
      (setq obj (vlax-ename->vla-object ent))
      (vla-transformby obj (vlax-tmatrix (cadr mat)))
      (command "Select" ent pause)
      (vla-transformby obj (vlax-tmatrix (car mat)))
      (vla-EndUndoMark doc)
      (princ)
    )
  )
)

;;;用于投影椭圆的测试:
(defun C:Projection (/ sel ent normal origin i ret)
  (setq sel (ssget '((0 . "ELLIPSE,ARC,CIRCLE"))))
  (setq normal (trans '(0 0 1) 1 0 T))
  (setq origin (getvar 'ucsorg))
  (if sel
    (repeat (setq i (sslength sel))
      (setq ent (ssname sel (setq i (1- i))))
      (if (setq ret (ELL:Projection ent normal origin))
        (apply 'Ent:Make_ELLIPSE_3D ret)
      )
    )
  )
)

;;; 切线矢量函数测试
(defun C:PointTangentOn ( / CEN DXF ENT MAJ NRM PAR PNT RAT RET SEL)
  (if (setq sel (ssget ":S" '((0 . "ELLIPSE"))))
    (progn
      (setq ent (ssname sel 0))
      (setq dxf (entget ent))
      (setq cen (cdr (assoc 10 dxf)))
      (setq maj (cdr (assoc 11 dxf)))
      (setq rat (cdr (assoc 40 dxf)))
      (setq Nrm (cdr (assoc 210 DXF)))
      (while (setq Pnt (getpoint "\n选取点: ")) 
        (setq Pnt (trans Pnt 1 0))
        (setq pnt (vlax-curve-getclosestpointto ent pnt))
        (setq par (vlax-curve-getParamAtPoint ent pnt))
        (setq ret (ELL:PointTangentOn par cen maj rat Nrm))
        (ent:make_line Pnt (mapcar '+ Pnt ret))
      )
      (princ)
    )
  )
)

;;; 上面是也可以由某个点的参数通过数学方法算出。
;;; 一般来说可以由vlax-curve-getFirstDeriv 算出，面是测试如下:
(defun C:TestDeriv (/ e o p param f1 f2)
  (if (setq e (car (entsel)))
    (progn 
      (setq o (vlax-ename->vla-object e))
      (while (setq p (getpoint "\n测试点:"))
        (setq p (trans p 1 0))
        (setq p (vlax-curve-getclosestpointto e p)) 
        (setq param (vlax-curve-getParamAtPoint e p))
        (setq f1 (vlax-curve-getFirstDeriv e param))
        (setq f2 (vlax-curve-getSecondDeriv e param))
        (Ent:make_line p (mapcar '+ p f1))
        (Ent:make_line p (mapcar '+ p f2))
      )
    )
  )
)
;;; 椭圆的切线函数测试
(defun C:PointTangentTo (/ m ANG n C DXF ENT MAJ PNT RET SEL)
  (if (setq sel (ssget ":S" '((0 . "ELLIPSE"))))
    (progn 
      (setq ent (ssname sel 0))
      (setq dxf (entget ent))
      (setq maj (cdr (assoc 11 dxf)))
      (setq m   (distance '(0 0 0) maj))
      (setq n   (* m (cdr (assoc 40 dxf))))
      (setq c   (cdr (assoc 10 dxf)))
      (setq ang (angle '(0 0 0) maj))
      (while (setq Pnt (getpoint "\n选取点: "))
        (setq Pnt (trans Pnt 1 0))
        (setq ret (ELL:PointTangentTo1 Pnt C m n ang))
	(UTI:BENCH 10
	  (list
	    (list 'ELL:PointTangentTo  Pnt C m n ang)
	    (list 'ELL:PointTangentTo1 Pnt C m n ang)	
	  )
	)  
        (foreach p ret
          (Ent:Make_line p Pnt)
        )
      )
    )
  )
)

;;; 椭圆的与直线相交函数的测试
(defun C:iel (/ p1 p2 sel ent dxf cen maj rat nrm a b ret)
  (initget 1)
  (setq P1 (getpoint "\n选取点1: "))
  (initget 1)
  (setq P2 (getpoint P1 "\n选取点2: "))
  (setq p1 (trans p1 1 0))
  (setq p2 (trans p2 1 0))
  (Ent:Make_Line p1 p2)
  (if (setq sel (ssget ":S" '((0 . "ELLIPSE"))))
    (progn
      (setq ent (ssname sel 0))
      (setq dxf (entget ent))
      (setq cen (cdr (assoc 10 dxf)))
      (setq maj (cdr (assoc 11 dxf)))
      (setq rat (cdr (assoc 40 dxf)))
      (setq Nrm (cdr (assoc 210 DXF)))
      (setq a   (distance '(0 0 0) maj))
      (setq b   (* rat a))
      (uti:bench 10000
        (list
	  (list 'ELL:IntersWithLine2D cen maj rat P1 P2)
	  (list 'ELL:IntersWithLine2D_1 cen maj rat P1 P2)
          ;(list 'ELL:IntersWithLine3D cen maj rat Nrm P1 P2)
	)
      )
      ;(setq ret (ELL:IntersWithLine2D cen maj rat P1 P2))
      ;(setq ret (ELL:IntersWithLine3D cen maj rat Nrm P1 P2))
      (setq ret (ELL:IntersWithLine2D_1 cen maj rat P1 P2))
      (and ret (mapcar 'Ent:Make_Point ret))
    )
  )
)

;;; 测试求极值距离程序
(defun C:GetExtremumDist (/ p sel ent dxf cen maj rat nrm a b)
  (initget 1)
  (setq P (getpoint "\n选取点1: "))
  (setq p (trans p 1 0))
  (Ent:Make_Point p)
  (if (setq sel (ssget ":S" '((0 . "ELLIPSE"))))
    (progn
      (setq ent (ssname sel 0))
      (setq dxf (entget ent))
      (setq cen (cdr (assoc 10 dxf)))
      (setq maj (cdr (assoc 11 dxf)))
      (setq rat (cdr (assoc 40 dxf)))
      (setq Nrm (cdr (assoc 210 DXF)))
      (setq a   (distance '(0 0 0) maj))
      (setq b   (* rat a))
      (UTI:bench
        10001
        (list
	  (list 'ELL:GetExtremumDist1 a b P)
          (list 'ELL:GetExtremumDist a b P)
          (list 'vlax-curve-getclosestpointto ent p)
        )
      )
      ;(mapcar 'Ent:Make_Point (ELL:GetExtremumDist1 a b P))
      (mapcar 'Ent:Make_Point (ELL:GetExtremumDist_3D cen maj rat Nrm P))
    )
  )
)

;;;测试椭圆的包围盒程序
;;;测试椭圆的最小包围盒程序
(defun c:test3 (/ i sel ent)
  (setq i 0)
  (if (setq sel (ssget '((0 . "ELLIPSE"))))              
    (repeat (sslength sel)
      (setq ent (ssname sel i))
      (Ent:Make_Poly (ELL:GetBoundingBox ent) T)
      (Ent:Make_Poly (ELL:getMinBox ent) T)
      (setq i (1+ i))
    )
  )
  (uti:bench
    10001
    (list
      (list 'Ell:getMinBox ent)
      (list 'Ell:GetMinBox1 Ent)
    )
  )
  (princ)
)

;;;测试共轭半径的椭圆
(defun C:JJ (/ c p q ret ent obj)
  (initget 1)
  (setq C (getpoint "\n中心"))
  (initget 1)
  (setq P (getpoint C "\n半径1"))
  (initget 1)
  (setq Q (getpoint C "\n半径2"))
  (foreach n (list C P Q)
    (Ent:make_Point (trans n 1 0))
  )
  (uti:bench 1000
    (list
      (list 'ELL:2J_1 C P Q)
      (list 'ELL:2J C P Q)
    )
  )
  (if (setq ret (ELL:2J C P Q))
    (progn
      (setq ent (apply 'Ent:Make_Ellipse ret))
      (setq obj (vlax-ename->vla-object ent))
      (vla-transformby obj (vlax-tmatrix (mat:u2w)))
    )
  )
)

(defun C:PX (/ M N P Q E)
  (initget 1)
  (setq M (getpoint "\n端点1"))
  (initget 1)
  (setq N (getpoint M "\n端点2"))
  (initget 1)
  (setq P (getpoint "\n点："))
  (setq M (trans M 1 0))
  (setq N (trans N 1 0))
  (setq P (trans P 1 0))
  (mapcar 'Ent:make_Point (list M N P))
  (setq E (ELL:XP M N P))
  (and E (apply 'Ent:Make_Ellipse E))
  (princ)
)

(defun C:XT (/ M N P Q E)
  (initget 1)
  (setq M (getpoint "\n端点1"))
  (initget 1)
  (setq N (getpoint M "\n端点2"))
  (initget 1)
  (setq P (getpoint "\n点1："))
  (initget 1)
  (setq Q (getpoint P "\n点2："))
  (setq M (trans M 1 0))
  (setq N (trans N 1 0))
  (setq P (trans P 1 0))
  (setq Q (trans Q 1 0))
  (mapcar 'Ent:make_Point (list M N P Q))
  (Ent:make_line P Q)
  (MISC:test 1001 '((ELL:XT M N P Q) (ELL:XT_1 M N P Q)))
  (setq E (ELL:XT M N P Q))
  (and E (apply 'Ent:Make_Ellipse E))
  (setq E (ELL:XT_1 M N P Q))
  (and E (apply 'Ent:Make_Ellipse E))
  (princ)
)

;;;测试用多段线画的椭圆改成真实的椭圆
(defun C:GetEllipseFromPoly(/ pl ret)
  (setq pl  (car (entsel)))
  (setq ret (ELL:GetEllipseFromPoly pl))
  (entmakeX
    (list
      '(0 . "ELLIPSE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbEllipse")
      (cons 10 (car ret))
      (cons 11 (cadr ret))
      (cons 40 (caddr ret))
    )
  )
  (princ)
)

(defun c:TP2D ()
  (initget 1)
  (setq J (getpoint "\n端点1"))
  (initget 1)
  (setq K (getpoint J "\n端点2"))
  (initget 1)
  (setq M (getpoint "\n端点1"))
  (initget 1)
  (setq N (getpoint M "\n端点2"))
  (initget 1)
  (setq P (getpoint "\n切点："))
  (initget 1)
  (setq Q (getpoint P "\n另一点："))
  (setq J (trans J 1 0))
  (setq K (trans K 1 0))
  (setq M (trans M 1 0))
  (setq N (trans N 1 0))
  (setq P (trans P 1 0))
  (setq Q (trans Q 1 0))
  (mapcar 'Ent:make_Point (list J K M N P Q))
  (Ent:make_line P Q)
  (Ent:make_line J K)
  (Ent:make_line M N) 
  (setq E (ELL:TP2D J K M N P Q))
  (and E (apply 'Ent:Make_Ellipse E))
  (princ)
)
(defun C:C2D2P  ()
  (initget 1)
  (setq C (getpoint "\n中心: "))
  (initget 1)
  (setq M (getpoint C "\n端点1"))
  (initget 1)
  (setq N (getpoint C "\n端点2"))
  (initget 1)
  (setq P (getpoint "\n一点："))
  (initget 1)
  (setq Q (getpoint P "\n另一点："))
  (setq C (trans C 1 0))
  (setq M (trans M 1 0))
  (setq N (trans N 1 0))
  (setq P (trans P 1 0))
  (setq Q (trans Q 1 0))
  (mapcar 'Ent:make_Point (list C M N P Q)) 
  (setq K (ELL:C2D2P C M N P Q))
  (and K (apply 'Ent:Make_Ellipse K))
  (princ)
)
(defun C:CM2T ()
  (initget 1)
  (setq C (getpoint "\n中心: "))
  (initget 1)
  (setq M (getpoint C "\n主要方向: "))
  (initget 1)
  (setq P (getpoint "\n交点: "))
  (initget 1)
  (setq D (getpoint P "\n直线1："))
  (initget 1)
  (setq E (getpoint P "\n直线2："))
  (setq C (trans C 1 0))
  (setq M (trans M 1 0))
  (setq P (trans P 1 0))
  (setq D (trans D 1 0))
  (setq E (trans E 1 0))
  (mapcar 'Ent:make_Point (list C M P D E))
  (Ent:make_line P D)
  (Ent:make_line P E)
  (Ent:make_line C M)
  (setq S (ELL:CM2T C M P D E))
  (and S (apply 'Ent:Make_Ellipse S))
  (princ)
)
(defun C:CPT2D ()
  (initget 1)
  (setq C (getpoint "\n中心: "))
  (initget 1)
  (setq D (getpoint C "\n共轭方向1: "))
  (initget 1)
  (setq E (getpoint C "\n共轭方向2: "))
  (initget 1)
  (setq P (getpoint "\n椭圆上一点: "))
  (initget 1)
  (setq M (getpoint  "\n直线端点1: "))
  (initget 1)
  (setq N (getpoint M "\n直线端点2: "))
　　
  (setq C (trans C 1 0))
  (setq D (trans D 1 0))
  (setq E (trans E 1 0))
  (setq P (trans P 1 0))
  (setq M (trans M 1 0))
  (setq N (trans N 1 0))
  
  (mapcar 'Ent:make_Point (list C D E P M N))
  (Ent:make_line M N)
  (Ent:make_line C E)
  (Ent:make_line C D)
  (setq S (ELL:CPT2D C D E P M N))
  (foreach x S (apply 'Ent:Make_Ellipse x))
  (princ)
)

(defun C:CP2T ()
  (initget 1)
  (setq C (getpoint "\n中心: "))
  (initget 1)
  (setq P (getpoint C "\n椭圆上一点: "))
  (initget 1)
  (setq I (getpoint "\n切线交点: "))
  (initget 1)
  (setq M (getpoint I "\n切线1: "))
  (initget 1)
  (setq N (getpoint I "\n切线2: "))
　　
  (setq C (trans C 1 0))
  (setq P (trans P 1 0))
  (setq I (trans I 1 0))
  (setq M (trans M 1 0))
  (setq N (trans N 1 0))
  
  (mapcar 'Ent:make_Point (list C P I M N))
  (Ent:make_line I M)
  (Ent:make_line I N)
  (Ent:make_line C P)
  (setq S (ELL:CP2T C P I M N))
  (foreach x S (apply 'Ent:Make_Ellipse x))
  (princ)
)

(defun C:CT2P()
  (initget 1)
  (setq C (getpoint "\n中心: "))
  (initget 1)
  (setq A (getpoint C "\n点1: "))
  (initget 1)
  (setq B (getpoint C "\n点2: "))
  (initget 1)
  (setq P (getpoint "\n切线点1: "))　　
  (initget 1)
  (setq Q (getpoint P "\n切线点2: "))　　
  (setq C (trans C 1 0))
  (setq A (trans A 1 0))
  (setq B (trans B 1 0))
  (setq P (trans P 1 0))
  (setq Q (trans Q 1 0))
  (mapcar 'Ent:make_Point (list C A B P Q))
  (Ent:make_line P Q)
  (foreach x (ELL:CT2P C A B P Q)
    (apply 'Ent:Make_Ellipse x)
  )
  (princ)
)

(defun C:testC3P (/ pt1 pt2 pt3 pt4 mod res)
  (initget "C P") 
  (setq mod (getkword "\nDraw an ellipse by [C3P(C)/4P(P)] <C>:"))
  (and (/= mod "P") (setq mod nil))
 
  (setq pt1 (getpoint "\nEnter 1st point: "))
  (setq pt2 (getpoint "\nEnter 2nd point: "))
  (setq pt3 (getpoint "\nEnter 3rd point: "))
  (if mod 
    (setq pt4 (getpoint "\nEnter 4th point: "))
    (setq pt4 (getpoint "\nEnter the center: "))
  )
  (if (and pt4 pt1 pt2 pt3)
    (progn
      (setq pt1 (trans pt1 1 0))
      (setq pt2 (trans pt2 1 0))
      (setq pt3 (trans pt3 1 0))
      (setq pt4 (trans pt4 1 0))
      
      (mapcar 'ENT:Make_Point (list pt4 pt1 pt2 pt3))
      (if mod
        (setq res (ELL:4P pt4 pt1 pt2 pt3))
        (progn
          ;(MISC:test 10001 '((ELL:C3p pt4 pt1 pt2 pt3) (ELL:C3P_1 pt4 pt1 pt2 pt3)))
          (setq res (ELL:C3P pt4 pt1 pt2 pt3))
        )
      )
      (if res
        (if (caddr res)
          (apply 'ENT:Make_Ellipse res)
          (apply 'ENT:Make_Circle res)
        )
        (princ "\nInvalid input or can't construct the ellipse!")
      )
    )
  )
  (princ)
)

(defun C:C3T ()
  (initget 1)
  (setq C (getpoint "\n中心: "))
  (initget 1)
  (setq D (getpoint "\n切线1起点: "))
  (initget 1)
  (setq E (getpoint D "\n切线1终点: "))
  (initget 1)
  (setq F (getpoint "\n切线2起点: "))　　
  (initget 1)
  (setq G (getpoint F "\n切线2终点: "))
  (initget 1)
  (setq H (getpoint "\n切线3起点: "))
  (initget 1)
  (setq I (getpoint H"\n切线3终点: "))　　

  (foreach p '(C D E F G H)
    (set p (trans (VL-SYMBOL-VALUE p) 1 0))
  )
  (mapcar 'Ent:make_Point (list C D E F G H I))
  (mapcar 'ent:make_line (list D F H) (list E G I))
  (if (and
	(setq J (inters D E F G nil))
	(setq K (inters F G H I nil))
	(setq M (inters H I D E nil))
	(setq S (ELL:C3T C J K M))
      )
    (apply 'Ent:Make_Ellipse S)
  )
  (princ)
)

(defun C:C3P ()
  (initget 1)
  (setq C (getpoint "\n中心: "))
  (initget 1)
  (setq D (getpoint "\n点1: "))
  (initget 1)
  (setq E (getpoint "\n点2: "))
  (initget 1)
  (setq F (getpoint "\n点3: "))　　
  
  (setq C (trans C 1 0))
  (setq D (trans D 1 0))
  (setq E (trans E 1 0))
  (setq F (trans F 1 0))
  
  (mapcar 'Ent:make_Point (list C D E F))
  (Ent:make_line D E)
  (Ent:make_line D F)
  (Ent:make_line E F)
  (setq S (ELL:C3T C D E F))
  (uti:bench
    10001
    (list
      (list 'ELL:C3P_2 C D E F)
      (list 'ELL:C3P C D E F)
      (list 'ELL:C3P_1 C D E F)
    )
  )
  
  (setq s (ELL:C3P C D E F))
  (and S (apply 'Ent:Make_Ellipse S))
  (setq S (ELL:C3P_1 C D E F))
  (and S (apply 'Ent:Make_Ellipse S))
  (setq S (ELL:C3P_2 C D E F))
  (and S (apply 'Ent:Make_Ellipse S))
  
  (princ)
)
(defun C:P2F ()
  (initget 1)
  (setq E (getpoint "\n焦点1: "))
  (initget 1)
  (setq F (getpoint E "\n焦点2: "))
  (initget 1)
  (setq P (getpoint "\n一点: "))
  
  (setq E (trans E 1 0))
  (setq F (trans F 1 0))
  (setq P (trans P 1 0))
  
  (mapcar 'Ent:make_Point (list E F P))
  (setq s (ELL:P2F E F P))
  (and S (apply 'Ent:Make_Ellipse S))
  (princ)
)
(defun C:T2F()
  (initget 1)
  (setq E (getpoint "\n焦点1: "))
  (initget 1)
  (setq F (getpoint E "\n焦点2: "))
  (initget 1)
  (setq P (getpoint "\n一点: "))
  (initget 1)
  (setq Q (getpoint P "\n另一点: "))
  
  (setq E (trans E 1 0))
  (setq F (trans F 1 0))
  (setq P (trans P 1 0))
  (setq Q (trans Q 1 0))
  
  (mapcar 'Ent:make_Point (list E F P Q))
  ;(apply 'Ent:Make_Ellipse (ELL:P2F E F P))
  (setq S (ELL:T2F E F P Q))
  (and S (apply 'Ent:Make_Ellipse))
  (princ)
)

(defun C:FPT ()
  (initget 1)
  (setq F (getpoint "\n焦点: "))
  (initget 1)
  (setq A (getpoint  "\n点: "))
  (initget 1)
  (setq P (getpoint "\n切点: "))
  (initget 1)
  (setq Q (getpoint P "\n另一点: "))
  
  (setq F (trans F 1 0))
  (setq A (trans A 1 0))
  (setq P (trans P 1 0))
  (setq Q (trans Q 1 0))

  (mapcar 'Ent:make_Point (list F A P Q))
  (Ent:Make_line P Q)
  (setq S (ELL:FPT F A P Q))
  (and S (apply 'Ent:Make_Ellipse s))
  (princ)
)

(defun C:FT2P ()
  (initget 1)
  (setq F (getpoint "\n焦点: "))
  (initget 1)
  (setq M (getpoint "\n切线点1: "))
  (initget 1)
  (setq N (getpoint M "\n切线点2: "))
  (initget 1)
  (setq P (getpoint "\n点1: "))
  (initget 1)
  (setq Q (getpoint "\n点2: "))
  
  (setq F (trans F 1 0))
  (setq M (trans M 1 0))
  (setq N (trans N 1 0))
  (setq P (trans P 1 0))
  (setq Q (trans Q 1 0))

  (mapcar 'Ent:make_Point (list F M N P Q))
  (Ent:Make_line M N)
  (setq S (ELL:FT2P F M N P Q))
  (foreach el S
    (apply 'Ent:Make_Ellipse el)
  )
  (princ)
)

(defun C:FP2T ()
  (initget 1)
  (setq F (getpoint "\n焦点: "))
  (initget 1)
  (setq P (getpoint "\n点: "))
  
  (initget 1)
  (setq J (getpoint "\n切线点1: ")) 
  (initget 1)
  (setq K (getpoint J "\n切线点2: "))
  
  (initget 1)
  (setq M (getpoint "\n切线点1: ")) 
  (initget 1)
  (setq N (getpoint M "\n切线点2: "))

  (setq F (trans F 1 0))
  (setq P (trans P 1 0))
  (setq J (trans J 1 0))
  (setq K (trans K 1 0))
  (setq M (trans M 1 0))
  (setq N (trans N 1 0))
 
  (mapcar 'Ent:make_Point (list F P J K M N))
  (Ent:Make_line M N)
  (Ent:Make_line J K)
  (setq S (ELL:FP2T F P J K M N))
  (foreach el S
    (apply 'Ent:Make_Ellipse el)
  )
  (princ)
)


(defun C:F3T()
  (initget 1)
  (setq F (getpoint "\n焦点: "))
  (initget 1)
  (setq P (getpoint "\n切线点1: "))
  (initget 1)
  (setq Q (getpoint P "\n切线点2: ")) 
  (initget 1)
  (setq J (getpoint "\n切线点1: ")) 
  (initget 1)
  (setq K (getpoint J "\n切线点2: "))
  (initget 1)
  (setq M (getpoint "\n切线点1: ")) 
  (initget 1)
  (setq N (getpoint M "\n切线点2: "))

  (setq F (trans F 1 0))
  (setq P (trans P 1 0))
  (setq Q (trans Q 1 0))
  (setq J (trans J 1 0))
  (setq K (trans K 1 0))
  (setq M (trans M 1 0))
  (setq N (trans N 1 0))
 
  (Ent:make_Point F)
  (Ent:Make_line P Q)
  (Ent:Make_line M N)
  (Ent:Make_line J K)
  (setq S (ELL:F3T F P Q J K M N))
  (and S (apply 'Ent:Make_Ellipse s))
  (princ)
)

(defun C:F3P ()
  (initget 1)
  (setq F (getpoint "\n焦点: "))
  (initget 1)
  (setq I (getpoint "\n点1: "))
  (initget 1)
  (setq J (getpoint "\n点2: ")) 
  (initget 1)
  (setq K (getpoint "\n点3: "))
  
  (setq F (trans F 1 0))
  (setq I (trans I 1 0))
  (setq J (trans J 1 0))
  (setq K (trans K 1 0))

  (mapcar 'Ent:make_Point (list F I J K))
  (setq S (ELL:F3P F I J K))
  (and S (apply 'Ent:Make_Ellipse s))
  (princ)
)
(defun C:P2T ()
  (initget 1)
  (setq V (getpoint "\n交点: "))
  (initget 1)
  (setq Q (getpoint V "\n切点1: "))
  (initget 1)
  (setq R (getpoint V "\n切点2: ")) 
  (initget 1)
  (setq P (getpoint "\n点: "))
  
  (setq V (trans V 1 0))
  (setq Q (trans Q 1 0))
  (setq R (trans R 1 0))
  (setq P (trans P 1 0))

  (mapcar 'Ent:make_Point (list V Q R P))
  (setq S (ELL:P2T V Q R P))
  (and S (apply 'Ent:Make_Ellipse s))
  (princ)
)

(defun C:2T3P()
  (initget 1)
  (setq en1 (car (entsel "\n选择第一条线:")))
  (initget 1)
  (setq en2 (car (entsel "\n选择第二条线:")))
  (initget 1)
  (setq A (getpoint "\n输入第一点:"))
  (initget 1)
  (setq B (getpoint "\n输入第二点:"))
  (initget 1)
  (setq C (getpoint "\n输入第三点:"))
  (if (and en1 en2 A B C)
    (progn 
      (setq dxf1 (entget en1))
      (setq dxf2 (entget en2))
      (setq t1 (list (cdr (assoc 10 dxf1)) (cdr (assoc 11 dxf1))))
      (setq t2 (list (cdr (assoc 10 dxf2)) (cdr (assoc 11 dxf2))))
      (mapcar 'ENT:MAKE_POINT (list A B C))
      (foreach s (ELL:2T3P t1 t2 A B C)
        (and s (apply 'ENT:MAKE_ELLIPSE s))
      )
    )
  )
)

(defun C:2P3T()
  (setq en1 (car (entsel "\n选择第一条线:")))
  (setq en2 (car (entsel "\n选择第二条线:")))
  (setq en3 (car (entsel "\n选择第三条线:")))
  (initget 1)
  (setq A (getpoint "\n输入第一点:"))
  (initget 1)
  (setq B (getpoint "\n输入第二点:"))
  (if (and en1 en2 en3 A B)
    (progn 
      (setq dxf1 (entget en1))
      (setq dxf2 (entget en2))
      (setq dxf3 (entget en3))
      (setq t1 (list (cdr (assoc 10 dxf1)) (cdr (assoc 11 dxf1))))
      (setq t2 (list (cdr (assoc 10 dxf2)) (cdr (assoc 11 dxf2))))
      (setq t3 (list (cdr (assoc 10 dxf3)) (cdr (assoc 11 dxf3))))
      (mapcar 'ENT:MAKE_POINT (list A B))
      (foreach s (ELL:2P3T t1 t2 t3 A B)
        (and s (apply 'ENT:MAKE_ELLIPSE s))
      )
    )
  )
)
(defun c:5T (/ p1 p2 p3 p4 p5 ret)
  (initget 1)
  (setq p1 (getpoint "\n输入第一点:"))
  (initget 1)
  (setq p2 (getpoint "\n输入第二点:"))
  (initget 1)
  (setq p3 (getpoint "\n输入第三点:"))
  (initget 1)
  (setq p4 (getpoint "\n输入第四点:"))
  (initget 1)
  (setq p5 (getpoint "\n输入第五点:"))
  (setq ret (ELL:5T p1 p2 p3 p4 p5))
  (and ret (apply 'Ent:Make_Ellipse ret))
  (Ent:Make_Poly (list p1 p2 p3 p4 p5))
  (princ)
)

(defun C:5P (/ p1 p2 p3 p4 p5 ret)
  (initget 1)
  (setq p1 (getpoint "\n输入第一点:"))
  (initget 1)
  (setq p2 (getpoint "\n输入第二点:"))
  (initget 1)
  (setq p3 (getpoint "\n输入第三点:"))
  (initget 1)
  (setq p4 (getpoint "\n输入第四点:"))
  (initget 1)
  (setq p5 (getpoint "\n输入第五点:"))
  (uti:bench
    1
    (list
      (list 'ELL:5P p1 p2 p3 p4 p5)
      ;(list 'ELL:5P_1 p1 p2 p3 p4 p5)
    )
  )
  (setq ret (ELL:5P p1 p2 p3 p4 p5))
  (and ret (apply 'Ent:Make_Ellipse ret))
  ;(setq ret (ELL:5P_1 p1 p2 p3 p4 p5))
  ;(and ret (apply 'Ent:Make_Ellipse ret))
  (Ent:Make_LWPoly (list p1 p2 p3 p4 p5) T)
  (princ)
)

(defun C:P4T (/ p1 p2 p3 p4 p5 ret)
  (initget 1)
  (setq p1 (getpoint "\n输入第一点:"))
  (initget 1)
  (setq p2 (getpoint "\n输入第二点:"))
  (initget 1)
  (setq p3 (getpoint "\n输入第三点:"))
  (initget 1)
  (setq p4 (getpoint "\n输入第四点:"))
  (initget 1)
  (setq p5 (getpoint "\n输入第五点:"))
  (setq ret (ELL:P4T p1 p2 p3 p4 p5))
  (foreach n  ret
       (apply 'Ent:Make_Ellipse n))
  (Ent:Make_Poly (list p1 p2 p3 p4))
  (ent:make_point p5)
  (princ)
)

(defun C:T4P (/ p1 p2 p3 p4 p5 p6 ret)
  (initget 1)
  (setq p1 (getpoint "\n输入第一点:"))
  (initget 1)
  (setq p2 (getpoint "\n输入第二点:"))
  (initget 1)
  (setq p3 (getpoint "\n输入第三点:"))
  (initget 1)
  (setq p4 (getpoint "\n输入第四点:"))
  (initget 1)
  (setq p5 (getpoint "\n输入第五点:"))
   (initget 1)
  (setq p6 (getpoint P5 "\n输入第六点:"))
  (setq ret (ELL:T4P p1 p2 p3 p4 p5 p6))
  (foreach n  ret
    (apply 'Ent:Make_Ellipse n)
  )
  (mapcar 'Ent:Make_Point (list p1 p2 p3 p4))
  (ent:make_line p5 p6)
  (princ)
)

(defun c:eee()
  (setq cen (getpoint "\n中心:"))
  (setq ang (getangle cen "\n角度:"))
  (setq a   (getdist Cen "\n长轴:"))
  (setq b   (getdist cen "\n短轴:"))
  (ent:make_ellipse cen a b ang)
)
;semiminor axis  semimajor axis
;;;测试程序
(defun c:test7 (/ pa pb pc pd ret)
  (initget 1)
  (setq pa (getpoint "\n输入第一点:"))
  (initget 1)
  (setq pb (getpoint "\n输入第二点:"))
  (initget 1)
  (setq pc (getpoint "\n输入第三点:"))
  (initget 1)
  (setq pd (getpoint "\n输入映射点:"))
  
  (Ent:Make_Poly (list pa pb pc pd) T)
  (setq ret (Ell:NearToCircleEllipse Pd pa pb pc))
  (and ret (apply 'Ent:Make_Ellipse ret))
  ;;(setq InC (Tri:InCenter pa pb pc))
  ;;(setq CCC (Tri:CircumCenter pa pb pc))
  ;;(setq Pd1 (Tri:Isogonal-Conjugate-Point Pd Pa Pb Pc))
  ;;(mapcar 'Ent:Make_Point (list pa pb pc pd Inc CCC Pd1))
  (princ)
)

;;;测试凸包
(defun C:hull (/ fil sel t0 ptlist pp 2Pi)
  (setq fil '((0 . "POINT,LINE,POLYLINE,LWPOLYLINE")))
  (setq sel (ssget fil))					;选择点集
  (setq ptlist (getpt sel))     				;构造点集
  (setq t0 (getvar "TDUSRTIMER"))       			;开始计时
  (setq pp (ALG:Graham ptlist))        				;求凸包
  (princ "\n用时")
  (princ (* (- (getvar "TDUSRTIMER") t0) 86400)) 		;结束计时
  (princ "秒")
  (if (null pp)
    (alert "点的有效数目太小，请重新输入!")
    (ent:make_poly PP T)
  )
  (gc)
  (princ)
)

;;;测试椭圆相交
(defun c:ii ()
  (setq e1 (car (entsel "\n选择椭圆1: ")))
  (setq s1 (ELL:GetParameters e1))
  (setq e2 (car (entsel "\n选择椭圆2: ")))
  (setq s2 (ELL:GetParameters e2))
  (foreach p (ELL:IntersWithEllipse S1 S2)
    (ENT:Make_point p)
    (princ "\nX坐标是: ")
    (princ (rtos (car p) 2 8))
    (princ ",")
    (princ (rtos (cadr p) 2 8))
  )
  (princ)
)
;;;测试椭圆和椭圆的公切线函数
(defun c:ttq ()
  (setq e1 (car (entsel "\n选择椭圆1: ")))
  (setq s1 (ELL:GetParameters e1))
  (setq e2 (car (entsel "\n选择椭圆2: ")))
  (setq s2 (ELL:GetParameters e2))

  (uti:bench 1000
    (list
      (list 'ELL:GetTangents  S1 S2)
      (list 'ELL:GetTangents2 S1 S2)
    )
  )
  (foreach p (ELL:GetTangents2 S1 S2)
    (apply 'ENT:Make_line p)
  )
  (princ)
)

;;;测试椭圆和圆的公切线函数
(defun c:tyq ()
  (setq e1 (car (entsel "\n选择椭圆: ")))
  (setq s1 (ELL:GetParameters e1))
  (setq m  (cadr S1))
  (setq n  (caddr S1))
  (setq e2 (car (entsel "\n选择圆: ")))
  (setq d2 (entget e2))
  (setq c2 (cdr (assoc 10 d2)))
  (setq r  (cdr (assoc 40 d2)))
  (foreach n (ELL:TangentToCircle m n C2 R)
    (apply 'Ent:make_line n)
  )
  (princ)	   
)

;;;测试求椭圆与椭圆交点函数
(defun c:ttj()
  (setq e1 (car (entsel "\n选取椭圆1:")))
  (setq e2 (car (entsel "\n选取椭圆2:")))
  (setq D1 (entget e1))
  (setq D2 (entget e2))
  (setq c1 (cdr (assoc 10 D1)))
  (setq c2 (cdr (assoc 10 D2)))
  (setq x1 (cdr (assoc 11 D1)))
  (setq x2 (cdr (assoc 11 D2)))
  (setq r1 (cdr (assoc 40 D1)))
  (setq r2 (cdr (assoc 40 D2)))
  (setq m1 (distance '(0 0) x1))
  (setq m2 (distance '(0 0) x2))
  (setq n1 (* r1 m1))
  (setq n2 (* r2 m2))
  (setq x0 (car c2))
  (setq y0 (cadr c2))
  (setq an (angle '(0 0) x2))
  (setq xx (ELL:GetInters2 m1 n1 x0 y0 m2 n2 an))
  (foreach s xx
    (if (and (equal (cadr s) 0 1e-6) (<= (abs (car s)) 1))    
      (progn
	(setq x (car s))
	(setq y (sqrt (- 1 (* x x))))
	(setq zx (+ x0 (* m2 x)))
	(setq zy (+ y0 (* n2 y)))
	(princ "\n所求交点是: ")
	(princ (list zx zy))
      )
    )
  )
  (princ)	
)

;;;测试方程系数
(defun c:tysx ()
  (setq ent (car (entsel "\n选择椭圆: ")))
  (setq dxf (entget ent))
  (setq cen (cdr (assoc 10 dxf)))
  (setq maj (cdr (assoc 11 dxf)))
  (setq rat (cdr (assoc 40 dxf)))
  (setq m (distance '(0 0 0) maj))
  (setq n (* rat m))
  (setq an (angle '(0 0 0) maj))
  (setq x0 (car cen))
  (setq y0 (cadr cen))
  (setq xs (ELL:GetFactors x0 y0 m n an))
  (setq ret (apply 'ELL:FromEquation xs))
  (setq vvv (sin (- (cadddr ret) an)))
  (if
    (and
      (equal (caar ret) x0 1e-6)
      (equal (cadar ret) y0 1e-6)
      (equal (cadr ret) m 1e-6)
      (equal (caddr ret) n 1e-6)
      (equal vvv 0 1e-6)
    )
    (princ "\n验证成功!")
    (princ "\n算法有误?")
  )
  (princ ret)
  (princ)
)
;;|;