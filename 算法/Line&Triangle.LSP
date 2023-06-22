;|*************************************************************;
软件作者: Highflybird                                          ;
软件用途: 为AutoCAD 的LISP定制的一些算法和函数(点，线，面部分) ;
日期地点: 2012.12.12 深圳                                      ;
修改时间: 2016.03.26 深圳                                      ;
程序语言: AutoLISP,Visual LISP                                 ;
版本号:   Ver. 1.0.16.0326                                     ;
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

;;;************************************************************;
;;; 几何部分                                                    
;;;************************************************************;

;;;-------------------------------------------------------------
;;; 功能: 比例缩放点                                            
;;; 输入: 要缩放的点pt,基点pBase，缩放因子k                     
;;; 输出: 缩放后的点位置                                        
;;;-------------------------------------------------------------
(defun GEO:Scale (Pt pBase k)
  (mapcar (function (lambda (u v) (+ u (* k (- v u))))) pBase Pt)
)

;;;-------------------------------------------------------------
;;; 功能: 比例缩放点2倍                                         
;;; 输入: 要缩放的点pt,基点pBase                                
;;; 输出: 缩放后的点位置                                        
;;;-------------------------------------------------------------
(defun GEO:Scale2 (Pt pBase)
  (mapcar (function (lambda (u v) (+ v (- v u)))) pBase Pt)
)

;;;-------------------------------------------------------------
;;; 功能: 两点之中点                                            
;;; 输入: 两点p1,P2                                             
;;; 输出: 中点位置                                              
;;;-------------------------------------------------------------
(defun GEO:Midpoint (p1 p2)
  (mapcar (function (lambda (e1 e2) (* (+ e1 e2) 0.5))) p1 p2)
)

;;;-------------------------------------------------------------
;;; 功能: 两个距离之中位值                                      
;;; 输入: 共线三点O,P,Q                                         
;;; 输出: 中位点位置                                            
;;;-------------------------------------------------------------
(defun GEO:MeanPoint (O P Q)
  (polar O (angle O P) (sqrt (* (distance O P) (distance O Q))))
)


;;;-------------------------------------------------------------
;;; 功能: 定比点P,使得P1P / PP2 = k （此函数于三维）            
;;; 输入: 两点p1,P2和比例系数k                                  
;;; 输出: 定比点位置                                            
;;;-------------------------------------------------------------
(defun GEO:Proportion (p1 p2 k)
  (if (/= k -1)
    (mapcar (function (lambda (x1 x2) (/ (+ x1 (* k x2)) (+ 1.0 k)))) p1 p2)
  )
)

;;;-------------------------------------------------------------
;;; 功能: 給定两对共轭点 A,B 和 P,Q (共线) 求中心点和等幂点。   
;;; 输入: 两对共轭点A,B和P,Q (共线)                             
;;; 输出: 中心点和等幂点(center and foci of involution)         
;;;-------------------------------------------------------------
(defun GEO:Cen_Foci (A B P Q  / M L an x x1 x2 cen len)
  (setq L  (distance A B))
  (setq an (angle A B))
  (setq x1 (distance A P))
  (setq x2 (distance A Q))
  
  (if (not (equal (angle A P) an 1e-6))
    (setq x1 (- x1))
  )
  (if (not (equal (angle A Q) an 1e-6))
    (setq x2 (- x2))
  )
  (if (equal (setq x (+ x1 x2)) l 1e-14)
    (progn
      (setq cen (polar A an 1e600))
      (setq mid (GEO:Midpoin A B)) 
      (list cen mid (polar A an 2e600))
    )
    (progn
      (setq cen (polar A an (/ (* x1 x2) (+ x1 x2 (- l)))))
      (setq len (sqrt (* (distance cen A) (distance cen B))))
      (list cen (polar cen an len) (polar cen an (- len)))
    )
  )
)

;;;-------------------------------------------------------------
;;; 功能: 两点法旋转某个点90度                                  
;;; 输入: 基点，矢量的第一点P1，第二点P1                        
;;; 输出: 旋转90度后点位置                                      
;;;-------------------------------------------------------------
(defun GEO:Rot90 (ptBase P1 P2)
  (mapcar '+ ptBase (MAT:Rot90 (mapcar '- p2 p1)))
)

;;;-------------------------------------------------------------
;;; 功能: 以基点旋转一点到指定的角度                     	
;;; 输入: 要旋转的点Pt，基点和旋转角度                  	
;;; 输出: 旋转后点位置                                   	
;;;-------------------------------------------------------------
(defun GEO:Rot2D (Pt PtBase Ang)  
  (mapcar '+ PtBase (MAT:Rot2D (mapcar '- Pt PtBase) Ang))
)

;;;-------------------------------------------------------------
;;; 功能: 以基点和角度镜像某点                                  
;;; 输入: 要镜像的点Pt，基点和镜像轴角度                 	
;;; 输出: 镜像点位置                                     	
;;; 说明: 只适用与二维情况下，但速度最快                 	
;;;-------------------------------------------------------------
(defun GEO:Mirror2D (Pt pBase Ang)
  (polar pBase (+ ang (- ang (angle pbase pt))) (distance pt pBase))
)

;;;-------------------------------------------------------------
;;; 功能: 镜像点（可以用于3D情况）                       	
;;; 输入: 要镜像的点Pt，镜像轴第一点和第二点             	
;;; 输出: 镜像点位置                                     	
;;; 说明: 可以适用于三维情况                             	
;;;-------------------------------------------------------------
(defun GEO:Mirror3D (Pt P1 P2 / v1 v2 dd P3 P4)
  (if (equal P1 P2 1e-8)
    (GEO:Scale2 P1 Pt)
    (setq v1 (mapcar '- Pt P1)
          v2 (mapcar '- P2 P1)
          dd (MAT:Dot v2 v2)
          P3 (GEO:Scale P2 P1 (/ (MAT:Dot v1 v2) dd))
          P4 (GEO:Scale2 P3 Pt)
    )    
  ) 
)

;;;-------------------------------------------------------------
;;; 功能: 镜像点（另一方法，相当于用 Mirror命令的结果）  	
;;; 输入: 要镜像的点Pt，镜像轴第一点和第二点             	
;;; 输出: 镜像点位置                                     	
;;;-------------------------------------------------------------
(defun GEO:Mirror2D-1 (Pt P1 P2 / v p)
  (setq v (mapcar '- p2 p1))
  (setq p (trans (mapcar '- Pt P1) 0 v))
  (setq p (list (- (car p)) (cadr p) (caddr p)))
  (mapcar '+ P1 (trans p v 0))
)

;;;-------------------------------------------------------------
;;; 功能: 计算有限点集的质心                             	
;;; 输入: 有限个点集  Pts                                	
;;; 输出: 质心坐标，用点表表示                           	
;;;-------------------------------------------------------------
(defun GEO:Centroid (Pts / )
  (MAT:vxs (apply 'mapcar (cons '+ pts)) (/ 1.0 (length pts)))
)

;;;-------------------------------------------------------------
;;; 功能: 计算多个物体的质心                                    
;;; 输入: 截面的质心列表CenList和相应的面积列表AreaList  	
;;; 输出: 一个数值，如果为正则是CCW(逆时针)，否则顺时针  	
;;;-------------------------------------------------------------
(defun GEO:Centroid_Composition (CenList AreaList / s)
  (setq S (apply '+ AreaList))
  (if (/= s 0)
    (list (Mat:vxs (MAT:SxVs CenList AreaList) (/ 1.0 s)) S)
  )
)

;;;************************************************************;
;;; 直线部分                                             	
;;;************************************************************;

;;;-------------------------------------------------------------
;;; 直线的方程 Coefficient Equation                             
;;; 参数: 两点                                           	
;;; 返回: 直线的方程Ax+By+C=0 的三个系数A,B,C            	
;;;-------------------------------------------------------------
(defun LINE:Equation (p1 p2)
  (list
    (- (cadr p1) (cadr p2))
    (- (car  p2) (car  p1))
    (- (* (car p1) (cadr p2)) (* (cadr p1) (car p2)))
  )
)

;;;-------------------------------------------------------------
;;; 直线的方程1                                          	
;;; 点矢量式方程  P0+k*Vector                            	
;;; 参数: 两点                                           	
;;; 返回: 直线的方程用一点和直线的方向矢量表达           	
;;;-------------------------------------------------------------
(defun LINE:Equation_1 (p0 p1)        
  (list P0 (mapcar '- p1 p0))
)

;;;-------------------------------------------------------------
;;; 功能: 偏移一条线段                                   	
;;; 输入: 两点和一个距离（负数代表直线段的下方）         	
;;; 输出: 偏移后的两点                                   	
;;;-------------------------------------------------------------
(defun LINE:Offset (p1 p2 d / a)
  (setq a (+ (angle p1 p2) (* pi 0.5)))
  (list (polar p1 a d) (polar p2 a d))
)


;;;-------------------------------------------------------------
;;; 功能: 求过直线外一点作已知角度的线与已知直线的交点   	
;;; 输入: 两点代表的直线MN和直线外一点P以及已知角a       	
;;; 输出: 交点或者nil                                    	
;;;-------------------------------------------------------------
(defun LINE:PAMN (p a M N /)
  (inters P (polar P a 1) M N nil)
)

;;;-------------------------------------------------------------
;;; 功能: 点Pt到直线P1P2的距离（带方向）                 	
;;; 输入: 要求的点Pt，和直线的两个端点P1,P2              	
;;; 输出: 带符号的距离，为正P1,P2,Pt逆时针，否则顺时针   	
;;;-------------------------------------------------------------
(defun LINE:Perpendicular_Distance (pt p1 p2 / A B C) 
  (setq A (- (cadr p1) (cadr p2)))
  (setq B (- (car  p2) (car  p1)))
  (setq C (- (* (car p1) (cadr p2)) (* (cadr p1) (car p2))))
  (if (not (and (= A 0) (= b 0)))
    (/ (+ (* A (car pt)) (* B (cadr pt)) C)
       (sqrt (+ (* A A) (* B B)))
    )
  )
)

;;;-------------------------------------------------------------
;;; 功能: 已知直线方程系数求点到直线的距离（带方向）     	
;;; 输入: 要求的点Pt，和直线方程的三个系数               	
;;; 输出: 带符号的距离，为正Pt在直线方向的上方，负则反之 	
;;;-------------------------------------------------------------
(defun LINE:Perpendicular_Distance_1 (Pt A B C / AA BB AB k x0 y0 x y D)
  (if (not (and (= a 0) (= b 0)))
    (progn 
      (setq AA (* A A))
      (setq BB (* B B))
      (setq AB (* A B))
      (setq k  (+ AA BB))
      (setq x0 (car pt))
      (setq y0 (cadr pt))
      
      (setq x  (/ (- (* BB x0) (* AB y0) (* A C)) k))
      (setq y  (/ (- (* AA y0) (* AB x0) (* B C)) k))
      (setq D  (/ (+ (* A x0) (* B y0) C) (sqrt k)))
      (list D (list x y))
    )
  )
)

;;;-------------------------------------------------------------
;;; 功能: 点到直线的距离（带方向）                       	
;;; 输入: 要求的点Pt，和直线的两个端点P1,P2              	
;;; 输出: 带符号的距离，为正P1,P2,Pt逆时针，否则顺时针   	
;;;-------------------------------------------------------------
(defun LINE:Perpendicular_Distance_2 (pt p1 p2 / )
  (car (trans (mapcar '- pt p1) 0 (mapcar '- p2 p1)))
)

;;;-------------------------------------------------------------
;;; 功能: 点到直线的距离（适合三维情况）                 	
;;; 输入: 要求的点Pt，和直线的两个端点P1,P2              	
;;; 输出: 所求距离                                       	
;;;-------------------------------------------------------------
(defun LINE:Perpendicular_Distance_3 (p0 p1 p2 / v0 v1)
  (setq v0 (mapcar '- P0 p1))
  (setq v1 (mapcar '- p2 p1))
  (/ (MAT:Norm3D (MAT:vxv v0 v1)) (MAT:Norm3D v1))
)

;;;-------------------------------------------------------------
;;; 功能: 点到直线的距离和垂足                           	
;;; 输入: 要求的点Pt，和直线的两个端点P1,P2              	
;;; 输出: 所求距离和垂足                                 	
;;;-------------------------------------------------------------
(defun LINE:Perpendicular_Foot (pt p1 p2 / d)
  (setq d (LINE:Perpendicular_Distance pt p1 p2))
  (list d (polar pt (- (angle p1 p2) (/ pi 2)) d))
)

(defun LINE:Perpendicular_Foot_2 (P p1 p2 / pt)
  (setq pt (mapcar '+ (MAT:Rot90 (mapcar '- p1 p2)) p))  
  (inters p1 p2 p pt nil)        
)

(defun LINE:Perpendicular_Foot_3 (p p1 p2 / p0)           	
  (setq p0 (trans (mapcar '- p p1) 0 (mapcar '- p2 p1)))
  (mapcar '+ p1 (list (car p0) (last p0) (cadr p0)))
)

;;;-------------------------------------------------------------
;;; 功能: 点到直线的垂足                                 	
;;; 输入: 要求的点Pt，和直线的两个端点P1,P2              	
;;; 输出: 所求的垂足                                     	
;;;-------------------------------------------------------------
(defun LINE:Perpendicular_Foot_1 (pt p1 p2)
  (inters pt (mapcar '+ pt (MAT:Rot90 (mapcar '- p1 p2))) p1 p2 nil)
)

;;;-------------------------------------------------------------
;;; 功能: 求空间两直线的最短距离                         	
;;; 输入: 两条直线的四个端点P1,P2,P3,P4                  	
;;; 输出: 所求距离                                       	
;;;-------------------------------------------------------------
(defun LINE:Distance_LineToLine (P1 P2 P3 P4 / v1 v2 v3)
  (setq v1 (mapcar '- p2 p1))
  (setq v2 (mapcar '- p4 p3))
  (setq v3 (MAT:vxv v1 v2))
  (/ (Mat:Dot (mapcar '- P1 P3) v3) (Mat:Norm3D v3))
)

;;;-------------------------------------------------------------
;;; 功能: 两条直线求交点函数(跟inters函数稍微有区别)     	
;;; 输入: 两条直线的四个端点P1,P2,P3,P4                  	
;;; 输出: nil 说明这两条平行或者共线，否则返回交点       	
;;;-------------------------------------------------------------
(defun LINE:Intersection (p1 p2 p3 p4 / DA DB DD X1 X2 X3 X4 Y1 Y2 Y3 Y4)
  (setq x1 (car  p1)
        x2 (car  p2)
        x3 (car  p3)
        x4 (car  p4)
        y1 (cadr p1)
        y2 (cadr p2)
        y3 (cadr p3)
        y4 (cadr p4)
  )
  (setq dd (- (* (- x1 x2) (- y3 y4)) (* (- x3 x4) (- y1 y2))))
  (setq da (- (* x1 y2) (* y1 x2)))
  (setq db (- (* x3 y4) (* y3 x4)))
  (if (not (equal dd 0 1e-8))
    (list (/ (- (* da (- x3 x4)) (* db (- x1 x2))) dd)
          (/ (- (* da (- y3 y4)) (* db (- y1 y2))) dd)
    )
  )
)

;;;-------------------------------------------------------------
;;; 功能: 两条直线的角平分线                             	
;;; 输入: 两条直线的四个端点P1,P2,P3,P4                  	
;;; 输出: 角平分线的两个端点                             	
;;;-------------------------------------------------------------
(defun LINE:Angular_Bisector (p1 p2 p3 p4 / an1 an2 an3 an4 int)
  (if (setq int (inters p1 p2 p3 p4 nil))
    (progn
      (setq eps 1e-6)
      (if (equal int p1 eps)
        (setq an1 (angle int p2))
        (setq an1 (angle int p1))
      )
      (if (equal int p3 eps)
        (setq an2 (angle int p4))
        (setq an2 (angle int p3))
      )
      (setq an3 (* (+ an1 an2) 0.5))
      (setq an4 (+ an3 (* 0.5 pi)))
      (list
        (list int (polar int an3 1000))
        (list int (polar int an4 1000))
      )
    )
    (list 
      (list
        (Geo:midpoint p1 p3)
        (Geo:midpoint p2 p4)
      )
    )
  )
)

;;;-------------------------------------------------------------
;;; 功能: 判断平面上的三点是否共线                       	
;;; 输入: 三点 P1,P2,P3                                  	
;;; 输出: T 说明三点共线，否则不共线                     	
;;;-------------------------------------------------------------
(defun LINE:Colinearity	(p1 p2 p3 /)
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


;;;-------------------------------------------------------------
;;; 功能: 判断空间上三点是否共线(跟上面的方法效率差不多) 	
;;; 输入: 三点 P1,P2,P3                                  	
;;; 输出: T 说明三点共线，否则不共线                     	
;;;-------------------------------------------------------------
(defun LINE:Colinearity3D (p1 p2 p3 / a1 a2)
  (equal (TRI:Det3P p1 p2 p3) 0 1e-8)
)

;;;-------------------------------------------------------------
;;; 功能: 判断两点是否在一条直线的同一侧                 	
;;; 输入: 要判断的两点点P1,P2和直线的两个端点Pa,Pb       	
;;; 输出: T 说明同侧，nil异侧                            	
;;;-------------------------------------------------------------
(defun LINE:IsSameSide (P1 P2 Pa Pb / d1 d2 eps)
  (setq eps 1e-6)
  (setq d1 (TRI:Det3P P1 PA PB))
  (setq d2 (TRI:Det3P P2 PA PB))
  (or (and (<= d1 eps) (<= d2 eps))
      (and (>= d1 (- eps)) (>= d2 (- eps)))
  )
)

;;;-------------------------------------------------------------
;;; 功能: 求空间两直线的最近距离点                	        
;;; 输入: 空间两直线L1,L2（用点和方向矢量表示）          	
;;; 输出: 最近距离的一对点                                   	
;;; 说明: 若直线L1=P1+t1*D1,L2=P2+t2*D2,则得到其最近距离的点时: 
;;;       t1=((P2-P1)XD2)·(D1XD2)/||D1XD2||^2                  
;;;       t2=((P2-P1)XD1)·(D1XD2)/||D1XD2||^2                  
;;;-------------------------------------------------------------
(defun Line:MinDist (L1 L2 / p1 p2 d1 d2 dv dd pv t1 t2)
  (setq p1 (car  L1))
  (setq d1 (cadr L1))
  (setq p2 (car  L2))
  (setq d2 (cadr L2))
  (setq dv (mat:vxv d1 d2))
  (setq dd (mat:Dot dv dv))
  (if (not (equal dd 0 1e-14))
    (progn
      (setq pv (mapcar '- p2 p1))
      (setq t1 (/ (mat:Dot (mat:vxv pv d2) dv) dd))
      (setq t2 (/ (mat:Dot (mat:vxv pv d1) dv) dd))
      (list
	(mapcar '+ p1 (mat:vxs d1 t1))
	(mapcar '+ p2 (mat:vxs d2 t2))
      )
    )
  )
)

;;;-------------------------------------------------------------
;;; 角平分线                                            	
;;;-------------------------------------------------------------
(defun GEO:Bisector (pt p1 p2 / a1 a2)
  (setq a1 (angle pt p1))
  (setq a2 (angle pt p2))
  (if (> a1 a2)
    (+ pi (* 0.5 (+ a1 a2)))
    (* 0.5 (+ a1 a2))
  )
)

;;;-------------------------------------------------------------
;;; 判断是否在角度内                                    	
;;;-------------------------------------------------------------
(defun GEO:IsInAngle (Pt p1 p2 p3)
  (< (GEO:bisector Pt p1 p3) (GEO:bisector Pt p1 p2))
)


;;;-------------------------------------------------------------
;;; 夹角（以P1为顶点，从p2到p3的有向角度）                      
;;; 正代表逆时针，负则反之                                      
;;;-------------------------------------------------------------
(defun GEO:Angle (p1 p2 p3 / a )
  (setq a (- (angle p1 p3) (angle p1 p2)))
  (if (< a 0)
    (if (< a (- pi))
      (+ a 6.283185307179586476925286766559)
      a
    )
    (if (> a pi)
      (- a 6.283185307179586476925286766559)
      a
    )
  )
)

(defun GEO:Angle1 (p1 an p2 / a )
  (setq a (- (angle p1 p2) an))
  (if (< a 0)
    (if (< a (- pi))
      (+ a 6.283185307179586476925286766559)
      a
    )
    (if (> a pi)
      (- a 6.283185307179586476925286766559)
      a
    )
  )
)

(defun GEO:angle2 (p0 p1 p2 / v1 v2)
  (setq v1 (mapcar '- p1 p0))
  (setq v2 (mapcar '- p2 p0))
  (atan
    (- (* (car v1) (cadr v2)) (* (car v2) (cadr v1)))
    (+ (* (car v1) (car v2)) (* (cadr v2) (cadr v1)))
  )
)

;;;-------------------------------------------------------------
;;; 功能: 三点构成的夹角                                	
;;; 输入: 顶点P0，角的两端点P1和P2。                   	        
;;; 输出: 所求夹角的弧度值（O 到Pi之间）。                      
;;;-------------------------------------------------------------
(defun Line:Angle (P0 P1 P2 / an)
  (setq an (abs (- (angle p0 P1) (angle P0 P2))))
  (if (> an pi)
    (- (+ pi pi) an)
    an
  )
)

;;;-------------------------------------------------------------
;;; 功能: 判断点在到线段的垂足是否在此线段区间中                
;;; 输入: 要判断的点P0和直线的两个端点P1，P2。             	
;;; 输出: T 说明满足条件，nil 则相反                            
;;;-------------------------------------------------------------
(defun GEO:IsBetween-p (p0 p1 p2 / a b c)
  (setq a (distance p1 p2))
  (setq b (distance p0 p1))
  (setq c (distance p0 p2))
  (> (* a a) (abs (* (+ b c) (- b c))))
)

;;;************************************************************;
;;; 空间平面部分                                         	
;;;************************************************************;

;;;-------------------------------------------------------------
;;; 功能: 点法线的平面方程                               	
;;; 输入: P0平面上的一点，N平面的法线矢量                	
;;; 输出: 平面方程的系数列表                             	
;;;-------------------------------------------------------------
(defun PLANE:Equation (P0 N)
  (append N (list (- (MAT:Dot P0 N))))
)

;;;-------------------------------------------------------------
;;; 功能: 三点式平面方程                                 	
;;; 输入: 平面上的三点                                   	
;;; 输出: 平面方程的系数列表                             	
;;;-------------------------------------------------------------
(defun PLANE:Equation_3P (P0 P1 P2 / v1 v2 N)
  (setq v1 (mapcar '- p1 p0))
  (setq v2 (mapcar '- P2 p0))
  (setq N  (MAT:vxv v1 v2))
  (PLANE:Equation P0 N)
)

;;;-------------------------------------------------------------
;;; 功能: 点到平面的距离（有向的距离）                   	
;;; 输入: 一点P和平面的方程为Ax+By+Cz+D=0的四个系数      	
;;; 输出: 该点到平面的距离                               	
;;;-------------------------------------------------------------
(defun PLANE:Distance (P A B C D)
  (if (and (zerop A) (zerop B) (zerop C))
    nil
    (/ (+ (* A (car P)) (* B (cadr P)) (* C (caddr P)) D)
       (distance '(0 0 0) (list A B C))
    )
  )
)

;;;-------------------------------------------------------------
;;; 功能: 点到三点决定的平面的距离（有向的距离）         	
;;; 输入: 一点P和平面的方程为Ax+By+Cz+D=0的四个系数      	
;;; 输出: 该点到平面的距离                               	
;;;-------------------------------------------------------------
(defun PLANE:Distance_1 (P p1 p2 p3 /) 
  (Apply 'PLANE:Distance (cons p (PLANE:Equation_3P p1 p2 p3)))
)

;;;-------------------------------------------------------------
;;; 功能: 点到三点决定的平面的距离和该点在平面上的投影点 	
;;; 输入: 一点P和三点P1,P2,P3决定的平面                  	
;;; 输出: 该点到平面的垂足                               	
;;;-------------------------------------------------------------
(defun PLANE:Perpendicular_Foot_1 (P p1 p2 p3 / F A B C D H N L)
  (setq F (PLANE:Equation_3P p1 p2 p3))
  (setq A (car f)
        B (cadr f)
        C (caddr f)
        D (last f)
  )
  (setq H (PLANE:Distance p A B C D))
  (setq N (List A B C))
  (setq L (distance '(0 0 0) N)) 
  (if (not (zerop L)) 
    (list H (Geo:scale (mapcar '+ p N) P (- (/ H L))))
  )
)

;;;-------------------------------------------------------------
;;; 功能: 点到三点决定的平面的距离和该点在平面上的投影点 	
;;; 输入: 一点P和通过P0法线矢量为Normal决定的平面        	
;;; 输出: 该点到平面的垂足                               	
;;;-------------------------------------------------------------
(defun PLANE:Perpendicular_Foot (P P0 Normal / l d)
  (setq l (distance '(0 0 0) Normal))
  (setq d (caddr (trans (mapcar '- p p0) 0 Normal T)))
  (mapcar '- P (mat:vxs Normal (/ d l)))
)

(defun PLANE:Perpendicular_Foot_2 (P P0 Normal / l d)
  (setq l (distance '(0 0 0) Normal))
  (setq l (* l l))
  (setq d (mat:dot Normal (mapcar '- p p0)))
  (mapcar '- P (mat:vxs Normal (/ d l)))
)

;;;-------------------------------------------------------------
;;; 功能: 求空间直线与平面的交点                         	
;;; 输入: 决定直线的两点Pa,Pb和三点P1,P2,P3决定的平面    	
;;; 输出: 该点到平面的距离                               	
;;;-------------------------------------------------------------
(defun PLANE:Line_Inters_Plane (Pa Pb A B C D / h1 h2)
  (setq h1 (Plane:Distance Pa A b c d))
  (setq h2 (plane:distance Pb a b c d))
  (if (and h1 h2)
    (cond
      ( (equal h1 0 1e-14) Pa)
      ( (equal h2 0 1e-14) Pb)
      (t (GEO:Proportion Pa Pb (- (/ h1 h2))))
    )
  )
)

;;;-------------------------------------------------------------
;;; 平面的距离，夹角，法线，和空间直线的夹角                    
;;;-------------------------------------------------------------

;;;-------------------------------------------------------------
;;; 功能: 获取平面的法向矢量                             	
;;; 输入: P1 P2 P3三点决定的平面                         	
;;; 输出: 该平面的法线矢量                               	
;;;-------------------------------------------------------------
(defun PLANE:NORMAL (p1 p2 p3 / v1 v2)
  (setq v1 (mapcar '- p2 p1))
  (setq v2 (mapcar '- p3 p1))
  (list
    (- (* (cadr  v1) (caddr v2)) (* (caddr v1) (cadr  v2)))
    (- (* (caddr v1) (car   v2)) (* (car   v1) (caddr v2)))
    (- (* (car   v1) (cadr  v2)) (* (cadr  v1) (car   v2)))
  )
)
		                                                
;;;-------------------------------------------------------------
;;; 功能: 获取两个平面的夹角                             	
;;; 输入: P1 P2 P3三点决定的平面                         	
;;; 输出: 该平面的法线矢量                               	
;;;-------------------------------------------------------------
(defun PLANE:ANGLE (plane1 plane2 / cosang sinang retang iszero)
  (setq plane1 (apply 'PLANE:NORMAL plane1))
  (setq plane2 (apply 'PLANE:NORMAL plane2))
  (setq isZero (* (distance '(0 0 0) plane1) (distance '(0 0 0) plane2)))
  (if (equal isZero 0 1e-8)
    0
    (setq cosang (/ (apply '+ (mapcar '* plane1 plane2)) isZero)
	  sinang (sqrt (* (1+ cosang) (- 1 cosang)))
	  retang (atan sinang cosang)
    )
  )
)
  
;;;************************************************************;
;;; 三角形部分                                           	
;;;************************************************************;

;;;-------------------------------------------------------------
;;; 功能: 判断是否构成三角形                             	
;;; 输入: 三边的长度a,b,c                                	
;;; 输出: 构成三角形则返回T，否则返回nil                 	
;;;-------------------------------------------------------------
(defun TRI:IsTriangle (a b c /)
  (and
    (> (+ a b) (+ c 1e-8))
    (> (+ b c) (+ a 1e-8))
    (> (+ c a) (+ b 1e-8))
  )
)

;;;-------------------------------------------------------------
;;; 功能: 求三角形外心   TRI:CircumCenter，ExCenter      	
;;; 输入: 给定不共线的三个点                             	
;;; 输出: 这三点的外接圆的圆心和半径                     	
;;; 说明: 尽管这样写很麻烦，显得代码很多，但运行却很快   	
;;;-------------------------------------------------------------
(defun TRI:CircumCenter (P0 P1 P2 / X0 Y0 X1 Y1 X2 Y2 DX1 DY1 DX2 DY2 D 2D C1 C2 CE)
  (setq X0  (car  P0)
        Y0  (cadr P0)
        X1  (car  P1)
        Y1  (cadr P1)
        X2  (car  P2)
        Y2  (cadr P2)
        DX1 (- X1 X0)
        DY1 (- Y1 Y0)
        DX2 (- X2 X0)
        DY2 (- Y2 Y0)
  )
  (setq D (- (* DX1 DY2) (* DX2 DY1)))
  (if (equal D 0 1e-14)
    nil
    (progn
      (setq 2D (+ D D)
            C1 (+ (* DX1 (+ X0 X1)) (* DY1 (+ Y0 Y1)))
            C2 (+ (* DX2 (+ X0 X2)) (* DY2 (+ Y0 Y2)))
            CE (List (/ (- (* C1 DY2) (* C2 DY1)) 2D)
                     (/ (- (* C2 DX1) (* C1 DX2)) 2D)
               )
      )
      (list CE (distance CE P0))
    )
  )
)

;;;-------------------------------------------------------------
;;; 功能: 三角形内心                                     	
;;; 公式: (aX1+bx2+cx3)/(a+b+c),(aY2+bY2+CY3)/(a+b+c)    	
;;; 输入: 给定不共线的三个点                             	
;;; 输出: 这三点的内切圆的圆心和半径                     	
;;;-------------------------------------------------------------
(defun TRI:InCenter (pa pb pc / a b c L I r)
  (setq a (distance pb pc))
  (setq b (distance pc pa))
  (setq c (distance pa pb))
  (setq L (+ a b c))
  (if (/= L 0.0)
    (setq I (MAT:SxVs (list pa pb pc) (list (/ a L) (/ b L) (/ c L)))
          R (list I (abs (LINE:Perpendicular_Distance I pa pb)))
    )
    (list pa 0)
  )
)

;;;-------------------------------------------------------------
;;; 功能: 三角形垂心                                     	
;;; 输入: 给定不共线的三个点                             	
;;; 输出: 这个三点形成的三角形的垂心                     	
;;;-------------------------------------------------------------
(defun TRI:OrthoCenter (pa pb pc / p1 p2)
  (setq p1 (GEO:Rot90 Pa pb pc))
  (setq p2 (GEO:Rot90 pb pc pa))
  (inters pa p1 pb p2 nil)
)

;;;-------------------------------------------------------------
;;; 功能: 三角形重心                                     	
;;; 输入: 给定不共线的三个点                             	
;;; 输出: 这个三点形成的三角形的重心                     	
;;;-------------------------------------------------------------
(defun TRI:Barycenter (p1 p2 p3)
  (mapcar (function (lambda (e1 e2 e3) (/ (+ e1 e2 e3) 3.0))) p1 p2 p3)
)

;;;-------------------------------------------------------------
;;; 功能: 三角形的九点圆                                 	
;;; 输入: 给定不共线的三个点                             	
;;; 输出: 这个三点形成的三角形的九点圆的圆心和半径       	
;;;-------------------------------------------------------------
(defun TRI:9P_Circle (pa pb pc)
  (apply
    'TRI:CircumCenter
    (mapcar 'GEO:Midpoint  (list pa pb pc) (list pb pc pa))
  )
)

;;;-------------------------------------------------------------
;;; 三线坐标转化为世界坐标      k = 2S/(ax+by+cz)        	
;;; 注意: 三线坐标跟笛卡尔坐标的表示上的不同             	
;;; 输入: 三线坐标P(list x y z)=>x:y:z和对应三点Pa,Pb,Pc 	
;;; 输出: 返回世界坐标系的点                             	
;;;-------------------------------------------------------------
(defun TRI:TCS->WCS (P Pa Pb Pc / x y z V1 V2 V3 p1 p2 p3 int)
  (setq V1 (LINE:Offset Pb Pc (car P)))
  (setq V2 (LINE:Offset Pc Pa (cadr p)))
  (setq V3 (LINE:Offset Pa Pb (caddr p)))
  (setq p1 (inters (car V2) (cadr V2) (car V3) (cadr V3) nil))
  (setq p2 (inters (car V3) (cadr V3) (car V1) (cadr V1) nil))
  (setq p3 (inters (car V1) (cadr V1) (car V2) (cadr V2) nil))
  (if (setq int (inters Pa P1 Pb P2 nil))
    int
    (if (setq int (inters Pb P2 Pc P3 nil))
      int
      (inters Pc P3 Pa P1 nil)
    )
  )
)
 
;;;-------------------------------------------------------------
;;; 功能: 相似重心,Lemoine Point ,or symmedian point     	
;;; 输入: 给定不共线的三个点                             	
;;; 输出: 这个三点形成的三角形的相似重心                 	
;;;-------------------------------------------------------------
(defun TRI:Symmedian_Point (Pa Pb Pc / a b c)
  (setq a (distance Pb Pc))
  (setq b (distance pc Pa))
  (setq c (distance Pa Pb))
  (TRI:TCS->WCS (list a b c) Pa Pb Pc)
)

;;;-------------------------------------------------------------
;;; 功能: 某点对给定三角形的等角共轭点                   	
;;; 输入: 一点Pt 和构成三角形的三点Pa Pb Pc              	
;;; 输出: 这点对给定三角形的等角共轭点                   	
;;;-------------------------------------------------------------
(defun TRI:Isogonal-Conjugate-Point (Pt Pa Pb Pc / Pt1 Pt2 Inc)
  (setq InC (car (TRI:InCenter Pa Pb Pc)))
  (setq Pt1 (GEO:Mirror3D Pt Pa Inc))
  (setq pt2 (GEO:Mirror3D Pt Pb Inc))
  (inters Pa Pt1 Pb Pt2 nil)
)

;;;-------------------------------------------------------------
;;; 功能: 某点对给定三角形的等角共轭点                   	
;;; 输入: 一点Pt 和构成三角形的三点Pa Pb Pc              	
;;; 输出: 这点对给定三角形的等角共轭点                   	
;;; 说明: 如果已知三角形内心，则可以简略计算             	
;;;-------------------------------------------------------------
(defun TRI:Isogonal-Conjugate-Point-1 (Pt Pa Pb Inc /)
  (inters Pa (GEO:Mirror3D Pt Pa Inc) Pb (GEO:Mirror3D Pt Pb Inc) nil)
)

;;;-------------------------------------------------------------
;;; 功能: 根据三角形的三边长获取三角形信息                     	
;;; 输入: 三边的边长a,b,c                                	
;;; 输出: 三角形的三个角度，面积和周长，内心和内切圆半径，旁切圆
;;;       的圆心和半径，外心和外接圆半径，垂心，重心，类似重心，
;;;       等周心以及九点圆圆心           			
;;;-------------------------------------------------------------
;|
http://en.wikipedia.org/wiki/Trilinear_coordinates              
where a, b, c are the respective sidelengths BC, CA, AB,and σ =
area of ABC.                                                    
A = 1 : 0 : 0                                                   
B = 0 : 1 : 0                                           	
C = 0 : 0 : 1                                           	
incenter = 1 : 1 : 1                                    	
centroid = bc:ca:ab = 1/a:1/b:1/c = cscA : cscB : cscC. 	
circumcenter = cos A : cos B : cos C.                   	
orthocenter = sec A : sec B : sec C.                    	
nine-point center = cos(B - C) : cos(C - A) : cos(A - B)	
symmedian point = a : b : c = sin A : sin B : sin C.    	
A-excenter = -1 : 1 : 1                                 	
B-excenter = 1 : -1 : 1                                 	
C-excenter = 1 : 1 : -1.                                	
;;;de Longchamps point                                  	
;;;http://en.wikipedia.org/wiki/De_Longchamps_point     	
;;;symmedian point                                      	
;;;http://en.wikipedia.org/wiki/Symmedian_point         	
http://mathworld.wolfram.com/TriangleCenter.html        	
|; 
(defun TRI:InfoBy3Sides (a b c / p S 2S Aa Ab Ac D K Ri Re Ra Rb Rc Ca Cb Cc Sa Sb Sc)
  (setq p  (* 0.5 (+ a b c)))                           	;半周长
  (setq S  (sqrt (* p (- p a) (- p b) (- p c))))        	;面积
  (setq Ri (/ S p))                                     	;内切圆半径
  (setq K  (* 2 Ri p))
  (setq Ra (/ k (+ b c (- a))))                         	;边A旁切圆半径
  (setq Rb (/ k (+ c a (- b))))                         	;边B旁切圆半径
  (setq Rc (/ k (+ a b (- c))))                         	;边C旁切圆半径
  (setq Re (/ (* a b c 0.25) S))                        	;外接圆半径
  (setq D  (+ Re Re))                                   	;外接圆直径
  (setq Ca (/ (+ (* b b) (* (+ c a) (- c a))) 2 b c))   	;角A余弦
  (setq Cb (/ (+ (* c c) (* (+ a b) (- a b))) 2 c a))   	;角B余弦
  (setq Cc (/ (+ (* a a) (* (+ b c) (- b c))) 2 a b))   	;角C余弦
  (setq Sa (/ a D))                                     	;角A正弦
  (setq Sb (/ b D))                                     	;角B正弦
  (setq Sc (/ c D))                                     	;角C正弦
  (setq Aa (atan Sa Ca))                                	;角A
  (setq Ab (atan Sb Cb))                                	;角B
  (Setq Ac (atan Sc Cc))                                	;角C
  (setq 2S (+ S S))
  (list
    (list Aa Ab Ac)                                 		;三个角
    (list S (+ p p))                                		;面积和周长
    (list '( 1  1  1) Ri)                           		;内心
    (list '(-1  1  1) Ra)                           		;边A旁切圆半径
    (list '( 1 -1  1) Rb)                           		;边B旁切圆半径
    (list '( 1  1 -1) Rc)                           		;边C旁切圆半径
    (list (list Ca Cb Cc) Re)                       		;外心
    (list (list (/ 1 Ca) (/ 1 Cb) (/ 1 Cc)))        		;垂心
    (list (list (/ 1 a) (/ 1 b) (/ 1 c)))           		;重心
    (list (list a b c))                             		;类似重心
    (list
      (list (cos (- Ab Ac)) (cos (- Ac Aa)) (cos (- Aa Ab)))	;九点圆圆心
      (* 0.5 Re)                                		;九点圆半径
    )
    (list
      (list
	(1- (/ 2S a (+ b c (- a))))
	(1- (/ 2S b (+ c a (- b))))
	(1- (/ 2S c (+ a b (- c))))
      )
    )                                               		;等周点(Isoperimetric Point)
  )
)

;;;-------------------------------------------------------------
;;;功能: 定义三点的行列式,即三点之倍面积                	
;;;输入: 三点P1,P2,P3                                   	
;;;输出: 这三点形成的三角形的面积的2倍，符号指示方向。  	
;;;-------------------------------------------------------------
(defun TRI:Det3P (p1 p2 p3)
  (- (* (- (car p2) (car p1)) (- (cadr p3) (cadr p1)))
     (* (- (car p3) (car p1)) (- (cadr p2) (cadr p1)))
  )
)

;;;-------------------------------------------------------------
;;;功能: 用海伦公式(Heron's formula)求三角形面积        	
;;;输入: 三角形的三个边长a,b,c                          	
;;;输出: 三角形面积                                     	
;;;-------------------------------------------------------------
(defun TRI:Area (a b c / p)
  (setq p (* 0.5 (+ a b c)))
  (sqrt (* p (- p a) (- p b) (- p c)))
)


;;;-------------------------------------------------------------
;;; 功能: 根据三边求三个角(利用余弦定理)                	
;;; 输入: 构成三角形的三边a,b,c                         	
;;; 输出: 返会三条边对应的三个角                        	
;;;-------------------------------------------------------------
(defun TRI:CosinesLaw (a b c / cc sc a1 a2)
  (if (TRI:IsTriangle a b c)
    (progn
      (setq a  (float a))                                       ;为了防止整除
      (setq cc (/ (+ (* a a) (* (+ b c) (- b c))) (+ a a) b))   ;角C的余弦
      (setq sc (sqrt (* (- 1 cc) (1+ cc))))                     ;角C的正弦
      (setq a1 (atan (* a sc) (- b (* a cc))))                  ;角A
      (setq a2 (atan sc cc))                                    ;角C
      (list a1 (- pi a1 a2) a2)                                 ;返回三个角度的列表
    )
  )
)


;;;-------------------------------------------------------------
;;;功能: 计算已知空间三点的三角形面积                   	
;;;输入: 空间三点 P1,P2,P3                              	
;;;输出: 三角形面积                                     	
;;;-------------------------------------------------------------
(defun TRI:Area3D (p1 p2 p3 / v1 v2 d1 d2 d3)
  (setq v1 (mapcar '- p2 p1))
  (setq v2 (mapcar '- p3 p1))
  (setq d1 (MAT:Det2 (car   v1) (cadr  v1) (car   v2) (cadr  v2)))
  (setq d2 (MAT:Det2 (cadr  v1) (caddr v1) (cadr  v2) (caddr v2)))
  (setq d3 (MAT:Det2 (caddr v1) (car   v1) (caddr v2) (car   v2)))
  (* 0.5 (sqrt (+ (* d1 d1) (* d2 d2) (* d3 d3))))
)

;;;************************************************************;
;;;多边形部分                                           	
;;;************************************************************;

;;;-------------------------------------------------------------
;;; 功能: 判断有序的点集是否为顺时针                            
;;; 输入: 有序的点集                                            
;;; 输出: 为真则顺时针，否则逆时针                              
;;;-------------------------------------------------------------
(defun ALG:Clockwise-p (lst)
  (minusp
    (apply '+ (mapcar 'MAT:Det2V lst (cons (last lst) lst)))
  )
)

;;;-------------------------------------------------------------
;;; 判断点是否在多边形（用顶点表示）内                          
;;; 方法: Pt与各个顶点的有向夹角和的绝对值如果等于2*pi则为真.   
;;; 参数: Pt  要判断的点                                        
;;;       lst 顶点列表                                          
;;; 返回: T 在多边形内，nil则在外                               
;;;-------------------------------------------------------------
(defun ALG:Inside-p (Pt lst /)
  (if (> (length lst) 2)					;至少要3点
    (equal
      (abs							;绝对值
	(apply
	  '+							;总和
	  (mapcar
	    (function (lambda (p q) (GEO:Angle pt p q)))        ;有向夹角
	    (cons (last lst) lst)
	    lst
	  )
	)
      )
      6.283185307179586476925286766559				;2*pi
      1e-4							;设低一点，防止累计误差
    )
  )
)

;;;-------------------------------------------------------------
;;; 功能: 计算多边形面积(为简单多边形，不自交的多边形)   	
;;; 输入: 多边形顶点列表  Pts                            	
;;; 输出: 一个数值，如果为正则是CCW(逆时针)，否则顺时针  	
;;; 参考: Centroid  Shoelace formula                     	
;;;-------------------------------------------------------------
(defun POLY:Area (pts)
  (* 0.5 (apply '+ (mapcar 'MAT:Det2V (cons (last pts) pts) pts)))
)

;;;-------------------------------------------------------------
;;; 功能: 计算多边形周长                                 	
;;; 输入: 多边形顶点列表  Pts                            	
;;; 输出: 一个数值，表示多边形周长                       	
;;;-------------------------------------------------------------
(defun POLY:Perimeter (pts IsClosed)
  (if isClosed
    (apply '+ (mapcar 'distance pts (cons (last pts) pts)))
    (apply '+ (mapcar 'distance pts (cdr pts)))
  )
)

;;;-------------------------------------------------------------
;;; 功能: 判断多边形的方向(为简单多边形，不自交的多边形)        
;;; 输入: 多边形顶点列表  Pts                                   
;;; 输出: 返回T则是CCW(逆时针)，否则顺时针               	
;;;-------------------------------------------------------------
(defun POLY:IsCCW (Pts)
  (> (POLY:Area pts) 0.0)
)

;;;-------------------------------------------------------------
;;; 功能: 获取多边形信息(质心，面积，周长)               	
;;; 输入: Pts---多边形顶点列表                           	
;;; 输出: 列表第一个为多边形的面积中心(质心)，用2d点表示。第二个
;;;       为数值，正数表示多边形方向是CCW (逆时针)，负数表示顺时
;;;       针；第三个为周长.                  			
;;; 参考: http://en.wikipedia.org/wiki/Centroid          	
;;;-------------------------------------------------------------
(defun POLY:Infomation (Pts / Pts1 Ai S lst cen)
  (setq Pts1 (MISC:1st->Last Pts))                              ;another point of every side
  (setq Ai   (mapcar 'MAT:Det2V Pts Pts1))                      ;area of every side 
  (setq S    (* (apply '+ Ai) 0.5))                             ;Total area
  (Setq Cen  (MAT:SxVs (mapcar 'MAT:v+v Pts Pts1) Ai))
  (setq Cen  (MAT:vxs Cen (/ 0.166666666666666667 S)))          ;base on the formula
  (list Cen S (apply '+ (mapcar 'distance pts pts1)))           ;Return Centroid,Total area and Perimeter
)

;;;-------------------------------------------------------------
;;;Circular segment                                     	
;;;弓的质心求以及弓形的面积                             	
;;;输入: C---圆心;Center                                	
;;;      R---半径;Radius                                	
;;;      A1--起始角;0 <= A1 <= 2*Pi Start Angle(Radian) 	
;;;      A2--终止角;0 <= A2 <= 2*Pi End Angle(Radian)   	
;;;      IsCW--是否顺时针                               	
;;;输出: 列表: 第一项为质心，第二项为面积，第三项为弧长 	
;;;-------------------------------------------------------------
(defun CIR:Circular_Segment (C R A1 A2 IsCW / A k d S e)
  (and isCW (setq A A2 A2 A1 A1 A))
  (if (> A1 A2)
    (setq A (- (+ pi pi A2) A1 ))
    (setq A (- A2 A1))
  )
  (setq k (sin (* 0.5 A)))
  (setq k (* 1.333333333333333333333 R k k k))
  (setq e (- A (sin A)))
  (setq S (* 0.5 R R e))
  (and IsCW (setq S (- S)))                             	;如果顺时针，面积为负
  (setq d (/ k e))
  (if (> A1 A2) (setq d (- d)))                         	;这种情况下要反向
  (list (polar C (* 0.5 (+ A1 A2)) d) S (* A R))
)

;;;-------------------------------------------------------------
;;;Circular sector                                      	
;;;扇形的质心，面积和周长                               	
;;;输入: C---圆心;Center                                	
;;;      R---半径;Radius                                	
;;;      A1--起始角;0 <= A1 <= 2*Pi Start Angle(Radian) 	
;;;      A2--终止角;0 <= A2 <= 2*Pi End Angle(Radian)   	
;;;      IsCW--是否顺时针                               	
;;;输出: 列表: 第一项为质心，第二项为面积，第三项为周长 	
;;;-------------------------------------------------------------
(defun CIR:Circular_Sector (C R A1 A2 IsCW / A d S L)
  (and IsCW (setq A A2 A2 A1 A1 A))
  (if (> A1 A2)
    (setq A (- (+ pi pi A2) A1))
    (setq A (- A2 A1))
  )
  (setq d (/ (* 4 R (sin (* 0.5 A))) 3 A))
  (setq S (* 0.5 A R R))                                
  (and IsCW (setq S (- S)))                             	;如果顺时针，面积为负
  (setq L (* R (+ A 2)))                                	;周长
  (if (> A1 A2) (setq d (- d)))                         	;这种情况下要反向
  (list (polar C (* 0.5 (+ A1 A2)) d) S L)
)

;;;-------------------------------------------------------------
;;;获得轻多段线的有弧段处的顶点的信息                   	
;;;输入: P1---顶点坐标(OCS)                             	
;;;      P2---下一顶点坐标(OCS)                         	
;;;      b----凸度（不为零）                            	
;;;输出: 列表: 第一项为质心，第二项为面积，第三项为弧长 	
;;;-------------------------------------------------------------
(defun POLY:Info_Bulge (P1 P2 b / D A k C R)
  (setq D (distance p1 p2))
  (setq A (angle p1 p2))
  (setq k (* d (1+ (* b b)) 0.25))
  (setq C (polar p1 (+ a (- (* pi 0.5) (* 2 (atan b)))) (/ k b)))
  (setq R (/ k (abs b)))
  (CIR:Circular_Segment C R (angle c p1) (angle c p2) (< b 0))
)

;;;-------------------------------------------------------------
;;;获得轻多段线的信息                                   	
;;;输入: LWPoly---轻多段线的实体名                      	
;;;输出: 列表: 第一项为质心，第二项为面积，第三项为弧长 	
;;;-------------------------------------------------------------
(defun POLY:Info_LWPoly (LWPoly / eps Object Points Number IsOpen I P P0 Q Ret b Cen1
                                  Area1 List1 List2 Part1 Leng1 Leng2 AreaLst CenLst)
  (setq eps 1e-6)
  (setq Object (vlax-ename->vla-object LWPoly))
  (setq Points (vlax-get Object 'Coordinates))
  (setq Number (/ (length Points) 2))
  (setq IsOpen (= (vla-get-closed Object) :vlax-false))
  (and IsOpen (setq Number (1+ Number)))
  (setq i 0)
   
  (setq p0  (list (car Points) (cadr Points)))
  (setq p p0)
  (repeat number
    (if (setq Points (cddr Points))
      (setq q (list (car Points) (cadr Points)))        	;下一顶点
      (setq q P0)                                       	;如果顶点是最后点，则取第一点
    )
    (if (not (equal p q eps))                           	;这步为的是消除重合的点。
      (progn
        (setq b (vla-getbulge Object i))                	;取得这点的凸度
        (if (or (/= b 0.0) (and (null points) IsOpen))  	;如果有凸度或者在末端
          (setq List1 (cons (list P b 0) List1))        	;则不计算这点长度
          (setq List1 (cons (list p b (distance p q)) List1))      
        )
        (if (and (/= b 0.0) (or Points (not IsOpen)))   	;如果有凸度(末端不封闭情况不计算)
          (setq List2 (cons (POLY:Info_Bulge p q b) List2))
        )
      )
    )
    (setq p q)
    (setq i (1+ i))
  )
  (setq list1 (reverse List1))
  (setq list2 (reverse list2))
  (setq part1 (POLY:Infomation (mapcar 'car list1)))    	;不含弧段的部分
  (setq Cen1  (car Part1))                              	;不含弧段部分的质心
  (setq Area1 (cadr Part1))                             	;不含弧段部分的面积
  (setq leng1 (apply '+ (mapcar 'last list1)))          	;不含弧段部分的总长
  (if List2                                             	;含弧段的部分
    (setq leng2   (apply '+ (mapcar 'last list2))       	;含弧段部分的总长
          CenLst  (cons Cen1 (mapcar 'car list2))       	;含弧段部分的质心
          AreaLst (cons Area1 (mapcar 'cadr list2))     	;含弧段部分的面积
          ret     (GEO:Centroid_Composition CenLst AreaLst)
          ret     (list (car ret) (cadr ret) (+ leng1 leng2))
    ) 
    (list Cen1 Area1 leng1)
  )
)

;;;************************************************************;
;;;数学部分                                             	
;;;************************************************************;

;;;-------------------------------------------------------------
;;;变号                                                 	
;;;-------------------------------------------------------------
(defun Math:Sign_reversal (y x)
  (if (< x 0) (- y) y)
)

;;;-------------------------------------------------------------
;;;判断是否异号                                         	
;;;-------------------------------------------------------------
(defun MATH:Opposite_Sign (x y)
  (or (and (> x 0) (< y 0)) (and (< x 0) (> y 0)))
)

;;;-------------------------------------------------------------
;;;判断是否同号                                         	
;;;-------------------------------------------------------------
(defun MATH:Same_Sign (x y)
  (or (and (> x 0) (> y 0)) (and (< x 0) (< y 0)))
)

;;;************************************************************;
;;;实体创建部分                                         	
;;;************************************************************;

;;;-------------------------------------------------------------
;;;创建一个点                                           	
;;;输入: 一个三维或者二维的点                           	
;;;输出: 点实体的图元名                                 	
;;;-------------------------------------------------------------
(defun Ent:Make_Point (p)
  (entmakex (list '(0 . "POINT") (cons 10 p)))
)

;;;-------------------------------------------------------------
;;;创建一个带颜色的点（此函数为测试或者其他用途）       	
;;;输入: 一个三维或者二维的点表和一个颜色号             	
;;;输出: 点实体的图元名                                 	
;;;-------------------------------------------------------------
(defun Ent:Make_ColorPoint (p c)
  (entmakex (list '(0 . "POINT") (cons 10 p) (cons 62 c)))
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

;;;-------------------------------------------------------------
;;;创建一条带颜色的直线段                                       
;;;输入: 两个三维或者二维的点和一个颜色号                       
;;;输出: 线段实体的图元名                               	
;;;-------------------------------------------------------------
(defun Ent:Make_ColorLine (p q c)
  (entmakeX (list '(0 . "LINE") (cons 10 p) (cons 11 q)(cons 62 c)))
)

;;;-------------------------------------------------------------
;;;创建一个由三条直线组成的三角形                       	
;;;输入: 三个三维或者二维的点                           	
;;;输出: 由三条直线组成的三角形                         	
;;;-------------------------------------------------------------
(defun Ent:Make_Triangle (p1 p2 p3)
  (mapcar 'Ent:Make_Line (list p1 p2 p3) (list p2 p3 p1))
)

;;;-------------------------------------------------------------
;;;创建一个三维多段线                                   	
;;;输入: 三维的点集                                     	
;;;输出: 三维多段线实体                                 	
;;;-------------------------------------------------------------
(defun Ent:Make_Poly (pts Closed / e)
  (if Closed
    (setq e (Entmake (list '(0 . "POLYLINE") '(70 . 9))))
    (setq e (Entmake (list '(0 . "POLYLINE") '(70 . 8))))
  )
  (foreach p pts
    (entmake (list '(0 . "VERTEX") '(70 . 32) (cons 10 p)))
  )
  (entmake '((0 . "SEQEND")))
  (entlast)
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

;;;-------------------------------------------------------------
;;; 创建圆实体                                           	
;;; 输入: 圆心C和半径R                                   	
;;; 输出: 圆的实体名                                     	
;;;-------------------------------------------------------------
(defun Ent:Make_Circle (C R)
  (entmakex (list '(0 . "CIRCLE") (cons 10 C) (cons 40 R)))
)

;;;-------------------------------------------------------------
;;; 创建弧实体（注意: 弧段总是逆时针方向的）             	
;;; 输入: 弧的圆心C和半径R以及起始角度A1和终止角度A2     	
;;; 输出: 弧的实体名                                     	
;;;-------------------------------------------------------------
(defun Ent:Make_ARC (C R A1 A2)
  (entmakeX
    (list
      '(0 . "ARC")
      (cons 10 C)
      (cons 40 R)
      (cons 50 A1)
      (cons 51 A2)
    )
  )
)

;;;************************************************************;
;;; 杂项                                                 	
;;;************************************************************;

;;;-------------------------------------------------------------
;;; 表中是否存在某个元素                                 	
;;;-------------------------------------------------------------
(defun MISC:IsExist (x lst func / ret)
  (while lst
    (if (apply func (list x (car lst)))
      (setq ret T lst nil)
    )
    (setq lst (cdr lst))
  )
  ret
) 

;;;-------------------------------------------------------------
;;; 根据x的函数求y                                       	
;;;-------------------------------------------------------------
(defun MISC:Apply (x y)
  (mapcar 'apply x (mapcar 'list y))
  ;;(mapcar 'eval (mapcar 'list x y))            	 ;Slower
)

;;;-------------------------------------------------------------
;;; 交换两个元素                                         	
;;;-------------------------------------------------------------
(defun UTI:SWAP (value1 value2 / temp)
  (setq temp (vl-symbol-value value1))
  (set value1 (vl-symbol-value value2))
  (set value2 temp)
)

;;;-------------------------------------------------------------
;;; 把表的第一项放到最后                                 	
;;;-------------------------------------------------------------
(defun MISC:1st->Last (lst)
  (append (cdr lst) (list (car lst)))
)

;;;-------------------------------------------------------------
;;; 把2d或者3d点集转化为表                               	
;;;-------------------------------------------------------------
(defun MISC:PtList->List (Ptlst / l)
  (reverse
    (foreach p Ptlst
      (foreach x p
        (setq l (cons x l))
      )
    )
  )
)

;;;-------------------------------------------------------------
;;; 把表转化为2d或者3d点集                               	
;;;-------------------------------------------------------------
(defun MISC:List->PtList (lst dim / l p x)
  (while lst 
    (setq p nil)
    (repeat dim
      (setq p (cons (car lst) p))
      (setq lst (cdr lst))
    )
    (setq l (cons (reverse p) l))
  )
  (reverse l)
)

;;;-------------------------------------------------------------
;;; 把表转化为变量                                       	
;;;-------------------------------------------------------------
(defun MISC:List->Variant (Ptlst / lst dim arr)         	; allocate space for an array of 2d or 3d points stored as doubles
  (setq lst (MISC:PtList->List Ptlst))
  (setq dim (cons 0 (1- (length lst))))
  (setq arr (vlax-make-safearray vlax-vbDouble dim))    	; array dimension, element type is vlax-vbDouble
  (vlax-make-variant (vlax-safearray-fill arr lst))     	; return array variant
)

;;;-------------------------------------------------------------
;;; 变量转化为表                                         	
;;;-------------------------------------------------------------
(defun MISC:Variant->List (var)
  (vlax-safearray->list (vlax-variant-value var))
)

;;;-------------------------------------------------------------
;;;测试用函数(benchMark function)                               
;;;-------------------------------------------------------------
(defun BenchCommon (Times Expressions / s)
  (defun Princ-Column (l value / s)
    (setq s (vl-princ-to-string value))
    (princ s)
    (repeat (- l (strlen s))
      (princ " ")
    )
  )
  (defun Print-Result (lst)
    (princ "\n")
    (mapcar 'princ-Column '(25 10 15 16) lst)
  )
  (foreach Func Expressions 
    (setq S (cons (BenchMark Func Times) S))
  )
  (princ "\nStatement                Times     Elapse(ms)     Average(ms/time)")
  (princ "\n------------------------------------------------------------------")
  (setq s (vl-sort s (function (lambda (a b) (< (caddr a) (caddr b))))))
  (mapcar 'Print-Result s) 
  (gc)
  s
)

;;;-------------------------------------------------------------
;;;方式一：采用函数名加参数的方式测试，不出错，但不方便         
;;;-------------------------------------------------------------
(defun UTI:Bench (Times Expressions / s)
  (defun Benchmark (Func times / TIME0 TIME1 Speed Value fName)
    (setq fName (car Func))
    (setq TIME0 (getvar "millisecs"))
    (repeat times
      (setq Value (apply fName (cdr func)))
    )
    (setq TIME1 (getvar "millisecs"))
    (setq TIME1 (- TIME1 TIME0 0.0))
    (setq Speed (/ TIME1 times))
    (list fName times TIME1 Speed Value)
  )
  (BenchCommon Times Expressions)
)

;;;-------------------------------------------------------------
;;;方式二：采用eval测试语句，方便，但偶尔出错                   
;;;-------------------------------------------------------------
(defun MISC:Test (Times Expressions / s)
  (defun Benchmark (Func times / TIME0 TIME1 Speed Value FName)
    (setq fName (car Func))
    (setq TIME0 (getvar "TDUSRTIMER"))
    (repeat times
      (setq Value (eval Func))
    )
    (setq TIME1 (getvar "TDUSRTIMER"))
    (setq TIME1 (* (- TIME1 TIME0) 86400000))
    (setq Speed (/ TIME1 times))
    (list fName times TIME1 Speed Value)
  )
  (BenchCommon Times Expressions)
)

;|用法示例：
两个函数：(fun1 a b c) ，(fun2 a b)
测试1000次
它们的测试语句分别如下：
(UIT:BENCH 1000
  (list
    (list 'fun1 a b c)
    (list 'fun2 a b)
  )
)
(MISC:Test 1000 '((fun1 a b c) (fun2 a b)))

;;;已知直线上一点的Z数值，求这点的坐标
;;;输入: p1,p2,空间直线上的两点，Z，要求的点的Z坐标
;;;输出: 如果存在则输出该点坐标，否则nil
(defun Geo:GetPosFromZ (p1 p2 z / k)
  (setq k (- (/ (- z (caddr p1) 0.0) (- z (caddr p2)))))
  (if (/= k -1)
    (mapcar (function (lambda (x1 x2) (/ (+ x1 (* k x2)) (+ 1.0 k)))) p1 p2)
  )
)

;|*************************************************************;
;;;以下为测试所用，大家可各取所需                        	
;;;************************************************************;

(defun c:testPerpendicularFoot (/ p p0 normal)
  (setq p '(1 2 43))
  (setq p0 (getvar 'ucsorg))
  (setq Normal (trans '(0 0 1) 1 0 T))
  
  (misc:test 10001
    '((PLANE:Perpendicular_Foot p p0 normal)
      (PLANE:Perpendicular_Foot_2 p p0 normal)
    )
  )
)

;;;测试坐标变换函数Mat:TransU2W和TransW2U
(defun C:TestTransU2W (/ x y s e d p q v)
  (initget 1)
  (setq x (getdist "\nX:"))
  (initget 1)
  (setq y (getdist "\nY:"))
  (initget 1)
  (setq s (ssget ":S" '((0 . "LINE"))))
  (if (and x y s)
    (progn
      (setq e (ssname s 0))
      (setq d (entget e))
      (setq p (cdr (assoc 10 d)))
      (setq q (cdr (assoc 11 d)))
      (setq v (MAT:TransU2W (list x y) p (mapcar '- q p)))
      (Ent:Make_Point V)
    )
  )
)

;;;测试旋转函数GEO:Rot2d
(defun c:TestRot2d ( / pt pb an)
  (initget 1)
  (setq pt (getpoint "\n要旋转的点:"))
  (initget 1)
  (setq pb (getpoint "\n基点:"))
  (initget 1) 
  (setq an (getangle "\n角度:"))
  (ent:make_point pt)
  (ent:make_point pb)
  (ent:make_point (GEO:Rot2d Pt Pb an))
  (princ)
)

;;;测试镜像函数
(defun c:TestMirror (/ p1 p2 pt s)
  (initget 1)
  (setq p1 (getpoint "\n1:"))
  (initget 1)
  (setq p2 (getpoint "\n2:"))
  (initget 1) 
  (setq pt (getpoint "\n要镜像的点:"))
  (grdraw p1 p2 1)
  (setq s (MiSC:Test 10000
                     '((GEO:Mirror2D pt p1 (angle p1 p2))
                       (GEO:Mirror3D Pt p1 p2)
                       (GEO:Mirror2D-1 Pt p1 p2)
                      )
          )
  )
  (mapcar 'Ent:Make_Line (list pt pt pt) (mapcar 'last s))
  (princ)
)



(defun c:tttt()
  (initget 9)
  (setq p1 (getpoint "\n第一点:"))
  (initget 9)
  (setq p2 (getpoint "\n第二点:"))
  (initget 9)
  (setq p3 (getpoint "\n第三点:"))
  (setq v1 (mapcar '- p2 p1))
  (setq v2 (mapcar '- p3 p1))
  (setq x1 (car v1))
  (setq x2 (car v2))
  (setq y1 (cadr v1))
  (setq y2 (cadr v2))
  (princ "\n角度是:")
  (princ (atan (- (* x1 y2) (* x2 y1)) (+ (* x1 x2) (* y1 y2))))
  (princ)
)
  

;;;测试垂足和垂距函数
(defun C:LPF(/ p1 p2 pt f a b c s)
  (initget 1)
  (setq p1 (getpoint "\n直线端点1:"))
  (initget 1)
  (setq p2 (getpoint "\n直线端点2:"))
  (initget 1)
  (setq pt (getpoint "\n要求的点p:"))

  (setq f (LINE:Equation p1 p2))
  (setq A (car f))
  (setq B (cadr f))
  (setq C (caddr f))

  (setq S (MISC:Test 10000 
                '((LINE:Perpendicular_Foot pt p1 p2)
                  (LINE:Perpendicular_Foot_1 pt p1 p2)
                  (LINE:Perpendicular_Foot_2 pt p1 p2)
                  (LINE:Perpendicular_Foot_3 pt p1 p2)
                 )
          )
  )

  (grdraw p1 p2 1)
  (Ent:Make_Point pt)
  (Ent:MakePoint-1 (cadr (last (car  s))) 1)
  (Ent:MakePoint-1 (last (cadr s)) 2)
  (Ent:MakePoint-1 (last (caddr s)) 3)
  (Ent:MakePoint-1 (last (cadddr s)) 4)
  
  (setq S (MISC:Test 10000
                '((LINE:Perpendicular_Foot pt p1 p2)
                  (LINE:Perpendicular_Distance_1 pt A B C)
                  (LINE:Perpendicular_Distance_2 pt P1 p2)
                  (LINE:Perpendicular_Distance_3 Pt P1 P2)
                  (MAT:TransW2U pt P1 (mapcar '- p2 p1)))
          )
  )
                        
  (princ (mapcar 'last s))
  (princ)
)

;;;测试线段相交函数
(defun C:Inters (/ p1 p2 p3 p4 s)
  (initget 1)
  (setq p1 (getpoint "\n1:"))
  (initget 1)
  (setq p2 (getpoint p1 "\n2:"))
  (initget 1)
  (setq p3 (getpoint "\n3:"))
  (initget 1)
  (setq p4 (getpoint p3 "\n4:"))

  (grdraw p1 p2 1)
  (grdraw p3 p4 2)
  (setq s (MISC:Test 100000
                     '((LINE:Intersection p1 p2 p3 p4)
                       (inters p1 p2 p3 p4 nil)
                      )
          )
  )
  (foreach p (mapcar 'last s)
    (Ent:make_Point p)
  )
)

;;;测试角平分线函数
(defun c:pf(/ e1 e2 d1 d2 p1 p2 p3 p4 ret)
  (setq e1 (car (entsel "\n直线1:")))
  (setq e2 (car (entsel "\n直线2:")))
  (setq d1 (entget e1))
  (setq d2 (entget e2))
  (setq p1 (cdr (assoc 10 d1)))
  (setq p2 (cdr (assoc 11 d1)))
  (setq p3 (cdr (assoc 10 d2)))
  (setq p4 (cdr (assoc 11 d2)))
  (setq ret (LINE:Angular_Bisector p1 p2 p3 p4))
  (foreach n ret
    (apply 'Ent:Make_line n)
  )
)

;;;测试偏移两点函数LINE:Offset
(defun C:LineOffset (/ p1 p2 d)
  (initget 1)
  (setq p1 (getpoint "\n1:"))
  (initget 1)
  (setq p2 (getpoint p1 "\n2:"))
  (initget 1)
  (setq d (getdist p1 "\n偏移距离:"))
  (Ent:make_line p1 p2)
  (apply 'Ent:make_line (LINE:Offset p1 p2 d))
  (princ)
)

;;;测试共线检测函数LINE:Colinearity,LINE:Colinearity_1
(defun C:Colinearity (/ p1 p2 p3)
  (setq eps 1e-6)
  (setq p1 (getpoint "\n1:"))
  (setq p2 (getpoint "\n2:"))
  (setq p3 (getpoint "\n3:"))
  (MISC:Test 100000
             '((LINE:Colinearity p1 p2 p3)
               (LINE:Colinearity3D p1 p2 p3)
              )
  )
  (princ)
)

;;;平面部分测试函数
(defun c:PlaneTest(/ pa pb p1 p2 p3 d1 d2 arg)
  (initget 1)
  (setq pa (getpoint "\npa:"))
  (setq pa (trans pa 1 0))
  (initget 1)
  (setq pb (getpoint "\npb:"))
  (setq pb (trans pb 1 0))
  
  (initget 1)
  (setq p1 (getpoint "\n1:"))
  (setq p1 (trans p1 1 0))
  (initget 1)
  (setq p2 (getpoint "\n2:"))
  (setq p2 (trans p2 1 0))
  (initget 1)

  (setq p3 (getpoint "\n3:"))
  (setq p3 (trans p3 1 0))
  
  (mapcar 'Ent:make_Point (list pa pb p1 p2 p3))
  
  (princ (PLANE:Distance_1 Pa p1 p2 p3))
  
  (setq d1 (PLANE:Perpendicular_Foot Pa p1 p2 p3))
  (setq d2 (PLANE:Perpendicular_Foot Pb p1 p2 p3))
  (setq arg (cons pa (cons Pb (PLANE:Equation_3P p1 p2 p3))))
  (setq ret (apply 'PLANE:Line_Inters_Plane arg))
  (Ent:make_Point (cadr d1))
  (Ent:make_Point (cadr d2))
  (Ent:make_Point ret)
  (princ (LINE:Distance_LineToLine pa pb p1 p2))
  (princ)
)

;;;三线坐标系统测试
(defun C:InfoBy3Sides (/ p1 p2 p3 a b c ret)
  (initget 1)
  (setq p1 (getpoint "\n1:"))
  (initget 1)
  (setq p2 (getpoint "\n2:"))
  (initget 1)
  (setq p3 (getpoint "\n3:"))
  (setq p1 (trans p1 1 0))
  (setq p2 (trans p2 1 0))
  (setq p3 (trans p3 1 0))
  (setq a  (distance p2 p3))
  (setq b  (distance p3 p1))
  (setq c  (distance p1 p2))
  (Ent:make_Poly (list p1 p2 p3))
  (setq ret (TRI:InfoBy3Sides a b c))
  (princ ret)
  (foreach n (cddr ret)
    (setq p (TRI:TCS->WCS (car n) p1 p2 p3))
    (Ent:make_Point p)
    (if (setq r (cadr n))
      (Ent:make_Circle p r)
    )
  )
  (princ)
)

;;;测试垂足是否在线段之间
(defun c:ppp ()
  (initget 1)
  (setq p0 (getpoint "\n测试点"))
  (initget 1)
  (setq p1 (getpoint "\n第一点"))
  (initget 1)
  (setq p2 (getpoint "\n第二点"))
  (if (p-t p0 p1 p2)
    (princ "\n亲，在!!!")
    (princ "\n亲，不在哦!!!!!")
  )
  (if (LINE:P-t p0 p1 p2)
    (princ "\n亲，在!!!")
    (princ "\n亲，不在哦!!!!!")
  )
  (if (LINE:P-t-2 p0 p1 p2)
    (princ "\n亲，在!!!")
    (princ "\n亲，不在哦!!!!!")
  )
  (ent:make_line p1 p2)
  (ent:make_point p0)
  (misc:test
    101
    '((LINE:P-t p0 p1 p2)
      (LINE:P-t-1 p0 p1 p2)
      (LINE:P-t-2 p0 p1 p2)
      (p-t p0 p1 p2)
      (p-t-1 p0 p1 p2)
     )
  )
  (princ)
)

;;;Test for "POLY:Info_LWPoly" "Geo:Centroid" "POLY:Area" "POLY:Perimeter" "POLY:Infomation"
;;;为段线的质心和面积的测试
(defun C:CentroidTest (/ sel ent en1 dxf pts cen aaa len ret i)
  (setq i -1)
  (setq sel (ssget '((0 . "*POLYLINE"))))
  (if sel
    (repeat (sslength sel)
      (setq ent (ssname sel (setq i (1+ i))))
      (setq obj (vlax-ename->vla-object ent))
      (setq dxf (entget ent))
      (if (= (cdr (assoc 0 DXF)) "POLYLINE")
        (setq pts (MISC:List->PtList (vlax-get obj 'coordinates) 3)
              Cen (GEO:Centroid pts)
              aaa (POLY:Area pts)
              len (POLY:Perimeter pts T)
              ret (POLY:Infomation pts)
              en1 (Ent:MakePoint-1 Cen 2)       
        )
        (setq ret (POLY:Info_LWPoly ent)
              aaa (vla-get-area obj)
        )
      )
      (setq cen (car ret))
      (setq len (vla-get-length obj))
      (Ent:MakePoint-1 cen 1)
      (princ (strcat "\n第" (itoa i) "个物体信息: "))
      (princ (list ret Cen aaa len))
      (princ)
    )
  )
)

;;;弧段的质心和面积的测试
(defun C:TestArcCentroid (/ A1 A2 C R E1 I O1 O2 O3 O4 P1 P2 S1 S2 SS V3 V4)
  (setq i -1)
  (if (setq ss (ssget '((0 . "ARC"))))
    (repeat (sslength ss)
      (setq e1 (ssname ss (setq i (1+ i))))
      (setq o1 (vlax-ename->vla-object e1))

      (setq C (vlax-get o1 'Center))
      (setq R (vla-get-radius o1))
      (setq A1 (vla-get-startangle o1))
      (setq A2 (vla-get-endangle o1))

      (setq V3 (CIR:Circular_Segment C R A1 A2 nil))    ;圆弧总是逆时针的
      (setq V4 (CIR:Circular_Sector C R A1 A2 nil))     ;圆弧总是逆时针的

      (setq p1 (vlax-curve-getstartpoint e1))           ;弧起点
      (setq p2 (vlax-curve-getendPoint e1))             ;弧终点
      (setq s1 (ssadd e1))
      (setq s1 (ssadd (Ent:Make_Line p1 p2) S1))

      (setq o2 (vla-copy o1))                           ;拷贝圆弧用来测试扇形
      (setq s2 (ssadd (vlax-vla-object->ename o2)))
      (setq s2 (ssadd (Ent:Make_Line p1 C) s2))
      (setq s2 (ssadd (Ent:Make_Line p2 C) s2))

      (command "region" s1 "")                          ;弓形计算与建模做比较
      (setq o3 (vlax-ename->vla-object (entlast)))
      (command "region" s2 "")                          ;扇形计算与建模做比较
      (setq o4 (vlax-ename->vla-object (entlast)))

      (Ent:MakePoint-1 (car V3) 1)                      ;计算出来的弓形质心
      (Ent:MakePoint-1 (car V4) 2)                      ;计算出来的扇形质心
      (Ent:MakePoint-1 (vlax-get o3 'centroid) 3)       ;弓形建模的质心
      (Ent:MakePoint-1 (vlax-get o4 'centroid) 4)       ;扇形建模的质心

      (princ (list V3 (vla-get-area O3) (vla-get-perimeter O3)))
      (princ (list V4 (vla-get-area O4) (vla-get-perimeter O4)))
      (princ)
    )
  )
)
;;;测试3点的行列式
(defun c:ttt()
  (initget 1)
  (setq p1 (getpoint "\n1:"))
  (initget 1)
  (setq p2 (getpoint p1 "\n2:"))
  (initget 1)
  (setq p3 (getpoint "\n3:"))

  (setq s (MISC:Test 100000
                     '((TRI:Det3p p1 p2 p3))
          )
  )
  (princ (mapcar 'last s))
  (princ)
)
(defun C:PPP()
  (setq pts nil)
  (while (setq p (getpoint "\n输入点:"))
    (setq pts (cons p pts))
  )
  (Ent:Make_Poly pts nil)
)
 
;;;获取截面的质心
(defun C:GetRegionCentroid (/ sel ent obj i)
  (setq i -1)
  (if (setq sel (ssget '((0 . "REGION"))))
    (repeat (sslength sel)
      (setq ent (ssname sel (setq i (1+ i))))
      (setq obj (vlax-ename->vla-object ent))
      (Ent:MakePoint-1 (vlax-get obj 'Centroid) 3)
    )
  )
)
(defun c:mean (/ p0 p1 p2 an dd)
  (initget 1)
  (setq p0 (getpoint "\n原点:"))
  (initget 1)
  (setq p1 (getpoint "\n第一点:"))
  (initget 1)
  (setq p2 (getpoint "\n第二点:"))
  (setq an (angle p1 p2))
  (setq dd (sqrt (* (distance p0 p1) (distance p0 p2))))
  (ent:make_point (polar p0 an dd))
  (ent:make_point (polar p0 an (- dd)))
  (princ)
)
;;;测试中位点
(defun c:fff ()
  (command "undo" "BE")
  (initget 1)
  (setq A (getpoint "\n输入第一点:"))
  (initget 1)
  (setq B (getpoint "\n输入第二点:"))
  (initget 1)
  (setq P (getpoint "\n输入第三点:"))
  (initget 1)
  (setq Q (getpoint "\n输入第四点:"))
  ;(mapcar 'ent:make_point (list A B P Q))
;;;  (foreach n (geo:cen_foci_1 a b p q)
;;;    (mapcar 'ent:make_point n)
;;;  )
  (mapcar 'ent:make_point (geo:cen_foci a b p q))
  (command "undo" "E")
  (princ)
)

(defun c:ppp (/ pts )
  (initget 1)
  (setq p (getpoint "\n点:"))
  (setq sel (ssget ":S" '((0 . "*POLYLINE"))))
  (if (and p sel)
    (progn
      (setq ent (ssname sel 0))
      (setq num (vlax-curve-getEndParam ent))
      (setq i 0)
      (repeat (fix num)
        (setq pt (vlax-curve-getPointAtParam ent i))
        (setq pts (cons pt pts))
        (setq i (1+ i))
      )
      (setq pts (reverse pts))
      (setq ret (ALG:Inside-p p pts))
      (if (ALG:Inside-p P pts )
	(princ "yes!")
	(princ "No!")
      )
    )
  )
)
(defun c:mm ()
  (initget 1)
  (setq L1 (getpoint "\n直线第一点:"))
  (initget 1)
  (setq L2 (getpoint L1 "\n直线第二点:"))
  (initget 1)
  (setq L3 (getpoint "\n直线第三点:"))
  (initget 1)
  (setq L4 (getpoint L3 "\n直线第四点:"))

  (ent:make_line L1 L2)
  (ent:make_line L3 L4)
  (setq L1 (list L1 (mapcar '- L2 L1)))
  (setq L2 (list L3 (mapcar '- L4 L3)))
  (apply 'ent:make_line (Line:MinDist  L1 L2))
  (princ)
)
(defun c:tttt()
  (setq sel (ssget '((0 . "3DFACE"))))
  (if (and sel (> (sslength sel) 1))
    (progn
      (setq en1 (ssname sel 0))
      (setq en2 (ssname sel 1))
      (setq d1  (entget en1))
      (setq d2  (entget en2))
      (setq plane1 (mapcar 'assoc '(10 11 12) (list d1 d1 d1)))
      (setq plane2 (mapcar 'assoc '(10 11 12) (list d2 d2 d2)))
      (setq plane1 (mapcar 'cdr plane1))
      (setq plane2 (mapcar 'cdr plane2))
      (setq ang (plane:angle plane1 plane2))
      (princ "\n两平面的夹角是:")
      (princ ang)
    )
  )
  (princ)
)
;|;
