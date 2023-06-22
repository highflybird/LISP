;|*************************************************************;
软件作者: Highflybird                                          ;
软件用途: 为AutoCAD 的LISP定制的一些算法和函数(圆和弧部分)     ;
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

;;;-------------------------------------------------------------
;;;判断点跟圆的位置关系                                 	
;;;输入：Pt,要判断的点，Cen,圆心，Rad半径               	
;;;输出：0 为在圆上，-1为在圆内，1为在圆外              	
;;;-------------------------------------------------------------
(defun CIR:GetPos (Pt Cen Rad / d)
  (setq d (distance cen pt))
  (cond
    ( (equal d Rad 1e-8) 0)
    ( (< d Rad) -1)
    ( t 1)
  )
)

;;;-------------------------------------------------------------
;;;判断两点对某个圆是否异侧                             	
;;;-------------------------------------------------------------
(defun CIR:Is_Opposite_Side (rad d1 d2)
  (or (and (> d1 rad) (< d2 rad)) (and (< d1 rad) (> d2 rad)))
)

;;;-------------------------------------------------------------
;;;判断两点对某个圆是否同侧                             	
;;;-------------------------------------------------------------
(defun CIR:Is_Same_Side (rad d1 d2)
  (or (and (> d1 rad) (> d2 rad)) (and (< d1 rad) (< d2 rad)))
)

;;;-------------------------------------------------------------
;;;This is known as the "secant-tangent theorem",       	
;;;"intersecting chords theorem",   "Secant Theorem"   		
;;;or the "power-of-a-point theorem"                    	
;;;割弦定理                                             	
;;;输入: 在圆上的一点Pc，一给定点P，给定圆圆心C和半径R  	
;;;输出: 经过定点Pt和PC的弦的另一点                     	
;;;-------------------------------------------------------------
(defun CIR:Intersecting_Chords (Pc P C R / d L eps)
  (setq eps 1e-6)
  (setq d (distance P C))
  (if (equal d R eps)
    P
    (polar P (angle P Pc) (/ (* (+ d R) (- d R)) (distance P Pc)))
  )
)


;;;-------------------------------------------------------------
;;;求直线和圆的交点                                     	
;;;输入：圆心，半径和直线的两个端点                     	
;;;输出：直线与圆的交点集(nil,一个点或者两个点)         	
;;;-------------------------------------------------------------
(defun CIR:Inters_Circle_Line (Cen rad PtA PtB / p d L A eps)
  (setq eps 1e-6)
  (setq p (Line:Perpendicular_Foot Cen PtA PtB))        	;圆心到直线的垂足
  (setq d (abs (car p)))                                	;垂距
  (setq p (cadr p))                                     	;垂足
  (if (equal d Rad eps)                                 	;如果垂距等于半径
    (list p)                                            	;相切
    (if (< d Rad)                                       	;如果垂距小于半径
      (progn 
        (setq L (sqrt (* (+ Rad d)(- Rad d))))          	;半弦长
        (setq A (angle PtA PtB))                        	;直线的与X轴线的交角
        (list (polar p a (- L)) (polar p a L))          	;有两个交点
      )
    )
  )
)

;;;-------------------------------------------------------------
;;;直线和圆的交点，为CIR:LCR相切圆修改                	        
;;;输入：圆心，半径，角度方向，与圆心的距离             	
;;;输出：直线与圆的交点集(nil,一个点或者两个点)         	
;;;-------------------------------------------------------------
(defun CIR:Inters_Circle_Line_1 (cen rad An Dist / d p l s)
  (setq d (abs dist))
  (if (equal d Rad 1e-6)
    (list (polar cen (- an (* pi 0.5)) Dist))           	;相切
    (if (< d Rad)
      (setq p (polar cen (- an (* pi 0.5)) Dist)        	;垂足
            L (sqrt (* (+ rad d)(- rad d)))             	;半弦长
            S (list (polar p an (- L))
                    (polar p an L)
              )
      )
    )
  )
)

;;;-------------------------------------------------------------
;;;圆和圆的交点                                         	
;;;输入：两个圆（用圆心和半径表示）                     	
;;;输出：两个圆的交集（nil，一个点或者两个点）          	
;;;-------------------------------------------------------------
(defun CIR:Inters_Circle_Circle (c1 r1 c2 r2 / eps m n L d H A B)
  (setq eps 1e-6)
  (setq m (+ r2 r1))
  (setq n (- r2 r1))
  (setq L (distance c1 c2))
  (cond
    ( (equal L 0 eps) nil)                              	;圆心重合，无解或无穷解
    ( (equal m l eps)                                   	;两个圆外切
      (list (polar c1 (angle c1 c2) r1))                
    )
    ( (equal (abs n) l eps)                             	;内切
      (if (< n 0)
        (list (polar C2 (angle c2 c1) (- r2)))
        (list (polar C2 (angle c2 c1) r2))
      )
    )
    ( (and (> m L) (< (abs n) L))                       	;相交
      (setq d (+ (* r1 r1) (* (+ L r2) (- L r2))))      	;根据余弦定理
      (setq d (/ d (+ L L)))
      (setq H (sqrt (* (+ r1 d) (- r1 d))))             	;交点与圆心连线垂直距离
      (setq A (angle c1 c2))                            	;圆心连线角
      (setq B (atan h d))                               
      (list (polar c1 (- A B) r1)                       	;有两个交点
            (polar c1 (+ A B) r1)
      )
    )
  )
)

;;;-------------------------------------------------------------
;;;圆外的一点对圆形的切点                               	
;;;输入: 给定圆的圆心和半径和圆外的一点Pt               	
;;;输出: 这点Pt对圆的切点集(nil，一个或者两个)          	
;;;-------------------------------------------------------------
(defun CIR:Point_Tangent (cen rad pt / d l a b p q r eps)
  (setq eps 1e-8)
  (setq d (distance cen pt))
  (if (equal d rad eps)
    (list pt)
    (if (> d rad)
      (setq l (sqrt (* (+ d rad) (- d rad)))
            a (atan l rad)
            b (angle cen pt)
            p (polar cen (+ b a) rad)
            q (polar cen (- b a) rad)
            r (list p q)
      )
    )
  )
)

;;;-------------------------------------------------------------
;;;两个圆的公切线                                       	
;;;输入: 给定两个圆的圆心和半径                         	
;;;输出: 这两个圆的公切线(nil，一个点或者点对集合)      	
;;;-------------------------------------------------------------
(defun CIR:Common_Tangent (c1 r1 c2 r2 / a b d eps p q m n u v ret L)
  (setq d (distance c1 c2))
  (setq a (angle c1 c2))
  (setq eps 1e-8)
  (cond
    ( (equal d 0 eps) nil)                              	;圆心重合，无解或者无穷解
    ( (equal d (abs (setq b (- r1 r2))) eps)            	;内切
      (setq p (polar c1 a (Math:Sign_reversal r1 b)))   	;切点
      (list (list p (polar p (+ a (* pi 0.5)) 1000)))   	;内切线
    )
    ( (> d b)
      (foreach r (list b (+ r1 r2))                     	;考虑内外公切两种情况
        (setq l (* (+ d r) (- d r)))
        (cond
          ( (equal l 0 eps)                             	;两个圆外切
            (setq p (polar c1 a r1))
            (setq q (polar p (+ a (* pi 0.5)) 1000))   
            (setq ret (cons (list p q) ret))
          )
          ( (> l 0)
            (setq u (atan (sqrt l) r))
            (setq v (- a u))
            (setq u (+ a u))
            (setq p (polar C1 u r1))
            (setq q (polar C1 v r1))
            (if (eq r b)                 
              (setq m (polar c2 u r2)                   	;外公切线
                    n (polar c2 v r2)
              )
              (setq m (polar c2 u (- r2))               	;内公切线
                    n (polar c2 v (- r2))
              )
            )
            (setq ret (cons (list p m) ret))             
            (setq ret (cons (list q n) ret))
          )
        )
      )
      Ret
    )
  )
)

;;;-------------------------------------------------------------
;;;作给定半径并经过两点的圆                             	
;;;输入: 不重合的两点和半径                             	
;;;输出: 无解或无穷解为nil,否则用中心半径表示的圆的列表 	
;;;-------------------------------------------------------------
(defun CIR:PPR (P1 P2 R / D M H A eps)
  (setq eps 1e-6)
  (setq M (GEO:Midpoint P1 P2))
  (setq D (distance P1 M))
  (cond
    ( (equal D 0 eps) nil)                              	;两点重合，无穷解
    ( (equal D R eps)                                   	;两点为直径的两个端点
      (list (list M R))
    )
    ( (> R D)                                           	;满足两点距离小于给定直径
      (setq H (sqrt (* (+ R D) (- R D))))
      (setq A (+ (angle P1 P2) (* pi 0.5)))
      (list (list (polar M A H) R)                      	;有两解
            (list (polar M A (- H)) R)
      )
    )
  )
)

;;;-------------------------------------------------------------
;;;经过一点和一直线相切的具有给定半径的圆               	
;;;输入: 点和直线的两个端点以及给定的半径               	
;;;输出: 无解或无穷解为nil,否则用中心半径表示的圆的列表 	
;;;-------------------------------------------------------------
(defun CIR:PLR (Pt0 PtA PtB Rad / H P D A B X Y eps)
  (setq eps 1e-6)
  (setq P (Line:Perpendicular_Foot Pt0 PtA PtB))
  (setq H (abs (car P)))
  (setq P (cadr P))
  (setq D (+ Rad Rad))
  (cond
    ( (equal H D eps)
      (list (list (GEO:Midpoint  Pt0 P) Rad))
    )
    ( (equal H 0 eps)
      (setq A (+ (angle PtA PtB) (/ pi 2)))
      (list
        (list (polar Pt0 A Rad) Rad)
        (list (polar Pt0 A (- Rad)) Rad)
      )
    )
    ( (< H D)
      (setq Y (- H Rad))
      (setq X (sqrt (* (+ Rad Y) (- Rad Y))))
      (setq B (atan X Y))
      (setq A (angle Pt0 P))
      (list
        (list (polar Pt0 (+ A B) Rad) Rad)
        (list (polar Pt0 (- A B) Rad) Rad)
      )
    )
  )
)

;;;-------------------------------------------------------------
;;;和两直线相切的具有给定半径的圆(一般来说是两相交直线)  	
;;;输入: 两直线的四个端点以及给定的半径                 	
;;;输出: 无解或无穷解为nil,否则用中心半径表示的圆的列表 	
;;;-------------------------------------------------------------
(defun CIR:LLR (PtA PtB PtC PtD Rad / a1 a2 a3 a4 a5 L1 L2 Int)
  (setq int (inters PtA PtB PtC PtD nil))
  (if int
    (progn
      (setq a1 (angle pta ptb))
      (setq a2 (angle ptc ptd))
      (setq a3 (* (+ a1 a2) 0.5))
      (setq a4 (* (- a1 a2) 0.5))
      (setq a5 (+ a3 (* pi 0.5)))
      (setq L1 (/ Rad (sin a4)))
      (setq L2 (/ Rad (cos a4)))
      (list
        (list (polar int a3 L1) Rad)
        (list (polar int a3 (- L1)) Rad)
        (list (polar int a5 L2) Rad)
        (list (polar int a5 (- L2)) Rad)
      )     
    )
  )
)
 
;;;-------------------------------------------------------------
;;;和一直线及一定圆相切的具有给定半径的圆               	
;;;输入: 直线的两个端点，圆的圆心和半径以及给定的半径   	
;;;输出: 无解或无穷解为nil,否则用中心半径表示的圆的列表 	
;;;-------------------------------------------------------------
(defun CIR:LCR (PtA PtB C0 R0 R1 / d a ret)
  (setq d (Line:Perpendicular_Distance C0 PtA PtB))
  (setq A (angle PtA PtB))
  (foreach h (list (+ d r1) (- d r1))
    (foreach r (list (+ r0 r1) (abs (- r0 r1)))
      (foreach p (CIR:Inters_Circle_Line_1 C0 R A h)
        (setq ret (cons (list p r1) ret))
      )
    )
  )
  (reverse ret)
)

;;;-------------------------------------------------------------
;;;与两个圆相切的具有给定半径的圆                       	
;;;输入: 两个圆的圆心和半径以及给定的半径               	
;;;输出: 无解或无穷解为nil,否则用中心半径表示的圆的列表 	
;;;-------------------------------------------------------------
(defun CIR:CCR (C1 R1 C2 R2 R0 / ret)
  (foreach R (list (cons (+ R1 R0) (+ R2 R0))
                   (cons (+ R1 R0) (abs (- R2 R0)))
                   (cons (abs (- R1 R0)) (+ R2 R0))
                   (cons (abs (- R1 R0)) (abs (- R2 R0)))
             )            
    (foreach p (CIR:Inters_Circle_Circle C1 (car R) C2 (cdr R))
      (setq ret (cons (list p R0) Ret))
    )
  )
  ret   
)

;;;-------------------------------------------------------------
;;;通过一定点和一定圆相切的具有给定半径的圆             	
;;;输入: 点，圆的圆心和半径以及给定的半径               	
;;;输出: 无解或无穷解为nil,否则用中心半径表示的圆的列表 	
;;;-------------------------------------------------------------
(defun CIR:PCR (P0 C0 R0 R1 / ret)
  (foreach r (list (+ r0 r1) (abs (- r0 r1)))
    (foreach p (CIR:Inters_Circle_Circle p0 r1 c0 r)
      (setq ret (cons (list p R1) ret))
    )
  )
  ret   
)

;;;-------------------------------------------------------------
;;;求三角形外心   TRI:CircumCenter，Or Called ExCenter  	
;;;尽管这样写很麻烦，显得代码很多，但运行却很快         	
;;;输入: 给定不共线的三个点                             	
;;;输出: 三点的外接圆(圆心和半径表示),nil表示三点共线   	
;;;-------------------------------------------------------------
(defun CIR:PPP (P0 P1 P2 / X0 Y0 X1 Y1 X2 Y2 DX1 DY1 DX2 DY2 D 2D C1 C2 CE)
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
;;;三角形内心  Tri:InCenter                             	
;;;(aX1+bx2+cx3)/(a+b+c)  (aY2+bY2+CY3)/(a+b+c)         	
;;;输入: 给定三个点                                     	
;;;输出: 三点的内切圆(圆心和半径表示)                   	
;;;-------------------------------------------------------------
(defun Tri:InCenter (pa pb pc / a b c L I r)
  (setq a (distance pb pc))
  (setq b (distance pc pa))
  (setq c (distance pa pb))
  (setq L (+ a b c))
  (if (/= L 0.0)
    (setq I (MAT:SxVs (list pa pb pc) (list (/ a L) (/ b L) (/ c L)))
          R (list I (abs (Line:Perpendicular_Distance I pa pb)))
    )
    (list pa 0)
  )
)

;;;-------------------------------------------------------------
;;;跟三条直线相切的圆形                                 	
;;;输入: 给定三条直线                                   	
;;;输出: 与这三条直线都相切的圆（内切圆和旁切圆）       	
;;;-------------------------------------------------------------
(defun CIR:LLL (P1a P1b P2a P2b P3a P3b / bs1 bs2 ret int rad)
  (setq bs1 (LINE:Angular_Bisector P1a p1b p2a p2b))            ;角平分线1
  (setq bs2 (LINE:Angular_Bisector P2a p2b p3a p3b))            ;角平分线2
  (foreach p bs1
    (foreach q bs2
      (setq int (inters (car p) (cadr p) (car q) (cadr q) nil))
      (if int							;如果角平分线相交
        (setq rad (LINE:Perpendicular_Distance int p1a p1b)     ;求出交点到任意线段距离即半径
              ret (cons (list int (abs rad)) ret)               ;加入到解集中
        )
      )
    )
  )
  ret    
)

;;;-------------------------------------------------------------
;;;点点切线圆,求通过两定点，和一直线相切的圆            	
;;;输入: P1,P2,通过的点,PA,PB直线上的两点。             	
;;;输出: 符合条件的圆的集合(nil，一个或多个圆的集合)    	
;;;-------------------------------------------------------------
(defun CIR:PPL (P1 P2 PA PB / AN D1 D2 P3 DD INT C1 C2 L1 L2 Mid NRM T1 T2 RT eps)
  (setq eps 1e-6)
  (if (and (not (equal p1 p2 eps))(Line:IsSameSide P1 P2 PA PB));同侧且不重合
    (progn
      (setq int (inters p1 p2 pa pb nil))
      (setq nrm (MAT:Rot90 (mapcar '- pa pb)))                  ;垂直PAPB的矢量
      (setq Mid (GEO:Midpoint p1 p2))                           ;P1P2的中点
      (if int
        (progn
          (setq l1 (distance int p1))
          (setq l2 (distance int p2))
          (setq P3 (GEO:Rot90 Mid p1 p2))
          (if (or (equal int p1 eps) (equal int p2 eps))        ;如果P1或P2跟交点重合则交点为切点
            (setq C1 (inters Mid P3 int (mapcar '+ int nrm) nil);两点的垂直平分线跟交点处的垂线的交点
                  RT (list (list C1 (distance C1 P1)))          ;为圆心，半径自然确定了。
            )
            (setq an (angle pA pB)                                     
                  dd (sqrt (* l1 l2))                           ;利用割弦定理得到交点到切点距离
                  t1 (polar int an dd)                          ;切点1
                  t2 (polar int an (- dd))                      ;切点2
                  C1 (inters Mid P3 t1 (mapcar '+ t1 nrm) nil)  ;两点的垂直平分线跟过切点1的垂线的交点
                  C2 (inters Mid P3 t2 (mapcar '+ t2 nrm) nil)  ;两点的垂直平分线跟过切点2的垂线的交点
                  RT (list (list C1 (distance C1 P1))           ;画圆1
                           (list C2 (distance C2 P2))           ;画圆2
                     )
            )
          )
        )
        (if (setq t1 (inters Mid (mapcar '+ Mid nrm) PA PB nil));两点跟直线平行的情况
          (list (CIR:PPP p1 p2 t1))
        )
      )
    )
  )
)

;;;-------------------------------------------------------------
;;;点线线切圆，求通过某定点并与两条直线相切的圆         	
;;;输入: P,通过的点,PA,PB,PC,PD两直线上的四个端点。     	
;;;输出: 符合条件的圆的集合(nil，一个或多个圆的集合)    	
;;;-------------------------------------------------------------
(defun CIR:PLL-1 (p pA pB Int / H L X r1 r2 an)         	;特殊情况下的处理P点在角平分线上
  (setq H (Line:Perpendicular_Distance p PA PB))
  (setq H (abs H))
  (setq L (distance p Int))
  (setq X (* H L))
  (setq r1 (/ X (+ L H)))
  (setq r2 (/ X (- L H)))
  (setq an (angle P int))
  (list (list (polar P an r1) r1)
        (list (polar p an (- r2)) r2)
  )
)
(defun CIR:PLL (P pA pB pC pD / A A1 A2 D D1 D2 EPS H INT L P1 P2 PM R)
  (setq eps 1e-6)
  (if (setq int (inters pA pB pC pD nil))               	;判断两直线是否相交
    (if (equal int P eps)                               	;如果交点跟定点重合
      (list (list int 0))                               	;视作半径为0
      (progn
        (if (equal int PA eps) (setq p1 PA PA PB PB P1))
        (if (equal int PC eps) (setq p2 PC PC PD PD P2))
        (setq D1 (Line:Perpendicular_Distance p int PA))	;定点到直线1距离
        (setq D2 (Line:Perpendicular_Distance p int PC))	;定点到直线2距离
        (setq a1 (angle int PA))                 
        (setq a2 (angle int PC))                 
        (setq a1 (* (+ a1 a2) 0.5))                     	;内角平分线
        (setq a2 (+ a1 (* pi 0.5)))                     	;外角平分线
        (setq p1 (GEO:Mirror2D P int a1))               	;对内角平分线的镜像点
        (setq p2 (GEO:Mirror2D P int a2))               	;对外角平分线的镜像点
        (if (or (equal p1 P eps) (equal p2 P eps))      	;如果跟角平分线重合
          (CIR:PLL-1 p pA pB Int)                       	;按特殊情况处理
          (if (or (equal d1 0 eps) (equal d2 0 eps))    	;如果定点在一条直线上
            (append                                     	;则内外角平分线都要考虑
              (CIR:PPL P p1 PA PB)                      
              (CIR:PPL P p2 PA PB)
            )   
            (if (MATH:Same_Sign d1 d2)                  	;剩下的转化为CIR:PPL问题
              (CIR:PPL P p2 PA PB)                      	;同号是外角平分线的镜像点
              (CIR:PPL P p1 PA PB)                      	;异号是内角平分线的镜像点
            )
          )
        )
      )
    )
    (progn                                              	;以下是为平行的两直线考虑
      (setq D1 (Line:Perpendicular_Foot p PA PB))       	;定点到直线的距离
      (setq D2 (Line:Perpendicular_Foot p PC PD))       	;定点到直线的距离
      (setq D  (Line:Perpendicular_Distance PA PC PD))  	;两平行直线的距离
      (setq D  (abs D))
      (setq P1 (cadr D1))
      (setq P2 (cadr D2))
      (setq D1 (abs (car D1)))
      (setq D2 (abs (car D2)))
      (setq pM (GEO:Midpoint P1 P2))                    	;两垂足的中点
      (cond
        ( (or (equal D1 0 eps) (equal D2 0 eps))        	;如果定点在某一直线上
          (list (list PM (* D 0.5)))                    	;两垂足的中点是圆心
        )
        ( (equal D (+ d1 d2) eps)                       	;定点只有在两条直线之间才有解
          (setq R (* D 0.5))                            	;半径是两平行直线距离的一半
          (setq H (- d1 R))
          (setq L (sqrt (- (* R R) (* H H))))
          (setq A (angle PA PB)) 
          (list (list (polar PM A L) R)                 	;此时有两个解
                (list (polar pM A (- L)) R)
          )
        )
      )
    )
  )
)


;;;-------------------------------------------------------------
;;;点点切的圆(求通过两不相同的点并与一个圆相切的圆)     	
;;;输入: 两定点和给定的一个圆的圆心及半径               	
;;;输出: 符合条件的圆的集合(nil，一个或多个圆的集合)    	
;;;-------------------------------------------------------------
(defun CIR:PPC (p1 p2 cen rad / PA PB pC PD PM RT an d1 d2 dd eps)
  (setq eps 1e-6)
  (setq dd (distance p1 p2))
  (setq d1 (distance cen p1))
  (setq d2 (distance cen p2))
  (setq pM (GEO:Midpoint p1 p2))
  (setq an (+ (angle p1 p2) (/ pi 2))) 
  (cond
    ( (equal dd 0 eps) nil)                     
    ( (or (equal d1 rad eps) (equal d2 rad eps))
      (if (equal d1 rad eps)
        (setq PA p1 PB p2)
        (setq PA p2 PB p1)
      )
      (setq pC (polar PM an rad))
      (setq pD (inters pM PC PA CEN nil))
      (if PD 
        (list (list pD (distance pD p1)))
      )
    )
    ( (equal d1 d2 eps)
      (setq pA (polar cen an rad))
      (setq pB (polar cen an (- rad)))
      (foreach p (list PA PB)
        (setq rt (cons (CIR:PPP p1 p2 p) rt))
      )
    )
    ( (Cir:Is_Same_Side rad d1 d2)
      (setq pA (CIR:Radical_Axis PM (/ dd 2) Cen rad))
      ;(setq pA (CIR:Inversion Cen rad PM))
      (setq PB (polar pA (+ (angle pA cen) (/ pi 2)) rad)) 
      (setq pC (inters pA pB p1 p2 nil))
      (foreach p (CIR:Point_Tangent cen rad pC)
        (setq rt (cons (CIR:PPP p1 p2 p) rt))
      )
    )
  )
)
;;;sqrt(((dee/dee)^2-1)*(dee^2))
;;;sqrt((x^2/a^2-1)*b^2)

;;;-------------------------------------------------------------
;;;把点圆圆切问题转化为点点圆切问题                     	
;;;Antihomologous Points                                	
;;;-------------------------------------------------------------
(defun CIR:PCC->PPC (pt c1 r1 c2 r2 / eps k p q d s)
  (setq eps 1e-8)
  (foreach a (list (+ r1 r2) (- r1 r2))
    (if (equal a 0 eps)                                 	;半径相等情况下作镜像
      (setq p (GEO:Midpoint c1 c2)
            q (GEO:Mirror2D pt p (+ (angle c1 c2) (/ pi 2)))
            s (cons q s)
      )
      (progn 
        (setq p (GEO:Scale c2 c1 (/ r1 a)))             	;对于内外切线交点的映射
        (setq d (distance pt p))
        (and (= a (+ r1 r2)) (setq d (- d)))
        (if (equal d 0 eps)
          s
          (setq k (/ (distance c1 c2) a)
                k (* r1 r2 (1- (* k k)))
                q (polar p (angle p pt) (/ k d))
                s (cons q s)
          )
        )
      )
    )
  )
)

;;;-------------------------------------------------------------
;;;点切切的圆(求通过一定点并与两给定圆相切的圆)         	
;;;输入: 一定点和给定的两个圆的圆心及半径               	
;;;输出: 符合条件的圆的集合(nil，一个或多个圆的集合)    	
;;;-------------------------------------------------------------
(defun CIR:PCC (pt Cen1 rad1 Cen2 rad2 / HC an c EPS H PM v x y)
  (setq eps 1e-8)
  (setq HC (CIR:PCC->PPC pt cen1 rad1 cen2 rad2))
  (setq pM (GEO:Midpoint  Cen1 Cen2))
  (foreach p HC
    (if (equal p pt eps)
      (progn 
        (setq H (Line:Perpendicular_Distance pt cen1 cen2))
        (setq x (distance cen1 pM))
        (setq x (* x x))
        (setq a (+ (angle cen1 cen2) (/ pi 2)))
        (foreach k (list (+ H rad1) (- H rad1))
          (if (not (equal k 0 eps))
            (setq y (/ (- (* K K) x) K 2)
                  c (polar pm a y)
                  v (cons (list c (distance pt c)) v)
            )
          )
        )
      )
      (setq v (append (CIR:PPC p pt cen1 rad1) v))
    )
  )
  v
)

;;;-------------------------------------------------------------
;;;点线圆切的圆(求通过一定点并与一直线和一圆都相切的圆) 	
;;;输入: 一定点和直线的两个端点给定的圆的圆心及半径     	
;;;输出: 符合条件的圆的集合(nil，一个或多个圆的集合)    	
;;;-------------------------------------------------------------
(defun CIR:PLC_1 (P0 Pa Pb R0 C0 / eps f a b c d x y y0 y1 dy k k1 a1 a2 s p)
  (setq eps 1e-8)
  (setq f (Line:Equation Pa Pb))
  (setq A (car f))
  (setq B (cadr f))
  (setq C (caddr f))
  (setq D (sqrt (+ (* A A) (* B B))))
  (setq y0 (/ (+ (* A (car C0)) (* B (cadr C0)) C) D))
  (setq y1 (/ (+ (* A (car P0)) (* B (cadr P0)) C) D))
  (setq dy (- y0 y1))
  (setq k1 (- (* dy (+ y0 y1)) (* R0 R0)))
  (setq a1 (angle Pa Pb))
  (setq a2 (+ a1 (* pi 0.5)))
  (setq s nil)
  (foreach R (list R0 (- R0))
    (setq k (+ dy R))
    (setq k (+ k k))
    (if (not (equal k 0 eps))
      (progn
        (setq y (/ k1 k))
        (setq p (polar C0 a2 (- y y0)))
        (setq x (* y1 (- (+ y y) y1)))
        (cond
          ( (equal x 0 eps)
            (setq s (cons (list p (abs y)) s))
          )
          ( (> x 0)
            (setq x (sqrt x))
            (setq s (cons (list (polar p a1 x) (abs y)) s))
            (setq s (cons (list (polar p a1 (- x)) (abs y)) s))
          )
        )
      )
    )
  )
  s
)

;;;-------------------------------------------------------------
;;;点线圆切的圆(求通过一定点并与一直线和一圆都相切的圆) 	
;;;输入: 一定点和直线的两个端点给定的圆的圆心及半径     	
;;;输出: 符合条件的圆的集合(nil，一个或多个圆的集合)    	
;;;-------------------------------------------------------------
(defun CIR:PLC (Pt0 PtA PtB Cen Rad / v1 v2 P1 P2 PO an q m i c s)
  (setq v1 (mapcar '- cen Pt0))
  (setq v2 (mapcar '- PtA ptb))
  (if (equal (MAT:Dot v1 v2) 0 1e-6)
    (CIR:PLC_1 Pt0 PtA PtB Rad Cen)                     	;定点在过圆心的垂直直线的线上
    (progn
      (setq p1 (CIR:Radical_Axis_Point Cen Rad Pt0))    	;等幂轴线
      (setq p2 (GEO:Rot90 P1 cen Pt0))                  	;等幂轴线另外一点
      (setq pO (inters P1 P2 PtA PtB nil))              	;等幂中心
      (setq an (+ (angle PtA PtB) (/ pi 2)))            	;垂直直线的角度
      (setq p1 (polar cen an rad))                      	;圆的直径端点1
      (setq p2 (polar cen an (- rad)))                  	;圆的直径端点2
      (foreach p (list p1 p2)                           	;对每个直径端点
        (setq q (CIR:Polar_Point_1 Cen Rad Pt0 p))      	;求出圆对到端点和Pt0的直线的极点
        (foreach n (CIR:Inters_Circle_Line Cen rad pO q)	;对每个极点和等幂中心的直线与圆的交点
          (setq m (Geo:MidPoint Pt0 n))                 	;中点
          (setq i (Geo:Rot90 m Pt0 n))                  	;垂直平分线
          (if (setq c (inters m i Cen n nil))           	;如果相交则交点是圆心
            (setq s (cons (list c (distance c n)) s))   	;加入到解集
          )
        )
      )
      s
    )
  )
)

;;;-------------------------------------------------------------
;;;切线切线切圆（切两定直线和一定圆的圆）               	
;;;输入: 两定直线的四个端点给定的圆的圆心及半径         	
;;;输出: 符合条件的圆的集合(nil，一个或多个圆的集合)    	
;;;-------------------------------------------------------------
(defun CIR:LLC (PA PB PC PD C0 R0 / Ret Pts eps Int a1 a2 a3 a4 Va Vc Vp Vx Vy L G S P1 P2 Pt c d r)
  (setq eps 1e-6)
  (defun CIR:Equal (P1 P2)
    (equal p1 p2 1e-6)
  )
  (if (setq int (inters PA PB PC PD nil))
    (progn
      (if (equal pa int eps)
        (mapcar 'set '(pb pa) (list pa pb))             	;确保Pa与int不重合
      )                                                 
      (if (equal pc int eps)
        (mapcar 'set '(pd pc) (list pc pd))             	;确保Pc与int不重合
      )                                                 
      (setq a1 (angle int pa))                          
      (setq a2 (angle int pc))
      (setq Va (mapcar '- Pa Int))                      	;交点到Pa的矢量
      (setq Vc (mapcar '- Pc Int))                      	;交点到Pc的矢量
      (setq a3 (* (- a1 a2) 0.5))                       	;角度差的一半
      (setq a4 (- a1 a3))                               	;角度和的一半
      (setq P1 (polar int a4 1000))                     	;角平分线1
      (setq P2 (polar int (+ a4 (* pi 0.5)) 1000))      	;角平分线2
      (foreach a (list 0 (* pi 0.5) Pi (* pi -0.5))
        (setq L (abs (/ r0 (sin (+ a3 a)))))            	;圆心到圆的极点的距离
        (setq G (polar C0 (+ a4 a) L))
        (setq S (CIR:Inters_Circle_Line C0 R0 Int G))   	;如果跟圆相交
        (if S 
          (progn
            (setq Vp (mapcar '- G int))
            (setq Vx (car (trans Vp 0 Va)))
            (setq Vy (car (trans Vp 0 Vc)))
            (if (or (equal vx 0 eps) (equal vy 0 eps))  	;如果跟其中一边相切
              (setq Pt (list p1 p2))                    	;则内角和外角平分线都要考虑
              (if (MATH:Opposite_Sign vx vy)            	;距离方向是否相反
                (setq Pt (list p1))                     	;如果距离方向相反则是内角平分线
                (setq Pt (list p2))                     	;如果距离方向相同则是外角平分线
              )
            )
            (foreach p S
              (if (not (MISC:IsExist p Pts 'CIR:Equal)) 	;避免重复
                (progn
                  (setq pts (cons p pts))
                  (foreach q Pt
                    (setq c (inters p C0 Int q nil))
                    (if c
                      (setq ret (cons (list c (distance c p)) ret))
                      (setq ret (append (CIR:PLL-1 p PA PB Int) ret))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    (progn
      (setq d (LINE:Perpendicular_Distance PA PC PD))
      (setq r (* d 0.5))
      (setq l (LINE:offset PC PD r))
      (grdraw (car l) (cadr l) 6)
      (setq r (abs r))
      (foreach x (list (+ r0  r) (abs (- r0 r)))
        (foreach c (CIR:Inters_Circle_Line C0 x (car l) (cadr l))
          (setq ret (cons (list c r) ret))
        )
      )
    )
  )
  ret
)

;;;-------------------------------------------------------------
;;;切线圆圆画圆（与一直线和两个圆都相切的圆）           	
;;;输入: 给定一条直线和两个圆                           	
;;;输出: 符合条件的圆的集合(nil，一个或多个圆的集合)    	
;;;-------------------------------------------------------------
(defun CIR:LCC (Pa Pb C1 R1 C2 R2)
  (if (LINE:Colinearity C1 Pa Pb)
    (mapcar 'set '(C1 R1 C2 R2) (list C2 R2 C1 R1))
  )
  (setq IC2 (CIR:Circle_Inversion C1 R1 C2 R2))
  (setq IC3 (CIR:Line_Inversion C1 R1 Pa Pb))
  (setq lst (append (list C1 R1) IC2 IC3))
  (setq ret nil)
  (foreach c (apply 'CIR:CCC lst)
    (setq ret (cons (CIR:Circle_Inversion C1 R1 (car c) (cadr c)) Ret))
  )
)


;;;-------------------------------------------------------------
;;;切切切的圆(与三个圆都相切的圆) Apollonius' problem   	
;;;输入: 三个给定的圆的圆心及半径                       	
;;;输出: 符合条件的圆的集合(nil，一个或多个圆的集合)    	
;;;-------------------------------------------------------------
(defun CIR:CCC (c1 r1 c2 r2 c3 r3 / CC1 CC2 CC3 CCC CEN DAT DMC EX1 EX2 EX3 HC1 HC2 HC3 HCS EPS NT1 NT2 NT3
                                    PC1 PC2 PC3 PS1 PS2 PS3 Pt1 Pt2 PT3 RR1 RR2 RR3 IN1 IN2 IN3 KA1 KB1 KC1
                                    LN1 MT1 MT2 MT3 RAD RET ISALL I AN ARG C IDX INC LST) 
  (setq RET nil)
  (setq eps 1e-6)
  (cond
    ( (and (equal c1 c2 eps) (equal c2 c3 eps)) nil)
    ( (LINE:Colinearity c1 c2 c3)
      (setq lst (list (list C1 R1) (list C2 R2) (list C3 R3)))
      (setq idx (vl-sort-i (mapcar 'cadr lst) '>))
      (setq C1  (nth (car idx) lst))
      (setq C2  (nth (cadr idx) lst))
      (setq C3  (nth (caddr idx) lst))
      (setq R1  (cadr C1))
      (setq R2  (cadr C2))
      (setq R3  (cadr C3))
      (setq C1  (car  C1))
      (setq C2  (car  C2))
      (setq C3  (car  C3))
      (if (equal C1 C2 eps)
        (setq an (angle c1 c3))
        (setq an (angle c1 c2))
      )
      (setq InC (polar C1 (+ an (* pi 0.5)) (+ R1 R1)))
      (setq arg nil)
      (setq ret nil)
      (foreach n (list (list C1 R1) (list C2 R2) (list C3 R3))
        (setq C (CIR:Circle_Inversion InC R1 (car n) (cadr n)))
        (setq arg (append c arg))
      )
      (foreach n (apply 'CIR:CCC arg)
        (setq C (CIR:Circle_Inversion InC R1 (car n) (cadr n)))
        (if (numberp (cadr c))
          (setq ret (cons c ret))
        )
      )
    )
    (t
      (setq DMC (CIR:Power_Center c1 r1 c2 r2 c3 r3))
      (setq HC1 (CIR:Homothetic_Center c2 r2 c3 r3))
      (setq HC2 (CIR:Homothetic_Center c3 r3 c1 r1))
      (setq HC3 (CIR:Homothetic_Center c1 r1 c2 r2))
      (setq in1 (car HC1))
      (setq in2 (car HC2))
      (setq in3 (car HC3))
      (setq ex1 (cadr HC1))
      (setq ex2 (cadr HC2))
      (setq ex3 (cadr HC3))

      (setq CCC (car DMC))
      (setq dat (cadr DMC))
      (setq Cen (list c1 c2 c3))
      (setq Rad (list r1 r2 r3))

      (setq HCs (list (list ex1 ex2 ex3 T)
                      (list in3 in1 ex2)
                      (list in1 in2 ex3)
                      (list in2 in3 ex1)
                )
      )
   
      (foreach x HCs
        (setq isAll (cadddr x))
        (setq Pt1 (car x))
        (setq Pt2 (cadr x))
        (setq pt3 (caddr x))

        (setq cc1 (car cen))
        (setq cc2 (cadr cen))
        (setq cc3 (caddr cen))

        (setq rr1 (car rad))
        (setq rr2 (cadr rad))
        (setq rr3 (caddr rad))

        (setq cen (append (cdr cen) (list (car cen))))
        (setq rad (append (cdr rad) (list (car rad))))

        (if (and (= (car Pt1) 1e400) (= (car Pt2) 1e400))
          (setq pc1 cc1 pc2 cc2 pc3 cc3)
          (progn
            (if (= (car Pt1) 1e400)
              (setq Ln1 (Line:Equation Pt2 pt3))
              (if (= (car Pt2) 1e400)
                (setq Ln1 (Line:Equation pt3 Pt1))
                (setq Ln1 (Line:Equation Pt1 Pt2))
              )
            )
            (setq kA1 (car Ln1)
                  kB1 (cadr ln1)
                  kC1 (caddr ln1)
                  pc1 (CIR:Polar_Point cc1 rr1 kA1 kB1 kC1)
                  pc2 (CIR:Polar_Point cc2 rr2 kA1 kB1 kC1)
                  pc3 (CIR:Polar_Point cc3 rr3 kA1 kB1 kC1)
            )
          )
        )
        (setq Ps1 (CIR:Inters_Circle_Line cc1 rr1 CCC PC1))
        (setq Ps2 (CIR:Inters_Circle_Line cc2 rr2 CCC pc2))
        (setq ps3 (CIR:Inters_Circle_Line cc3 rr3 CCC pc3))

        (setq mt1 (car ps1))
        (setq mt2 (car ps2))
        (setq mt3 (car ps3))

        (setq nt1 (cadr ps1))
        (setq nt2 (cadr ps2))
        (setq nt3 (cadr ps3))

        (if (and nt1 nt2 nt3)
          (if isAll
            (setq RET (cons (CIR:PPP mt1 mt2 mt3) RET)
                  RET (cons (CIR:PPP nt1 nt2 nt3) RET)
            )
            (setq RET (cons (CIR:PPP mt1 nt2 nt3) RET)
                  RET (cons (CIR:PPP nt1 mt2 mt3) RET)
            )
          )
          (if (and mt1 mt2 mt3)
            (progn 
              (setq nt1 mt3)
              (and (equal mt1 mt3 eps) (setq nt1 mt2))
              (setq RET (append (CIR:PPC mt1 nt1 cc1 rr1) RET))
            )
          )
        )
      )
    )
  )
  RET
)

;;;-------------------------------------------------------------
;;;按照某个函数升序排序                                 	
;;;-------------------------------------------------------------
(defun MISC:ASort (l f)
  (vl-sort l (function (lambda (e1 e2) (< (f e1) (f e2)))))
)

;;;-------------------------------------------------------------
;;;按照某个函数降序排序                                 	
;;;-------------------------------------------------------------
(defun MISC:DSort (l f)
  (vl-sort l (function (lambda (e1 e2) (> (f e1) (f e2)))))
)

;;;-------------------------------------------------------------
;;;反号                                                 	
;;;-------------------------------------------------------------
(defun MISC:RevSign (f)
  (if (eq f +) - +)
)

;;;-------------------------------------------------------------
;;;切切切的圆(与三个圆都相切的圆) 另一种方法            	
;;;输入: 三个给定的圆的圆心及半径                       	
;;;输出: 符合条件的圆的集合(nil，一个或多个圆的集合)    	
;;;注明: 此种作法在某些情况下不完全，还是以前面方法为准 	
;;;-------------------------------------------------------------
(defun CIR:CCC_1 (c1 r1 c2 r2 c3 r3 / Ret C D1 D2 EPS LST R S1 S2)
  (setq eps 1e-6)
  (setq lst (mapcar 'cons (list c1 c2 c3) (list r1 r2 r3)))
  (setq lst (MISC:ASort lst cdr))
  (setq c1  (caar lst))
  (setq c2  (caadr lst))
  (setq c3  (caaddr lst))
  (setq r1  (cdar lst))
  (setq r2  (cdadr lst))
  (setq r3  (cdaddr lst))
  (foreach f (mapcar 'cons (list + + - -) (list + - + -))
    (setq s1 (car f))
    (setq s2 (cdr f))
    (foreach p (CIR:PCC c1 c2 (s1 r2 r1) c3 (s2 r3 r1))
      (setq c  (car p))
      (setq r  (cadr p))
      (setq d1 (distance c c2))
      (setq d2 (distance c c3))
      (cond
        ( (equal (abs (s1 d1 r2)) (abs (s2 d2 r3)) eps)
          (setq ret (cons (list c (- r r1)) ret))
        )
        ( (equal (abs ((MISC:RevSign s1) d1 r2))
                 (abs ((MISC:RevSign s2) d2 r3))
                 eps
          )
          (setq ret (cons (list c (+ r r1)) ret))
        )
      )
    )
  )
  ret
)

;;;-------------------------------------------------------------
;;;点点Per画圆                                          	
;;;-------------------------------------------------------------
(defun CIR:PPT (Pt1 Pt2 PtM PtN / eps mid pt3 cen)
  (setq eps 1e-8)
  (setq Mid (Geo:MidPoint Pt1 Pt2))
  (setq Pt3 (GEO:Rot90 Mid Pt1 Pt2))
  (if (setq cen (inters Mid Pt3 PtM PtN nil))
    (list (list cen (distance Cen Pt1)))
    (if (LINE:Colinearity Mid PtM PtN)
      (list (list Mid (distance Mid Pt1)))
    )
  )
)

;;;-------------------------------------------------------------
;;;点切线Per画圆                                        	
;;;-------------------------------------------------------------
(defun CIR:PLT (Pt0 PtA PtB PtM PtN / eps ang Pt1)
  (setq eps 1e-8)
  (setq ang (angle PtM PtN))
  (setq Pt1 (GEO:Mirror2D Pt0 PtM ang))
  (if (equal pt1 pt0 eps)
    (CIR:PLL Pt0 PtA PtB (GEO:Mirror2D PtA PtM ang) (GEO:Mirror2D PtB PtM ang))
    (CIR:PPL Pt0 Pt1 PtA PtB)
  )
)

;;;-------------------------------------------------------------
;;;点切圆Per画圆                                        	
;;;-------------------------------------------------------------
(defun CIR:PCT (Pt0 Cen Rad PM PN / eps AN C0 C1 C2 D1 D2 D3 D4 IsA IsB L PT PT1 S C R R1 R2)
  (setq eps 1e-8)
  (setq Pt1 (GEO:Mirror3D Pt0 PM PN))
  (setq C0  (GEO:Mirror3D Cen PM PN))
  (setq IsA (equal Pt0 Pt1 eps))
  (setq IsB (equal Cen C0 eps))
  (if (and IsA IsB)
    (progn
      (setq d1 (distance Pt0 Cen))
      (if (equal d1 0 eps)
        (setq an (angle PM PN))
        (setq an (angle Pt0 cen))
      )
      (setq R1 (* (+ d1 rad) 0.5))
      (setq R2 (* (- d1 rad) 0.5))
      (setq C1 (polar Pt0 an R1))
      (setq C2 (polar Pt0 an R2))
      (list (list C1 R1) (list C2 (abs R2)))
    )
    (if IsA
      (progn 
        (and (equal Pt0 PM eps) (setq Pt PM PM PN PN Pt))
        (setq an (angle PM Pt0))
        (setq Pt (trans (mapcar '- Cen Pt0) 0 (mapcar '- Pt0 pM)))
        (setq d1 (car pt))
        (setq d2 (caddr Pt))
        (setq d3 (- d2 rad))
        (setq d4 (+ d2 rad))
        (setq L  (+ (* d1 d1) (* d3 d4)))
        (foreach d (list (+ d3 d3) (+ d4 d4))
          (if (not (equal d 0 eps))
            (setq r (/ L d)
                  c (polar Pt0 an (/ L d))
                  S (cons (list c (abs r)) S) 
            )
          )
        )
        S
      )
      (CIR:PPC Pt0 Pt1 Cen Rad)
    )
  )
)

;;;-------------------------------------------------------------
;;;切线切线Per画圆                                      	
;;;-------------------------------------------------------------
(defun CIR:LLT (PA PB PC PD PM PN / eps int p1 p2 an cen per ret)
  (setq eps 1e-8)
  (if (setq int (inters PA PB PC PD nil))
    (progn
      (setq an (* (+ (angle pa pb) (angle pc pd)) 0.5))
      (foreach a (list an (+ an (* pi 0.5)))
        (setq p1 (polar int a 100))
        (if (setq cen (inters int p1 PM PN nil))
          (setq Per (LINE:Perpendicular_Foot Cen PA PB)
                ret (cons (list cen (abs (car per))) ret)
          )
        )
      )
      ret
    )
    (progn 
      (setq P1  (GEO:Midpoint PA PC))
      (setq P2  (polar p1 (angle Pa Pb) 100))
      (setq Cen (inters P1 P2 PM PN nil))
      (setq Per (LINE:Perpendicular_Foot Cen PA PB))
      (setq ret (list (list cen (abs (car per)))))
    )
  )
)

;;;-------------------------------------------------------------
;;;切线切圆Per画圆                                      	
;;;-------------------------------------------------------------
(defun CIR:LCT (PtA PtB Cen Rad PtM PtN / Pt1 Pt2)
  (setq Pt1 (GEO:Mirror3d PtA PtM PtN))
  (setq Pt2 (GEO:Mirror3d PtB PtM PtN))
  (vl-remove-if-not
    (function (lambda (x) (LINE:Colinearity (car x) PtM PtN)))  
    (CIR:LLC PtA PtB Pt1 Pt2 Cen Rad)
  )
)

;;;-------------------------------------------------------------
;;;两个圆的位似中心 Internal,External Homothetic Center 	
;;;也就是公切线的交点(当然也包括没有公切线时候的情况)   	
;;;输入: 两个圆的圆心和半径                             	
;;;输出: 第一个为内位似中心，第二个为外位似中心         	
;;;-------------------------------------------------------------
(defun CIR:Homothetic_Center (c1 r1 c2 r2 /)
  (if (equal r1 r2 1e-14)
    (list (GEO:Midpoint c1 c2) '(1e400 1e400 0))        	;中点和无穷远点
    (list (GEO:Scale c2 c1 (/ r1 (+ r1 r2)))            	;内位似中心
          (GEO:Scale c2 c1 (/ r1 (- r1 r2)))            	;外位似中心
    )
  )
)

;;;-------------------------------------------------------------
;;;点的反演                                             	
;;;输入: 圆心，半径和一点                              		
;;;输出: nil或者反演点                                  	
;;;-------------------------------------------------------------
(defun CIR:Inversion (c r p / d)
  (setq d (distance c p))
  (if (equal d 0 1e-8)
    nil                                                 	;圆心处没有反演点
    (polar c (angle c p) (/ (* r r) d))                 	;根据反演公式计算
  )
)

;;;-------------------------------------------------------------
;;;直线对圆的反演                                       	
;;;输入: 圆心，半径和一点                               	
;;;输出: 一个用圆心和半径表示的圆或者直线本身           	
;;;-------------------------------------------------------------
(defun CIR:Line_Inversion (Cen Rad P1 P2 / d p q c r)
  (setq p (Line:Perpendicular_Foot Cen P1 P2))
  (setq d (car p))
  (setq p (cadr p))
  (if (equal d 0.0 1e-8)
    (list P1 P2)                                        	;经过圆心的直线是本身
    (setq q (CIR:Inversion Cen Rad p)
          c (GEO:Midpoint q Cen)
          r (list c (distance c Cen))                   	;把直线反演成圆
    )
  )
)


;;;-------------------------------------------------------------
;;;圆对圆的反演                                         	
;;;输入: 反演圆的圆心半径和被反演的圆的圆心，半径       	
;;;输出: 一个用圆心和半径表示的圆或者一条直线的两个端点 	
;;;-------------------------------------------------------------
(defun CIR:Circle_Inversion (C0 R0 C R / an P1 P2 d)
  (if (equal C0 C 1e-8)
    (list C0 (* (/ R0 R) R0))
    (progn
      (setq an (angle C C0))
      (setq p1 (CIR:Inversion C0 R0 (polar C an R)))
      (setq p2 (CIR:Inversion C0 R0 (polar C an (- R))))
      (if (and p1 p2)
        (list (GEO:Midpoint p1 p2) (* (distance p1 p2) 0.5))
	(progn
	  (and p2 (setq p1 p2))
	  ;(list p1 (polar p1 (- an (* pi 0.5)) R0)
	  (setq d (distance p1 c0))
	  (setq d (sqrt (abs (* (+ r0 d) (- r0 d)))))	 
	  (list
	    (polar p1 (- an (* pi 0.5)) d)
	    (polar p1 (+ an (* pi 0.5)) d)
	  )            
	)
      )
    )
  )
)

;;;-------------------------------------------------------------
;;;线段(用系数表示)对圆的极点                           	
;;;-------------------------------------------------------------
(defun CIR:Polar_Point (cen rad A B C / p d)
  (setq p (Line:Perpendicular_Distance_1 Cen A B C))
  (setq d (car p))
  (setq p (cadr p))
  (if (/= d 0)
    (polar cen (angle cen p) (/ (* rad rad) (abs d)))
  )
)

;;;-------------------------------------------------------------
;;;线段(用两点表示)对圆的极点                           	
;;;-------------------------------------------------------------
(defun CIR:Polar_Point_1 (cen rad Pa Pb / p d)
  (setq p (Line:Perpendicular_Foot Cen Pa Pb))
  (setq d (car p))
  (setq p (cadr p))
  (if (/= d 0)
    (polar cen (angle cen p) (/ (* rad rad) (abs d)))
  )
)

;;;-------------------------------------------------------------
;;;等幂轴                                               	
;;;输入: 给定的两个圆的圆心和半径                       	
;;;输出: 这两个圆的等幂轴与它们的圆心连线的交点         	
;;;-------------------------------------------------------------
(defun CIR:Radical_Axis (c1 r1 c2 r2 / l d)
  (setq l (distance c1 c2))
  (if (equal l 0 1e-8)
    (setq d (if (> r1 r2) 1e400 -1e400))
    (setq d (* 0.5 (+ L (/ (* (+ r1 r2) (- r1 r2)) L))))
  )
  (polar c1 (angle c1 c2) d)
)

(defun c:dmz()
  (setq e1 (car (entsel "\n圆1")))
  (setq e2 (car (entsel "\n圆2")))
  (setq d1 (entget e1))
  (setq d2 (entget e2))
  (setq c1 (cdr (assoc 10 d1)))
  (setq c2 (cdr (assoc 10 d2)))
  (setq r1 (cdr (assoc 40 d1)))
  (setq r2 (cdr (assoc 40 d2)))
  (setq cc (CIR:Radical_Axis c1 r1 c2 r2))
  (setq ve (mapcar '- c1 c2))
  (setq ve (list (- (cadr ve)) (car ve)))
  
  (ent:make_xline cc ve)
  (princ)
)


;;;-------------------------------------------------------------
;;;等幂中心(三个圆形的等幂轴的交点)    Radical center,  	
;;;also called the power center of three circles        	
;;;输入: 三个圆（用中心半径表示）                       	
;;;输出: 这三个圆的等幂中心                             	
;;;-------------------------------------------------------------
(defun CIR:Power_Center (c1 r1 c2 r2 c3 r3 / p1 p2 q1 q2 CC d k)
  (setq P1 (CIR:Radical_Axis c1 r1 c2 r2))
  (setq P2 (CIR:Radical_Axis c2 r2 c3 r3))
  (if (and p1 p2)
    (progn
      (setq q1 (GEO:Rot90 p1 c1 c2))
      (setq q2 (GEO:Rot90 p2 c2 c3))
      (setq CC (inters p1 q1 p2 q2 nil))
      (if CC
        (progn
          (setq d (distance CC C1))
          (setq k (* (+ d r1) (- d r1)))
          (list cc K)
        )
      )
    )
  )
)

;;;-------------------------------------------------------------
;;;阿波罗尼奥斯圆Apollonian circles                      	
;;;-------------------------------------------------------------
(defun CIR:Apollonian_Circle (P1 P2 k / k k1 k2 d rt)
  (if (and (> k 0) (/= k 1))
    (setq k1 (* k k)
          k2 (/ 1.0 (1- k1))
          d  (distance p1 p2)
          Rt (list (mapcar (function (lambda (i j) (* k2 (- (* k1 j) i)))) p1 p2)
                   (abs (* d k k2))
             )
    )
  )
)

;;;-------------------------------------------------------------
;;;共轴圆                                               	
;;;-------------------------------------------------------------
(defun CIR:Coaxal_Circle (c1 r1 c2 r2 / P1 P2 d1 d2 RT)
  (foreach HC (CIR:Homothetic_Center c1 r1 c2 r2)
    (if (= (car HC) 1e400)
      (setq P1 (Geo:MidPoint C1 C2)
            P2 (polar P1 (+ (angle C1 C2) (* pi 0.5)) 1000)
            RT (cons (list P1 P2) RT)
      )
      (setq p1 (polar C1 (angle HC C1) r1)
            p2 (polar c2 (angle HC C2) (- r2))
            d1 (distance HC p1)
            d2 (distance HC p2)
            RT (cons (list HC (sqrt (* d1 d2))) RT)
      )
    )
  )
)

;;;-------------------------------------------------------------
;;;共轴圆的特殊情况（其中一个圆退化为一点）             	
;;;-------------------------------------------------------------
(defun CIR:Radical_Axis_Point (C R P / l d)
  (if (equal c p 1e-8)
    nil
    (progn
      (setq l (distance c p))
      (setq d (/ (+ (* r r) (* l l)) 2 l))
      (polar c (angle c p) d)
    )
  )
)

;;;-------------------------------------------------------------
;;;卡斯蒂郎求解                                         	
;;;Castillon's Problem                                  	
;;;输入: 一给定圆（圆心半径表示）和不重合的三定点。     	
;;;输出：圆上三点，使得三已知点分别通过这三点形成的边   	
;;;-------------------------------------------------------------
(defun CIR:Castillon (Cen Rad pa pb pc / EPS INT1 INT2 L d1 d2 d3 LST P0 P1 P2 x0 y0 RET)
  (defun Check (Func lst d eps)
    (apply Func (mapcar (function (lambda (x) (equal x d eps))) lst))
  )
  (setq eps 1e-6)
  (setq d1 (abs (- rad (distance pa cen))))
  (setq d2 (abs (- rad (distance pb cen))))
  (setq d3 (abs (- rad (distance pc cen))))
  (if (Check 'or (list d1 d2 d3) 0 eps)
    (if (Check 'and (list d1 d2 d3) 0 eps)
      (list (list pa pb pc))
      (setq l   (vl-sort-i (list d1 d2 d3) '<)
            lst (list pa pb pc)
            pa  (nth (car l) lst)
            pb  (nth (cadr l) lst)
            pc  (nth (caddr l) lst)
            p0  Pa
            p1  (CIR:Intersecting_Chords P0 pb Cen Rad)
            p2  (CIR:Intersecting_Chords P1 PC Cen Rad)
            ret (cons (list p0 p1 p2) ret)
            p1  (CIR:Intersecting_Chords P0 pc Cen Rad)
            p2  (CIR:Intersecting_Chords P1 pb Cen Rad)
            ret (cons (list P0 p1 p2) ret)
      )
    )
    (progn
      (foreach an (list 1 2 3)
        (setq x0 (+ (* rad (cos an)) (car Cen)))
        (setq y0 (+ (* rad (sin an)) (cadr Cen)))
        (setq p0 (list x0 y0 0))
        (setq p1 p0)
        (foreach p (list pa pb pc)
          (setq P2 (CIR:Intersecting_Chords P1 P cen rad))
          (setq p1 p2)
        )
        (setq lst (cons (cons p0 p2) lst))
      )
      ;;到这步得到三个二重映射点。
      (setq int1 (inters (caar lst) (cdadr lst) (cdar lst) (caadr lst) nil))
      (setq int2 (inters (caar lst) (cdaddr lst) (cdar lst) (caaddr lst) nil))
      (if (and int1 int2)
        (foreach q (CIR:Inters_Circle_Line cen rad int1 int2)
          (setq l (list q))
          (setq p1 q)
          (foreach p (list pa pb)
            (setq p2 (CIR:Intersecting_Chords p1 p cen rad))
            (setq l (cons p2 l))
            (setq p1 p2)
          )
          (setq ret (cons (reverse l) ret))
        )
      )
    )
  )
)

;;|------------------------------------------------------------;
;;;以下样例仅供测试。                                   	
;;;-------------------------------------------------------------

;;;-------------------------------------------------------------
;;;A sample for Apollonian circles                      	
;;;阿波罗尼斯圆的测试样例                               	
;;;-------------------------------------------------------------
(defun c:cac (/ p1 p2 k rt)
  (initget 1)
  (setq p1 (getpoint "\n点1:"))
  (initget 2)
  (setq p2 (getpoint p1 "\n点2:"))
  (initget 7)
  (setq k  (getreal "\n比例:"))
  (Ent:Make_Line p1 P2)
  (setq rt (CIR:Apollonian_Circle P1 P2 k))
  (and rt (apply 'Ent:Make_Circle rt))
)

;;;-------------------------------------------------------------
;;;A sample for CIR:coaxl_Circle                        	
;;;共轴圆的测试样例                                     	
;;;-------------------------------------------------------------
(defun C:CXC (/ sel d1 d2 c1 c2 r1 r1 rt)
  (setq sel (ssget '((0 . "CIRCLE,ARC"))))
  (if (and sel (>= (sslength sel) 2))
    (progn
      (setq d1 (entget (ssname sel 0)))
      (setq d2 (entget (ssname sel 1)))
      (setq c1 (cdr (assoc 10 d1)))
      (setq c2 (cdr (assoc 10 d2)))
      (setq r1 (cdr (assoc 40 d1)))
      (setq r2 (cdr (assoc 40 d2)))
      (setq rt (CIR:Coaxal_Circle c1 r1 c2 r2))
      (foreach c rt
        (if (numberp (cadr c))
          (apply 'Ent:Make_Circle c)
          (apply 'Ent:Make_Line c)
        )
      )
    )
  )
)


;;;-------------------------------------------------------------
;;;A sample for CIR:Radical_Axis_Point                  	
;;;共轴圆特例的测试样                                   	
;;;-------------------------------------------------------------
(defun c:rax1(/ e p d r c ret)
  (setq e (car (entsel "\n圆1:")))
  (setq p (getpoint "\n点:"))
  (if (and e p)
    (progn
      (setq d (entget e))
      (setq r (cdr (assoc 40 d))
            c (cdr (assoc 10 d))
      )
      (Ent:Make_Point p)
      (setq ret (CIR:Radical_Axis_Point C R p))
      (and ret (Ent:Make_Point ret))
    )
  )
)

;;;A sample for Castillon's problem
(defun C:Castillon(/ ss pa pb pc ent dxf rad cen ret)
  (setq ss (ssget ":S" '((0 . "CIRCLE,ARC"))))
  (initget 1)
  (setq pa (getpoint "\n点1:"))
  (initget 2)
  (setq pb (getpoint "\n点2:"))
  (initget 3)
  (setq pc (getpoint "\n点3:"))
  (if ss
    (progn
      (setq ent (ssname ss 0))
      (setq dxf (entget ent))
      (setq rad (cdr (assoc 40 dxf)))
      (setq cen (cdr (assoc 10 dxf)))
      (mapcar 'Ent:Make_Point (list pa pb pc))
      (setq ret (CIR:Castillon Cen Rad pa pb pc))
      (foreach p ret
        (apply 'Ent:Make_Triangle p)
      )
    )
  )
)



;;;测试点点Per画圆
(defun C:PPT (/ s1 p1 p2 dxf pta ptb ret)
  (prompt "\n选取线段: ")
  (setq s1 (ssget ":S" '((0 . "LINE"))))
  (initget 1)
  (setq p1 (getpoint "\n点1:"))
  (initget 1)
  (setq p2 (getpoint "\n点1:"))
  (if (and s1 p1 P2)
    (progn 
      (setq dxf (entget (ssname s1 0)))
      (setq ptA (cdr (assoc 10 dxf)))
      (setq ptB (cdr (assoc 11 dxf)))
      (grdraw p1 p2 1)
      (setq ret (CIR:PPT P1 P2 PtA PtB))
      (foreach p Ret
        (apply 'ENt:Make_circle p)
      )
    )
  )
)



;;;测试点点Per画圆
(defun C:PLT (/ sel Pt0 d1 d2 PtA PtB PtC PtD Ret)
  (prompt "\n选取线段: ")
  (setq sel (ssget '((0 . "LINE"))))
  (initget 1)
  (setq Pt0 (getpoint "\n点1:"))
  (if (and sel Pt0 (>= (sslength sel) 2))
    (progn 
      (setq d1  (entget (ssname sel 0)))
      (setq d2  (entget (ssname sel 1)))
      (setq ptA (cdr (assoc 10 d1)))
      (setq ptB (cdr (assoc 11 d1)))
      (setq ptC (cdr (assoc 10 d2)))
      (setq ptD (cdr (assoc 11 d2)))
      (setq ret (CIR:PLT Pt0 PtA PtB PtC PtD))
      (foreach p Ret
        (apply 'ENt:Make_circle p)
      )
    )
  )
)
(defun c:LCC(/ s1 s2 d1 d2 d3 pa pb c1 r1 c2 r2 ret)
  (prompt "\n选线: ")
  (setq s1 (ssget "_+.:E:S:L" '((0 . "LINE"))))
  (prompt "\n选圆: ")
  (setq s2 (ssget '((0 . "ARC,CIRCLE"))))
  (if (and s1 s2 (>= (sslength s2) 2))
    (progn 
      (setq d1 (entget (ssname s1 0)))
      (setq d2 (entget (ssname s2 0)))
      (setq d3 (entget (ssname s2 1)))
      (setq pA (cdr (assoc 10 d1)))
      (setq pB (cdr (assoc 11 d1)))
      (setq C1 (cdr (assoc 10 d2)))
      (setq R1 (cdr (assoc 40 d2)))
      (setq C2 (cdr (assoc 10 d3)))
      (setq R2 (cdr (assoc 40 d3)))
      (command "undo" "be")
      (setq ReT (CIR:LCC Pa Pb C1 R1 C2 R2))
      (foreach p ReT
        (apply 'ENt:Make_circle p)
      )
      (command "undo" "e")
    )
  )
  (princ)
)
 
;;;测试点点Per画圆
(defun C:LLT (/ sel Pt0 d1 d2 PtA PtB PtC PtD Ret)
  (prompt "\n选取线段: ")
  (setq sel (ssget '((0 . "LINE"))))
  (if (and sel (>= (sslength sel) 3))
    (progn 
      (setq d1 (entget (ssname sel 0)))
      (setq d2 (entget (ssname sel 1)))
      (setq d3 (entget (ssname sel 2)))
      (setq pA (cdr (assoc 10 d1)))
      (setq pB (cdr (assoc 11 d1)))
      (setq pC (cdr (assoc 10 d2)))
      (setq pD (cdr (assoc 11 d2)))
      (setq pM (cdr (assoc 10 d3)))
      (setq pN (cdr (assoc 11 d3)))
      (setq RT (CIR:LLT PA PB PC PD PM PN))
      (foreach p RT
        (apply 'ENt:Make_circle p)
      )
    )
  )
)


;;;测试切线切圆Per画圆
(defun C:PCT (/ ss1 ss2 pt0 dx1 dx2 ptM ptN Cen Rad Ret)
  (prompt "\n选线: ")
  (setq ss1 (ssget "_+.:E:S:L" '((0 . "LINE"))))
  (prompt "\n选圆: ")
  (setq ss2 (ssget "_+.:E:S:L" '((0 . "ARC,CIRCLE"))))
  (initget 1)
  (setq Pt0 (getpoint "\n选取点:"))
  (if (and ss1 ss2)
    (progn 
      (setq dx1 (entget (ssname ss1 0)))
      (setq dx2 (entget (ssname ss2 0)))
      (setq ptM (cdr (assoc 10 dx1)))
      (setq ptN (cdr (assoc 11 dx1)))
      (setq cen (cdr (assoc 10 dx2)))
      (setq rad (cdr (assoc 40 dx2)))
      (Ent:Make_Point Pt0)
      (setq Ret (CIR:PCT Pt0 Cen Rad PtM PtN))
      (foreach p Ret
        (apply 'ENt:Make_circle p)
      )
    )
  )
)

;;;测试切线切圆Per画圆
(defun C:LCT (/ s1 s2 d1 d2 d3 pa pb pc pd c0 r0 ret)
  (prompt "\n选线: ")
  (setq s1 (ssget  '((0 . "LINE"))))
  (prompt "\n选圆: ")
  (setq s2 (ssget "_+.:E:S:L" '((0 . "ARC,CIRCLE"))))
  (if (and s1 s2 (>= (sslength s1) 2))
    (progn 
      (setq d1 (entget (ssname s1 0)))
      (setq d2 (entget (ssname s1 1)))
      (setq d3 (entget (ssname s2 0)))
      (setq pA (cdr (assoc 10 d1)))
      (setq pB (cdr (assoc 11 d1)))
      (setq pC (cdr (assoc 10 d2)))
      (setq pD (cdr (assoc 11 d2)))
      (setq c0 (cdr (assoc 10 d3)))
      (setq r0 (cdr (assoc 40 d3)))
      (command "undo" "be")
      (setq ret (CIR:LCT Pa Pb C0 R0 Pc Pd))
      (foreach p ret
        (apply 'ENt:Make_circle p)
      )
      (command "undo" "e")
    )
  )
  (princ)
)
;;;测试切线切圆Per画圆
(defun C:LLC (/ s1 s2 d1 d2 d3 pa pb pc pd c0 r0 ret)
  (prompt "\n选线: ")
  (setq s1 (ssget  '((0 . "LINE"))))
  (prompt "\n选圆: ")
  (setq s2 (ssget "_+.:E:S:L" '((0 . "ARC,CIRCLE"))))
  (if (and s1 s2 (>= (sslength s1) 2))
    (progn 
      (setq d1 (entget (ssname s1 0)))
      (setq d2 (entget (ssname s1 1)))
      (setq d3 (entget (ssname s2 0)))
      (setq pA (cdr (assoc 10 d1)))
      (setq pB (cdr (assoc 11 d1)))
      (setq pC (cdr (assoc 10 d2)))
      (setq pD (cdr (assoc 11 d2)))
      (setq c0 (cdr (assoc 10 d3)))
      (setq r0 (cdr (assoc 40 d3)))
      (command "undo" "be")
      (setq ret (CIR:LLC PA PB PC PD C0 R0))
      (foreach p ret
        (apply 'ENt:Make_circle p)
      )
      (command "undo" "e")
    )
  )
  (princ)
)

;;; A sample for CIR:Inters_Circle_Line
;;; 线圆求交示例
(defun C:CIL (/ s1 s2 dx1 dx2 pta ptb cen rad ret)
  (prompt "\n选取线段: ")
  (setq s1 (ssget ":S" '((0 . "LINE"))))
  (prompt "\n选取圆: ")
  (setq s2 (ssget ":S" '((0 . "CIRCLE,ARC"))))
  (if (and s1 s2)
    (progn 
      (setq dx1 (entget (ssname s1 0)))
      (setq dx2 (entget (ssname s2 0)))
      (setq ptA (cdr (assoc 10 dx1)))
      (setq ptB (cdr (assoc 11 dx1)))
      (setq cen (cdr (assoc 10 dx2)))
      (setq rad (cdr (assoc 40 dx2)))
      (setq ret (CIR:Inters_Circle_Line cen rad PtA PtB))
      (foreach p Ret
        (Ent:Make_Point p)
      )
    )
  )
)

;;; A sample for CIR:LLR
;;; CIR:LLR样例
(defun c:LLR (/ sel rad en1 en2 dx1 dx2 pta ptb ptc ptd ret)
  (setq sel (ssget '((0 . "LINE"))))
  (initget 1)
  (setq rad (getdist "\n半径:"))
  (if (and sel (>= (sslength sel) 2))
    (progn
      (setq en1 (ssname sel 0))
      (setq en2 (ssname sel 1))
      (setq dx1 (entget en1))
      (setq dx2 (entget en2))
      (setq PtA (cdr (assoc 10 d1)))
      (setq PtB (cdr (assoc 11 d1)))
      (setq PtC (cdr (assoc 10 d2)))
      (setq PtD (cdr (assoc 11 d2)))
      (if (setq ret (CIR:LLR PtA PtB PtC PtD Rad))
        (foreach p ret
          (apply 'Ent:Make_Circle p)
        )
      )
    )
  )
)

;;; A sample for CIR:PLR
;;; CIR:PLR样例
(defun c:PLR (/ pt0 sel rad ent dxf pta ptb ret)
  (initget 1)
  (setq pt0 (getpoint "\n1:"))
  (initget 1)
  (setq sel (ssget ":S" '((0 . "LINE"))))
  (initget 7)
  (setq rad (getdist "\n半径:"))
  (if (and sel pt0 rad)
    (progn
      (setq ent (ssname sel 0))
      (setq dxf (entget ent))
      (setq PtA (cdr (assoc 10 dxf)))
      (setq PtB (cdr (assoc 11 dxf)))
      (if (setq ret (CIR:PLR Pt0 PtA PtB Rad))
        (foreach p ret
          (apply 'Ent:Make_Circle p)
        )
      )
    )
  )
)


;;; A sample for CIR:PPR
;;; CIR:PPR样例
(defun c:PPR (/ pta ptb rad ret)
  (initget 1)
  (setq pta (getpoint "\n1:"))
  (initget 1)
  (setq ptb (getpoint "\n2:"))
  (initget 7)
  (setq rad (getdist "\n半径:"))
  (if (setq ret (CIR:PPR pta ptb rad))
    (foreach p ret
      (apply 'Ent:Make_Circle p)
    )
  )
)

;;; A sample for CIR:LCR
;;; CIR:LCR样例
(defun c:LCR (/ s1 s2 r1 d1 d2 pa pb c0 r0)
  (prompt "\n选择直线")
  (setq s1 (ssget ":S" '((0 . "LINE"))))
  (prompt "\n选择圆")
  (setq s2 (ssget ":S" '((0 . "CIRCLE,ARC"))))
  (initget 7)
  (setq r1 (getdist "\n半径:"))
  (if (and s1 s2 r1)
    (progn
      (setq d1 (entget (ssname s1 0)))
      (setq d2 (entget (ssname s2 0)))
      (setq pa (cdr (assoc 10 d1)))
      (setq pb (cdr (assoc 11 d1)))
      (setq c0 (cdr (assoc 10 d2)))
      (setq r0 (cdr (assoc 40 d2)))
      (foreach c (CIR:LCR PA PB C0 R0 R1)
        (apply 'Ent:Make_Circle c)
      )
    )
  )
)

;;; A sample for CIR:CCR
;;; CIR:CCR样例
(defun C:CCR (/ s1 r0 d1 d2 c1 c2 r1 r2)
  (prompt "\n选择两个圆")
  (setq s1 (ssget  '((0 . "CIRCLE,ARC"))))
  (initget 7)
  (setq r0 (getdist "\n半径:"))
  (if (and s1 (>= (sslength s1) 2))
    (progn
      (setq d1 (entget (ssname s1 0)))
      (setq d2 (entget (ssname s1 1)))
      (setq c1 (cdr (assoc 10 d1)))
      (setq r1 (cdr (assoc 40 d1)))
      (setq c2 (cdr (assoc 10 d2)))
      (setq r2 (cdr (assoc 40 d2)))
      (foreach c (CIR:CCR C1 R1 C2 R2 R0)
        (apply 'Ent:Make_Circle c)
      )
    )
  )
)

;;; A sample for CIR:PCR
;;; CIR:PCR样例
(defun c:PCR (/ p0 ss r1 dxf c0 r0)
  (initget 1)
  (setq p0 (getpoint "\n点:"))
  (setq p0 (trans p0 1 0))
  (prompt "\n选择圆")
  (setq ss (ssget ":S" '((0 . "CIRCLE,ARC"))))
  (initget 7)
  (setq r1 (getdist "\n半径:"))
  (if (and ss p0 r1)
    (progn
      (setq dxf (entget (ssname ss 0)))
      (setq c0  (cdr (assoc 10 dxf)))
      (setq r0  (cdr (assoc 40 dxf)))
      (Ent:Make_Point P0)
      (foreach c (CIR:PCR P0 C0 R0 R1)
        (apply 'Ent:Make_Circle c)
      )
    )
  )
)


;;;Sample
(defun C:CIV (/ pnt cir dxf rad cen)
  (initget 1)
  (setq pnt (getpoint "\n请选取一点:"))
  (if (setq cir (car (entsel "\n选取圆或者弧:")))
    (progn
      (setq dxf (entget cir))
      (setq rad (cdr (assoc 40 dxf)))
      (setq cen (cdr (assoc 10 dxf)))
      (Ent:Make_Point pnt)
      (if (setq pt (CIR:Inversion cen rad pnt))
        (Ent:Make_Point pt)
      )
      (foreach p (CIR:Point_Tangent cen rad pnt)
        ;(Ent:Make_Point p)
      )
    )
  )
)
;;;Sample
(defun C:CLI(/ ent p1 p2 dxf rad cen ret)
  (setq ent (car (entsel)))
  (setq p1  (getpoint "\nP1:"))
  (setq p2  (getpoint "\nP2:"))
  (if (and ent p1 p2)
    (progn
      (setq dxf (entget ent))
      (setq rad (cdr (assoc 40 dxf)))
      (setq cen (cdr (assoc 10 dxf)))
      (setq ret (CIR:Line_Inversion cen rad p1 p2))
      (Ent:Make_Line p1 p2)
      (apply 'Ent:Make_Circle ret)
    )
  )
)

;;;Sample圆对圆的反演
(defun C:CCI (/ e0 e1 d0 d1 r0 r1 c0 c1 ret)
  (setq e0 (car (entsel "\n选择圆1:")))
  (setq e1 (car (entsel "\n选择圆2:")))
  (if (and e0 e1)
    (progn
      (setq d0 (entget e0))
      (setq d1 (entget e1))
      (setq r0 (cdr (assoc 40 d0)))
      (setq c0 (cdr (assoc 10 d0)))
      (setq r1 (cdr (assoc 40 d1)))
      (setq c1 (cdr (assoc 10 d1)))
      (setq ret (CIR:Circle_Inversion C0 R0 C1 R1))
      (if (numberp (cadr ret))
        (apply 'Ent:Make_Circle ret)
        (apply 'Ent:Make_Line ret)
      )
    )
  )
)

;;;A sample for CIR:Inters_Circle_Circle
;;;圆圆求交点
;;;测试位似中心
(defun C:HC (/ ent1 ent2 dxf1 dxf2 rad1 rad2 cen1 cen2)
  (setq ent1 (car (entsel "\n选取圆1:")))
  (setq ent2 (car (entsel "\n选取圆2:")))
  ;(setq pt (getpoint "\n点取:"))
  (if (and ent1 ent2)
    (progn 
      (setq dxf1 (entget ent1))
      (setq dxf2 (entget ent2))
      (setq rad1 (cdr (assoc 40 dxf1)))
      (setq rad2 (cdr (assoc 40 dxf2)))
      (setq cen1 (cdr (assoc 10 dxf1)))
      (setq cen2 (cdr (assoc 10 dxf2)))
      (setq i 1)
      ;(Ent:Make_Point pt)
      (setq ret1 (CIR:Radical_Axis cen1 rad1 cen2 rad2))
      (setq ret2 (CIR:Common_Tangent cen1 rad1 cen2 rad2))
      (setq ret3 (CIR:Homothetic_Center cen1 rad1 cen2 rad2))
      (setq ret4 (CIR:Inters_Circle_Circle cen1 rad1 cen2 rad2))
      (setq ret5 (CIR:Circle_Inversion cen1 rad1 cen2 rad2))
      (if (numberp (cadr ret5))
        (apply 'Ent:Make_Circle ret5)
        (apply 'Ent:Make_Line ret5)
      )
      (and ret1 (Ent:Make_point ret1))
      (and ret3 (mapcar 'Ent:make_Point ret3))
      (and ret4 (mapcar 'Ent:make_Point ret4))
      (foreach p ret2
        (apply 'Ent:make_line p)
      )
    )
  )
)
;;;测试程序
(defun C:PPP (/ pa pb pc pd ret)
  (initget 1)
  (setq pa (getpoint "\n输入第一点:"))
  (initget 1)
  (setq pb (getpoint "\n输入第二点:"))
  (initget 1)
  (setq pc (getpoint "\n输入第三点:"))
  (initget 1)
  (setq pd (getpoint "\n输入映射点:"))
  
  (Ent:Make_Poly (list pa pb pc))
  (setq InC (Tri:InCenter pa pb pc))
  (setq CCC (CIR:PPP pa pb pc))
  (setq Pd1 (Tri:Isogonal-Conjugate-Point Pd Pa Pb Pc))
  (setq Otc (tri:OrthoCenter pa pb pc))
  (setq 9pc (Tri:9P_Circle pa pb pc))
  (mapcar 'Ent:Make_Point (list pa pb pc pd Pd1 Otc))
  (foreach n (list Inc CCC 9pc)
    (apply 'Ent:Make_Circle n)
  )
  (princ)
)

;;;测试点点线
(defun c:PPL(/ p1 p2 ln dxf pa pb ret)
  (setq p1 (getpoint "\n第一点:"))
  (setq p2 (getpoint "\n第二点:"))
  (setq ln (car (entsel "\n请选择线段")))
  (if (and p1 p2 ln)
    (progn 
      (setq dxf (entget ln))
      (setq ptA (cdr (assoc 10 dxf)))
      (setq ptb (cdr (assoc 11 dxf)))
      (setq ret (CIR:PPL p1 p2 PtA PtB))
      (Ent:Make_Line p1 p2)
      (if (VL-CONSP ret)
        (foreach n ret
          (and n (apply 'Ent:Make_Circle n))
        )
      )
    )
  )
)

(defun getDelta (x1 y1 r1 x2 y2 r2)
  (* 0.5 (+ (* (+ r1 r2) (- r1 r2)) (* (+ x2 x1) (- x2 x1)) (* (+ y2 y1) (- y2 y1))))
)

(defun d2r (d)
  (/ (* d pi) 180)
)

(defun r2d (r)
  (/ (* r 180) pi)
)

(defun getyx (x1 y1 r1 x2 y2 r2 a0 a1 a2 / sa)
  (if (equal (setq sa (sin a0)) 0 1e-14)
    0
    (/ (getdelta x1 y1 r1 x2 y2 r2)
       (- y2 y1 (/ (- (* r2 (cos a2)) (* r1 (cos a1))) sa))
    )
  )
)

(defun getrx (x1 y1 r1 x2 y2 r2 a0 a1 a2 / sa)
  (/ (getdelta x1 y1 r1 x2 y2 r2)
     (- (* r1 (cos a1))
	(* r2 (cos a2))
	(* (- y1 y2) (sin a0))
     )
  )
)

  

;;;测试点线线圆
(defun C:PLL(/ pt l1 l2 d1 d2 pa pb pc pd)
  (setq pt (getpoint "\n选择点:"))
  (setq l1 (car (entsel "\n请选择线段")))
  (setq l2 (car (entsel "\n请选择线段")))
  (if (and pt l1 l2)
    (progn 
      (setq d1 (entget l1))
      (setq d2 (entget l2))
      (setq pa (cdr (assoc 10 d1)))
      (setq pb (cdr (assoc 11 d1)))
      (setq pc (cdr (assoc 10 d2)))
      (setq pd (cdr (assoc 11 d2)))
      (Ent:Make_Point pt)       
      (setq ret (CIR:PLL pt pa pb Pc Pd))
      (if (VL-CONSP ret)
        (foreach n ret
          (and n (Ent:Make_Circle (car n) (cadr n)))
        )
      )
    )
  )
)

;;;测试点切切圆
(defun C:PCC(/ pt e1 e2 d1 d2 r1 r2 rt)
  (initget 1)
  (setq pt (getPoint "\n点1："))
  (setq e1 (ssget ":S" '((0 . "CIRCLE,ARC"))))
  (setq e2 (ssget ":S" '((0 . "CIRCLE,ARC"))))
  (if (and pt e1 e2)
    (progn
      (setq d1 (entget (ssname e1 0)))
      (setq d2 (entget (ssname e2 0)))
      (setq r1 (cdr (assoc 40 d1)))
      (setq r2 (cdr (assoc 40 d2)))
      (setq c1 (cdr (assoc 10 d1)))
      (setq c2 (cdr (assoc 10 d2)))
      (setq rt (CIR:PCC pt c1 r1 c2 r2))
      (Ent:Make_Point pt)
      (if rt
        (foreach n rt
          (apply 'Ent:Make_Circle n)
        )
      )
    )
  )
)

;;;测试点点线圆
(defun C:PPC(/ p1 p2 en dxf rad cen ret)
  (initget 1)
  (setq p1 (getPoint "\n点1："))
  (initget 1)
  (setq p2 (getPoint "\n点2："))
  (setq en (car (entsel "\n选一个圆：")))
  (if (and p1 p2 en)
    (progn
      (setq dxf (entget en))
      (setq rad (cdr (assoc 40 dxf)))
      (setq cen (cdr (assoc 10 dxf)))
      (setq ret (CIR:PPC p1 p2 cen rad))
      (Ent:Make_Point p1)
      (Ent:Make_Point p2)
      (if ret
        (foreach n ret
          (apply 'Ent:Make_Circle n)
        )
      )
    )
  )
)

;;;测试与三圆都相切的圆(即阿波罗尼斯圆)函数
(defun c:rax (/ e1 e2 e3 d1 d2 d3 c1 c2 c3 r1 r2 r3)
  (setq e1 (car (entsel "\n圆1:")))
  (setq e2 (car (entsel "\n圆2:")))
  (setq e3 (car (entsel "\n圆3:")))
  (if (and e1 e2 e3)
    (progn
      (setq d1  (entget e1))
      (setq d2  (entget e2))
      (setq d3  (entget e3))
      (setq r1  (cdr (assoc 40 d1))
            r2  (cdr (assoc 40 d2))
            r3  (cdr (assoc 40 d3))
            c1  (cdr (assoc 10 d1))
            c2  (cdr (assoc 10 d2))
            c3  (cdr (assoc 10 d3))
      )
      (command "undo" "be")
      (vla-put-color (vlax-ename->vla-object e1) 1)
      (vla-put-color (vlax-ename->vla-object e2) 2)
      (vla-put-color (vlax-ename->vla-object e3) 3)
      (MiSC:test
        100 
        '((CIR:CCC c1 r1 c2 r2 c3 r3)
          (CIR:CCC_1 c1 r1 c2 r2 c3 r3)
        )
      )
      (foreach p (CIR:CCC c1 r1 c2 r2 c3 r3)
        (and p (apply 'Ent:Make_Circle p))
      )
      (command "undo" "e")
      (princ)
    )
  )
)

;;;测试点切切圆
(defun C:PLC(/ pt e1 e2 d1 d2 r1 r2 ret)
  (initget 1)
  (setq pt0 (getPoint "\nPoint:"))
  (princ "\nSelect a line:")
  (setq en1 (ssget ":S" '((0 . "LINE"))))
  (princ "\nSelect a circle:")
  (setq en2 (ssget ":S" '((0 . "CIRCLE"))))
  (if (and pt0 en1 en2)
    (progn
      (setq d1  (entget (ssname en1 0)))
      (setq d2  (entget (ssname en2 0)))
      (setq ptA (cdr (assoc 10 d1)))
      (setq ptB (cdr (assoc 11 d1)))
      (setq rad (cdr (assoc 40 d2)))
      (setq cen (cdr (assoc 10 d2)))
      (Ent:Make_Point pt0)
      (setq ret (CIR:PLC pt0 ptA ptB Cen Rad))
      (foreach n ret
        (apply 'Ent:Make_Circle n)
      )
    )
  )
)

;;;测试极点和极线的样例
(defun C:RAL(/ e1 e2 d1 d1 c r f p1 p2 rt)
  (setq e1 (car (entsel "\n线:")))
  (setq e2 (car (entsel "\n圆:")))
  (if (and e1 e2)
    (progn
      (setq d1 (entget e1))
      (setq d2 (entget e2))  
      (setq C  (cdr (assoc 10 d2)))
      (setq r  (cdr (assoc 40 d2)))
      (setq p1 (cdr (assoc 10 d1)))
      (setq p2 (cdr (assoc 11 d1)))
      (setq f  (Line:Equation p1 p2))
      (setq rt (CIR:Polar_Point c r (car f) (cadr f) (caddr f)))
      (and rt (Ent:Make_Point rt))
    )
  )
)

;;;测试三线切圆函数
(defun c:LLL(/ e1 e2 e3 d1 d2 d3 p1 p2 p3 p4 p5 p6 ret)
  (prompt "\n请选择三条直线:")
  (if (and (setq ss (ssget '((0 . "LINE"))))
           (>= (sslength ss) 3)
      )
    (progn
      (setq e1 (ssname ss 0))
      (setq e2 (ssname ss 1))
      (setq e3 (ssname ss 2))
      (setq d1 (entget e1))
      (setq d2 (entget e2))
      (setq d3 (entget e3))
      (setq p1 (cdr (assoc 10 d1)))
      (setq p2 (cdr (assoc 11 d1)))
      (setq p3 (cdr (assoc 10 d2)))
      (setq p4 (cdr (assoc 11 d2)))
      (setq p5 (cdr (assoc 10 d3)))
      (setq p6 (cdr (assoc 11 d3)))
      (setq ret (CIR:LLL p1 p2 p3 p4 p5 p6))
      (foreach n ret
        (apply 'Ent:Make_Circle n)
      )
    )
  )
)


;;; 拉马努金椭圆周长公式
(defun ALG:GetEllipseL1 (a b / k)
  (if (> b a) (setq k b b a a k))
  (setq k (/ (float (- a b)) (+ a b)))
  (setq k (* 3 k k))
  (* pi (+ a b) (1+ (/ k (+ 10 (sqrt (- 4 k))))))
) 

(defun ALG:getEllipseL2 (a b / k m q n)
  (if (> b a) (setq k b b a a k))
  (setq k (/ (float (- a b)) (+ a b)))
  (setq m (* 3 k k))
  (setq q (+ a b))
  (setq n (+ 14.233 (* 13.981 (expt k 6.42))))
  (+ (* pi q (1+ (/ m (+ 10 (sqrt (- 4 m))))))
     (* q (- 4 (/ (* 14 pi) 11)) (expt k n))
  )
)
	
;;;-------------------------------------------------------------
;;;程序结尾                                             	
;;;----------------------------------------------------------;|;
