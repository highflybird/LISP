;|*************************************************************;
软件作者: Highflybird                                          ;
软件用途: 为AutoCAD 的LISP定制的一些算法和函数(透视变换部分)   ;
日期地点: 2019.04.21 深圳                                      ;
修改时间: 2019.04.21 深圳                                      ;
程序语言: AutoLISP,Visual LISP                                 ;
版本号:   Ver. 1.0.19.0421                                     ;
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

(defun GetPPT (p p1 p2 p3 p4 / ANG EPS MDX MDY PM PN PX PY PZ)
  (setq mdx (inters p1 p2 p3 p4 nil))
  (setq mdy (inters p1 p4 p2 p3 nil))
  (setq ang (angle mdy mdx))
  (setq eps 1e-6)
  (setq pz (polar p1 ang 1000))
  (setq px (inters p1 pz p2 p3 nil))
  (setq py (inters p1 pz p3 p4 nil))
  (setq pm (inters p1 pz p mdy nil))
  (setq pn (inters p1 pz p mdx nil))
  (list
    (/ (distance p1 pm) (distance p1 px))
    (/ (distance p1 pn) (distance p1 py))
  )
)

(defun GetPPT1 (s q1 q2 q3 q4 / ANG EPS MDX MDY QM QN QX QY QZ SX SY)
  (setq mdx (inters q1 q2 q3 q4 nil))
  (setq mdy (inters q1 q4 q2 q3 nil))
  (setq ang (angle mdy mdx))
  (setq eps 1e-6)
  (setq qz (polar q1 ang 1000))
  (setq qx (inters q1 qz q2 q3 nil))
  (setq qy (inters q1 qz q3 q4 nil))
  (setq sx (car s))
  (setq sy (- (cadr s)))
  (setq qm (polar q1 ang (* sx (distance q1 qx))))
  (setq qn (polar q1 ang (* sy (distance q1 qy))))
  (inters mdx qn mdy qm nil)
)
  

;;;-----------------------------------------------------------;;
;;; 功能: 主程序一,通过对选择物体的包围盒和对应的四个变换角点 ;;
;;;       形成一种透视变换，得到选择物体的透视图。            ;;
;;; 参数: 无。                                                ;;
;;; 返回: 无。                                                ;;
;;;-----------------------------------------------------------;;
(defun c:tts (/ ENT I MT OBJ P0 P1 P2 P3 Q0 Q1 Q2 Q3 PP RC SEL STR SYM)
  (if (setq sel (ssget '((0 . "*LINE,CIRCLE,ELLIPSE,ARC,INSERT"))))
    (progn
      (setq i 0)
      (repeat (sslength sel)
        (setq ent (ssname sel i))
        (setq obj (vlax-ename->vla-object ent))
        (vla-GetBoundingBox obj 'p0 'p2)			;得到包围框
        (setq p0 (vlax-safearray->list p0))			;得到左下角
        (setq p2 (vlax-safearray->list p2))			;得到右上角
        (setq RC (cons p2 (cons p0 RC)))			;把点坐标加入到包围框点集中
        (Ent:Divide ent nil 30 'pp)                      	;等分曲线
        (setq i (1+ i))
      )
      ;;获取选择集的包围盒
      (setq p0 (apply 'mapcar (cons 'min RC))) 			;左下角
      (setq p2 (apply 'mapcar (cons 'max RC))) 			;右上角
      (setq p3 (list (car p0) (cadr p2) (caddr p0))) 		;左上角
      (setq p1 (list (car p2) (cadr p0) (caddr p2))) 		;右下角
      ;;交互选取四个映射点
      (while (setq q0 (getpoint "\n拾取第1个点:"))
        (setq i 1)
        (repeat 3
          (initget 9)
          (setq str (strcat "\n拾取第" (itoa (1+ i)) "个点:"))
          (setq sym (read (strcat "q" (itoa i))))
          (set sym (getpoint str))
          (set sym (trans (eval sym) 1 0))
          (setq i (1+ i))
        )
	;;如果满足任三点不共线
	(if (and		
	      (GEO:Verify4P p0 p1 p2 p3)
	      (GEO:Verify4P q0 q1 q2 q3)
            )
	  (progn
	    ;;绘制映射四边形
	    (ENT:MAKE_LWPOLY (list q0 q1 q2 q3) t)		
	    ;;获得变换矩阵
            (setq mt (Mat:PerspectiveTrans p0 p1 p2 p3 q0 q1 q2 q3))
	    ;;绘制透视图
            (TransformByEntity pp mt)
	  )
	  (princ "\n选取的物体或者点不符合要求!")
	)
      )
    )
  )
  (princ)
)

;;;-----------------------------------------------------------;;
;;; 功能: 主程序二--通过从原象四边形到映射四边形形成的相应的透;;
;;;       视变换，然后对选择物体的采样点进行透视变换，最后形成;;
;;;       选择物体的透视图。                                  ;;
;;; 参数: 无。                                                ;;
;;; 返回: 无。                                                ;;
;;;-----------------------------------------------------------;;
(defun c:ttt (/ E0 E1 ENT I M0 M1 M2 MT O0 O1 P0 P1 P2 P3 PP Q0 Q1 Q2 Q3 SEL)
  (initget 1)
  (if (and
	(setq e0 (car (entsel "\n拾取原象四边形(多段线):")))
	(setq e1 (car (entsel "\n拾取映射四边形(多段线):")))
	(setq o0 (vlax-ename->vla-object e0))
	(setq o1 (vlax-ename->vla-object e1))
	(vlax-property-available-p o0 'constantwidth)
	(vlax-property-available-p o1 'constantwidth)
	(>= (vlax-curve-getendparam e0) 4)
	(>= (vlax-curve-getendparam e1) 4)
	(setq sel (ssget '((0 . "*LINE,CIRCLE,ELLIPSE,ARC,INSERT"))))
      )
    (progn
      ;;获取原象四边形
      (setq p0 (POLY:GetCoordinate o0 0))
      (setq p1 (POLY:GetCoordinate o0 1))
      (setq p2 (POLY:GetCoordinate o0 2))
      (setq p3 (POLY:GetCoordinate o0 3))
      ;;获取映射四边形
      (setq q0 (POLY:GetCoordinate o1 0))
      (setq q1 (POLY:GetCoordinate o1 1))
      (setq q2 (POLY:GetCoordinate o1 2))
      (setq q3 (POLY:GetCoordinate o1 3))
      (if (and (GEO:Verify4P p0 p1 p2 p3)
               (GEO:Verify4P q0 q1 q2 q3)
	  )
	(progn 
          (setq m0 (Mat:SquareToQuadrilateral p0 p1 p2 p3))	;单位正方形到映射四边形的变换
          (setq m1 (Mat:QuadrilateralToSquare p0 p1 p2 p3))	;映射四边形到单位正方形的变换
          (setq m2 (Mat:SquareToQuadrilateral q0 q1 q2 q3))	;单位正方形到原映四边形的变换
	  ;(setq m22 m2)
	  ;(setq xxx (anm q0 q1 q2 q3))
          (setq mt (mat:mxm m1 m2))                             ;最终的透视变换
          (setq i 0)
          (setq pp nil)
	  (repeat (sslength sel)
	    (setq ent (ssname sel i))
            (Ent:Divide ent nil 30 'pp)                         ;获取每条曲线类的等分点坐标
	    (setq i (1+ i))
          )
	  (TransformByEntity pp mt)				;对每条曲线类的点进行透视变换
	)
	(princ "\n选取四边形不符合要求!")
      )
    )
  )
  (princ)
)

;;;-----------------------------------------------------------;;
;;; 功能: 采样得到点并变换和赋予原物体属性                    ;;
;;; 参数: pp----曲线的图元名称                                ;;
;;;       mat----变换矩阵（对于插入块为其变换矩阵，其他为nil）;;
;;; 返回: 无。                                                ;;
;;; 说明: 此矩阵为3X3 矩阵，因此，对于平面点来说不能直接相乘  ;;
;;;-----------------------------------------------------------;;
(defun TransformByEntity (data mat / e l S)
  (foreach p data
    (setq S (Mat:TransFromPoints mat (cadr p)))
    (setq l (Ent:GetCommonProperties (vlax-ename->vla-object (car p))))
    (setq e (Ent:Make_LWPoly S (vlax-curve-isclosed (car p))))
    (Ent:PutCommonProperties (vlax-ename->vla-object e) l)
  )
)
  
;;;-----------------------------------------------------------;;
;;; 功能: 等分曲线（可以是嵌套插入块）                        ;;
;;; 参数: ent----曲线的图元名称                               ;;
;;;       mat----变换矩阵（对于插入块为其变换矩阵，其他为nil）;;
;;;       Seg----等分数或者等分距离                           ;;
;;;       Dat----返回点列表(相当于传址)，每个元素为点集和图元 ;;
;;; 返回: 无。                                                ;;
;;; 说明: 此矩阵为3X3 矩阵，因此，对于平面点来说不能直接相乘  ;;
;;;-----------------------------------------------------------;;
(defun Ent:Divide (ent mat Seg Dat / eBLK next M0 dxf typ PTS TYP)
  (setq dxf (entget ent))
  (setq typ (cdr (assoc 0 dxf)))
  (cond
    ( (= typ "LINE")
      (setq pts (list (cdr (assoc 10 dxf)) (cdr (assoc 11 dxf))))
    )
    ( (= typ "LWPOLYLINE")
      (setq pts (Ent:DivideLWPolyline ent Seg))
    )
    ( (or (= typ "ARC")
	  (= typ "CIRCLE")
	  (= typ "ELLIPSE")
	  (wcmatch typ "*LINE")
      )
      (setq pts (ENT:DivideCurve ent Seg))
    )
    ( (= typ "INSERT")                                          ;可以对插入块（含嵌套）做透视变换
      (setq m0 (MAT:RefGeom ent))
      (setq m0 (Mat:DispToMatrix (car m0) (cadr m0)))
      (if mat
	(setq mat (mat:mxm mat m0))
	(setq mat m0)
      )
      (setq eblk (TBLobjname "BLOCK" (cdr (assoc 2 dxf))))	;图块定义的图元名
      (setq next (cdr (assoc -2 (entget eblk))))  		;图块定义的组码图块定义的第一个图元
      (while next
	(Ent:Divide next mat Seg dat)                         	;遍历图块定义
        (setq next (entnext next))                            	;图块定义的下一个实体
      )	
    )
  )
  (if pts
    (progn
      (if mat
        (setq pts (mapcar
		    (function
		      (lambda (p / x y)
                        (setq x (car p))
			(setq y (cadr p))
			(list
			  (+ (* (caar mat) x) (* (cadar mat) y) (last (car mat)))
			  (+ (* (caadr mat) x) (* (cadadr mat) y) (last (cadr mat)))
			)
		      )
		    )
		    pts
		  )
	 )
      )
      (set dat (cons (list ent pts) (eval dat)))
    )
  )
)

;;;-----------------------------------------------------------;;
;;; 功能: 根据四点和其对应的透视点获取透视变换矩阵            ;;
;;; 参数: p0,p1,p2,p3-----要变换的四个角点（任意三点不共线）  ;;
;;;       q0,q1,q2,q3-----变换后的四个映射点（任意三点不共线）;;
;;; 返回: 此透视变换的矩阵。                                  ;;
;;; 说明: 此矩阵为3X3 矩阵，因此，对于平面点来说不能直接相乘  ;;
;;;-----------------------------------------------------------;;
(defun Mat:PerspectiveTrans (p0 p1 p2 p3 q0 q1 q2 q3)
  (mat:mxm
    (Mat:QuadrilateralToSquare p0 p1 p2 p3)
    (Mat:SquareToQuadrilateral q0 q1 q2 q3)
  )
)

;;;-----------------------------------------------------------;;
;;; 功能: 获取单位正方形到指定四个角点的透视变换矩阵          ;;
;;; 参数: p0,p1,p2,p3-----变换后的四个映射点（任意三点不共线）;;
;;; 返回: 此透视变换的矩阵。                                  ;;
;;;-----------------------------------------------------------;;
(defun Mat:SquareToQuadrilateral (p0 p1 p2 p3 /
  A13 A23 D12 DX1 DX2 DX3 DY1 DY2 DY3 PTS X0 X1 X2 X3 Y0 Y1 Y2 Y3)
  (setq pts (list p0 p1 p2 p3))
  (mapcar 'set '(x0 x1 x2 x3) (mapcar 'car pts))
  (mapcar 'set '(y0 y1 y2 y3) (mapcar 'cadr pts))
  (setq dx3 (- (+ x0 x2) x1 x3))
  (setq dy3 (- (+ y0 y2) y1 y3))
  (if (and (equal dx3 0 1e-6) (equal dy3 0 1e-6))
    (list
      (list (- x1 x0) (- y1 y0) 0)
      (list (- x2 x1) (- y2 y1) 0)
      (list x0 y0 1.)
    )
    (progn
      (setq dx1 (- x1 x2))
      (setq dx2 (- x3 x2))
      (setq dy1 (- y1 y2))
      (setq dy2 (- y3 y2))
      (setq d12 (mat:det2 dx1 dy1 dx2 dy2))
      (setq a13 (/ (mat:det2 dx3 dy3 dx2 dy2) d12))
      (setq a23 (/ (mat:det2 dx1 dy1 dx3 dy3) d12))
      (list
	(list (- (* x1 (1+ a13)) x0) (- (* y1 (1+ a13)) y0) a13)
	(list (- (* x3 (1+ a23)) x0) (- (* y3 (1+ a23)) y0) a23)
	(list x0 y0 1.0)
      )
    )
  )
)

(defun anm (p0 p1 p2 p3 / x1 x2 x3 y1 y2 y3 A11 A12 A13 A21 A22 A23
	                  A31 A32 A33 DET DX1 DX2 DX3 DY1 DY2 DY3)
  (setq p1 (mapcar '- p1 p0))
  (setq p2 (mapcar '- p2 p0))
  (setq p3 (mapcar '- p3 p0))
  (setq x1 (car p1))
  (setq x2 (car p2))
  (setq x3 (car p3))
  (setq y1 (cadr p1))
  (setq y2 (cadr p2))
  (setq y3 (cadr p3))
  (setq dx1 (- x1 x2))
  (setq dy1 (- y1 y2))
  (setq dx2 (- x3 x2))
  (setq dy2 (- y3 y2))
  (setq dx3 (- x2 x1 x3))
  (setq dy3 (- y2 y1 y3))
  (setq det (mat:det2 dx1 dy1 dx2 dy2))
  (setq a13 (/ (mat:det2 dx3 dy3 dx2 dy2) det))
  (setq a23 (/ (mat:det2 dx1 dy1 dx3 dy3) det))
  (setq a11 (* (1+ a13) x1))
  (setq a21 (* (1+ a23) x3))
  (setq a31 0.0)
  (setq a12 (* (1+ a13) y1))
  (setq a22 (* (1+ a23) y3))
  (setq a32 0.0)
  (setq a33 1.0)
  (list
    (list a11 a12 a13)
    (list a21 a22 a23)
    (list a31 a32 a33)
  )
)


;;;-----------------------------------------------------------;;
;;; 功能: 获取指定四个角点到单位正方形的透视变换矩阵          ;;
;;; 参数: p0,p1,p2,p3-----变换前的四个角点（任意三点不共线）  ;;
;;; 返回: 此透视变换的矩阵。                                  ;;
;;;-----------------------------------------------------------;;
(defun Mat:quadrilateralToSquare (p0 p1 p2 p3)
  (Mat:buildAdjoint (Mat:SquareToQuadrilateral p0 p1 p2 p3))
)

;;;-----------------------------------------------------------;;
;;; 功能: 3阶伴随矩阵                                         ;;
;;; 参数: 三阶矩阵(3X3矩阵)                                   ;;
;;; 返回: 此矩阵的伴随矩阵。                                  ;;
;;;-----------------------------------------------------------;;
(defun Mat:buildAdjoint	(mat / a11 a12 a13 a21 a22 a23 a31 a32 a33)
  (mapcar 'set '(a11 a12 a13) (car mat))
  (mapcar 'set '(a21 a22 a23) (cadr mat))
  (mapcar 'set '(a31 a32 a33) (caddr mat))
  (list
    (list
      (Mat:det2 a22 a32 a23 a33)
      (Mat:det2 a32 a12 a33 a13)
      (Mat:det2 a12 a22 a13 a23)
    )
    (list
      (Mat:det2 a23 a33 a21 a31)
      (Mat:det2 a33 a13 a31 a11)
      (Mat:det2 a13 a23 a11 a21)
    )
    (list
      (Mat:det2 a21 a31 a22 a32)
      (Mat:det2 a31 a11 a32 a12)
      (Mat:det2 a11 a21 a12 a22)
    )
  )
)

;;;-----------------------------------------------------------;;
;;; 功能: 根据透视变换矩阵变换点                              ;;
;;; 参数: 一点P 和三阶透视变换矩阵(3X3矩阵)的各元素值         ;;
;;; 返回: 变换后的点坐标。                                    ;;
;;;-----------------------------------------------------------;;
(defun Mat:TransFromPoint (p a11 a12 a13 a21 a22 a23 a31 a32 a33 / x y d)
  (setq x (car p))
  (setq y (cadr p))
  (setq d (+ (* a13 x) (* a23 y) a33))
  (list
    (/ (+ (* a11 x) (* a21 y) a31) d)
    (/ (+ (* a12 x) (* a22 y) a32) d)
  )
)

;;;-----------------------------------------------------------;;
;;; 功能: 根据透视变换矩阵变换点集                            ;;
;;; 参数: 三阶透视变换矩阵(3X3矩阵)和要变换的点集Pts          ;;
;;; 返回: 变换后的点集。                                      ;;
;;;-----------------------------------------------------------;;
(defun Mat:TransFromPoints (mat pts /)
  (mapcar
    (function
      (lambda (p)
	(apply 'Mat:TransFromPoint (cons p (apply 'append mat)))
      )
    )
    pts
  )
)

;;;-----------------------------------------------------------;;
;;; 功能: 检测平面四点是否有效，即不存在三点共线的情况        ;;
;;; 参数: p0,p1,p2,p3----要检测的四点                         ;;
;;; 返回: 如果存在三点共线，则返回nil,否则返回T。             ;;
;;;-----------------------------------------------------------;;
(defun GEO:Verify4P (p0 p1 p2 p3 / a b c d e f)
  (mapcar
    'set
    '(a b c d e f)
    (mapcar
      'distance
      (list p0 p1 p2 p3 p3 p3)
      (list p1 p2 p0 p1 p2 p0)
    )
  )
  (apply
    'and
    (mapcar
      'TRI:IsTriangle
      (list a b e f)
      (list b d f a)
      (list c e c d)
    )
  )
)

;;;-----------------------------------------------------------;;
;;; 功能: 等分一般曲线                                        ;;
;;; 参数: e----要等分的图元名                                 ;;
;;;       seg----为整数按数等分，浮点数则按距等分             ;;
;;; 返回: 等分后的点坐标表。                                  ;;
;;;-----------------------------------------------------------;;
(defun ENT:DivideCurve (e seg / L p S x)
  (setq L (vlax-curve-getDistAtParam e (vlax-curve-getendparam e)))
  (if (= (type seg) 'INT)
    (setq seg (/ L seg))
  )
  (setq x 0)
  (while (< x L)
    (setq p (vlax-curve-getpointatdist e x))
    (setq S (cons p S))
    (setq x (+ x seg))
  )
  (if (vlax-curve-isclosed e)
    (reverse S)
    (setq S (cons (vlax-curve-getendpoint e) S)
	  S (reverse S)
    )
  )
)

;;;-----------------------------------------------------------;;
;;; 功能: 等分轻多段线（只等分弧段，不等分直线段）            ;;
;;; 参数: pline----轻多段线图元                               ;;
;;;       n----为整数按数等分，浮点数则按距等分               ;;
;;; 返回: 等分后的点坐标表。                                  ;;
;;;-----------------------------------------------------------;;
(defun Ent:DivideLWPolyline (pline n / B I NUM OBJ P S U X)
  (setq obj (vlax-ename->vla-object pline))
  (if (< n 16)
    (setq n 16)	
    (setq n (fix n))
  )
  (setq u (/ 1.0 n))
  (setq i 0)
  (repeat (fix (vlax-curve-getendparam pline))
    (setq p (vlax-curve-getpointatparam pline i))
    (setq s (cons p s))
    (setq b (vla-getbulge obj i))
    (setq x i)
    (if (/= b 0.0)
      (repeat (1- n)
	(setq x (+ x u))
	(setq p (vlax-curve-getPointAtParam pline x))
	(if p
	  (setq s (cons p s))
	)
      )
    )
    (setq i (1+ i))
  )
  (if (vlax-curve-isclosed pline)
    (reverse s)
    (setq p (vlax-curve-getendpoint pline)
	  s (cons p s)
	  s (reverse s)
    )
  )
)

;;;-----------------------------------------------------------;;
;;; 功能: 获取多段线顶点坐标                                  ;;
;;; 参数: obj----轻多段线对象                                 ;;
;;;       Index----顶点号                                     ;;
;;; 返回: 此点的坐标。                                        ;;
;;;-----------------------------------------------------------;;
(defun POLY:GetCoordinate (obj Index)
  (vlax-safearray->list
    (vlax-variant-value
      (vla-get-Coordinate obj Index)
    )
  )
)

;;;-----------------------------------------------------------;;
;;; 功能: 获取物体的通用属性                                  ;;
;;; 参数: o----物体的vla-object值                             ;;
;;; 返回: 此物体的属性(图层，线型，线型比例，线宽，颜色)列表。;;
;;;-----------------------------------------------------------;;
(defun Ent:GetCommonProperties (o)
  (mapcar
    (function (lambda (f) (vlax-get o f)))
    '(layer linetype linetypescale lineweight truecolor) 
  )
)

;;;-----------------------------------------------------------;;
;;; 功能: 修改物体的通用属性                                  ;;
;;; 参数: o----物体的vla-object值                             ;;
;;;       lst----属性(图层，线型，线型比例，线宽，颜色)列表。 ;;
;;; 返回: 无                                                  ;;
;;;-----------------------------------------------------------;;
(defun Ent:PutCommonProperties (o lst) 
  (mapcar
    (function (lambda (f p) (vlax-put o f p)))
    '(layer linetype linetypescale lineweight truecolor)
    lst
  )
)

;|
(defun c:ccc ()
  (setq TIME0 (getvar "millisecs"))
  (setq TIME1 (getvar "millisecs"))
  (setq TIME1 (- TIME1 TIME0 0.0))
  (princ "\n费时:")
  (princ time1)
  (initget 9)
  (setq p0 (getpoint "\nP0:"))
  (initget 9)
  (setq p1 (getpoint "\nP1:"))
  (initget 9)
  (setq p2 (getpoint "\nP2:"))
  (initget 9)
  (setq p3 (getpoint "\nP3:"))
  (if (GEO:Verify4P p0 p1 p2 p3)
    (princ "\nYes!")
    (princ "\nNo!")
  )
  (princ)
)
;|;

