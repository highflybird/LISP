;;;=====================================================================
;;; 功能: 交互测试四边形面积和周长划分函数。                            
;;; 参数: 无。                                                          
;;; 返回: 无。                                                          
;;;=====================================================================
(defun C:SSS (/ pa pb pc pd p0 e ratio)
  (initget 9)
  (setq pa (trans (getpoint "\n点1") 1 0))
  (initget 9)
  (setq pb (trans (getpoint "\n点2") 1 0))
  (initget 9)
  (setq pc (trans (getpoint "\n点3") 1 0))
  (initget 9)
  (setq pd (trans (getpoint "\n点4") 1 0))
  (initget 9)
  (setq p0 (trans (getpoint "\n输入点:") 1 0))
  (initget 6)
  (setq ratio (getreal "\n等分比:"))
  (and (null ratio) (setq ratio 1.0))
  
  (Ent:Make_LWPoly (list pa pb pc pd) 1)
  (ent:make_point p0)
  
  (uti:bench 1 
    (list
      (list 'DIV:Quadrangle-s pa pb pc pd p0)
      (list 'DIV:Poly-S (list pa pb pc pd) p0 ratio) 
    )
  )
  (uti:bench 1 
    (list
      (list 'DIV:Quadrangle-P pa pb pc pd p0)
      (list 'DIV:Poly-P (list pa pb pc pd) p0 ratio)
    )
  )

  (foreach n '((DIV:Quadrangle-S 1) (DIV:Quadrangle-P 3))
    (foreach p (apply (car n) (list pa pb pc pd p0))
      (setq e (apply 'Ent:Make_Line p))
      (vla-put-color (vlax-ename->vla-object e) (cadr n))
    )
  )
  (foreach n '((DIV:Poly-S 2) (DIV:Poly-P 4))
    (foreach p (apply (car n) (list (list pa pb pc pd) p0 ratio))
      (setq e (apply 'Ent:Make_Line p))
      (vla-put-color (vlax-ename->vla-object e) (cadr n))
    )
  )
)

;;;=====================================================================
;;; 功能: 交互测试三角形面积和周长划分函数。                            
;;; 参数: 无。                                                          
;;; 返回: 无。                                                          
;;;=====================================================================

(defun C:AAA (/ E P0 PA PB PC)
  (initget 9)
  (setq pa (trans (getpoint "\n点1") 1 0))
  (initget 9)
  (setq pb (trans (getpoint "\n点2") 1 0))
  (initget 9)
  (setq pc (trans (getpoint "\n点3") 1 0))
  (initget 9)
  (setq p0 (trans (getpoint "\n输入点:") 1 0))
  (Ent:Make_LWPoly (list pa pb pc) 1)
  (ent:make_point p0)
  (foreach n '((DIV:Triangle-S 1) (DIV:Triangle-P 3))
    (foreach p (apply (car n) (list pa pb pc p0))
      (setq e (apply 'Ent:Make_Line p))
      (vla-put-color (vlax-ename->vla-object e) (cadr n))
    )
  )
)

;;;=====================================================================
;;; 功能: 交互测试多边形面积和周长划分函数。                            
;;; 参数: 无。                                                          
;;; 返回: 无。                                                          
;;;=====================================================================
(defun C:PPP (/ pts pt p0 e)
  (setq pts nil)
  (while (setq pt (getpoint "\n多边形顶点<按空格退出点取>:"))
    (setq pt (trans pt 1 0))
    (setq pts (cons pt pts))
  )
  (setq pts (reverse pts))
  (initget 9)
  (setq p0 (trans (getpoint "\n经过点:") 1 0))
  (initget 6)
  (setq ratio (getreal "\n等分比:"))
  (and (null ratio) (setq ratio 1.0))
  (Ent:Make_LWPoly pts 1)
  (ent:make_point p0)
  (foreach n '((div:poly-S 1) (div:poly-P 3))
    (foreach p (apply (car n) (list pts p0 ratio))
      (setq e (apply 'ent:make_line p))
      (vla-put-color (vlax-ename->vla-object e) (cadr n))
    )
  )
)

;;;=====================================================================
;;; 功能: 过角内指定点形成指定面积所求方程的解。                        
;;; 说明: 此方程为: 经过角内(角度为A)一点P(x0,y0),作与角形成面积为指定值
;;;       S的三角形，这样的解是一个一元二次方程的解。                   
;;;       方程: x^2*y0/2S-x+x0-cot(a)*y0=0                              
;;; 参数: 指定点: P (x0,y0).                                            
;;;       指定角度: A (可正负，逆正顺负)                                
;;;       指定面积: S (可正负，与角的方向有关)                          
;;; 返回: 所求方程的解。                                                
;;;=====================================================================
(defun DIV:AreaEquation	(x0 y0 A S / sc 2s ss)
  (setq 2s (+ S S))
  (setq sc (sin A))
  (if (not (equal sc 0 1e-8))
    (foreach x (DIV:QuadricEquation (/ y0 2s) -1 (- x0 (/ (* y0 (cos a)) sc)))
      (if (equal x 0 1e-8)
	ss
	(setq ss (cons (cons x (/ 2s x sc)) ss))
      )
    )
  )
)

;;;=====================================================================
;;; 功能: 过角内指定点形成指定长度所求方程的解。                        
;;; 参数: 指定点: P (x0,y0).                                            
;;;       指定角度: A (可正负，逆正顺负)                                
;;;       指定长度: L                                                   
;;; 返回: 所求方程的解。                                                
;;; 说明: 方程式为: x^2+(cot(a/2)*y0-L-X0)*x+(x0-cot(a)*y0)*L=0         
;;;=====================================================================
(defun DIV:PerimeterEquation (x0 y0 An L / k1 k2)
  (if (equal y0 0 1e-8)
    (list x0 L)								;指定点在一条边上
    (if (/= 0 (setq k1 (sin an))) 		                	;如果为不平行
      (progn
	(setq k1 (/ 1 k1))						;角的余割
        (setq k2 (* (cos an) k1))					;角的余切
        (DIV:QuadricEquation 1 (- (* (+ k1 k2) y0) L X0) (* L (- x0 (* k2 y0))))
      )
    )
  )
)

;;;=====================================================================
;;; 功能: 经过角内一点P(x0,y0),作一线段与角形成面积为指定值S的三角形.   
;;; 参数: I-------------角的顶点.                                       
;;;       P0------------指定点(一般在角内部)                            
;;;       P1,P2,P3,P4---角的两条边P1P2和P3P4                            
;;;       S0------------指定面积                                        
;;;       ls------------解集                                            
;;; 返回: 所求线段的解集。                                              
;;;=====================================================================
(defun DIV:Angle-S (I p0 p1 p2 p3 p4 S L / An A1 A2 P Q Vx X0 Y0)
  (if (equal p1 I 1e-8) (setq p p1 p1 p2 p2 p))                        	;这个情况必须考虑
  (if (equal p4 I 1e-8) (setq p p3 p3 p4 p4 p))
  (setq Vx (mapcar '- P1 I))						;设置I为原点，IP1为X轴
  (MAT:TransByVector P0 I Vx 'x0 'y0)					;转换P0的坐标
  (setq a1 (angle I p1))
  (setq a2 (angle I p4))
  (setq an (- a2 a1))							;求出∠P1IP2夹角,倾角不为0
  (if (equal s 0 1e-8)                                            	;当要求的面积为0时
    (if (equal (MAT:DET3P p0 p1 p4) 0 1e-8)                             ;只有P0与P1P4共线时才符合要求
      (setq L (cons (list p1 p4) L))
    )
    (foreach n (DIV:AreaEquation x0 y0 an S) 				;对于方程的每一个解
      (setq p (polar I a1 (car n)))					;所求线段的起点
      (setq q (polar I a2 (cdr n)))					;所求线段的终点
      (if (and (GEO:Between p P1 p2) (GEO:Between q P3 p4))		;如果每个端点为有效解
        (setq L (cons (list p q) L)) 					;则加入到解集
      )
    )
  )
  L
)

;;;=====================================================================
;;; 功能: 经过角内一点P(x0,y0),作一线段与角形成长度为指定值L的三角形.   
;;; 参数: I-------------角的顶点.                                       
;;;       P0------------指定点(一般在角内部)                            
;;;       P1,P2,P3,P4---角的两条边P1P2和P3P4                            
;;;       L-------------指定长度                                        
;;;       ls------------解集                                            
;;; 返回: 所求线段的解集。                                              
;;;=====================================================================
(defun DIV:Angle-P (I P0 P1 P2 P3 P4 L ls / A1 A2 P Q VX X0 Y0)
  (if (equal p1 I 1e-8) (setq p p1 p1 p2 p2 p))                        	;这个情况必须考虑
  (if (equal p4 I 1e-8) (setq p p3 p3 p4 p4 p))
  (setq a1 (angle I P1))
  (setq a2 (angle I p4))
  (setq vx (mapcar '- p1 I))						;设置I为原点，IP1为X轴
  (MAT:TransByVector p0 I vx 'x0 'y0)					;转换P0的坐标
  (foreach x (DIV:PerimeterEquation x0 y0 (- a2 a1) L)			;对于方程的每一个解
    (setq p (polar I a1 x))						;所求线段的起点
    (setq q (polar I a2 (- L x)))					;所求线段的终点
    (if (and (GEO:Between p p1 p2) (GEO:Between q p4 p3))		;如果每个端点为有效解
      (setq ls (cons (list p q) ls))					;则加入到解集
    )
  )
  ls
)

;;;=====================================================================
;;; 功能: 经过两条边内一点P0,作一线段与两边形成面积为指定值S.           
;;; 参数: P0------------指定点                                          
;;;       P1,P2,P3,P4---多边形的两条边P1P2和P3P4                        
;;;       S-------------指定面积                                        
;;;       L-------------解集                                            
;;; 返回: 所求线段的解集。                                              
;;;=====================================================================
(defun DIV:2Sides-S (p0 p1 p2 p3 p4 S L / A D I P Q R V X0 Y0 X4 Y4)
  (if (GEO:ISPARALLEL p1 p2 p4 p3)  					;平行或共线情况
    (progn
      (setq v (mapcar '- p2 p1))					;则以P1P2为X轴线方向,P1为原点
      (MAT:TransByVector P0 P1 v 'x0 'y0)				;转换P0坐标
      (MAT:TransByVector P4 P1 v 'x4 'y4)				;转换P4坐标
      (if (/= y4 0)							;不共线
        (setq d (/ (+ S S) y4)
	      r (- (* y0 (+ d x4)) (* x0 y4)) 				;直接计算
        )
      )
      (if r
        (if (equal (/ (+ y0 y0) y4) 1 1e-6)				;如果此点刚过位于中腰线上
          (if (equal (/ r (distance p1 p2)) 0 1e-6) 			;此种情况有无穷解 
	    (progn
	      (if (null (setq I (inters p1 p4 p2 p3 nil)))
	        (setq I (polar p0 (angle p1 p4) 1))
	      )
	      (setq p (inters I p0 p1 p2 nil))
	      (setq q (inters I p0 p3 p4 nil))
	      (if (and p q)
	        (setq L (cons (list p q) L))				;这里只选取一个解
	      )
	    )
          )
          (setq r (/ r (- (+ y0 y0) y4))				;否则直接按照方程求解
	        a (angle p1 p2) 
                p (polar p1 a r) 
	        q (polar p4 a (- d r)) 
                L (DIV:IsValid p q p1 p2 p3 p4 L)
          )
        )
      )
    )
    (setq I (inters p1 p2 p3 p4 nil)					;两条边的交点为原点
	  S (+ S (* 0.5 (MAT:DET3P p1 p4 I)))                        	;经过P0点夹角形成的面积
	  L (DIV:Angle-S I p0 p1 p2 p3 p4 S L)
    )
  )
  L
)

;;;=====================================================================
;;; 功能: 经过两条边内一点P0,作一线段与两边形成长度为指定值L.           
;;; 参数: P0------------指定点                                          
;;;       P1,P2,P3,P4---多边形的两条边P1P2和P3P4                        
;;;       S-------------指定面积                                        
;;;       ls------------解集                                            
;;; 返回: 所求线段的解集。                                              
;;;=====================================================================
(defun DIV:2Sides-P (p0 p1 p2 p3 p4 L ls / a1 a2 v x0 y0 x4 y4 I p q r e f)
  (setq a1 (angle p1 p2))
  (setq a2 (angle p4 p3))
  (if (equal (sin (- a2 a1)) 0 1e-6)  					;平行或共线情况
    (progn
      (setq v (mapcar '- p2 p1))                                        ;则以P1P2为X轴线方向,P1为原点
      (MAT:TransByVector P0 P1 v 'x0 'y0)                               ;转换P0坐标
      (MAT:TransByVector P4 P1 v 'x4 'y4)                               ;转换P4坐标
      (if (equal (+ y0 y0) y4 1e-6)
	(if (equal (* y0 (+ L x4)) (* x0 y4) 1e-5)
	  (progn 
	    (if (null (setq I (inters p1 p4 p2 p3 nil)))                
	      (setq I (polar p0 (angle p1 p4) 1))
	    )
	    (setq p (inters I p0 p1 p2 nil))
	    (setq q (inters I p0 p3 p4 nil))
	    (setq Ls (cons (list p q) Ls))                              ;这里只选取一个解
	  )
        )
        (progn
	  (setq r (/ (- (* y0 (+ L x4)) (* y4 x0)) (+ y0 y0 (- y4))))	;(L*y0-x0*y1+x1*y0)/(2*y0-y1)
	  (setq p (polar p1 a1 r))
	  (setq q (polar p4 a2 (- L r)))
	  (if (and (GEO:Between p p1 p2) (GEO:Between q p3 p4))
	    (setq ls (cons (list p q) ls))
 	  )
        )
      )
    )
    (progn
      (setq I (inters p1 p2 p3 p4 nil))					;两条边的交点为原点
      (setq e (distance I p1))
      (setq f (distance I p4))
      (if (< e (distance I P2))                           		;这个情况必须考虑
	(setq L (+ e f L))
	(setq L (- (+ e f) L))
      )
      (setq ls (DIV:Angle-P I P0 P1 P2 P3 P4 L ls))
    )
  )
  Ls
)

;;;=====================================================================
;;; 功能: 经过三角形内一点P(x0,y0)，作一线段等分三角形面积。            
;;; 参数: 三角形的三个顶点: PA,PB,PC和指定点P0.                         
;;; 返回: 所求的线段的解集。                                            
;;;=====================================================================
(defun DIV:Triangle-S (pa pb pc p0 / S ls)
  ;;先求出三角形面积的一半(等分面积)
  (setq S (* 0.25 (MAT:DET3P pa pb pc)))
  (foreach n (list (list pa pb pc) (list pb pc pa) (list pc pa pb))
    (setq pa (car n))
    (setq pb (cadr n))
    (setq pc (caddr n))
    (setq ls (DIV:Angle-S pb p0 pc pb pb pa S ls))
  )	   
  (reverse ls)
)

;;;=====================================================================
;;; 功能: 经过三角形内一点P(x0,y0)，作一线段等分三角形周长。            
;;; 参数: 三角形的三个顶点: PA,PB,PC和指定点P0.                         
;;; 返回: 所求的线段的解集。                                            
;;;=====================================================================
(defun DIV:Triangle-P (pa pb pc p0 / L ls)
  ;;先求出三角形周长的一半(等分面积)
  (setq L (* 0.5 (+ (distance pa pb) (distance pb pc) (distance pc pa))))
  (foreach n (list (list pa pb pc) (list pb pc pa) (list pc pa pb))
    (setq pa (car n))
    (setq pb (cadr n))
    (setq pc (caddr n))
    (setq ls (DIV:Angle-P pb p0 pc pb pb pa L ls))
  )	   
  (reverse ls)
)

;;;=====================================================================
;;; 功能: 经过多边形内一点P(x0,y0)，作一线段等分多边形面积。            
;;; 参数: 多边形的顶点列表和指定点P0.                                   
;;; 返回: 所求的线段的解集。                                            
;;;=====================================================================
(defun DIV:Poly-S (pts p0 ratio / ls P1 P2 P3 P4 S0 S1 S2 ss)
  (setq ss (mapcar
	     (function (lambda (x y) (list x y (Mat:det2v x y))))	;边的列表
	     (cons (last pts) pts)
	     pts
	   )
  )
  (setq ratio (/ ratio (1+ ratio)))
  (setq ss (vl-remove-if (function GEO:isInvalidSide) ss))		;移除长度为零的边
  (setq S0 (* ratio (apply '+ (mapcar 'caddr ss))))                  	;求出多边形的面积
  (while (cdr ss)                                                       ;遍历每一条边
    (setq p1 (caar ss))                                                 ;初始边起点
    (setq p2 (cadar ss))                                                ;初始边终点
    (setq s1 (caddar ss))						;面积的初始值
    (foreach n (setq ss (cdr ss))                                       ;遍历剩下的边
      (setq p3 (car n))                                                 ;边的起点
      (setq p4 (cadr n))						;边的终点
      (setq S1 (+ S1 (caddr n)))                        		;S1为累计面积
      (setq S2 (* 0.5 (- S1 S0 (mat:det2v p1 p4))))                 	;S2为要求的面积
      (setq ls (DIV:2Sides-S p0 p1 p2 p3 p4 S2 ls))                   	;按两条边函数处理
    )
  )
  (reverse ls)
)

;;;=====================================================================
;;; 功能: 经过多边形内一点P(x0,y0)，作一线段等分多边形周长。            
;;; 参数: 多边形的顶点列表和指定点P0.                                   
;;; 返回: 所求的线段的解集。                                            
;;;=====================================================================
(defun DIV:Poly-P (pts p0 ratio / ls L0 L1 L2 P1 P2 P3 P4 SS)
  (setq	ss (mapcar
	     (function (lambda (x y) (list x y (distance x y))))
	     (cons (last pts) pts)
	     pts
	   )
  )
  (setq ratio (/ ratio (1+ ratio)))
  (setq ss (vl-remove-if (function GEO:isInvalidSide) ss))		;移除长度为零的边
  (setq L0 (* ratio (apply '+ (mapcar 'caddr ss))))
  (while (cdr ss)
    (setq p1 (caar ss))
    (setq p2 (cadar ss))
    (setq L1 (caddar ss))
    (foreach n (setq ss (cdr ss))
      (setq p3 (car n))
      (setq p4 (cadr n))
      (setq L1 (+ L1 (caddr n)))
      (setq L2 (- L1 L0))
      (if (equal L2 0 1e-8)
	(if (GEO:ISPARALLEL p0 p1 p0 p4)
	  (setq ls (cons (list p1 p4) ls))
	)
	(setq ls (DIV:2Sides-P p0 p1 p2 p3 p4 L2 ls))
      )
    )
  )
  (reverse ls)
)

;;;=====================================================================
;;; 功能: 经过四边形内一点P(x0,y0)，作一线段面积等分四边形。            
;;; 参数: 四边形的四个顶点: PA,PB,PC,PD和指定点P0.                      
;;; 返回: 所求的线段的解集。                                            
;;;=====================================================================
(defun DIV:Quadrangle-S (pa pb pc pd p0 / ls ps S0 S1 p1 p2 p3 p q)
  ;;先求出四边形面积的一半(等分面积)
  (setq S0 (* 0.25 (+ (MAT:DET3P pa pb pc) (MAT:DET3P pa pc pd))))	
  ;;考虑邻边构成的三角形情况
  (foreach n (list (list pa pb pc)
		   (list pb pc pd)
		   (list pc pd pa)
		   (list pd pa pb)
	     )
    (setq p1 (car n))
    (setq p2 (cadr n))
    (setq p3 (caddr n))
    (setq ls (DIV:Angle-S p2 p0 p3 p2 p2 p1 S0 ls))
  )
  ;;然后考虑对边的情况
  (foreach n (list (list pa pb pc pd) (list pb pc pd pa))
    (setq pa (car n))
    (setq pb (cadr n))
    (setq pc (caddr n))
    (setq pd (cadddr n))
    (if	(GEO:IsParallel pa pb pd pc)				
      (progn
	;;特殊情况: 两条对边平行
	(setq p1 (GEO:MidPoint pa pb))
	(setq p2 (GEO:MidPoint pc pd))
	(setq p3 (GEO:MidPoint p1 p2))
	(if (equal p3 p0 1e-6)
	  ;;如果给定点与中点重合，有无穷解，此处只取一解
	  (setq ls (cons (list p1 p2) ls))				
	  (progn
	    (setq p (inters p3 p0 pa pb nil))               		;线段与对边的交点1
	    (setq q (inters p3 p0 pc pd nil))				;线段与对边的交点2
	    (if	(and p q (GEO:Between p pa pb) (GEO:Between q pc pd))   ;如果为有效解
	      (setq ls (cons (list p q) ls))				;则加入解集
	    )
	  )
	)
      )
      (progn
	;;一般情况: 两条对边相交
        (setq p2 (inters pa pb pc pd nil))				;求出交点
	(setq S1 (- S0 (* 0.5 (Mat:det3p Pa p2 pd))))			;交点构成的三角形面积
	(setq ls (DIV:Angle-S p2 p0 pa Pb pc pd S1 ls))			;然后求解角内过某点形成面积为S1的线段
      )
    )
  )
  (reverse ls)
)

;;;=====================================================================
;;; 功能: 经过四边形内一点P(x0,y0)，作一线段等分四边形周长。            
;;; 参数: 四边形的四个顶点: PA,PB,PC,PD和指定点P0.                      
;;; 返回: 所求的线段的解集。                                            
;;;=====================================================================
(defun DIV:Quadrangle-P (pa pb pc pd p0 / L L0 L1 L2 ld le lf s P PM PX Q)
  ;;先求出四边形周长的一半(等分周长)
  (setq L1 (list pa pb pc pd))
  (setq L2 (list pb pc pd pa))
  (setq L0 (* 0.5 (apply '+ (mapcar 'distance L1 L2))))
  ;;考虑邻边构成的三角形情况
  (foreach n (list (list pa pb pc)
		   (list pb pc pd)
		   (list pc pd pa)
		   (list pd pa pb)
	     ) 
    (setq pa (car n))
    (setq pb (cadr n))
    (setq pc (caddr n))
    (setq s (DIV:Angle-P pb P0 Pc pb Pb Pa L0 s))
  )
  ;;然后考虑对边的情况
  (foreach n (list L1 L2)
    (setq pa (car n))
    (setq pb (cadr n))
    (setq pc (caddr n))
    (setq pd (cadddr n))
    (if	(GEO:IsParallel pa pb pd pc)
      (progn
	;;特殊情况: 两条对边平行
        (setq ld (distance pa pd))
        (setq Px (polar pa (angle pa pb) (- L0 ld)))
        (setq Pm (GEO:MidPoint px Pd))
        (if (equal Pm p0 1e-6)
          (if (> (distance pc pd) (distance pa pb))
	    (setq p (GEO:MidPoint pa pb)
		  q (inters p p0 pc pd nil)
		  s (cons (list p q) s)
	    )
	    (setq p (GEO:MidPoint pc pd)
		  q (inters p p0 pa pb nil)
		  s (cons (list p q) s)
	    )
	  )
          (progn
            (setq p (inters Pm p0 pa pb nil))
            (setq q (inters Pm p0 pc pd nil))
            (if (and p q (GEO:Between p pa pb) (GEO:Between q pc pd))
	      (setq s (cons (list p q) s))
            )
          )
        )
      )
      (progn
	;;一般情况: 两条对边相交
        (setq Px (inters pa pb pc pd nil))
	(setq ld (distance pd pa))
        (setq le (distance px pa))
        (setq lf (distance px pd))
        (if (< (distance pa px) (distance pb px))
	  (setq L (+ le lf l0 (- ld))
		s (DIV:Angle-P px P0 Pb Pa Pd Pc L s)
	  )
	  (setq L (+ le lf ld (- l0))
		s (DIV:Angle-P px P0 Pa Pb Pc Pd L s)
	  )
	)
      )
    ) 
  )
  (reverse s)
)

;;;=====================================================================
;;; 功能: 求解一元二次方程。                                            
;;; 参数: 一元二次方程的三个系数: A,B,C.                                
;;; 返回: 所求方程的解。                                                
;;;=====================================================================
(defun DIV:QuadricEquation (A B C / delta)
  (if (equal A 0 1e-8)
    (if (not (zerop B))
      (list (/ (- C) (float B)))
    )
    (if (equal (setq delta (- (* B B) (* 4 A C))) 0 1e-12)              ;判别式b^2-4ac
      (list (/ B -2.0 A))						;不考虑重根情况
      (if (> delta 0)
        (list
	  (/ (+ B (sqrt delta)) -2.0 A)                                 ;(-b+sqrt(b^2-4ac))/2a
	  (/ (- B (sqrt delta)) -2.0 A)					;(-b-sqrt(b^2-4ac))/2a
        )
      )
    )
  )
)

;;;=====================================================================
;;; 功能: 判断点是否在两点形成线段之间。                                
;;; 输入: 要判断的点P,两点: A,B.                                        
;;; 输出: 真则返回T,否则返回nil.                               	        
;;;=====================================================================
(defun GEO:Between (p A B / d)
  (if (/= (setq d (distance A B)) 0)
    (equal (/ (+ (distance p A) (distance p B)) d) 1 1e-6)		
  )
)

;;;=====================================================================
;;; 功能: 定义三点的行列式，即三点之倍面积                              
;;; 输入: 三点P1, P2, P3                                                
;;; 输出: 此三点的行列式值。                                            
;;;=====================================================================
(defun MAT:det3P (p1 p2 p3 /)
  (- (* (- (car p2) (car p3)) (- (cadr p2) (cadr p1)))
     (* (- (car p2) (car p1)) (- (cadr p2) (cadr p3)))
  )
)

;;;=====================================================================
;;; 功能: 判别两条线是否平行                                            
;;; 输入: 两条线AB和CD的端点                                            
;;; 输出: 平行则返回T,否则返回nil。                                     
;;;=====================================================================
(defun GEO:IsParallel (A B C D)
  (equal (sin (- (angle A B) (angle C D))) 0 1e-6)
)

;;;=====================================================================
;;; 功能: 判别所求的解是否有效                                          
;;; 输入: 解的两个点以及所在的区间                                      
;;; 输出: 有效则加入解集，无效则不做变动。                              
;;;=====================================================================
(defun DIV:IsValid (p q p1 p2 p3 p4 L)
  (if (and (GEO:Between p P1 p2) (GEO:Between q P3 p4))
    (cons (list p q) L)
    L
  )
)

;;;=====================================================================
;;; 功能: 判别一条边的两端点是否重合                                    
;;; 输入: 一条边                                                        
;;; 输出: 是则返回T,否则返回nil。                                       
;;;=====================================================================
(defun GEO:isInvalidSide (seg)
  (equal (car seg) (cadr seg) 1e-8)
)

;;;=====================================================================
;;; 功能: 两点之中点                                                    
;;; 输入: 两个点                                                        
;;; 输出: 中点坐标                                                      
;;;=====================================================================
(defun GEO:MidPoint (p q)
  (mapcar (function (lambda (x y) (* 0.5 (+ x y)))) p q)
)

;;;=====================================================================
;;; 功能: 坐标转换                                                      
;;; 输入: 要转换的点P和原点O，坐标矢量Vx，及转换后的X,Y坐标             
;;; 输出: 无                                                            
;;;=====================================================================
(defun MAT:TransByVector (P O Vx SymbolX SymbolY / v)
  (setq v (trans (mapcar '- P O) 0 Vx))
  (set SymbolX (caddr v))
  (set SymbolY (car v))
)

(defun MAT:TransByVector1 (P O V1 SymbolX SymbolY / CS D1 D2 DD V2)
  (setq v2 (mapcar '- p O))
  (setq d1 (distance v1 '(0 0 0)))
  (setq d2 (distance v2 '(0 0 0)))
  (if (or (zerop d1) (zerop d2))
    (progn 
      (set SymbolY 0)
      (set SymbolX 0)
    )
    (progn
      (setq cs (apply '+ (mapcar '* v1 v2)))
      (setq dd (- (* (car v1) (cadr v2)) (* (car v2) (cadr v1))))
      (set SymbolY  (/ dd d1))
      (set SymbolX  (/ cs d1))
    )
  )
)
  

;;;=====================================================================
;;; 两个2d向量的叉积的数值                                              
;;; 输入: 两个点（或者两个向量）                                        
;;; 输出: 一个数值.如果为正则是逆时针,两向量形成的平面法线向量向上，为负
;;;       则是顺时针，为零则两向量共线或平行。                          
;;;=====================================================================
(defun MAT:Det2V (v1 v2)
  (- (* (car v1) (cadr v2)) (* (car v2) (cadr v1)))
)

;;;=====================================================================
;;; 功能: 计算多边形面积(为简单多边形，不自交的多边形)   	        
;;; 输入: 多边形顶点列表  Pts                            	        
;;; 输出: 一个数值，如果为正则是CCW(逆时针)，否则顺时针  	        
;;; 参考: Centroid  Shoelace formula                     	        
;;;=====================================================================
(defun POLY:Area (pts)
  (* 0.5 (apply '+ (mapcar 'MAT:Det2V (cons (last pts) pts) pts)))
)

;;;=====================================================================
;;; 功能: 计算多边形周长                                 	        
;;; 输入: 多边形顶点列表  Pts                            	        
;;; 输出: 一个数值，表示多边形周长                       	        
;;;=====================================================================
(defun POLY:Perimeter (pts IsClosed)
  (if isClosed
    (apply '+ (mapcar 'distance pts (cons (last pts) pts)))
    (apply '+ (mapcar 'distance pts (cdr pts)))
  )
)

;;;=====================================================================
;;; 功能: 交换两个数                                     	        
;;; 输入: 两个数                         	                        
;;; 输出: 交换它们                       	                        
;;;=====================================================================
(defun DIV:SWAP (SymbolX SymbolY / TempSx)
  (setq TempSx (VL-SYMBOL-VALUE SymbolX))
  (set SymbolX (VL-SYMBOL-VALUE Symboly))
  (set SymbolY TempSx)
)

;;;=====================================================================
;;; 功能: 创建一个点                                       	        
;;; 输入: 三维的点                           	                        
;;; 输出: 线段实体的图元名                               	        
;;;=====================================================================
(defun Ent:Make_Point (p)
  (entmakeX (list '(0 . "POINT") (cons 10 p)))
)

;;;=====================================================================
;;; 功能: 创建一条直线段                                       	        
;;; 输入: 两个三维或者二维的点                           	        
;;; 输出: 线段实体的图元名                               	        
;;;=====================================================================
(defun Ent:Make_Line (p q)
  (entmakeX (list '(0 . "LINE") (cons 10 p) (cons 11 q)))
)

;;;=====================================================================
;;;创建轻多段线                                         	        
;;;输入: 二维的点集, 是否闭合(闭合的为1,不闭合为0)                      
;;;输出: 轻多段线实体名                                 	        
;;;=====================================================================
(defun Ent:Make_LWPoly (pts closed /)
  (entmakeX                                              
    (vl-list*
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity") 
      '(100 . "AcDbPolyline")
      (cons 90 (length pts))                       		        ;顶点个数
      (cons 70 closed)                  		        	;闭合的为1,不闭合为0
      (mapcar (function (lambda (x) (cons 10 x))) pts)  	        ;多段线顶点
    )
  )
)

;;;=====================================================================
;;;测试用函数(benchMark function)                                       
;;;=====================================================================
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
  (defun Princ-Column (l value / s)
    (setq s (vl-princ-to-string value))
    (princ s)
    (repeat (- l (strlen s))
      (princ " ")
    )
  )
  (defun Print-Result (lst)
    (princ "\n")
    (mapcar 'princ-Column '(19 9 14 16) lst)
  )
  (foreach Func Expressions 
    (setq S (cons (BenchMark Func Times) S))
  )
  (princ "\nStatement          Times    Elapse(ms)    Average(ms/time)")
  (princ "\n----------------------------------------------------------")
  (setq s (vl-sort s (function (lambda (a b) (< (caddr a) (caddr b))))))
  (mapcar 'Print-Result s) 
  (gc)
  s
)


 