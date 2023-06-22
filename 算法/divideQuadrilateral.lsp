;;;=====================================================================
;;; ����: ���������ı���������ܳ����ֺ�����                            
;;; ����: �ޡ�                                                          
;;; ����: �ޡ�                                                          
;;;=====================================================================
(defun C:SSS (/ pa pb pc pd p0 e ratio)
  (initget 9)
  (setq pa (trans (getpoint "\n��1") 1 0))
  (initget 9)
  (setq pb (trans (getpoint "\n��2") 1 0))
  (initget 9)
  (setq pc (trans (getpoint "\n��3") 1 0))
  (initget 9)
  (setq pd (trans (getpoint "\n��4") 1 0))
  (initget 9)
  (setq p0 (trans (getpoint "\n�����:") 1 0))
  (initget 6)
  (setq ratio (getreal "\n�ȷֱ�:"))
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
;;; ����: ��������������������ܳ����ֺ�����                            
;;; ����: �ޡ�                                                          
;;; ����: �ޡ�                                                          
;;;=====================================================================

(defun C:AAA (/ E P0 PA PB PC)
  (initget 9)
  (setq pa (trans (getpoint "\n��1") 1 0))
  (initget 9)
  (setq pb (trans (getpoint "\n��2") 1 0))
  (initget 9)
  (setq pc (trans (getpoint "\n��3") 1 0))
  (initget 9)
  (setq p0 (trans (getpoint "\n�����:") 1 0))
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
;;; ����: �������Զ����������ܳ����ֺ�����                            
;;; ����: �ޡ�                                                          
;;; ����: �ޡ�                                                          
;;;=====================================================================
(defun C:PPP (/ pts pt p0 e)
  (setq pts nil)
  (while (setq pt (getpoint "\n����ζ���<���ո��˳���ȡ>:"))
    (setq pt (trans pt 1 0))
    (setq pts (cons pt pts))
  )
  (setq pts (reverse pts))
  (initget 9)
  (setq p0 (trans (getpoint "\n������:") 1 0))
  (initget 6)
  (setq ratio (getreal "\n�ȷֱ�:"))
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
;;; ����: ������ָ�����γ�ָ��������󷽳̵Ľ⡣                        
;;; ˵��: �˷���Ϊ: ��������(�Ƕ�ΪA)һ��P(x0,y0),������γ����Ϊָ��ֵ
;;;       S�������Σ������Ľ���һ��һԪ���η��̵Ľ⡣                   
;;;       ����: x^2*y0/2S-x+x0-cot(a)*y0=0                              
;;; ����: ָ����: P (x0,y0).                                            
;;;       ָ���Ƕ�: A (������������˳��)                                
;;;       ָ�����: S (����������ǵķ����й�)                          
;;; ����: ���󷽳̵Ľ⡣                                                
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
;;; ����: ������ָ�����γ�ָ���������󷽳̵Ľ⡣                        
;;; ����: ָ����: P (x0,y0).                                            
;;;       ָ���Ƕ�: A (������������˳��)                                
;;;       ָ������: L                                                   
;;; ����: ���󷽳̵Ľ⡣                                                
;;; ˵��: ����ʽΪ: x^2+(cot(a/2)*y0-L-X0)*x+(x0-cot(a)*y0)*L=0         
;;;=====================================================================
(defun DIV:PerimeterEquation (x0 y0 An L / k1 k2)
  (if (equal y0 0 1e-8)
    (list x0 L)								;ָ������һ������
    (if (/= 0 (setq k1 (sin an))) 		                	;���Ϊ��ƽ��
      (progn
	(setq k1 (/ 1 k1))						;�ǵ����
        (setq k2 (* (cos an) k1))					;�ǵ�����
        (DIV:QuadricEquation 1 (- (* (+ k1 k2) y0) L X0) (* L (- x0 (* k2 y0))))
      )
    )
  )
)

;;;=====================================================================
;;; ����: ��������һ��P(x0,y0),��һ�߶�����γ����Ϊָ��ֵS��������.   
;;; ����: I-------------�ǵĶ���.                                       
;;;       P0------------ָ����(һ���ڽ��ڲ�)                            
;;;       P1,P2,P3,P4---�ǵ�������P1P2��P3P4                            
;;;       S0------------ָ�����                                        
;;;       ls------------�⼯                                            
;;; ����: �����߶εĽ⼯��                                              
;;;=====================================================================
(defun DIV:Angle-S (I p0 p1 p2 p3 p4 S L / An A1 A2 P Q Vx X0 Y0)
  (if (equal p1 I 1e-8) (setq p p1 p1 p2 p2 p))                        	;���������뿼��
  (if (equal p4 I 1e-8) (setq p p3 p3 p4 p4 p))
  (setq Vx (mapcar '- P1 I))						;����IΪԭ�㣬IP1ΪX��
  (MAT:TransByVector P0 I Vx 'x0 'y0)					;ת��P0������
  (setq a1 (angle I p1))
  (setq a2 (angle I p4))
  (setq an (- a2 a1))							;�����P1IP2�н�,��ǲ�Ϊ0
  (if (equal s 0 1e-8)                                            	;��Ҫ������Ϊ0ʱ
    (if (equal (MAT:DET3P p0 p1 p4) 0 1e-8)                             ;ֻ��P0��P1P4����ʱ�ŷ���Ҫ��
      (setq L (cons (list p1 p4) L))
    )
    (foreach n (DIV:AreaEquation x0 y0 an S) 				;���ڷ��̵�ÿһ����
      (setq p (polar I a1 (car n)))					;�����߶ε����
      (setq q (polar I a2 (cdr n)))					;�����߶ε��յ�
      (if (and (GEO:Between p P1 p2) (GEO:Between q P3 p4))		;���ÿ���˵�Ϊ��Ч��
        (setq L (cons (list p q) L)) 					;����뵽�⼯
      )
    )
  )
  L
)

;;;=====================================================================
;;; ����: ��������һ��P(x0,y0),��һ�߶�����γɳ���Ϊָ��ֵL��������.   
;;; ����: I-------------�ǵĶ���.                                       
;;;       P0------------ָ����(һ���ڽ��ڲ�)                            
;;;       P1,P2,P3,P4---�ǵ�������P1P2��P3P4                            
;;;       L-------------ָ������                                        
;;;       ls------------�⼯                                            
;;; ����: �����߶εĽ⼯��                                              
;;;=====================================================================
(defun DIV:Angle-P (I P0 P1 P2 P3 P4 L ls / A1 A2 P Q VX X0 Y0)
  (if (equal p1 I 1e-8) (setq p p1 p1 p2 p2 p))                        	;���������뿼��
  (if (equal p4 I 1e-8) (setq p p3 p3 p4 p4 p))
  (setq a1 (angle I P1))
  (setq a2 (angle I p4))
  (setq vx (mapcar '- p1 I))						;����IΪԭ�㣬IP1ΪX��
  (MAT:TransByVector p0 I vx 'x0 'y0)					;ת��P0������
  (foreach x (DIV:PerimeterEquation x0 y0 (- a2 a1) L)			;���ڷ��̵�ÿһ����
    (setq p (polar I a1 x))						;�����߶ε����
    (setq q (polar I a2 (- L x)))					;�����߶ε��յ�
    (if (and (GEO:Between p p1 p2) (GEO:Between q p4 p3))		;���ÿ���˵�Ϊ��Ч��
      (setq ls (cons (list p q) ls))					;����뵽�⼯
    )
  )
  ls
)

;;;=====================================================================
;;; ����: ������������һ��P0,��һ�߶��������γ����Ϊָ��ֵS.           
;;; ����: P0------------ָ����                                          
;;;       P1,P2,P3,P4---����ε�������P1P2��P3P4                        
;;;       S-------------ָ�����                                        
;;;       L-------------�⼯                                            
;;; ����: �����߶εĽ⼯��                                              
;;;=====================================================================
(defun DIV:2Sides-S (p0 p1 p2 p3 p4 S L / A D I P Q R V X0 Y0 X4 Y4)
  (if (GEO:ISPARALLEL p1 p2 p4 p3)  					;ƽ�л������
    (progn
      (setq v (mapcar '- p2 p1))					;����P1P2ΪX���߷���,P1Ϊԭ��
      (MAT:TransByVector P0 P1 v 'x0 'y0)				;ת��P0����
      (MAT:TransByVector P4 P1 v 'x4 'y4)				;ת��P4����
      (if (/= y4 0)							;������
        (setq d (/ (+ S S) y4)
	      r (- (* y0 (+ d x4)) (* x0 y4)) 				;ֱ�Ӽ���
        )
      )
      (if r
        (if (equal (/ (+ y0 y0) y4) 1 1e-6)				;����˵�չ�λ����������
          (if (equal (/ r (distance p1 p2)) 0 1e-6) 			;�������������� 
	    (progn
	      (if (null (setq I (inters p1 p4 p2 p3 nil)))
	        (setq I (polar p0 (angle p1 p4) 1))
	      )
	      (setq p (inters I p0 p1 p2 nil))
	      (setq q (inters I p0 p3 p4 nil))
	      (if (and p q)
	        (setq L (cons (list p q) L))				;����ֻѡȡһ����
	      )
	    )
          )
          (setq r (/ r (- (+ y0 y0) y4))				;����ֱ�Ӱ��շ������
	        a (angle p1 p2) 
                p (polar p1 a r) 
	        q (polar p4 a (- d r)) 
                L (DIV:IsValid p q p1 p2 p3 p4 L)
          )
        )
      )
    )
    (setq I (inters p1 p2 p3 p4 nil)					;�����ߵĽ���Ϊԭ��
	  S (+ S (* 0.5 (MAT:DET3P p1 p4 I)))                        	;����P0��н��γɵ����
	  L (DIV:Angle-S I p0 p1 p2 p3 p4 S L)
    )
  )
  L
)

;;;=====================================================================
;;; ����: ������������һ��P0,��һ�߶��������γɳ���Ϊָ��ֵL.           
;;; ����: P0------------ָ����                                          
;;;       P1,P2,P3,P4---����ε�������P1P2��P3P4                        
;;;       S-------------ָ�����                                        
;;;       ls------------�⼯                                            
;;; ����: �����߶εĽ⼯��                                              
;;;=====================================================================
(defun DIV:2Sides-P (p0 p1 p2 p3 p4 L ls / a1 a2 v x0 y0 x4 y4 I p q r e f)
  (setq a1 (angle p1 p2))
  (setq a2 (angle p4 p3))
  (if (equal (sin (- a2 a1)) 0 1e-6)  					;ƽ�л������
    (progn
      (setq v (mapcar '- p2 p1))                                        ;����P1P2ΪX���߷���,P1Ϊԭ��
      (MAT:TransByVector P0 P1 v 'x0 'y0)                               ;ת��P0����
      (MAT:TransByVector P4 P1 v 'x4 'y4)                               ;ת��P4����
      (if (equal (+ y0 y0) y4 1e-6)
	(if (equal (* y0 (+ L x4)) (* x0 y4) 1e-5)
	  (progn 
	    (if (null (setq I (inters p1 p4 p2 p3 nil)))                
	      (setq I (polar p0 (angle p1 p4) 1))
	    )
	    (setq p (inters I p0 p1 p2 nil))
	    (setq q (inters I p0 p3 p4 nil))
	    (setq Ls (cons (list p q) Ls))                              ;����ֻѡȡһ����
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
      (setq I (inters p1 p2 p3 p4 nil))					;�����ߵĽ���Ϊԭ��
      (setq e (distance I p1))
      (setq f (distance I p4))
      (if (< e (distance I P2))                           		;���������뿼��
	(setq L (+ e f L))
	(setq L (- (+ e f) L))
      )
      (setq ls (DIV:Angle-P I P0 P1 P2 P3 P4 L ls))
    )
  )
  Ls
)

;;;=====================================================================
;;; ����: ������������һ��P(x0,y0)����һ�߶εȷ������������            
;;; ����: �����ε���������: PA,PB,PC��ָ����P0.                         
;;; ����: ������߶εĽ⼯��                                            
;;;=====================================================================
(defun DIV:Triangle-S (pa pb pc p0 / S ls)
  ;;����������������һ��(�ȷ����)
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
;;; ����: ������������һ��P(x0,y0)����һ�߶εȷ��������ܳ���            
;;; ����: �����ε���������: PA,PB,PC��ָ����P0.                         
;;; ����: ������߶εĽ⼯��                                            
;;;=====================================================================
(defun DIV:Triangle-P (pa pb pc p0 / L ls)
  ;;������������ܳ���һ��(�ȷ����)
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
;;; ����: �����������һ��P(x0,y0)����һ�߶εȷֶ���������            
;;; ����: ����εĶ����б��ָ����P0.                                   
;;; ����: ������߶εĽ⼯��                                            
;;;=====================================================================
(defun DIV:Poly-S (pts p0 ratio / ls P1 P2 P3 P4 S0 S1 S2 ss)
  (setq ss (mapcar
	     (function (lambda (x y) (list x y (Mat:det2v x y))))	;�ߵ��б�
	     (cons (last pts) pts)
	     pts
	   )
  )
  (setq ratio (/ ratio (1+ ratio)))
  (setq ss (vl-remove-if (function GEO:isInvalidSide) ss))		;�Ƴ�����Ϊ��ı�
  (setq S0 (* ratio (apply '+ (mapcar 'caddr ss))))                  	;�������ε����
  (while (cdr ss)                                                       ;����ÿһ����
    (setq p1 (caar ss))                                                 ;��ʼ�����
    (setq p2 (cadar ss))                                                ;��ʼ���յ�
    (setq s1 (caddar ss))						;����ĳ�ʼֵ
    (foreach n (setq ss (cdr ss))                                       ;����ʣ�µı�
      (setq p3 (car n))                                                 ;�ߵ����
      (setq p4 (cadr n))						;�ߵ��յ�
      (setq S1 (+ S1 (caddr n)))                        		;S1Ϊ�ۼ����
      (setq S2 (* 0.5 (- S1 S0 (mat:det2v p1 p4))))                 	;S2ΪҪ������
      (setq ls (DIV:2Sides-S p0 p1 p2 p3 p4 S2 ls))                   	;�������ߺ�������
    )
  )
  (reverse ls)
)

;;;=====================================================================
;;; ����: �����������һ��P(x0,y0)����һ�߶εȷֶ�����ܳ���            
;;; ����: ����εĶ����б��ָ����P0.                                   
;;; ����: ������߶εĽ⼯��                                            
;;;=====================================================================
(defun DIV:Poly-P (pts p0 ratio / ls L0 L1 L2 P1 P2 P3 P4 SS)
  (setq	ss (mapcar
	     (function (lambda (x y) (list x y (distance x y))))
	     (cons (last pts) pts)
	     pts
	   )
  )
  (setq ratio (/ ratio (1+ ratio)))
  (setq ss (vl-remove-if (function GEO:isInvalidSide) ss))		;�Ƴ�����Ϊ��ı�
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
;;; ����: �����ı�����һ��P(x0,y0)����һ�߶�����ȷ��ı��Ρ�            
;;; ����: �ı��ε��ĸ�����: PA,PB,PC,PD��ָ����P0.                      
;;; ����: ������߶εĽ⼯��                                            
;;;=====================================================================
(defun DIV:Quadrangle-S (pa pb pc pd p0 / ls ps S0 S1 p1 p2 p3 p q)
  ;;������ı��������һ��(�ȷ����)
  (setq S0 (* 0.25 (+ (MAT:DET3P pa pb pc) (MAT:DET3P pa pc pd))))	
  ;;�����ڱ߹��ɵ����������
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
  ;;Ȼ���ǶԱߵ����
  (foreach n (list (list pa pb pc pd) (list pb pc pd pa))
    (setq pa (car n))
    (setq pb (cadr n))
    (setq pc (caddr n))
    (setq pd (cadddr n))
    (if	(GEO:IsParallel pa pb pd pc)				
      (progn
	;;�������: �����Ա�ƽ��
	(setq p1 (GEO:MidPoint pa pb))
	(setq p2 (GEO:MidPoint pc pd))
	(setq p3 (GEO:MidPoint p1 p2))
	(if (equal p3 p0 1e-6)
	  ;;������������е��غϣ�������⣬�˴�ֻȡһ��
	  (setq ls (cons (list p1 p2) ls))				
	  (progn
	    (setq p (inters p3 p0 pa pb nil))               		;�߶���ԱߵĽ���1
	    (setq q (inters p3 p0 pc pd nil))				;�߶���ԱߵĽ���2
	    (if	(and p q (GEO:Between p pa pb) (GEO:Between q pc pd))   ;���Ϊ��Ч��
	      (setq ls (cons (list p q) ls))				;�����⼯
	    )
	  )
	)
      )
      (progn
	;;һ�����: �����Ա��ཻ
        (setq p2 (inters pa pb pc pd nil))				;�������
	(setq S1 (- S0 (* 0.5 (Mat:det3p Pa p2 pd))))			;���㹹�ɵ����������
	(setq ls (DIV:Angle-S p2 p0 pa Pb pc pd S1 ls))			;Ȼ�������ڹ�ĳ���γ����ΪS1���߶�
      )
    )
  )
  (reverse ls)
)

;;;=====================================================================
;;; ����: �����ı�����һ��P(x0,y0)����һ�߶εȷ��ı����ܳ���            
;;; ����: �ı��ε��ĸ�����: PA,PB,PC,PD��ָ����P0.                      
;;; ����: ������߶εĽ⼯��                                            
;;;=====================================================================
(defun DIV:Quadrangle-P (pa pb pc pd p0 / L L0 L1 L2 ld le lf s P PM PX Q)
  ;;������ı����ܳ���һ��(�ȷ��ܳ�)
  (setq L1 (list pa pb pc pd))
  (setq L2 (list pb pc pd pa))
  (setq L0 (* 0.5 (apply '+ (mapcar 'distance L1 L2))))
  ;;�����ڱ߹��ɵ����������
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
  ;;Ȼ���ǶԱߵ����
  (foreach n (list L1 L2)
    (setq pa (car n))
    (setq pb (cadr n))
    (setq pc (caddr n))
    (setq pd (cadddr n))
    (if	(GEO:IsParallel pa pb pd pc)
      (progn
	;;�������: �����Ա�ƽ��
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
	;;һ�����: �����Ա��ཻ
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
;;; ����: ���һԪ���η��̡�                                            
;;; ����: һԪ���η��̵�����ϵ��: A,B,C.                                
;;; ����: ���󷽳̵Ľ⡣                                                
;;;=====================================================================
(defun DIV:QuadricEquation (A B C / delta)
  (if (equal A 0 1e-8)
    (if (not (zerop B))
      (list (/ (- C) (float B)))
    )
    (if (equal (setq delta (- (* B B) (* 4 A C))) 0 1e-12)              ;�б�ʽb^2-4ac
      (list (/ B -2.0 A))						;�������ظ����
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
;;; ����: �жϵ��Ƿ��������γ��߶�֮�䡣                                
;;; ����: Ҫ�жϵĵ�P,����: A,B.                                        
;;; ���: ���򷵻�T,���򷵻�nil.                               	        
;;;=====================================================================
(defun GEO:Between (p A B / d)
  (if (/= (setq d (distance A B)) 0)
    (equal (/ (+ (distance p A) (distance p B)) d) 1 1e-6)		
  )
)

;;;=====================================================================
;;; ����: �������������ʽ��������֮�����                              
;;; ����: ����P1, P2, P3                                                
;;; ���: �����������ʽֵ��                                            
;;;=====================================================================
(defun MAT:det3P (p1 p2 p3 /)
  (- (* (- (car p2) (car p3)) (- (cadr p2) (cadr p1)))
     (* (- (car p2) (car p1)) (- (cadr p2) (cadr p3)))
  )
)

;;;=====================================================================
;;; ����: �б��������Ƿ�ƽ��                                            
;;; ����: ������AB��CD�Ķ˵�                                            
;;; ���: ƽ���򷵻�T,���򷵻�nil��                                     
;;;=====================================================================
(defun GEO:IsParallel (A B C D)
  (equal (sin (- (angle A B) (angle C D))) 0 1e-6)
)

;;;=====================================================================
;;; ����: �б�����Ľ��Ƿ���Ч                                          
;;; ����: ����������Լ����ڵ�����                                      
;;; ���: ��Ч�����⼯����Ч�����䶯��                              
;;;=====================================================================
(defun DIV:IsValid (p q p1 p2 p3 p4 L)
  (if (and (GEO:Between p P1 p2) (GEO:Between q P3 p4))
    (cons (list p q) L)
    L
  )
)

;;;=====================================================================
;;; ����: �б�һ���ߵ����˵��Ƿ��غ�                                    
;;; ����: һ����                                                        
;;; ���: ���򷵻�T,���򷵻�nil��                                       
;;;=====================================================================
(defun GEO:isInvalidSide (seg)
  (equal (car seg) (cadr seg) 1e-8)
)

;;;=====================================================================
;;; ����: ����֮�е�                                                    
;;; ����: ������                                                        
;;; ���: �е�����                                                      
;;;=====================================================================
(defun GEO:MidPoint (p q)
  (mapcar (function (lambda (x y) (* 0.5 (+ x y)))) p q)
)

;;;=====================================================================
;;; ����: ����ת��                                                      
;;; ����: Ҫת���ĵ�P��ԭ��O������ʸ��Vx����ת�����X,Y����             
;;; ���: ��                                                            
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
;;; ����2d�����Ĳ������ֵ                                              
;;; ����: �����㣨��������������                                        
;;; ���: һ����ֵ.���Ϊ��������ʱ��,�������γɵ�ƽ�淨���������ϣ�Ϊ��
;;;       ����˳ʱ�룬Ϊ�������������߻�ƽ�С�                          
;;;=====================================================================
(defun MAT:Det2V (v1 v2)
  (- (* (car v1) (cadr v2)) (* (car v2) (cadr v1)))
)

;;;=====================================================================
;;; ����: �����������(Ϊ�򵥶���Σ����Խ��Ķ����)   	        
;;; ����: ����ζ����б�  Pts                            	        
;;; ���: һ����ֵ�����Ϊ������CCW(��ʱ��)������˳ʱ��  	        
;;; �ο�: Centroid  Shoelace formula                     	        
;;;=====================================================================
(defun POLY:Area (pts)
  (* 0.5 (apply '+ (mapcar 'MAT:Det2V (cons (last pts) pts) pts)))
)

;;;=====================================================================
;;; ����: ���������ܳ�                                 	        
;;; ����: ����ζ����б�  Pts                            	        
;;; ���: һ����ֵ����ʾ������ܳ�                       	        
;;;=====================================================================
(defun POLY:Perimeter (pts IsClosed)
  (if isClosed
    (apply '+ (mapcar 'distance pts (cons (last pts) pts)))
    (apply '+ (mapcar 'distance pts (cdr pts)))
  )
)

;;;=====================================================================
;;; ����: ����������                                     	        
;;; ����: ������                         	                        
;;; ���: ��������                       	                        
;;;=====================================================================
(defun DIV:SWAP (SymbolX SymbolY / TempSx)
  (setq TempSx (VL-SYMBOL-VALUE SymbolX))
  (set SymbolX (VL-SYMBOL-VALUE Symboly))
  (set SymbolY TempSx)
)

;;;=====================================================================
;;; ����: ����һ����                                       	        
;;; ����: ��ά�ĵ�                           	                        
;;; ���: �߶�ʵ���ͼԪ��                               	        
;;;=====================================================================
(defun Ent:Make_Point (p)
  (entmakeX (list '(0 . "POINT") (cons 10 p)))
)

;;;=====================================================================
;;; ����: ����һ��ֱ�߶�                                       	        
;;; ����: ������ά���߶�ά�ĵ�                           	        
;;; ���: �߶�ʵ���ͼԪ��                               	        
;;;=====================================================================
(defun Ent:Make_Line (p q)
  (entmakeX (list '(0 . "LINE") (cons 10 p) (cons 11 q)))
)

;;;=====================================================================
;;;����������                                         	        
;;;����: ��ά�ĵ㼯, �Ƿ�պ�(�պϵ�Ϊ1,���պ�Ϊ0)                      
;;;���: ������ʵ����                                 	        
;;;=====================================================================
(defun Ent:Make_LWPoly (pts closed /)
  (entmakeX                                              
    (vl-list*
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity") 
      '(100 . "AcDbPolyline")
      (cons 90 (length pts))                       		        ;�������
      (cons 70 closed)                  		        	;�պϵ�Ϊ1,���պ�Ϊ0
      (mapcar (function (lambda (x) (cons 10 x))) pts)  	        ;����߶���
    )
  )
)

;;;=====================================================================
;;;�����ú���(benchMark function)                                       
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


 