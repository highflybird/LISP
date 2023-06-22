;|*************************************************************;
�������: Highflybird                                          ;
�����;: ���������ɳ���                                       ;
��������: 2020.10.25 ����                                      ;
��������: 2021.09.08 ����                                      ;
��������: AutoLISP,Visual LISP                                 ;
�汾��:   Ver. 1.21.0904                                       ;
===============================================================;
================================================================
�����Ϊ��Դ���: �����ǿ�Դ����:                               
----------------------------------------------------------------
��ҳ���������� GPLЭ�鿪��Դ���룬���������ɴ������޸ģ�������
�����Լ��������ǰ����:                                         
                                                                
һ. ֻҪ���ڱ���Դ�����ÿһ���������Ժ�ǡ���س����Ȩ����������
    �����֤��������û�е����������������𣬲��ͳ���һ���ÿ����
    ���ĳ��������һ�����֤�ĸ�������Ϳ����κ�ý�帴�ƺͷ�����
    �յ���ԭʼ�����Դ���롣��Ҳ����Ϊת�ø�����ʵ���ж���ȡһ��
    ���ã����������ȵõ���ͬ�⡣                                
��. ������޸ı���Դ�����һ���򼸸������������κβ��֣��Դ���
    �ɻ��ڳ������Ʒ��ֻҪ��ͬʱ���������������������Ϳ��԰�ǰ
    ���һ���Ҫ���ƺͷ�����һ�����޸ĵĳ������Ʒ��          
  1.��������޸ĵ��ļ��и�����ȷ˵�������޸�����һ�ļ����������
    �����ڡ�                                                    
  2.�����ʹ�㷢����������Ʒ�������������ȫ����һ���֣������
    �ɳ����ȫ���򲿷���������Ʒ�������������Ϊ���尴���֤����
    ���ʹ�á�                                                  
  3.����޸ĵĳ���������ʱ�Խ�����ʽ��ȡ��������ʹ���ڿ�ʼ��
    �볣��Ľ���ʹ�÷�ʽʱ��ӡ����ʾ����: �����ʵ��İ�Ȩ������û
    �е������������������ṩ���������������û����԰������֤����
    ���·��������˵�����������û���ο�����һ���֤�ĸ���������
    ������: ���ԭʼ�����Խ�����ʽ��������������ӡ������������
    ��Ļ��ڳ������ƷҲ�Ͳ��ô�ӡ������                        
��. ֻҪ����ѭһ��������涨�����Ϳ�������ʹ�ò�������Դ���룬��
    ����ԭ�ⲻ���ر���ԭ������Ϣ��                              
================================================================
**************************************************************|;

;;;=============================================================
;;; ����: ����Բ׶�������ĵ���ƴ����ߣ�ƽ����X�ᣩ             
;;; ����: ���������ĸ���                                        
;;; ���: ���ƴ�Բ׶���ߣ��������UCS,���������ƽ���ڴ�UCS��   
;;;-------------------------------------------------------------
(defun c:yzx (/ p1 p2 p3 p4 ret a b c)
  (initget 9)
  (setq p1 (getpoint "\n�������һ��:"))
  (initget 9)
  (setq p2 (getpoint "\n������ڶ���:"))
  (initget 9)
  (setq p3 (getpoint "\n�����������:"))
  (initget 9)
  (setq p4 (getpoint "\n��������ĵ�:"))
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
;;; ����: ����˫���ߵ����ĵ���������᳤����˫����              
;;; ����: ��������������������᳤                            
;;; ���: ���ƴ�Բ׶���ߣ��������UCS,���������ƽ���ڴ�UCS��   
;;;-------------------------------------------------------------
(defun c:sqx (/ a b c)
  (initget 9)
  (setq c (getpoint "\n�����[����(S)]<Ĭ��>:"))
  (initget 7)
  (setq a (getdist c "\n���᳤a:"))
  (initget 7)
  (setq b (getdist c "\n���᳤b:"))

  (ent:make_xline1 c (list a b))
  (ent:make_xline1 c (list a (- b)))
  
  (foreach n (GEO:ConicByAxis a b c (+ a a) nil)
    (apply 'ent:SplineBy3p1 n)
  )
  (princ)
)

;;;=============================================================
;;; ����: ����˫���ߵ����ĵ���������᳤��ȡSPLINE���������    
;;; ����: �������᳤a,b������C��xȡֵ��Χ��isY�Ƿ���Y��Գ�     
;;; ����: ����SPLINE�Ĳ������������Ȩ�أ�                      
;;;-------------------------------------------------------------
(defun GEO:ConicByAxis (a b c x isY / cx cy mx nx px py qx qy wt)
  (if (> x a)
    (progn
      (setq wt (/ x a))
      (setq py (* b (sqrt (1- (* wt wt)))))                     ;���Yֵ-->�˴�Ҫ����ܷ�ƽ��?
      (setq mx (/ a wt))			                ;���Ƶ�����
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
;;; ����: ����Բ׶�������ĵ���ƴ�����(ƽ����X����)             
;;; ����: Բ׶�������ĵ�p1,p2,p3,p4                             
;;; ����: Բ׶���ߵĲ���                                        
;;;-------------------------------------------------------------
(defun GEO:ConicBy4P (p1 p2 p3 p4 / A B C D CX CY LA LB MAT RET
		                    X1 X2 X3 X4 Y1 Y2 Y3 Y4)
  ;;Ϊ�˼򻯼��㣬����P1Ϊԭ��
  (setq p2 (mapcar '- p2 p1))					
  (setq p3 (mapcar '- p3 p1))
  (setq p4 (mapcar '- p4 p1))
  (mapcar 'set '(x1 x2 x3 x4) (mapcar 'car (list p1 p2 p3 p4)))
  (mapcar 'set '(y1 y2 y3 y4) (mapcar 'cadr (list p1 p2 p3 p4)))
  ;;�������꣬��ȡ������ϵ��
  (setq mat (list
	      (list (* x2 x2) x2 y2 (- (* y2 y2))) 
              (list (* x3 x3) x3 y3 (- (* y3 y3))) 
	      (list (* x4 x4) x4 y4 (- (* y4 y4)))
	    )
  )
  (setq ret (Mat:Gauss_Equations mat))				;��˹��Ԫ���ⷽ����
  (mapcar 'set '(A B C) ret)					;�������߷���ϵ��
  (if (and A (not (Mat:Zerop A)))				;�˷����н��Ҳ�Ϊ������
    (setq D  (* 0.25 (+ (* C C) (/ (* B B) A))))
  )
  (if (and D (not (Mat:Zerop D)))                               ;��Ӧ�᳤Ϊ0
    (progn    
      (setq la (sqrt (abs (/ D A))))                            ;�볤��ֵ
      (setq lb (sqrt (abs D)))		                        ;�����ֵ
      (setq cx (+ x1 (/ b -2 A)))		                ;����X����
      (setq cy (+ y1 (* -0.5 c)))                               ;����Y����
      (setq x2 (+ x1 x2) x3 (+ x1 x3) x4 (+ x1 x4))             ;�Ѹ��������λ
      (setq y2 (+ y1 y2) y3 (+ y1 y3) y4 (+ y1 y4))		;�Ѹ��������λ
      (if (> A 0)
	(if (> D 0)
	  (list (list cx cy) la lb)                             ;����һ����Բ
	)
	(if (> D 0)                                             
	  (list 
	    (list (list cx cy) la lb)                           ;���ĺ��������᳤
	    (Ent:GetSplineOfConic lb la cy cx y1 y2 y3 y4 T)    ;����Y��ԳƵ�˫����
	  )
	  (list 
	    (list (list cx cy) la lb)                           ;���ĺ��������᳤
	    (Ent:GetSplineOfConic la lb cx cy x1 x2 x3 x4 nil)  ;����X��ԳƵ�˫����
	  )
	)
      )
    )
  )
)

;;;=============================================================
;;; ����: ��ȡԲ׶���߲������Ӻ���                              
;;; ����: �������᳤��la,lb����������cx,cy�Լ��ĵ�Xֵ���Ƿ�Y�Գ�
;;; ����: ����SPLINE�Ĳ������������Ȩ�أ�                      
;;;-------------------------------------------------------------
(defun Ent:GetSplineOfConic (la lb cx cy x1 x2 x3 x4 isY /
			     d1 d2 d3 d4 px py qx qy mx nx wt) 
  (setq d1 (abs (- x1 cx)))
  (setq d2 (abs (- x2 cx)))
  (setq d3 (abs (- x3 cx)))
  (setq d4 (abs (- x4 cx)))
  (setq px (max d1 d2 d3 d4))					;���Xֵ
  (setq wt (/ px la))			                        ;�ڶ���Ȩ��
  (setq py (* lb (sqrt (1- (* wt wt)))))                        ;���Yֵ-->�˴�Ҫ����ܷ�ƽ��?
  (setq mx (/ la wt))			                        ;���Ƶ�����
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
;;; ����: �����������������������                              
;;; ����: ���������������ϵ�������                              
;;; ���: ���������߶Σ��������UCS,���������ƽ���ڴ�UCS��     
;;;-------------------------------------------------------------
(defun c:pwx1 (/ P1 P2 P3 S)
  (initget 9)
  (setq p1 (getpoint "\n�������һ��:"))
  (initget 9)
  (setq p2 (getpoint "\n������ڶ���:"))
  (initget 9)
  (setq p3 (getpoint "\n�����������:"))
  (if (setq S (MATH:GetArgumentsBy3P p1 p2 p3))
    (apply 'ent:SplineBy3p1 s)
    (alert "������Ĳ���������!")
  )
  (princ)
)

;;;=============================================================
;;; ����: ���������ߵ�����ϵ�����������ߣ�ԭ��Ϊָ�����(0,0)   
;;; ����: �������������߷��̵�����ϵ��a,b,c                     
;;; ���: ���������߶Σ��������UCS,���������ƽ���ڴ�UCS��     
;;;-------------------------------------------------------------
(defun c:pwx2 (/ a b c m n p s)
  (initget 3)
  (setq a (getreal "\n�����������߷���ϵ��a:"))
  (initget 1)
  (setq b (getreal "\n�����������߷���ϵ��b:"))
  (initget 1)
  (setq c (getreal "\n�����������߷���ϵ��c:"))
  (initget 1)
  (setq m (getdist "\n�������½�:"))
  (initget 1)
  (setq n (getdist "\n�������Ͻ�:"))
  (if (equal m n 1e-8)
    (alert "���½첻����ȣ����������롣")
    (progn
      (setq p (getpoint "\n�����������Դ�������ԭ��<Ĭ��(0,0)>:"))
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
;;; ����: �����������������������                              
;;; ����: �����ߵ������յ��Լ���ֵ��                          
;;; ���: �����������                                          
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
  (setq p1 (getpoint "\n��������ʼ��:"))
  (setq x1 (car p1))
  (setq y1 (cadr p1))
  
  (initget 9)
  (setq p2 (getpoint "\n��������ֹ��:"))
  (setq x2 (car p2))
  (setq y2 (cadr p2))
  
  (initget 9)
  (setq p3 (getpoint "\n��������ͣ��ߣ���:"))
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
      "��������㲻����Ҫ��!����������Ҫ�����������:
      \n1. ����������ߵ����㲻���ߡ�
      \n2. ��͵������ߵ��Yֵ�����˵�֮�䡣
      \n3. ��ʼ�����ֹ���Xֵ����ͬ��"
    )
    (progn
      (setq m (- (+ y1 y2) y3 y3))
      (setq n (* 2 (sqrt (* (- y1 y3) (- y2 y3)))))
      (setq k (- x1 x2))
      (setq d (/ (- y1 y2) k))
      (setq k (* k k))
      ;;���ݷ�����������߷��̵�����ϵ��a,b
      (if (and (>= y3 y1) (>= y3 y2))
	(setq a (/ (- m n) k))
	(setq a (/ (+ m n) k))
      )
      (setq b (- d (* a (+ x1 x2))))
      ;;����˵�����
      (setq k1 (+ b (* 2 a x1)))
      (setq q1 (polar p1 (atan k1) 1000))
      ;;�����һ���˵�����
      (setq k2 (+ b (* 2 a x2)))
      (setq q2 (polar p2 (atan k2) 1000))
      ;;�������ཻ����Ϊ���м�Ŀ��Ƶ�
      (setq pt (inters p1 q1 p2 q2 nil))
      (setq x3 (/ b -2 a))
      ;;�������㷽ʽ��������
      (ENT:SplineBy3P1 p1 pt p2 1)
    )
  )
  (princ)
)

;;;=============================================================
;;; ����: �����������������ȡ�����߷���ϵ��                    
;;; ����: ������������p1,p2,p3 ��������ٶ�ά��               
;;; ���: �����߷���y=Ax^2+Bx+C������ϵ��A,B,C                  
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
;;; ����: �����������������ȡ��ȡԲ׶���߲���                  
;;; ����: ������������p1,p2,p3 ��������ٶ�ά��               
;;; ���: CAD�е������ߵ����㡡����������������������������   ��
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
;;; ����: ����������ϵ����ȡԲ׶���߲���                        
;;; ����: ������ϵ��a,b,c�����½�m,n�Լ���Ե㣨����㣩        
;;; ���: �����ߵ����˵㼰�����߽���                            
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
;;; �ж�һ��������Χ���Ƿ�Ϊ0                               
;;;=============================================================
(defun Mat:Zerop (x)
  (equal x 0 1e-14)
)

;;;=============================================================
;;; �ж�ĳһ���Ƿ�Ҫ�����˹��Ԫ���ĵݹ����                    
;;;=============================================================
(defun Mat:Join_Gauss (n l / y)
  (setq n (cdr n))
  (setq y (cdr (reverse n)))
  (if (or (null y) (vl-every 'Mat:Zerop y))			;ϵ�����ȫΪ0�������ı�
    l
    (cons n l)						        ;ϵ�������ȫΪ0
  )
)

;;;=============================================================
;;; ��˹��Ԫ��֮����ֽ�                                        
;;; �������ϳ��������޸�                                        
;;;=============================================================
(defun Mat:Gauss (m / A B L R x y)
  (if (car m)
    (progn
      (setq r (car m))
      (setq x (abs (car r)))
      (foreach n (cdr m)
        (if (> (setq y (abs (car n))) x)
          (setq r n x y)					;��ȡ����Ԫ�ؾ���ֵȡ���ֵ������
        )
      )				
      (setq a (float (car r)))					;��ת��Ϊ����������ֹ����
      (if (equal a 0 1e-14)
	(Mat:Gauss (mapcar 'cdr m))                 		;ȥ��ȫ��Ϊ�����
	(progn 
	  (setq r (mat:vxs r (/ 1.0 a)))			;��һ��
	  (foreach n m
	    (if (equal (setq b (car n)) 0 1e-14)                ;�Ƿ���Ԫ��Ϊ0
	      (setq l (Mat:Join_Gauss n l))
	      (setq n (mapcar '- n (mat:vxs r b))               ;��Ԫ��
	            l (Mat:Join_Gauss n l)        		;����ͬ������ȥ
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
;;;�Ӹ�˹��Ԫ���õ��������ξ���ش��ⷽ��                       
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
;;;��һ�η�����                                                 
;;;=============================================================
(defun Mat:Gauss_Equations (mat)
  (Mat:TriangularForm (Mat:Gauss mat))
)

;;;=============================================================
;;; �����˱���(ϵ��)                                            
;;; Vector x Scalar - Lee Mac                                   
;;; Args: v - vector in R^n, s - real scalar                    
;;;=============================================================
(defun MAT:vxs ( v s )
  (mapcar (function (lambda ( n ) (* n s))) v)
)

;;;=============================================================
;;; �������ĵ��                                                
;;; Vector Dot Product                                          
;;; Input: v1,v2 -vectors in R^n                                
;;;=============================================================
(defun MAT:Dot (v1 v2)
  (apply '+ (mapcar '* v1 v2))
)

;;;-------------------------------------------------------------
;;; ����: �ж�ƽ���ϵ������Ƿ���                       	
;;; ����: ���� P1,P2,P3                                  	
;;; ���: T ˵�����㹲�ߣ����򲻹���                     	
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
;;; ����: ���������Xֵ��������                                 
;;; ����: ����p1,p2,p3�����                                  
;;; ���: ����Xֵ��С�������������                             
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
;;;����һ����(WCS)                                           	
;;;����: һ����ά���߶�ά�ĵ�                           	
;;;���: ��ʵ���ͼԪ��                                 	
;;;-------------------------------------------------------------
(defun Ent:Make_Point (p)
  (entmakex (list '(0 . "POINT") (cons 10 p)))
)

;;;-------------------------------------------------------------
;;;����һ����(UCS)                                           	
;;;����: һ����ά���߶�ά�ĵ�                           	
;;;���: ��ʵ���ͼԪ��                                 	
;;;-------------------------------------------------------------
(defun Ent:Make_Point1 (p)
  (entmakex (list '(0 . "POINT") (cons 10 (trans p 1 0))))
)

;;;-------------------------------------------------------------
;;;����һ��ֱ�߶�(WCS)                                       	
;;;����: ������ά���߶�ά�ĵ�                           	
;;;���: �߶�ʵ���ͼԪ��                               	
;;;-------------------------------------------------------------
(defun Ent:Make_Line (p q)
  (entmakeX (list '(0 . "LINE") (cons 10 p) (cons 11 q)))
)

;;;-------------------------------------------------------------
;;;����һ��ֱ�߶�(UCS)                                       	
;;;����: ������ά���߶�ά�ĵ�                           	
;;;���: �߶�ʵ���ͼԪ��                               	
;;;-------------------------------------------------------------
(defun Ent:Make_Line1 (p q)
  (Ent:Make_Line (trans p 1 0) (trans q 1 0))
)

;;;-------------------------------------------------------------
;;; ����һ������(WCS)                                         	
;;; ����: ����ͨ���Ļ���ͷ���ʸ��                              
;;; ���: �����������ͼԪ��                               	
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
;;; ����һ������(UCS)                                         	
;;; ����: ����ͨ���Ļ���ͷ���ʸ��                              
;;; ���: �����������ͼԪ��                               	
;;;-------------------------------------------------------------
(defun Ent:make_XLine1 (p v)
  (Ent:make_XLine (trans p 1 0) (trans v 1 0 T))
)

;;;-------------------------------------------------------------
;;; ������Բ(WCS)                                               
;;; ����: ��Բ������center,����major,����ratio                  
;;; ���: �ɹ�������Բ��ͼԪ�������򷵻�nil                     
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
;;; ������Բ(UCS)                                               
;;; ����: ��Բ������center,�볤��la,�����lb                    
;;; ���: �ɹ�������Բ��ͼԪ�������򷵻�nil                     
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
;;; ����: ��Spline����Բ׶����(WCS)                             
;;; ����: Բ׶���߲����������Լ�Ȩ��                            
;;; ���: ����������Բ׶����                                    
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
;;; ����: ��Spline����Բ׶����(UCS)                             
;;; ����: Բ׶���߲����������Լ�Ȩ��                            
;;; ���: ����������Բ׶����                                    
;;;=============================================================
(defun ENT:SplineBy3P1 (p1 p2 p3 w)
  (ENT:SplineBy3P (trans p1 1 0) (trans p2 1 0) (trans p3 1 0) w)
)

;;;-------------------------------------------------------------
;;;����������                                         	
;;;����: ��ά�ĵ㼯                                     	
;;;���: ������ʵ����                                 	
;;;-------------------------------------------------------------
(defun Ent:Make_LWPoly (pts closed /)
  (entmakeX                                              
    (VL-LIST*
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbPolyline")
      (cons 90 (length pts))                      	        ;�������
      (cons 70 (if closed 1 0))                          	;�պϵ�
      (mapcar (function (lambda (x) (cons 10 x))) pts)  	;����߶���
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
  (setq a (getreal "\n�����������߷���ϵ��a:"))
  (initget 1)
  (setq x (getdist "\n�����뷶Χ:"))
  (setq p (getpoint "\n�����������Դ�������ԭ��<Ĭ��(0,0)>:"))
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
  (princ (strcat "\n�������:" y " �ٷֱ�:" z))
  (princ)
)
   
(vl-load-com)
(princ "\nYZX �Ǹ���Բ׶�������ĸ�����ơ�")
(princ "\nSQX �Ǹ���˫���ߵ����ĺ��������᳤���ơ�")
(princ "\nPWX1 �Ǹ�������������������ơ�")
(princ "\nPWX2 �Ǹ��������߷��̵�����ϵ�����ơ�")
(princ "\nPWX3 �Ǹ������˵����ͣ��ߣ�����ơ�")
(princ)