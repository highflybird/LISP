;|*************************************************************;
�������: Highflybird                                          ;
�����;: ΪAutoCAD ��LISP���Ƶ�һЩ�㷨�ͺ���(͸�ӱ任����)   ;
���ڵص�: 2019.04.21 ����                                      ;
�޸�ʱ��: 2019.04.21 ����                                      ;
��������: AutoLISP,Visual LISP                                 ;
�汾��:   Ver. 1.0.19.0421                                     ;
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
;;; ����: ������һ,ͨ����ѡ������İ�Χ�кͶ�Ӧ���ĸ��任�ǵ� ;;
;;;       �γ�һ��͸�ӱ任���õ�ѡ�������͸��ͼ��            ;;
;;; ����: �ޡ�                                                ;;
;;; ����: �ޡ�                                                ;;
;;;-----------------------------------------------------------;;
(defun c:tts (/ ENT I MT OBJ P0 P1 P2 P3 Q0 Q1 Q2 Q3 PP RC SEL STR SYM)
  (if (setq sel (ssget '((0 . "*LINE,CIRCLE,ELLIPSE,ARC,INSERT"))))
    (progn
      (setq i 0)
      (repeat (sslength sel)
        (setq ent (ssname sel i))
        (setq obj (vlax-ename->vla-object ent))
        (vla-GetBoundingBox obj 'p0 'p2)			;�õ���Χ��
        (setq p0 (vlax-safearray->list p0))			;�õ����½�
        (setq p2 (vlax-safearray->list p2))			;�õ����Ͻ�
        (setq RC (cons p2 (cons p0 RC)))			;�ѵ�������뵽��Χ��㼯��
        (Ent:Divide ent nil 30 'pp)                      	;�ȷ�����
        (setq i (1+ i))
      )
      ;;��ȡѡ�񼯵İ�Χ��
      (setq p0 (apply 'mapcar (cons 'min RC))) 			;���½�
      (setq p2 (apply 'mapcar (cons 'max RC))) 			;���Ͻ�
      (setq p3 (list (car p0) (cadr p2) (caddr p0))) 		;���Ͻ�
      (setq p1 (list (car p2) (cadr p0) (caddr p2))) 		;���½�
      ;;����ѡȡ�ĸ�ӳ���
      (while (setq q0 (getpoint "\nʰȡ��1����:"))
        (setq i 1)
        (repeat 3
          (initget 9)
          (setq str (strcat "\nʰȡ��" (itoa (1+ i)) "����:"))
          (setq sym (read (strcat "q" (itoa i))))
          (set sym (getpoint str))
          (set sym (trans (eval sym) 1 0))
          (setq i (1+ i))
        )
	;;������������㲻����
	(if (and		
	      (GEO:Verify4P p0 p1 p2 p3)
	      (GEO:Verify4P q0 q1 q2 q3)
            )
	  (progn
	    ;;����ӳ���ı���
	    (ENT:MAKE_LWPOLY (list q0 q1 q2 q3) t)		
	    ;;��ñ任����
            (setq mt (Mat:PerspectiveTrans p0 p1 p2 p3 q0 q1 q2 q3))
	    ;;����͸��ͼ
            (TransformByEntity pp mt)
	  )
	  (princ "\nѡȡ��������ߵ㲻����Ҫ��!")
	)
      )
    )
  )
  (princ)
)

;;;-----------------------------------------------------------;;
;;; ����: �������--ͨ����ԭ���ı��ε�ӳ���ı����γɵ���Ӧ��͸;;
;;;       �ӱ任��Ȼ���ѡ������Ĳ��������͸�ӱ任������γ�;;
;;;       ѡ�������͸��ͼ��                                  ;;
;;; ����: �ޡ�                                                ;;
;;; ����: �ޡ�                                                ;;
;;;-----------------------------------------------------------;;
(defun c:ttt (/ E0 E1 ENT I M0 M1 M2 MT O0 O1 P0 P1 P2 P3 PP Q0 Q1 Q2 Q3 SEL)
  (initget 1)
  (if (and
	(setq e0 (car (entsel "\nʰȡԭ���ı���(�����):")))
	(setq e1 (car (entsel "\nʰȡӳ���ı���(�����):")))
	(setq o0 (vlax-ename->vla-object e0))
	(setq o1 (vlax-ename->vla-object e1))
	(vlax-property-available-p o0 'constantwidth)
	(vlax-property-available-p o1 'constantwidth)
	(>= (vlax-curve-getendparam e0) 4)
	(>= (vlax-curve-getendparam e1) 4)
	(setq sel (ssget '((0 . "*LINE,CIRCLE,ELLIPSE,ARC,INSERT"))))
      )
    (progn
      ;;��ȡԭ���ı���
      (setq p0 (POLY:GetCoordinate o0 0))
      (setq p1 (POLY:GetCoordinate o0 1))
      (setq p2 (POLY:GetCoordinate o0 2))
      (setq p3 (POLY:GetCoordinate o0 3))
      ;;��ȡӳ���ı���
      (setq q0 (POLY:GetCoordinate o1 0))
      (setq q1 (POLY:GetCoordinate o1 1))
      (setq q2 (POLY:GetCoordinate o1 2))
      (setq q3 (POLY:GetCoordinate o1 3))
      (if (and (GEO:Verify4P p0 p1 p2 p3)
               (GEO:Verify4P q0 q1 q2 q3)
	  )
	(progn 
          (setq m0 (Mat:SquareToQuadrilateral p0 p1 p2 p3))	;��λ�����ε�ӳ���ı��εı任
          (setq m1 (Mat:QuadrilateralToSquare p0 p1 p2 p3))	;ӳ���ı��ε���λ�����εı任
          (setq m2 (Mat:SquareToQuadrilateral q0 q1 q2 q3))	;��λ�����ε�ԭӳ�ı��εı任
	  ;(setq m22 m2)
	  ;(setq xxx (anm q0 q1 q2 q3))
          (setq mt (mat:mxm m1 m2))                             ;���յ�͸�ӱ任
          (setq i 0)
          (setq pp nil)
	  (repeat (sslength sel)
	    (setq ent (ssname sel i))
            (Ent:Divide ent nil 30 'pp)                         ;��ȡÿ��������ĵȷֵ�����
	    (setq i (1+ i))
          )
	  (TransformByEntity pp mt)				;��ÿ��������ĵ����͸�ӱ任
	)
	(princ "\nѡȡ�ı��β�����Ҫ��!")
      )
    )
  )
  (princ)
)

;;;-----------------------------------------------------------;;
;;; ����: �����õ��㲢�任�͸���ԭ��������                    ;;
;;; ����: pp----���ߵ�ͼԪ����                                ;;
;;;       mat----�任���󣨶��ڲ����Ϊ��任��������Ϊnil��;;
;;; ����: �ޡ�                                                ;;
;;; ˵��: �˾���Ϊ3X3 ������ˣ�����ƽ�����˵����ֱ�����  ;;
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
;;; ����: �ȷ����ߣ�������Ƕ�ײ���飩                        ;;
;;; ����: ent----���ߵ�ͼԪ����                               ;;
;;;       mat----�任���󣨶��ڲ����Ϊ��任��������Ϊnil��;;
;;;       Seg----�ȷ������ߵȷ־���                           ;;
;;;       Dat----���ص��б�(�൱�ڴ�ַ)��ÿ��Ԫ��Ϊ�㼯��ͼԪ ;;
;;; ����: �ޡ�                                                ;;
;;; ˵��: �˾���Ϊ3X3 ������ˣ�����ƽ�����˵����ֱ�����  ;;
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
    ( (= typ "INSERT")                                          ;���ԶԲ���飨��Ƕ�ף���͸�ӱ任
      (setq m0 (MAT:RefGeom ent))
      (setq m0 (Mat:DispToMatrix (car m0) (cadr m0)))
      (if mat
	(setq mat (mat:mxm mat m0))
	(setq mat m0)
      )
      (setq eblk (TBLobjname "BLOCK" (cdr (assoc 2 dxf))))	;ͼ�鶨���ͼԪ��
      (setq next (cdr (assoc -2 (entget eblk))))  		;ͼ�鶨�������ͼ�鶨��ĵ�һ��ͼԪ
      (while next
	(Ent:Divide next mat Seg dat)                         	;����ͼ�鶨��
        (setq next (entnext next))                            	;ͼ�鶨�����һ��ʵ��
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
;;; ����: �����ĵ�����Ӧ��͸�ӵ��ȡ͸�ӱ任����            ;;
;;; ����: p0,p1,p2,p3-----Ҫ�任���ĸ��ǵ㣨�������㲻���ߣ�  ;;
;;;       q0,q1,q2,q3-----�任����ĸ�ӳ��㣨�������㲻���ߣ�;;
;;; ����: ��͸�ӱ任�ľ���                                  ;;
;;; ˵��: �˾���Ϊ3X3 ������ˣ�����ƽ�����˵����ֱ�����  ;;
;;;-----------------------------------------------------------;;
(defun Mat:PerspectiveTrans (p0 p1 p2 p3 q0 q1 q2 q3)
  (mat:mxm
    (Mat:QuadrilateralToSquare p0 p1 p2 p3)
    (Mat:SquareToQuadrilateral q0 q1 q2 q3)
  )
)

;;;-----------------------------------------------------------;;
;;; ����: ��ȡ��λ�����ε�ָ���ĸ��ǵ��͸�ӱ任����          ;;
;;; ����: p0,p1,p2,p3-----�任����ĸ�ӳ��㣨�������㲻���ߣ�;;
;;; ����: ��͸�ӱ任�ľ���                                  ;;
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
;;; ����: ��ȡָ���ĸ��ǵ㵽��λ�����ε�͸�ӱ任����          ;;
;;; ����: p0,p1,p2,p3-----�任ǰ���ĸ��ǵ㣨�������㲻���ߣ�  ;;
;;; ����: ��͸�ӱ任�ľ���                                  ;;
;;;-----------------------------------------------------------;;
(defun Mat:quadrilateralToSquare (p0 p1 p2 p3)
  (Mat:buildAdjoint (Mat:SquareToQuadrilateral p0 p1 p2 p3))
)

;;;-----------------------------------------------------------;;
;;; ����: 3�װ������                                         ;;
;;; ����: ���׾���(3X3����)                                   ;;
;;; ����: �˾���İ������                                  ;;
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
;;; ����: ����͸�ӱ任����任��                              ;;
;;; ����: һ��P ������͸�ӱ任����(3X3����)�ĸ�Ԫ��ֵ         ;;
;;; ����: �任��ĵ����ꡣ                                    ;;
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
;;; ����: ����͸�ӱ任����任�㼯                            ;;
;;; ����: ����͸�ӱ任����(3X3����)��Ҫ�任�ĵ㼯Pts          ;;
;;; ����: �任��ĵ㼯��                                      ;;
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
;;; ����: ���ƽ���ĵ��Ƿ���Ч�������������㹲�ߵ����        ;;
;;; ����: p0,p1,p2,p3----Ҫ�����ĵ�                         ;;
;;; ����: ����������㹲�ߣ��򷵻�nil,���򷵻�T��             ;;
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
;;; ����: �ȷ�һ������                                        ;;
;;; ����: e----Ҫ�ȷֵ�ͼԪ��                                 ;;
;;;       seg----Ϊ���������ȷ֣��������򰴾�ȷ�             ;;
;;; ����: �ȷֺ�ĵ������                                  ;;
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
;;; ����: �ȷ������ߣ�ֻ�ȷֻ��Σ����ȷ�ֱ�߶Σ�            ;;
;;; ����: pline----������ͼԪ                               ;;
;;;       n----Ϊ���������ȷ֣��������򰴾�ȷ�               ;;
;;; ����: �ȷֺ�ĵ������                                  ;;
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
;;; ����: ��ȡ����߶�������                                  ;;
;;; ����: obj----�����߶���                                 ;;
;;;       Index----�����                                     ;;
;;; ����: �˵�����ꡣ                                        ;;
;;;-----------------------------------------------------------;;
(defun POLY:GetCoordinate (obj Index)
  (vlax-safearray->list
    (vlax-variant-value
      (vla-get-Coordinate obj Index)
    )
  )
)

;;;-----------------------------------------------------------;;
;;; ����: ��ȡ�����ͨ������                                  ;;
;;; ����: o----�����vla-objectֵ                             ;;
;;; ����: �����������(ͼ�㣬���ͣ����ͱ������߿���ɫ)�б�;;
;;;-----------------------------------------------------------;;
(defun Ent:GetCommonProperties (o)
  (mapcar
    (function (lambda (f) (vlax-get o f)))
    '(layer linetype linetypescale lineweight truecolor) 
  )
)

;;;-----------------------------------------------------------;;
;;; ����: �޸������ͨ������                                  ;;
;;; ����: o----�����vla-objectֵ                             ;;
;;;       lst----����(ͼ�㣬���ͣ����ͱ������߿���ɫ)�б� ;;
;;; ����: ��                                                  ;;
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
  (princ "\n��ʱ:")
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

