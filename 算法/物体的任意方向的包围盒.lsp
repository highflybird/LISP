;|*************************************************************;
�������: Highflybird                                          ;
�����;: ��ȡ��ĳ���Ƕ��»���UCS�µ�����BOX   ����������������;
���ڵص�: 2021.08.30 ����                                      ;
��������: AutoLISP,Visual LISP                                 ;
�汾��:   Ver. 1.0.21.0830                                     ;
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

(defun c:ttt(/ ent i sel pts ang box rec)
  (vla-StartUndoMark (LM:acdoc))
  (setq ang (angle '(0 0 0) (getvar "ucsxdir")))
  (setq Pts nil)
  (if (setq sel (ssget '((0 . "*LINE,ARC,ELLIPSE,CIRCLE,TEXT,POINT,ATTDEF,INSERT"))))
    (repeat (setq i (sslength sel))
      (setq rec nil)
      (setq ent (ssname sel (setq i (1- i))))
      (setq rec (Ent:BoxByAngle ent ang 'pts))
      ;;(setq pts (cons rec pts))
      (setq rec (GEO:GetBoxBySet rec ang))			;����Ϊ������.
      (Ent:Make_LWPoly rec T)					;����Ϊ������.
    )
  )
  ;(setq pts (apply 'append pts))
  (setq box (GEO:GetBoxBySet pts ang))
  ;;(setq box (GEO:GetBoxByUCS pts))
  (Ent:Make_LWPoly box T)
  (vla-EndUndoMark (LM:acdoc))
  (princ)
)

(defun Ent:BoxByAngle (ent ang Points / dxf name p1 p2 lst)
  (setq dxf (entget ent))
  (setq name (cdr (assoc 0 dxf)))
  (cond
    ( (= name "LINE")
      (setq p1 (vlax-curve-getstartpoint ent))
      (setq p2 (vlax-curve-getendpoint ent))
      (set Points (cons p1 (vl-symbol-value Points)))
      (set Points (cons p2 (vl-symbol-value Points)))
      (GEO:GetBoxBySet (list p1 p2) ang)
    )
    ( (or
	(= name "LWPOLYLINE")
        (= name "POLYLINE")
        (= name "SPLINE")
        (= name "ARC")
        (= name "CIRCLE")
        (= name "ELLIPSE")
      )
      (Ent:BoxOfCurve ent ang Points)
    )
    ( (or (= name "TEXT") (= name "ATTDEF"))
      (setq lst (Ent:TextBox ent))
      (foreach n lst
	(set Points (cons n (vl-symbol-value Points)))
      )
      lst
    )
    ( (= name "INSERT")
      (setq r (cdr (assoc 50 dxf)))
      (setq m (MAT:RefGeom ent))
      (setq b (tblobjname "BLOCK" (cdr (assoc 2 dxf))))
      (setq y nil)
      (setq z nil)
      (while (setq b (entnext b))
	(setq rec (Ent:BoxByAngle b (- ang r) 'z))
	(if rec
	  (progn 
	    (setq rec (GEO:GetBoxBySet rec (- ang r)))
	    ;(setq rec (GEO:RotatePoints rec r))
	    (setq rec (mat:transpoints rec m))
	    (Ent:Make_LWPoly rec T)
	    (foreach n rec
	      (setq y (cons n y))
	    )
	  )
	)
      )
      (foreach x y
	(set points (cons x (vl-symbol-value points)))
      )
      y
    )
    ( (= name "POINT")
      (setq p1 (cdr (assoc 10 dxf)))
      (set Points (cons p1 (vl-symbol-value Points)))
      (list p1)
    )
  )
)

;;;-------------------------------------------------------------
;;; ����: ��ĳһ�����������������İ�Χ�е�                    
;;; ����: ent--ʵ��ͼԪ��                                       
;;;       ang--������X��н�                                    
;;; ���: ��Χ�е�                                              
;;;-------------------------------------------------------------
(defun Ent:BoxOfCurve (ent ang Points / ll ur R90 cen LEN P Vx Vy lst)
  (vla-GetBoundingBox (vlax-ename->vla-object ent) 'll 'ur)
  (setq ll  (vlax-safearray->list ll))
  (setq ur  (vlax-safearray->list ur))
  (setq R90 (* 0.5 pi))
  (setq cen (GEO:Midpoint ll ur))
  (setq len (distance ll ur))
  (setq Vx  (list (cos ang) (sin ang) 0))
  (setq Vy  (list (- (sin ang)) (cos ang) 0))
  (foreach n (list 0 R90 PI (+ R90 pi))
    (setq p (polar cen (+ ang n) len))
    (if (zerop (rem n pi))
      (setq p (vlax-curve-getClosestPointToProjection ent p Vy))
      (setq p (vlax-curve-getClosestPointToProjection ent p Vx))
    )
    (set Points (cons p (VL-SYMBOL-VALUE Points)))
    (setq lst (cons p lst))
  )
)

;;;-------------------------------------------------------------
;;; ��������ǵ�text���ĸ��ǵ�                                  
;;;-------------------------------------------------------------
(defun Ent:TextBox (ent / dxf rec ins rot pt1 pt2 pt3 pt4)
  (setq dxf (entget ent))					;;��ѡȡ���ı�����Ļ�ȡһЩ����
  (setq rec (textbox dxf))					;�ı��İ�Χ����
  (setq ins (cdr (assoc 10 dxf)))                               ;�����
  (setq rot (cdr (assoc 50 dxf)))                               ;��ת��
  (setq pt1 (car rec))
  (setq pt2 (cadr rec)) 
  (setq pt3 (list (car pt2) (cadr pt1)))
  (setq pt4 (list (car pt1) (cadr pt2)))
  (if (not (equal rot 0 1e-6))                                  ;�������ת��
    (setq pt1 (GEO:RotByAngle pt1 rot)
	  pt2 (GEO:RotByAngle pt2 rot)
          pt3 (GEO:RotByAngle pt3 rot)
	  pt4 (GEO:RotByAngle pt4 rot)
    )
  )
  (list
    (mapcar '+ pt1 ins)
    (mapcar '+ pt3 ins)
    (mapcar '+ pt2 ins)
    (mapcar '+ pt4 ins)
  )
)

;;;-------------------------------------------------------------
;;; ������ǵ�text���ĸ��ǵ�                                    
;;;-------------------------------------------------------------
(defun Ent:TextBox1 (ent / DXF INS ISX LEN OBL PT1 PT2 PT3 PT4 REC ROT)
  (setq dxf (entget ent))					;;��ѡȡ���ı�����Ļ�ȡһЩ����
  (setq rec (textbox dxf))					;�ı��İ�Χ����
  (setq ins (cdr (assoc 10 dxf)))                               ;�����
  (setq rot (cdr (assoc 50 dxf)))                               ;��ת��
  (setq obl (cdr (assoc 51 dxf)))                               ;��б��
  (setq isX (cdr (assoc 71 dxf)))
  (setq pt1 (car rec))
  (setq pt2 (cadr rec))
  (if (or (= isX 2) (= isX 4))
    (setq obl (+ (* pi 0.5) obl)
	  pt1 (list (car pt1) (cadr pt2) (caddr pt1))
	  pt2 (list (car pt2) (cadar rec) (caddr pt2))   
    )
    (setq obl (- (* pi 0.5) obl))
  )  
  (setq len (distance pt1 pt2))
  (setq pt3 (inters Pt1 (polar pt1 0 len) pt2 (polar pt2 obl len) nil))
  (setq pt4 (inters Pt1 (polar pt1 obl len) pt2 (polar pt2 0 len) nil))
  (if (not (equal rot 0 1e-6))
    (setq pt1 (GEO:RotByAngle pt1 rot)
	  pt2 (GEO:RotByAngle pt2 rot)
          pt3 (GEO:RotByAngle pt3 rot)
	  pt4 (GEO:RotByAngle pt4 rot)
    )
  )
  (list
    (mapcar '+ pt1 ins)
    (mapcar '+ pt2 ins)
    (mapcar '+ pt3 ins)
    (mapcar '+ pt4 ins)
  )
  ;(ent:make_line pt1 pt2)
  ;(Ent:Make_LWPoly (list pt1 pt3 pt2 pt4) T)
)

;;;-------------------------------------------------------------
;;; ���Ե����ı�����                                            
;;;-------------------------------------------------------------
(defun c:tt ()
  (if (setq ent (car (entsel "\n��ѡȡ�����ı�: "))) 
    (Ent:Make_LWPoly (Ent:TextBox ent) T)
  )
)
 
;;;-------------------------------------------------------------
;;; ����: ��ĳһ������ĵ㼯�İ�Χ��                            
;;; ����: pts--�㼯                                             
;;;       ang--������X��н�                                    
;;; ���: ��Χ�е�(WCS)                                         
;;;-------------------------------------------------------------
(defun GEO:GetBoxBySet (pts ang / pMin pMax)
  (setq pts (mapcar (function (lambda (p) (GEO:RotByAngle p (- ang)))) pts))
  (setq pMin (apply 'mapcar (cons 'min pts)))
  (setq pMax (apply 'mapcar (cons 'max pts)))
  (mapcar
    (function (lambda (p) (GEO:RotByAngle p ang)))
    (list
      (list (car pMin) (cadr pMin))
      (list (car pMax) (cadr pMin))
      (list (car pMax) (cadr pMax))
      (list (car pMin) (cadr pMax))
    )
  )
)

;;;-------------------------------------------------------------
;;; ����: ��UCS����ĵ㼯�İ�Χ��                               
;;; ����: pts--�㼯                                             
;;; ���: ��Χ�е�(WCS)                                         
;;;-------------------------------------------------------------
(defun GEO:GetBoxByUCS (pts / pMin pMax)
  (setq pts (mapcar (function (lambda (p) (trans p 0 1))) pts))
  (setq pMin (apply 'mapcar (cons 'min pts)))
  (setq pMax (apply 'mapcar (cons 'max pts)))
  (mapcar
    (function (lambda (p) (trans p 1 0)))
    (list
      (list (car pMin) (cadr pMin))
      (list (car pMax) (cadr pMin))
      (list (car pMax) (cadr pMax))
      (list (car pMin) (cadr pMax))
    )
  )
)

;;;-------------------------------------------------------------
;;; ��һ���㼯ʩ�Ӿ���任                                      
;;;-------------------------------------------------------------
(defun MAT:TransPoints (pts mat)
  (mapcar
    (function
      (lambda (p)
	(mapcar '+ (mat:mxv (car mat) p) (cadr mat))
      )
    )
    pts
  )
)

;;;-------------------------------------------------------------
;;; ��һ���㼯ʩ����ת�任                                      
;;;-------------------------------------------------------------
(defun GEO:RotatePoints (pts ang)
  (mapcar (function (lambda (p) (GEO:RotByAngle p ang))) pts)
)
 
;;;-------------------------------------------------------------
;;; ���ܣ�ͼ��ı任����                                        
;;;-------------------------------------------------------------
(defun MAT:RefGeom (ename / DXF ang nrm mat DISP sx sy sz sa ca)
  (setq	DXF (entget ename)
	ang (cdr (assoc 50 DXF))
	nrm (cdr (assoc 210 DXF))
	sx  (cdr (assoc 41 DXF))
	sy  (cdr (assoc 42 DXF))
	sz  (cdr (assoc 43 DXF))
	sa  (sin ang)
	ca  (cos ang)
  )
  (list
    (setq mat (MAT:mxm
		(mapcar
		  (function (lambda (v) (trans v 0 nrm T)))
		  '((1.0 0.0 0.0) (0.0 1.0 0.0) (0.0 0.0 1.0))
		)
		(list
		  (list (* ca sx) (- (* sa sy)) 0.0)
		  (list (* sa sx) (* ca sy) 0.0)
		  (list 0 0 sz)
		)
	      )
    )
    (mapcar
      '-
      (trans (cdr (assoc 10 DXF)) nrm 0)
      (MAT:mxv
	mat
	(cdr (assoc 10 (tblsearch "BLOCK" (cdr (assoc 2 DXF)))))
      )
    )
  )
)


;;;-------------------------------------------------------------
;;; ���ܣ�ͼ��ı任����������,                               
;;; ���룺����յ�ͼԪ��,                                       
;;; ���������յı任����������                              
;;;-------------------------------------------------------------
(defun MAT:RevRefGeom (ename / dxf ang nrm mat disp)
  (setq dxf (entget ename))
  (setq ang (- (cdr (assoc 50 dxf))))
  (setq nrm (cdr (assoc 210 dxf)))
  (list
    (setq mat (MAT:mxm
		(list (list (/ 1 (cdr (assoc 41 dxf))) 0.0 0.0)
		      (list 0.0 (/ 1 (cdr (assoc 42 dxf))) 0.0)
		      (list 0.0 0.0 (/ 1 (cdr (assoc 43 dxf))))
		)
		(MAT:mxm
		  (list	(list (cos ang) (- (sin ang)) 0.0)
			(list (sin ang) (cos ang) 0.0)
			'(0.0 0.0 1.0)
		  )
		  (mapcar (function (lambda (v) (trans v nrm 0 T)))
			  '((1.0 0.0 0.0) (0.0 1.0 0.0) (0.0 0.0 1.0))
		  )
		)
	      )
    )

    (mapcar
      '-
      (cdr (assoc 10 (tblsearch "BLOCK" (cdr (assoc 2 dxf)))))
      (MAT:mxv mat (trans (cdr (assoc 10 dxf)) nrm 0))
    )
  )
)




;;;-------------------------------------------------------------
;;; ����: ����֮�е�                                            
;;; ����: ����p1,P2                                             
;;; ���: �е�λ��                                              
;;;-------------------------------------------------------------
(defun GEO:Midpoint (p1 p2)
  (mapcar (function (lambda (e1 e2) (* (+ e1 e2) 0.5))) p1 p2)
)

;;;-------------------------------------------------------------
;;; ����: λ�Ƶ�                                                
;;; ����: ��P��λ����v                                          
;;; ���: λ�ƺ��λ��                                          
;;;-------------------------------------------------------------
(defun Geo:displacement (p v)
  (mapcar '+ p v)
)

;;;-------------------------------------------------------------
;;; ����: ��ת��                                                
;;; ����: ��P���Ƕ�ang                                          
;;; ���: �е�λ��                                              
;;;-------------------------------------------------------------
(defun GEO:RotByAngle (p ang / C S)
  (setq c (cos ang))
  (setq s (sin ang))
  (list
    (- (* C (car p)) (* S (cadr p)))
    (+ (* S (car p)) (* C (cadr p)))
  )
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

;;;-------------------------------------------------------------
;;;����һ��ֱ�߶�                                       	
;;;����: ������ά���߶�ά�ĵ�                           	
;;;���: �߶�ʵ���ͼԪ��                               	
;;;-------------------------------------------------------------
(defun Ent:Make_Line (p q)
  (entmakeX (list '(0 . "LINE") (cons 10 p) (cons 11 q)))
)

;;;-------------------------------------------------------------
;;;��������                                         	        
;;;����: ���ε������ǵ�                                    	
;;;���: ���ε�ʵ����                                  	        
;;;-------------------------------------------------------------
(defun Ent:Make_Rectangle (ll ur /)
  (entmakeX
    (list 
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbPolyline")
      '(90 . 4)
      '(70 . 1)
      (list 10 (car ll) (cadr ll))
      (list 10 (car ur) (cadr ll))
      (list 10 (car ur) (cadr ur))
      (list 10 (car ll) (cadr ur))
    )
  )
)

;;;-------------------------------------------------------------
;;; Active Document  -  Lee Mac                                 
;;; Returns the VLA Active Document Object                      
;;;-------------------------------------------------------------
(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)