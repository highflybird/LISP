;|**************************************************************
������������ͳ�ƹ���                                          
****************************************************************
�������: Highflybird                                           
�����;: ������������ͳ��                                    
���ڵص�: 2016.03.13 ����                                       
��������: AutoLISP,Visual LISP                                  
�汾��:   Ver. 1.0.160313                                       
================================================================
�����Ϊ��Դ���: �����ǿ�Դ����:                               
----------------------------------------------------------------
��ҳ���������� GPLЭ�鿪��Դ���룬���������ɴ������޸ģ�������
�����Լ��������ǰ����:                                         
                                                                
һ. ֻҪ���ڱ���Դ�����ÿһ���������Ժ�ǡ���س����Ȩ����������
�����֤��������û�е����������������𣬲��ͳ���һ���ÿ��������
���������һ�����֤�ĸ�������Ϳ������κ�ý�帴�ƺͷ������յ���
ԭʼ�ĳ����Դ���롣��Ҳ����Ϊת�ø�����ʵ���ж���ȡһ�����ã���
�������ȵõ���ͬ�⡣                                            
                                                                
��. ������޸ı���Դ�����һ���򼸸������������κβ��֣��Դ���
�ɻ��ڳ������Ʒ��ֻҪ��ͬʱ���������������������Ϳ��԰�ǰ���
һ���Ҫ���ƺͷ�����һ�����޸ĵĳ������Ʒ��                  
1.��������޸ĵ��ļ��и�����ȷ��˵��: ���޸�����һ�ļ����������
  �����ڡ�                                                      
2.�����ʹ�㷢����������Ʒ�������������ȫ����һ���֣��������
  �����ȫ���򲿷���������Ʒ�������������Ϊ���尴���֤�������
  ʹ�á�                                                        
3.����޸ĵĳ���������ʱ�Խ�����ʽ��ȡ��������ʹ���ڿ�ʼ����
  ����Ľ���ʹ�÷�ʽʱ��ӡ����ʾ����: �����ʵ��İ�Ȩ������û�е�
  �����������������ṩ���������������û����԰������֤�������·�
  �������˵�����������û���ο�����һ���֤�ĸ��������������: 
  ���ԭʼ�����Խ�����ʽ��������������ӡ��������������Ļ��ڳ���
  ����ƷҲ�Ͳ��ô�ӡ������                                      
                                                                
��. ֻҪ����ѭһ��������涨�����Ϳ�������ʹ�ò�������Դ���룬��
����ԭ�ⲻ���ر���ԭ������Ϣ��                                  
**************************************************************|;
(prompt "\nͳ����ֵ����Ϊ:AM.��������Ϊ:ASET")
(vl-load-com)
(defun C:am (/ d    e    h    i    n    o    p    x    ll   ur
	       pt   fil  sel  Lst  foo  val  str  sum  prop Data 
	       *DOC mode PREC Unit pref suff fact )
  ;;��ȡ����
  (if (null (setq Data (getenv "Statistics")))
    (setq Data (SetDefault))
    (setq Data (read Data))
  )
  ;;����ѡ��
  (setq fil "")
  (foreach n (S:TypeList)
    (if	(= "1" (cdr (assoc n Data)))
      (if (= "OTHER" n)
	(setq fil (strcat fil "*,"))
	(setq fil (strcat fil n ","))
      )
    )
  )
  (if (setq Sel (ssget (list (cons 0 fil))))
    (progn
      ;;��������,����,λ��
      (setq prop (GetTextProps))
      (setq mode (cdr (assoc "Position" Data)))
      (setq Prec (atoi (cdr (assoc "Precision" Data))))
      (setq Unit (getvar "LUNITS"))
      ;;ǰ׺�Ͳ�����λ���������ӣ����㺯��
      (GetFactor&Unit Data 'Suff 'fact 'foo)
      (if (= "1" (cdr (assoc "HasPrefix" Data)))
	(setq pref (cdr (assoc "Prefix" Data)))
	(setq pref "")
      )
      ;;���û��˱�־
      (setq *DOC (vla-get-activeDocument (vlax-get-acad-object)))
      (vla-StartUndoMark *DOC)
      ;;ȡ��ÿ��ͼԪ����Ϣ�Ͳ���ֵ
      ;(setq TIME (getvar "millisecs"))
      (setq i 0)
      (setq sum 0)
      (repeat (sslength Sel)
	(setq e (ssname sel i))
	(setq o (vlax-ename->vla-object e))
	(setq n (substr (vla-get-objectname o) 5))
	(setq h (vla-get-Handle o))
	(setq x (list e n h))
	(setq i (1+ i))

	(vla-GetBoundingBox o 'll 'ur)
	(setq ll (vlax-safearray->list ll))
	(setq ur (vlax-safearray->list ur))
	(setq Pt (S:Text-Position ll ur mode))

	(setq val (* fact (apply foo (list e))))
	(setq str (rtos val Unit Prec))
	(setq Lst (cons (list X str Pt) Lst))
	(setq Sum (+ val Sum))
      )
      ;(setq TIME (- (getvar "millisecs") TIME))
      ;(princ "\n����ʱ��")
      ;(princ time)
      ;;����ֵ������
      ;(setq Lst (vl-sort Lst 'S:Compare))
      (setq Lst (reverse lst))
      ;;�����
      (initget 1)
      (setq foo (cdr (assoc "Output" Data)))
      (if (or (= foo "DRAWING") (= foo "TABLE"))
	(setq pt (trans (getpoint "\n�����:") 1 0))
	(setq pt '(0 0 0))
      )
      ;;ͳ�Ʋ���ֵ
      (setq str (rtos Sum Unit Prec))
      (setq val (strcat (itoa (length lst)) "��"))
      (setq val (list (list nil "�ܼ�" val) str pt))
      (if (= "1" (cdr (assoc "JustTotal" Data)))
	(setq Lst (list val))
	(setq lst (append Lst (list val)))
      )
      ;;Ϊ���������ʽ����
      (setq Str (strcat "����ֵ(" suff ")"))
      (setq val (list (list nil "ͼԪ����" "���") str pt))
      (setq lst (cons val lst))
      ;;�����ʽ
      (setq foo (read (strcat "S:Output-By-" foo)))
      (apply foo (list Lst pref suff prop))			;((eval foo) Lst pref suff prop)
      (vla-EndUndoMark *DOC)
    )
    (alert "��û��ѡȡ�������������ȷ������!")
  )
  (princ)
)

;;;-------------------------------------------------------------
;;; ��Ļ������ļ�����Ĺ�ͬ��                                  
;;;-------------------------------------------------------------
(defun S:OutPut-Common (lst pref suff file)
  (if file
    (defun S:Princ (str f) (princ str f))
    (defun S:Princ (str f) (princ str))
  )
  (S:Princ "ÿ����������Ϊ: " file)
  (foreach n Lst
    (S:Princ (strcat "\n" (cadar n) ": " (caddar n) ": " (cadr n)) file)			
  )
)

;;;-------------------------------------------------------------
;;; ��Ļ��ʾ                                                    
;;;-------------------------------------------------------------
(defun S:Output-By-Window (lst pref suff prop)
  (textSCR)
  (S:OutPut-Common lst pref suff nil)
  (textSCR)
)

;;;-------------------------------------------------------------
;;; д�ļ���ʽ���                                              
;;;-------------------------------------------------------------
(defun S:Output-By-File (lst pref suff prop / file)
  (if (setq file (getfiled "�����ļ���: " "C:/temp/" "txt" 5))
    (progn
      (setq file (open file "w"))
      (S:OutPut-Common lst pref suff file)
      (close file)
    )
    (alert "����ļ�ʧ��!")
  )
)

;;;-------------------------------------------------------------
;;; �ñ��ʽ���                                              
;;;-------------------------------------------------------------
(defun S:Output-By-Table (lst pref suff A1 / A2 H0 H1 P P0 P1 Q S X0 X1 X2 X3 Y Y0 Z0)
  (initget 1)                                       
  (setq A1 (subst '(72 . 0) (assoc 72 A1) A1))			;�����
  (setq A2 (subst '(72 . 2) (assoc 72 A1) A1))			;�Ҷ���
  (setq p0 (caddr (last lst)))					;�������
  (setq x0 (car p0))						;�����X
  (setq y0 (cadr p0))						;�����Y
  (setq z0 (caddr p0))                                          ;�����Z
  (setq h0 (cdr (assoc 40 A1)))					;���ָ�
  (setq h1 (* h0 0.2))
  (setq x1 (+ (*  8 h0) x0))					;���ڶ�������Xֵ
  (setq x2 (+ (* 12 h0) x0))					;������������Xֵ
  (setq x3 (+ (* 24 h0) x0))					;�����ĸ�����Xֵ
  ;;���̧ͷ��
  (setq P1 (list x3 y0 z0))
  (Ent:make-line P0 P1)
  ;;����񣬲���д����
  (setq y y0)
  (foreach n Lst
    (setq y (- y h0))
    (S:Make-Text nil (cadar  n) (list (+ x0 h1) y z0) A1)	;ͼԪ����
    (S:Make-Text nil (caddar n) (list (- x2 h1) y z0) A2)	;ͼԪ���
    (S:Make-Text nil (cadr   n) (list (- x3 h1) y z0) A2)	;ͼԪ����ֵ
    (setq y (- y h0))
    (setq p (list x0 y z0))
    (setq q (list x3 y z0))
    (ent:make-line p q)						;�������
  )
  (ent:make-line P0 p)						;����һ������
  (ent:make-line P1 q)						;�����ĸ�����
  (ent:make-line (list x1 y0 z0) (list x1 y z0))		;���ڶ�������
  (ent:make-line (list x2 y0 z0) (list x2 y z0))		;������������
)

;;;-------------------------------------------------------------
;;; дExcel��ʽ���                                             
;;;-------------------------------------------------------------
(defun S:Output-By-Excel (lst pref suff prop / exl)
  (if (setq exl (vlax-get-or-create-object "Excel.Application"))
    (S:FillCells exl lst pref suff)
    (alert "�Ҳ���Excel������߲�����������Excel�������˳�!")
  )
)
(defun S:FillCells (exl lst pref suff / books book sheets sheet cells row prec col str)
  ;;��excel�����л�ȡexcel��book,sheet�͵�Ԫ�񼯺�
  (setq books  (vlax-get-property exl 'Workbooks))
  (setq book   (vlax-invoke-method books 'add))
  (setq sheets (vlax-get-property book 'worksheets))
  (setq sheet  (vlax-get-property sheets 'item 1))
  (setq cells  (vlax-get-property sheet 'cells))
  (setq col    (vlax-get-property sheet 'range "C:C"))
  ;;����ֵ�ľ�ȷλ��
  (setq Prec   (atoi (GetKeyData "Precision")))
  (setq prec   (strcat (rtos 0 (getvar "LUNITS") Prec) "_ "))    
  (vlax-put-property cells 'numberformat "@")			;������Ԫ��Ϊ�ı�
  (vlax-put-property cells 'ColumnWidth 12)			;�п�
  (vlax-put-property col 'numberformat prec)                    ;��ֵ��ȷλ
  (vlax-put-property col 'ColumnWidth 16)                       ;����ֵ�п�
  (vlax-put-property col 'HorizontalAlignment 4)
  ;;���ÿ��
  (setq row 1)
  (foreach n Lst
    (vlax-put-property cells 'item row 1 (cadar  n))
    (vlax-put-property cells 'item row 2 (caddar n))
    (vlax-put-property cells 'item row 3 (cadr   n))
    (setq row (1+ row))
  )
  (vlax-put-property exl 'UserControl 1)
  (vlax-put-property exl 'Visible 1)
)

;;;-------------------------------------------------------------
;;; ��ͼ�б�ע���                                              
;;;-------------------------------------------------------------
(defun S:Output-By-Drawing (lst pref suff prop / str)
  (foreach n (cdr Lst)
    (S:Make-Text
      (car n)
      (strcat pref (cadr n) suff)
      (caddr n)
      prop
    )
  )
)

;;;-------------------------------------------------------------
;;; ��ȡ����ĳ���                                              
;;;-------------------------------------------------------------
(defun Ent:Get-Length (ent / D N O)
  (setq O (vlax-ename->vla-object ent))
  (setq D (entget ent))
  (setq N (cdr (assoc 0 D)))
  (cond
    ( (vlax-property-available-p O 'length)
      (vla-get-length O)
    )
    ( (vlax-property-available-p O 'Perimeter)
      (vla-get-Perimeter O)
    )
    ( (member N '("ARC" "CIRCLE" "LINE" "POLYLINE" "LWPOLYLINE" "SPLINE" "ELLIPSE"))
      (vlax-curve-getdistatparam ent (vlax-curve-getendparam ent))
    )
    ( (= N "MLINE") (ml-length D))
    (T 0)
  )
)

;;;-------------------------------------------------------------
;;; MLINE�ĳ���                                                 
;;;-------------------------------------------------------------
(defun ml-length (DXF / pts)
  (setq pts (vl-remove-if-not '(lambda (x) (= (car x) 11)) dxf))
  (setq pts (mapcar 'cdr pts))
  (length-of-verties pts (zerop (logand (cdr (assoc 71 dxf)) 2)))
)

;;;-------------------------------------------------------------
;;; n����ĳ���                                                 
;;;-------------------------------------------------------------
(defun length-of-verties (pts IsClose /)
  (if isClose
    (apply '+ (mapcar 'distance pts (cons (last pts) pts)))
    (apply '+ (mapcar 'distance pts (cdr pts)))
  )
)
 
;;;-------------------------------------------------------------
;;; ��ȡ��������                                              
;;;-------------------------------------------------------------
(defun Ent:Get-Area (ent / O N D)
  (setq O (vlax-ename->vla-object ent))
  (setq D (entget ent))
  (setq N (cdr (assoc 0 D)))
  (cond
    ( (and (wcmatch N "*LINE") (/= N "MLINE"))
      (vlax-curve-getArea ent)
    )
    ( (vlax-property-available-p O 'area)
      (vla-get-area O)
    )
    ( (= N "3DFACE")
      (Area-of-Verties
        (list
	  (cdr (assoc 10 D))
	  (cdr (assoc 11 D))
	  (cdr (assoc 12 D))
	  (cdr (assoc 13 D))
        )
      )
    )
    ( (= N "SOLID") 
      (Area-of-Verties
        (list
	  (cdr (assoc 10 D))
	  (cdr (assoc 11 D))
	  (cdr (assoc 13 D))
	  (cdr (assoc 12 D))
        )
      )
    )
    (T 0)
  )
)

;;;-------------------------------------------------------------
;;; n��������                                                 
;;;-------------------------------------------------------------
(defun S:Det2 (p1 p2)
  (- (* (car p1) (cadr p2)) (* (car p2) (cadr p1)))
)
(defun Area-of-Verties(pts /)
  (* 0.5 (abs (apply '+ (mapcar 'S:Det2 (cons (last pts) pts) pts))))
)


;;;-------------------------------------------------------------
;;; ��ȡ��������                                              
;;;-------------------------------------------------------------
(defun Ent:Get-Volume (ent / O N D area thick)
  (setq O (vlax-ename->vla-object ent))
  (setq D (entget ent))
  (setq N (cdr (assoc 0 D)))
  (cond
    ( (vlax-property-available-p O 'Volume)
      (vla-get-Volume O)
    )
    ( (and
	(> (setq area (Ent:Get-Area ent)) 0)
	(numberp (setq thick (cdr (assoc 39 D))))
      )
      (* area thick)
    )
    (T 0)
  )
)

;;;-------------------------------------------------------------
;;; ����������ƱȽ�����ͼԪ                                    
;;;-------------------------------------------------------------
(defun S:Compare (x1 x2 / N1 N2)
  (setq N1 (cadar x1))					 
  (setq N2 (cadar x2))					 
  (if (= N1 N2)
    (< (caddar x1) (caddar x2))
    (< N1 N2)
  )
)

;;;-------------------------------------------------------------
;;; ����֮����е�                                              
;;;-------------------------------------------------------------
(defun GEO:MidPoint (p1 p2)
  (mapcar (function (lambda (x y) (* (+ x y) 0.5))) p1 p2)
)

;;;-------------------------------------------------------------
;;; ȡ���ı��Ĳ����                                            
;;;-------------------------------------------------------------
(defun S:Text-Position (ll ur mode)
  (cond
    ( (= mode "Center")
      (GEO:MidPoint ll ur)
    )
    ( (= mode "Upon")
      (GEO:MidPoint (list (car ll) (cadr ur)) ur)
    )
    ( (= mode "Below")
      (GEO:MidPoint (list (car ur) (cadr ll)) ll)
    )
    ( (= mode "Specify")
      (initget 1)
      (trans (getpoint (GEO:MidPoint ll ur) "\n��ָ����:") 1 0)
    )
  )
)

;;;-------------------------------------------------------------
;;; ��ע����                                                    
;;;-------------------------------------------------------------
(defun S:Make-Text (Info string Insert prop /)
  (entmakeX
    (append
      (list
	'(0 . "TEXT")
	(cons 1 string)
	(cons 10 Insert)
	(cons 11 Insert)
      )
      prop
    )
  )
)

;;;-------------------------------------------------------------
;;; �������㻭ֱ�߶�                                            
;;;-------------------------------------------------------------
(defun Ent:Make-line (p q)
  (entmakeX (list '(0 . "LINE") (cons 10 p) (cons 11 q)))
)
