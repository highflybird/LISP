;|*************************************************************;
�������: Highflybird                                          ;
�����;: ΪAutoCAD ��LISP���Ƶ�һЩ�㷨�ͺ���(���㲿��)       ;
���ڵص�: 2019.07.03 ����                                      ;
��������: AutoLISP,Visual LISP                                 ;
�汾��:   Ver. 1.19.0703                                       ;
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
;;; ����Ŀ��: �ַ����ʽתΪ��������Ҫ���ڶ�ε���ʱ�����ٶ�    
;;; ����: expr--�ַ����ʽ,sFunc--������,sArg--���������б�     
;;; ���: ���庯��������������                                  
;;; ����: (CAL:Expr2Func "sin(x)+20*y" 'test '(x y))            
;;; ���: ������һ����Ϊtest�ĺ�������������Ϊx y               
;;; ע��: �������������͸�������Ʃ��"2/3"��"2/3.0"�����ͬ��    
;;;       �����Զ��庯����ǰ��������Ҫ���أ�                    
;;;       ���ÿ�ѧ���㷨����Ӧ����LISP�е��﷨�����������ţ�    
;;;       ���ʽӦ�����﷨Ҫ��С���ͳ˺Ų��ܰ�ʡ��д����      
;;;=============================================================
(defun CAL:Expr2Func (expr sFunc sArgs / lst)	
  (setq lst (CAL:Separate expr))				;�Ȱ������ſո������������ַ�
  (setq lst (CAL:Operators lst '((^ . expt)) ()))	        ;�˷����ݣ�������
  (setq lst (CAL:Operators lst '((* . *) (/ . /) (% . rem)) ()));��γ˳�����ģ����
  (setq lst (CAL:Operators lst '((+ . +) (- . -)) ()))		;�����Ӽ�������
  (defun-q-list-set sFunc (cons sArgs lst))			;���ɺ���
  sFunc
)

;;;=============================================================
;;; ����Ŀ��: �ַ����ʽ��ֵ                                    
;;; ����: expr--�ַ����ʽ                                      
;;; ���: ������ʽ�Ľ��                                      
;;; ����: (CAL:Expr2Value "sin(1)+20*2")                        
;;; ���: 40.8415                                               
;;;=============================================================
(defun CAL:Expr2Value (expr / lst)
  (setq lst (CAL:Separate expr))				;�Ȱ������ſո������������ַ�
  (setq lst (CAL:Operators lst '((^ . expt)) ()))	        ;�˷����ݣ�������
  (setq lst (CAL:Operators lst '((* . *) (/ . /) (% . rem)) ()));��γ˳�����ģ����
  (setq lst (CAL:Operators lst '((+ . +) (- . -)) ()))		;�����Ӽ�������
  (eval (car lst))						;��ֵ
)

;;;=============================================================
;;; ����Ŀ��: �ȷ����������+-*/%^������������������������ֵ��
;;; ���򵥼������ƥ�䡣                                        
;;; ����: expr--�ַ����ʽ                                      
;;; ���: ������������������ͱ�������ֵ���б�                  
;;;=============================================================
(defun CAL:Separate (expr / CHAR FUNS LASTCHAR LST Temp LBRACKET RBRACKET next)
  (setq expr (vl-string-translate "{[]}\t\n," "(())   " expr))  ;�滻{[]}\t\n,�ַ�
  (setq expr (strcase expr t))					;ȫ��תΪСд
  (setq funs '("+" "-" "*" "/" "^" "%" ))		        ;���ջ���������ָ��ַ�
  (setq Temp "")
  (setq lst "(")
  (setq Lbracket 0)						;�����ż�����
  (setq Rbracket 0)						;�����ż�����
  (while (/= expr "")
    (setq char (substr expr 1 1))                               ;�ַ����ĵ�һ���ַ�
    (setq next (substr expr 2 1))				;�ַ����ĵڶ����ַ�
    (if	(or (= char "(")
	    (= char ")")					;����һ���Ƿָ���
	    (and (= char " ") (/= next "(") (/= next " "))      ;������������Ŀո��
	    (and (member char funs)				;������������зָ�
	         (not (CAL:isScientific temp lastchar char))    ;���Կ�ѧ������
	    )	       	                                                           
	)
      (progn
	(if (CAL:IsFunction (Read temp))			;���Ϊ��ͨ����
	  (setq	lst	 (strcat lst "(" Temp " " )		;�������������������ǰ
		Lbracket (1+ Lbracket)				;�����ż�������1
	  )
	  (progn
	    (and (= char "(") (setq Lbracket (1+ Lbracket)))    ;�����ż�������1
	    (and (= char ")") (setq Rbracket (1+ Rbracket)))	;�����ż�������1
	    (setq lst (strcat lst Temp " " char " "))
	  )
	)
	(setq Temp "")                                          ;����Ǻ����������ſո�֮�࣬���ڴ˴����¿�ʼ  
      )
      (setq Temp (strcat Temp char))                            ;������ȡ����ַ�
    )
    (setq expr (substr expr 2))					;�ַ���ʣ�µ��ַ�
    (setq lastchar char)
  )
  (if (/= Lbracket Rbracket)					;������Ų�ƽ��
    (alert "���Ų�ƥ��(Mismatched Brackets)!")			;������Ϣ
    (read (strcat lst Temp ")"))				;����תΪ��
  )
)

;;;=============================================================
;;; ����Ŀ��: ����+-*/%^�����������ϵ�����                    
;;; ����: lst-�ѷָ�ı�,funs-�������������,Recursive-�Ƿ�ݹ� 
;;; ���: ������������������ͱ�������ֵ���б�                  
;;;=============================================================
(defun CAL:Operators (lst funs Recursive / fun L n)
  (foreach a lst
    (if	(listp a)
      (setq a (CAL:Operators a funs T))				;���Ԫ��Ϊ����ݹ��ȥ
    )
    (if (= (type a) 'INT)
      (setq a (float a))
    )
    (if	(setq fun (cdr (assoc (car L) funs)))                   ;ǰһ������Ϊ+-*/%^�����
      (if (or (null (setq n (cadr L)))                          ;ǰǰһ������Ϊ��
	      (and (VL-SYMBOLP n) (CAL:IsFunction n))           ;�����Ǻ�������
	  )
	(setq L (cons (list fun a) (cdr L)))                    ;���뽻��λ��
	(setq L (cons (list fun n a) (cddr L)))	                ;����������Ͳ�����λ��
      )
      (setq L (cons a L))                                       ;�����Ĳ����ı�
    )                                            
  )
  (setq n (car L))
  (if (and Recursive (not (cadr L)) (or (listp n) (numberp n))) ;����ǵݹ��,����ֻ��һ��Ԫ�أ������Ԫ��Ϊ���������
    n								;��ô��ֻȡ���Ԫ�أ��Է�ֹ�������ų���
    (reverse L)							;cons�����ķ�ת����
  )
)

;;;=============================================================
;;; ����Ŀ��: �ж�һ�������ǲ�����ͨ�������ڲ��������Զ��庯����
;;;=============================================================
(defun CAL:IsFunction (n / s)
  (setq s (type (eval n)))
  (and (or (= s 'SUBR) (= s 'USUBR)) (not (member n '(+ - * /))))
)

;;;=============================================================
;;; ����Ŀ��: ���һ���ַ����Ƿ��ǿ�ѧ������(�Ƿ��и��÷���?)   
;;;=============================================================
(defun CAL:isScientific (temp lastchar char)
  (and (= lastchar "e") (numberp (read (strcat temp char "0"))))
)

;;;=============================================================
;;; ����Ŀ��: ��麯�����ʽת�����Ľ��                        
;;; ����: lst,��cal:expr2func��õı�                           
;;; ���: ������ʽ���зǲ�����δ��ֵ�ı��������򷵻�nil, ����T
;;; ����: (CAL:CheckFunc (CAL:Expr2func "sin(a)+20*2" 'fx '(x)))
;;; ���: nil                                                   
;;;=============================================================
(defun CAL:CheckFunc (lst / isOK CAL:TempSym Args)
  (setq IsOK T)
  (setq Args (car lst))
  (while (setq lst (cdr lst))                                   
    (setq CAL:TempSym (car lst))                                ;�Ա��е�ÿ��Ԫ��
    (if	(listp CAL:TempSym)					;������Ԫ��Ϊ��
      (if CAL:TempSym
	(setq IsOk (CAL:CheckFunc (cons Args CAL:TempSym)))	;�Ҳ�Ϊ����ݹ��ȥ
	(setq IsOk nil)                                         ;��������Ϊ��
      )
      (if (and (vl-symbolp CAL:TempSym)                         ;�����һ������
	       (not (member CAL:TempSym Args))			;�Ҳ�Ϊ�������еķ���
	       (not (vl-symbol-value CAL:TempSym))              ;��δ��ֵ
	  )
	(setq IsOk nil)						;������Ϊ��
      )
    )
    (if	(null IsOK)
      (setq lst nil)
    )
  )
  IsOK
)

;;;=============================================================
;;;���º���Ϊ�Զ����һЩ�򵥵���ѧ����                         
;;;=============================================================
(defun r2d (x) (* 57.2957795130823208768 x))                    ;����ת��
(defun d2r (x) (* 0.01745329251994329577 x))                    ;��ת����
(defun int (x) (atoi (rtos x 2 0)))                             ;��������ȡ������
(defun ceil (x) (1+ (fix x)))                                   ;�컨�庯��
(defun ln (x) (log x))            				;��eΪ�׵Ķ�������
(defun log10 (x) (* (log x) 0.43429448190325182765))            ;��10Ϊ�׵Ķ�������
(defun exp10 (x) (expt 10 x))					;��10Ϊ�׵�ָ������
(defun pow (x y) (expt x y))                                    ;ָ������
(defun tan (x) (/ (sin x) (cos x)))				;���к���
(defun cot (x) (/ (cos x) (sin x)))				;���к���
(defun sec (x) (/ 1 (cos x)))                                   ;�����
(defun csc (x) (/ 1 (sin x)))					;����
(defun asin (x) (atan x (sqrt (- 1 (* x x)))))                  ;�����Һ���
(defun acos (x) (atan (sqrt (- 1 (* x x))) x))			;�����Һ���
(defun sinh (x) (* 0.5 (- (exp x) (exp (- x)))))		;˫�����Һ���
(defun cosh (x) (* 0.5 (+ (exp x) (exp (- x)))))		;˫�����Һ���
(defun tanh (x) (- 1 (/ 2 (1+ (exp (+ x x)))))) 		;˫�����к���
(defun coth (x) (/ 1 (tanh x)))					;˫�����к���
(defun sech (x) (/ 1 (cosh x)))					;˫�������
(defun csch (x) (/ 1 (sinh x)))					;˫������
(defun asinh (x) (log (+ x (sqrt (1+ (* x x))))))		;��˫�����Һ���=log(x+sqrt(x*x+1))
(defun acosh (x) (log (+ x (sqrt (1- (* x x))))))       	;��˫�����Һ���=log(x+sqrt(x*x-1))
(defun atanh (x) (log (sqrt (/ (+ 1 x)(- 1 x)))))		;��˫�����к���=log(sqrt((1+x)/(1-x)))
(defun revSign (x) (- x))					;���ź���
(defun reciprocal (x) (/ 1.0 x))				;����
(defun sqr (x) (* x x))						;ƽ������
(defun cube (x) (* x x x))					;��������
(defun cuberoot	(x)						;����������
  (if (minusp x)
    (- (expt (- x) 0.333333333333333333333))
    (expt x 0.333333333333333333333)
  )
)
(defun round (x / y)						;�������뺯��
  (setq y (fix x))
  (if (< (abs (- x y)) 0.5)
    y
    (if (< x 0)
      (1- y)  
      (1+ y)
    )
  )
)

;|
;;; ����:
;;; (CAL:Separate "(sin(-x)-cos(-x+(1+8*(2/7))+2^4-5))*0.5-0.5e-20+20*cos(x)+20")
;;; ���: ((SIN - X) - (COS - X + (1 + 8 * (2 / 7)) + 2 ^ 4 - 5))
;;; (CAL:Expr2Func "(sin(+x)-cos(-x+(1+8*(2/7))+(2^4)-5))*0.5-0.5e-20+20*cos(x)+20" 'test '(x))
;;; ���: ������һ����Ϊtest�ĺ�������������Ϊx
;;; (CAL:Expr2Value "(sin(+0.5)-cos(-pi+(1+8*(2/7))+(2^4)-5))*0.5-0.5e-20+20*cos(pi/2)+20")
;;; ���: 20.6616
;;; �����ǹ���������������������
;;; ����һ:��cal��������
;;; ��:(cal "1+4+5*2+(5+5)/2+((6+6)/2+(5+5)/2)") 
;;; �ŵ㣺CAD���ú�����
;;; ȱ�㣺�������Ҫ����Ҫ����cal����.�������Ǻ������Զ��ѱ���������ֵ���Ϊ�Ƕȡ�
;;; ������:wcs�ű����Է�,�޺������һ�ַ���
;;; (setq wcs (vla-GetInterfaceObject (vlax-get-acad-object) "ScriptControl"))
;;; (vlax-put-property wcs "language" "vbs")
;;; (vla-eval wcs "1+4+5*2+(5+5)/2+((6+6)/2+(5+5)/2)")  ;���� ->31.0
;;; �ŵ㣺�ܰ���vb���﷨ֱ�Ӽ��㡣
;;; ȱ�㣺���Զ�����ʽΪ���������������Զ��庯������64λCAD�ϴ˷��в�ͨ����Ϊ���ܴ����ű�����
;;; ��������Ϊ��CAD�л��ƺ���ͼ��
(defun c:test1(/ expr a b d x y e pts)
  (setq expr (getstring "\n��������ʽ:"))
  (initget 1)
  (setq a (getreal "\n�Ͻ�:"))
  (initget 1)
  (setq b (getreal "\n�½�:"))
  (if (CAL:EXPR2FUNC  expr 'test '(x))
    (progn
      (setq d (/ (- b a) 1000.0))
      (setq x a)
      (setq pts nil)
      (repeat 1000
	(setq x (+ x d))
	(setq y (test x))
	(setq pts (cons (list x y 0) pts))
      )
      (setq pts (reverse pts))
      (setq e (Entmake (list '(0 . "POLYLINE") '(70 . 8))))
      (foreach p pts
        (entmake (list '(0 . "VERTEX") '(70 . 32) (cons 10 p)))
      )
      (entmake '((0 . "SEQEND")))
      (entlast)
    )
  )
)
;;; ��CAD�л��Ʋ�������
;;; x=a*(2*cos(t)-cos(2*t))
;;; y=a*(2*sin(t)-sin(2*t))
(defun c:test2 (/ expr1 expr2 a b d k x y pts e)
  (setq expr1 "3*(2*cos(k)-cos(2*k))")
  (setq expr2 "3*(2*sin(k)-sin(2*k))")
  (setq a 0)
  (setq b (+ pi pi))
  (CAL:EXPR2FUNC expr1 'fx '(k))
  (CAL:EXPR2FUNC expr2 'fy '(k))
  (setq d (/ (- b a) 360))
  (setq k a)
  (setq pts nil)
  (repeat 360
    (setq k (+ k d))
    (setq x (fx k))
    (setq y (fy k))
    (setq pts (cons (list x y 0) pts))
  )
  (setq pts (reverse pts))
  (setq e (Entmake (list '(0 . "POLYLINE") '(70 . 9))))
  (foreach p pts
    (entmake (list '(0 . "VERTEX") '(70 . 32) (cons 10 p)))
  )
  (entmake '((0 . "SEQEND")))
  (entlast)
)
;;; ����Ϊ�����������ٶȿ����
(defun c:test3(/ str1 str2 x)
  (setq str1 "(sin(+x)-cos(-x+(1+8*(2/7.0))+(2^4)-5))*0.5-0.5e-20+20*cos(x)+20")
  (setq str2 "(sin(r2d(x))-cos(r2d(-x+(1+8*(2/7.0))+(2^4)-5)))*0.5-0.5e-20+20*cos(r2d(x))+20")
  (CAL:Expr2Func str1 'f1 '(x))
  (setq x 12)
  (uti:bench 1000
    (list
      (cons 'f1 '(12))
      (cons 'CAL:Expr2Value (list str1))
      (cons 'cal (list str2))
    )
  )
)
���ʽһ����˵�����֣�ǰ׺���ʽ����׺���ʽ����׺���ʽ�����к�׺���ʽ�ֽ����沨�����ʽ��
��׺���ʽ�����������˼ά��ʽ��һ�ֱ��ʽ������˼�壬���ǲ������ڲ��������м䡣
��ǰ׺���ʽ�ͺ�׺���ʽ�в������ֱ��ڲ�������ǰ��Ͳ������ĺ��档
��д���ʽʱ������ϰ������׺���ʽ��Ʃ�� 1+2*3-4/5�����Ұ��ղ����������ȼ����м��㡣
Ȼ��LISP������һ��ǰ׺���ʽ��Ϊ�˰ѱ��ʽתΪLISP����������ֵ����Ҫ���з��룬��Ӵ��������ź��޸ĺ�����˳��
��������Ŀ�ľ���ʹ����һ������򵥡�
��Ȼ��CAD���汾��Ҳ�м����ַ�ʽ���������������ǵ���ȱ�����Һ������ۡ�
�������˷�ʫ��һЩ���룬�ڴ�����л��
;;|;