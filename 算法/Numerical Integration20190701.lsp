;|*************************************************************;
�������: Highflybird                                          ;
�����;: ΪAutoCAD ��LISP���Ƶ�һЩ�㷨�ͺ���(��ѧ����ֵ����) ;
���ڵص�: 2019.07.14 ����                                      ;
��������: AutoLISP,Visual LISP                                 ;
�汾��:   Ver. 1.16.0714                                       ;
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
;;; �ø��ַ�������ֵĳ���                                      
;;;=============================================================
(defun C:Quadrature (/ ID OK DCL_FILE)
  (setq id (load_dialog (setq Dcl_File (MATH::INT:Write_Dcl))))	;�ӶԻ����еõ����ʽ
  (vl-file-delete Dcl_File)					;ɾ����ʱ�Ի����ļ�
  (setq ok 2)
  (if (new_dialog "dcl_Integration" id)
    (progn
      (VL-CATCH-ALL-APPLY 'MATH::INT:GetSettings)		;��ȡĬ������
      (action_tile "help" "(MATH::INT:Help 1)")			;����
      (foreach k '(0 1 2 3 4 5 6 7 8)
	(setq k (strcat "K" (itoa k)))
        (action_tile k "(MATH::INT:OnBtn $key)")                ;��ť��������Ӧ��ԵĻ��ַ���
      )		   
      (setq ok (start_dialog))
    )
  )
  (unload_dialog ID)
  (princ)
)

(defun C:JF (/)
  (VL-CATCH-ALL-APPLY 'C:Quadrature)
  (princ)
)

;;;=============================================================
;;; �ӻ���������ȡ�ϴ�����                                      
;;;=============================================================
(defun MATH::INT:GetSettings (/ data)
  (if (setq Data (getenv "Intergration"))
    (foreach k (read data)
      (set_tile (car k) (cdr k))
    )
  )
)

;;;=============================================================
;;; ���Ի�������                                              
;;;=============================================================
(defun MATH::INT:CheckInput (symS symA symB symN / e f)
  (setq e (exp 1))
  (set symS (get_tile "F"))
  (set symA (MATH::INT:MyRead (get_tile "A")))
  (set symB (MATH::INT:MyRead (get_tile "B")))
  (set symN (MATH::INT:MyRead (get_tile "N")))
  (setq f (CAL:Expr2Func (eval s) 'MATH::INT:func '(x)))
  (apply 'and (mapcar 'eval '(F symS symA symB symN)))   
)


;;;=============================================================
;;; ��ť��������Ӧ��Ӧ�ĺ��������                              
;;;=============================================================
(defun MATH::INT:OnBtn (key / DATA s N a b OldZIN m EPS RET tm0 map msg foo tmp idx)
  (setq m (VL-CATCH-ALL-APPLY 'MATH::INT:CheckInput '(s a b n)))
  (if (or (vl-catch-all-error-p m) (not m) (equal a b 1e-8))
    (if (vl-catch-all-error-p m) 
      (set_tile "info" (vl-catch-all-error-message m))
      (set_tile "info" "��Ч����!")
    )
    (progn
      ;;������ȹ��ߣ�����Ϊ15λ�ľ���
      (if (> n 20)
	(setq n 15)
	(setq n (fix (abs n)))
      )
      ;;���������С�������䣬�򽻻�����
      (if (< b a)
	(setq tmp a a b b tmp)
      )
      ;;��ס�Ի������룬�����´�
      (setq OldZIN (getvar "DIMZIN"))
      (setvar "DIMZIN" 8)
      (set_tile "N" (itoa n))
      (set_tile "A" (rtos a 2 20))
      (set_tile "B" (rtos b 2 20))
      (setq data (list (cons "F" s)
		       (cons "N" (itoa n))
		       (cons "A" (rtos a 2 20))
		       (cons "B" (rtos b 2 20))
		 )
      )
      (setvar "DIMZIN" OldZIN)
      (setenv "Intergration" (VL-PRIN1-TO-STRING data))
      ;;��ʼ�������
      (setq eps (expt 0.1 n))
      (setq tm0 (getvar "TDUSRTIMER"))
      (setq map (MATH::INT:GetMethods))				;���ּ��㷽����
      (setq idx (atoi (substr key 2)))				
      (setq foo (nth idx map))					;��ȡ������ֵĺ���
      (setq ret (VL-CATCH-ALL-APPLY foo (list a b eps)))        ;��ȡ����ֵ
      (if (vl-catch-all-error-p ret)
	(set_tile "info" (vl-catch-all-error-message ret))	;�����̷����˴���
        (if (null ret)
	  (set_tile "info" "�����˴�����ֵ���Ϊ��!")	
	  (progn
	    ;(MATH::INT:Bench 100 a b eps)
            (setq ret (rtos ret 2 20))
            (setq msg (get_attr key "label"))
            (setq msg (strcat msg "��Ľ��Ϊ:" ret))
            (set_tile (strcat "R" (itoa idx)) ret)             	;��ʾ�����
            (set_tile "info" msg)				;��ʾ�����
            (princ (strcat "\n" msg))				;��ӡ�����
            (princ "\n��ʱ:")
            (princ (* (- (getvar "TDUSRTIMER") tm0) 86400))
            (princ "��.")
	  )
        )
      )
    )
  )
)

;;;=============================================================
;;; ���ֻ��ֲ���                                                
;;;=============================================================
(defun MATH::INT:Bench (n a b eps)
  (UTI:BENCH
    n
    (list
      (list 'MATH::INT:Romberg a b eps)
      (list 'MATH::INT:Gauss-Legendre a b eps)
      (list 'MATH::INT:Simpson a b eps)
    )
  )
)

;;;=============================================================
;;; ���ַ�����                                                  
;;;=============================================================
(defun MATH::INT:GetMethods ()
  '(MATH::INT:Romberg
    MATH::INT:Simpson
    MATH::INT:Atrapezia
    MATH::INT:Trapezia
    MATH::INT:Gauss-Legendre
    Math::INT:Gauss-Chebyshev
    Math::INT:Gauss-Laguerre
    Math::INT:Gauss-Hermite
    MATH::INT:Gauss-Jacobi
   )
)

;;;=============================================================
;;; ���ʽ��ֵ��Ҳ������cal����                                 
;;;=============================================================
(defun MATH::INT:MyRead (str / e)
  (setq e (exp 1))
  (CAL:Expr2Value str)
)

;;;=============================================================
;;; ������˵��: help and instruction                            
;;;=============================================================
(defun MATH::INT:Help (n)
  (if (= n 1)
    (if	(= "CHS" (getvar "Locale"))
      (alert
	"����ʽֻ���ܷ���xΪ����,���淶�ܿ��ܳ���!
	\n��������LISP���õ���ѧ������Ҳ�����Զ��庯��!
	\nָ����^��ʾ��+-*/��ʾ�Ӽ��˳����˺Ų���ʡ�ԡ�
	\n�����ܲ��ö��ַ������,һ����˵��������ַ���졣
	\n��ʲô����email: highflybird@qq.com
	\n����: highflybird ����2019.07"
      )
      (alert
	"Standard expression only accepts \"x\" as a variale!
	\nThe fastest is Romberg Integration,the slowest is Trapezoidal rule(Be careful!).
	\nRecommendation:Don't set a high precision at first,promote it step by step.
	\nEspically for the Trapezoidal rule, It won't work well on some circumstances.
	\nIt's an Open Source Software. Thanks for your advice or bug reports.
	\nAuthor: highflybird  Email: highflybird@qq.com  Date:2019.07."
      )
    )
    (set_tile "info" "���ʽ�Ƿ����߿�����.")
  )
)

;;;=============================================================
;;; �������õ�-��˹���������ϵ�� ���������6����ϵ��           
;;; p0(x) = 1                                                   
;;; p1(x) = x                                                   
;;; p2(x) = (3*x^2-1)/2                                         
;;; p3(x) = (5*x^3-3*x)/2                                       
;;; p4(x) = (35*x^4-30*x^2+3)/8                                 
;;; p5(x) = (63*x^5-70*x^3+15*x)/8                              
;;; p6(x) = (231*x^6-315*x^4+105*x^2-5)/16                      
;;; x-2                                                         
;;; x-1                                                         
;;; 9x-5                                                        
;;; 216*x^2-216*x+49                                            
;;; 45000*x^2-32200*x+5103                                      
;;; 2025000*x^3-2025000*x^2+629325*x-58564                      
;;; 142943535000*x^3-1130712534000*x^2+27510743799*x-1976763932 
;;; ����: x1,x2 ����(һ����˵��-1..1)��n ��������,eps��������   
;;; ���: ���õ�-��˹���������ϵ��,�õ����ʾ                
;;; http://mathworld.wolfram.com/Legendre-GaussQuadrature.html  
;;;=============================================================
(defun Math::Int:Legendre_Polynomial (x1 x2 n eps / xi wi FI I ITER J M P1 P2 P3 PP XL XM Z Z1)
  (setq m (/ (1+ n) 2))
  (setq xm (* 0.5 (+ x2 x1)))
  (setq xl (* 0.5 (- x2 x1)))
  (setq i 1)
  (repeat m
    (setq z (cos (/ (* pi (- i 0.25)) (+ n 0.5))))
    (setq iter 0)
    (while (and (not (equal z z1 eps)) (< iter 1000))
      (setq p1 1.0)
      (setq p2 0.0)
      (setq j 1)
      (repeat n
	(setq p3 p2)
	(setq p2 p1)
	(setq p1 (/ (- (* z p2 (+ j j -1.)) (* (1- j) p3)) j))
	(setq j (1+ j))
      )
      (setq pp (/ (* n (- (* z p1) p2)) (1- (* z z))))
      (setq z1 z)
      (setq z (- z1 (/ p1 pp)))
      (setq iter (1+ iter))
    )
    (setq fi (/ xl 0.5 (- 1 (* z z)) pp pp))
    (setq xi (cons (cons (1- i) (- xm (* xl z))) xi))
    (setq wi (cons (cons (1- i) fi) wi))
    (if (/= (1- i) (- n i))
      (setq xi (cons (cons (- n i) (+ xm (* xl z))) xi)
	    wi (cons (cons (- n i) fi) wi)
      )
    )
    (setq i (1+ i)) 
  )
  (MATH::INT:Bind xi wi)
)

;;;=============================================================
;;; ���õ�-��˹�������                                         
;;;=============================================================
(defun MATH::INT:Gauss-Legendre (a b eps / AA BB EP FX G H I L M P S W X)
  (setq l '((-0.93246951420315202787 . 0.17132449237917034504)  
	    (-0.66120938646626451363 . 0.36076157304813860756)     
	    (-0.23861918608319690859 . 0.46791393457269104739)  
	    ( 0.23861918608319690859 . 0.46791393457269104739)     
            ( 0.66120938646626451363 . 0.36076157304813860756)     
	    ( 0.93246951420315202787 . 0.17132449237917034504)
	  )
  )                                                         
  ;(setq L (Math::Int:Legendre_Polynomial -1 1 100 2e-20))
  (setq m 1)
  (setq h (- b a))
  (setq s (abs (* 0.001 h)))
  (setq p 1e100) 	
  (setq ep (1+ eps))
  (while (and (>= ep eps) (> (abs h) s))
    (setq g 0)
    (setq i 1)
    (repeat m
      (setq bb (+ a (* i h)))
      (setq aa (- bb h))
      (setq w 0)
      (foreach k l
	(setq x (* 0.5 (+ bb aa (* (- bb aa) (car k)))))
	(setq fx (MATH::INT:func x))
	(setq w (+ w (* fx (cdr k))))
      )
      (setq g (+ g w))
      (setq i (1+ i))
    ) 
    (setq g (* g h 0.5))
    (setq ep (/ (abs (- g p)) (1+ (abs g))))
    (setq p g)
    (setq m (1+ m))
    (setq h (/ (- b a) m))
  )
  g
)

;;;=============================================================
;;; ���õ�-��˹�������(��һ��������Щ)                         
;;;=============================================================
(defun MATH::INT:Gauss-Legendre1 (a b eps / FLAG G H L N X Y)
  (setq n 1)                                                      
  (setq flag T)							;�Ƿ���е���
  (while (and (< n 100) flag)	
    (setq g 0)
    (setq L (Math::Int:Legendre_Polynomial a b n eps))
    (foreach w L
      (setq x (car w))
      (setq y (MATH::INT:func x))
      (setq g (+ g (* (cdr w) y)))
    )
    (if (equal g h eps)
      (setq flag nil)
      (setq n (+ n n)
	    h g
      )
    )
  )
  g
)

;;;=============================================================
;;; ��˹-���׶��ػ���                                           
;;; ����: ���� e^(-x^2)*f(x)�Ĺ������(������-INF..INF��)       
;;;=============================================================
(defun Math::INT:Gauss-Hermite (a b eps / n L g x y)
  (setq n 100)
  (setq L (Math::INT:GetHermite n))
  (setq g 0)
  (foreach w L
    (setq x (car w))
    (setq y (MATH::INT:func x))
    (setq g (+ g (* (cdr w) y)))
  )
  g
)

;;;=============================================================
;;; ��ȡ���׶���ϵ��                                            
;;;=============================================================
(defun Math::INT:GetHermite (n / xi wi EPS FI I ITS J M MAXIT P1 P2 P3 PIM4 PP Z Z1)
  (setq eps 1e-15)
  (setq PIM4 0.7511255444649425)
  (setq maxIt 10)
  (setq m (/ (1+ n) 2))
  (setq i 0)
  (while (< i m)
    (if (= i 0)
      (setq z (- (sqrt (+ n n 1)) (* 1.85575 (expt (+ n n 1) (/ -1 6.)))))
      (if (= i 1)
	(setq z (- z (/ (* 1.14 (expt n 0.426)) z)))
	(if (= i 2)
	  (setq z (- (* 1.86 z) (* 0.86 (cdr (assoc 0 xi)))))
	  (if (= i 3)
	    (setq z (- (* 1.91 z) (* 0.91 (cdr (assoc 1 xi)))))
	    (setq z (- (+ z z) (cdr (assoc (- i 2) xi))))
	  )
	)
      )
    )
    (setq its 0)
    (while (and (not (equal z z1 eps)) (< its MAXIT))
      (setq p1 pIM4)
      (setq p2 0.0)
      (setq j 0)
      (repeat n
	(setq p3 p2)
	(setq p2 p1)
	(setq p1 (- (* z p2 (sqrt (/ 2.0 (1+ j)))) (* p3 (sqrt (/ j (+ 1.0 j))))))
	(setq j (1+ j))
      )
      (setq pp (* p2 (sqrt (+ n n))))
      (setq z1 z)
      (setq z (- z1 (/ p1 pp)))
      (setq its (1+ its))
    )
    (setq fi (/ 2.0  pp pp))
    (setq xi (cons (cons i z) xi))
    (setq wi (cons (cons i fi) wi))
    (if (/= i (- n 1 i))
      (setq xi (cons (cons (- n 1 i) (- z)) xi)
	    wi (cons (cons (- n 1 i) fi) wi)
      )
    )
    (setq i (1+ i))
  )
  (MATH::INT:Bind xi wi)
)

;;;=============================================================
;;; ��˹-�ſ˱Ȼ���                                             
;;; ����: ���� f(x)*((1-x)^a)*((1+x)^b)�Ļ���(������-1..1��)    
;;;=============================================================
(defun MATH::INT:Gauss-Jacobi (a b eps / ALF ARGS BET G N X Y flag g0)
  (if (setq args (UTI:InputBox))
    (progn
      (setq flag T)						;�Ƿ���е���
      (setq n 10)
      (while (and (< n 100) flag)	
        (setq alf (car args))
        (setq bet (cadr args))
        (setq g 0)
        (foreach w (MATH::INT:GetJacobiPolynomial n alf bet)
          (setq x (car w))
          (setq y (MATH::INT:func x))
          (setq g (+ g (* (cdr w) y)))
        )
	(if (equal g g0 eps)
	  (setq flag nil g0 g)
	  (setq n (+ n n) g0 g)
	)
      )
    )
  )
)

;;;=============================================================
;;; ��˹-�ſ˱Ȼ���                                             
;;; ����: �����˹-�ſ˱Ȼ���ϵ����                             
;;;=============================================================
(defun MATH::INT:GetJacobiPolynomial (n alf bet /
				      xi wi A ALFBET AN B BN C EPS FI FLG I ITS
				      J MAXIT P1 P2 P3 PP R1 R2 R3 TEMP Z Z1)
  (setq maxIT 40)
  (setq eps 1e-15)
  (setq alf (float alf))
  (setq bet (float bet))
  (setq i 0)
  (repeat n									;for (i=0;i<n;i++) {
    (cond
      ( (= i 0)									;if (i == 0) {
        (setq an (/ alf n))							;an=alf/n;
        (setq bn (/ bet n))							;bn=bet/n;
        (setq r1 (* (1+ alf) (+ (/ 2.78 (+ 4.0 (* n n))) (/ (* 0.768 an) n))))	;r1=(1.0+alf)*(2.78/(4.0+n*n)+0.768*an/n);
        (setq r2 (+ 1.0 (* 1.48 n) (* 0.96 bn) (* 0.452 an an) (* 0.83 an bn)))	;r2=1.0+1.48*an+0.96*bn+0.452*an*an+0.83*an*bn;
        (setq z  (- 1 (/ r1 r2)))						;z=1.0-r1/r2;
      )
      ( (= i 1)									;else if (i == 1)
        (setq r1 (/ (+ 4.1 alf) (* (1+ alf) (1+ (* 0.156 alf)))))		;r1=(4.1+alf)/((1.0+alf)*(1.0+0.156*alf));
        (setq r2 (1+ (/ (* 0.06 (- n 8.0) (1+ (* 0.12 alf))) n)))		;r2=1.0+0.06*(n-8.0)*(1.0+0.12*alf)/n;
        (setq r3 (1+ (/ (* 0.012 bet (1+ (* 0.25 (abs alf)))) n)))		;r3=1.0+0.012*bet*(1.0+0.25*fabs(alf))/n;
        (setq z  (- z (* (- 1 z) r1 r2 r3)))					;z -= (1.0-z)*r1*r2*r3;
      )
      ( (= i 2)									;else if (i == 2)
        (setq r1 (/ (+ 1.67 (* 0.28 alf)) (1+ (* 0.37 alf))))			;r1=(1.67+0.28*alf)/(1.0+0.37*alf);
        (setq r2 (1+ (/ (* 0.22 (- n 8.0)) n)))					;r2=1.0+0.22*(n-8.0)/n;
        (setq r3 (1+ (/ (* 8.0 bet) (* (+ 6.28 bet) n n))))			;r3=1.0+8.0*bet/((6.28+bet)*n*n);
        (setq z  (- z (* (- (cdr (assoc 0 xi)) z) r1 r2 r3)))			;z -= (x[0]-z)*r1*r2*r3;
      )
      ( (= i (- n 2))								;else if (i == n-2)
        (setq r1 (/ (1+ (* 0.235 bet)) (+ 0.766 (* 0.119 bet))))		;r1=(1.0+0.37*bet)/(1.67+0.28*bet);
        (setq r2 (/ 1.0 (1+ (/ (* 0.639 (- n 4.0)) (1+ (* 0.71 (- n 4.0)))))))	;r2=1.0/(1.0+0.639*(n-4.0)/(1.0+0.71*(n-4.0)));
        (setq r3 (/ 1.0 (1+ (/ (* 20.0 alf) (* (+ 7.5 alf) n n))))) 		;r3=1.0/(1.0+20.0*alf/((7.5+alf)*n*n));
        (setq z  (+ z (* (- z (cdr (assoc (- n 4) xi))) r1 r2 r3))) 		;z += (z-x[n-4])*r1*r2*r3;
      )
      ( (= i (1- n))								;else if (i == n-1)
        (setq r1 (/ (1+ (* 0.37 bet))(+ 1.67 (* 0.28 bet))))			;r1=(1.0+0.37*bet)/(1.67+0.28*bet);
        (setq r2 (/ 1.0 (1+ (/ (* 0.22 (- n 8.0)) n))))				;r2=1.0/(1.0+0.22*(n-8.0)/n);
        (setq r3 (/ 1.0 (1+ (/ (* 8.0 alf) (* (+ 6.28 alf) n n)))))             ;r3=1.0/(1.0+8.0*alf/((6.28+alf)*n*n));
       	(setq z  (+ z (* (- z (cdr (assoc (- n 3) xi))) r1 r2 r3))) 		;z += (z-x[n-3])*r1*r2*r3;
      )
      (t									;else {
        (setq z (* 3 (- (cdr (assoc (1- i) xi)) (cdr (assoc (- i 2) xi)))))	;z=3.0*x[i-1]-3.0*x[i-2]+x[i-3];
        (setq z (+ z (cdr (assoc (- i 3) xi))))
      )
    )
    (setq alfbet (+ alf bet))							;alfbet=alf+bet;
    (setq its 1)
    (setq flg T)
    (while (and flg (<= its maxIt))						;for (its=1;its<=MAXIT;its++)
      (setq temp (+ 2.0 alfbet))						;temp=2.0+alfbet;
      (setq p1 (/ (+ (- alf bet) (* temp z)) 2.0))				;p1=(alf-bet+temp*z)/2.0;
      (setq p2 1.0)								;p2=1.0;
      (setq j 2)								
      (while (<= j n)								;for (j=2;j<=n;j++) {
	(setq p3 p2)								;p3=p2;
	(setq p2 p1)								;p2=p1;
	(setq temp (+ j j alfbet))						;temp=2*j+alfbet;
	(setq a (* 2 j (+ j alfbet) (- temp 2.0)))				;a=2*j*(j+alfbet)*(temp-2.0);
	(setq b (* (1- temp) (- (* alf alf) (* bet bet) (* temp (- 2 temp) z))));b=(temp-1.0)*(alf*alf-bet*bet+temp*(temp-2.0)*z);
	(setq c (* 2 (+ j -1 alf) (+ j -1 bet) temp))				;c=2.0*(j-1+alf)*(j-1+bet)*temp;
        (setq p1 (/ (- (* b p2) (* c p3)) a))					;p1=(b*p2-c*p3)/a;
	(setq j (1+ j))
      )
      
      (setq pp (/ (+ (* N (- ALF BET (* TEMP Z)) P1)(* 2 (+ N ALF)(+ N BET) P2))
		  (* TEMP (- 1.0 (* Z Z)))
	       )
      )										;pp=(n*(alf-bet-temp*z)*p1+2.0*(n+alf)*(n+bet)*p2)/(temp*(1.0-z*z));
      (setq z1 z)								;z1=z;
      (setq z (- z1 (/ p1 pp)))							;z=z1-p1/pp;
      (setq its (1+ its))
      (if (equal z z1 eps)
	(setq flg nil)
      )
    )
    (if (> its MAXIT)
      (princ "\nToo many iterations in gaujac!")				;if (its > MAXIT) nrerror("too many iterations in gaujac");
    )
    (setq xi (cons (cons i z) xi))						;x[i]=z;
    (setq fi (exp (- (Math::gammln (+ alf n))
		     (- (Math::gammln (+ bet n)))
		     (Math::gammln (1+ n))
		     (Math::gammln (+ n alfbet 1))
		  )
	     )
    )
    (setq fi (/ (* fi temp (expt 2.0 alfbet)) pp p2))				;w[i]=exp(gammln(alf+n)+gammln(bet+n)-gammln(n+1.0)-gammln(n+alfbet+1.0))*temp*pow(2.0,alfbet)/(pp*p2);	
    (setq wi (cons (cons i fi) wi))		
    (setq i (1+ i))
  )
  (MATH::INT:Bind xi wi)
)


;;;=============================================================
;;; ��˹-���Ƕ�����                                             
;;; ����: ���� e^(-x)*f(x)�Ĺ������(������0..INF��)            
;;;=============================================================
(defun Math::INT:Gauss-Laguerre (a b eps / n flag L g h x y maxN)
  (setq n 2)
  (setq flag T)							;�Ƿ���е���
  (setq maxN 40)
  (while (and flag (< n maxN))	
    (setq g 0)
    (setq L (Math::INT:GetLaguerre n 0))
    (foreach w L
      (setq x (car w))
      (setq y (MATH::INT:func x))
      (setq g (+ g (* (cdr w) y)))
    )
    (if (equal g h eps)
      (setq flag nil)
      (setq n (+ n n)
	    h g
      )
    )
  )
  g
)

;;;=============================================================
;;; ��ȡ���Ƕ�ϵ��                                              
;;;=============================================================
(defun Math::INT:GetLaguerre (n alf / xi wi AI EPS I ITS J MAXIT P1 P2 P3 PP X XI2 YI Z Z1)
  (setq maxIt 10)
  (setq eps 1e-15)
  (setq i 0)
  (repeat n
    (if	(= i 0)
      (setq z (/ (* (1+ alf) (+ 3 (* 0.92 alf)))
		 (+ 1 (* 2.4 n) (* 1.8 alf))
	      )
      )
      (if (= i 1)
	(setq
	  z (+ z (/ (+ 15 (* 6.25 alf)) (+ 1 (* 0.9 alf) (* 2.5 n))))
	)
	(setq ai  (1- i)
	      xi2 (cdr (assoc (- i 2) xi))
	      x	  (/ (*
		       (+
			 (/ (1+ (* 2.55 AI)) (* 1.9 AI))
			 (/ (* 1.26 AI ALF) (1+ (* 3.5 AI)))
		       )
		       (- Z XI2)
		     )
		     (1+ (* 0.3 ALF))
		  )
	      z	  (+ z x)
	)
      )
    )
    (setq its 0)
    (while (and (not (equal z z1 eps)) (< its maxIt))
      (setq p1 1)
      (setq p2 0)
      (setq j 0)
      (repeat n
	(setq p3 p2)
	(setq p2 p1)
	(setq p1 (/ (- (* (+ J J 1.0 ALF (- Z)) P2) (* (+ J ALF) P3))
		    (1+ J)
		 )
	)
	(setq j (1+ j))
      )
      (setq pp (/ (- (* n p1) (* p2 (+ n alf))) z))
      (setq z1 z)
      (setq z (- z1 (/ p1 pp)))
      (setq its (1+ its))
    )
    (setq xi (cons (cons i z) xi))
    (setq yi (-	(/ (exp (- (Math::gammln (+ alf n)) (Math::gammln n)))
		   (* pp n p2)
		)
	     )
    )
    (setq wi (cons (cons i yi) wi))
    (setq i (1+ i))
  )
  (MATH::INT:Bind xi wi)
)

;;;=============================================================
;;; ��˹-�б�ѩ�����                                           
;;; �˷���������� sqrt(1-x^2)*f(x)�͵Ļ����кܸߵ�Ч�ʡ�       
;;; ����: foo ������,arg ���Ա���x��Ĳ����б�, n����������     
;;; ���: ������ֵ���ֵ.                                       
;;; ˵��: �˻��ַ�Ч�ʽϸߣ�nȡֵһ��10�����Ҿʹﵽ��Ч���㾫��.
;;; http://mathworld.wolfram.com/Chebyshev-GaussQuadrature.html 
;;;=============================================================
(defun Math::INT:Gauss-Chebyshev (a b eps / FI FLAG I N S0 SX WI XI)
  (setq n 3)							;һ����˵����6-5�����ҾͿ��Դﵽ������㾫��
  (setq flag T)							;�Ƿ���е���
  (while (and (< n 1000) flag)					;��������������100
    (setq wi (/ pi n))						;���е�Ȩֵ��Ϊpi/n
    (setq sx 0)
    (setq i 1)
    (repeat n
      (setq xi (cos (/ (* pi (- i 0.5)) n)))			;x ��
      (setq fi (MATH::INT:func xi))				;x ��ĺ���ֵ
      (setq sx (+ sx fi))
      (setq i  (1+ i))
    )
    (setq sx (* wi sx))						;ͳһ����Ȩֵ
    (if (equal sx s0 eps)					;�Ƿ����㾫��Ҫ��
      (setq flag nil)						;�����ٵ���
      (setq n (+ n n)						;���������������
	    s0 sx						;�洢����ֵ
      )
    )
  )
  sx
)

;;;=============================================================
;;; ����: ��һ����Բ����                                        
;;; ����: Phi �� k < 1                                          
;;; ���: ������Բ����ֵ                                        
;;;=============================================================
(defun Math:Elliptic_Integral_1 (phi kCoff / )
  (defun MATH::INT:func (x / s)
    (setq s (* kCoff (sin x)))
    (/ 1.0 (sqrt (* (- 1 s) (1+ s))))
  )
  (MATH::INT:Romberg 0 phi 1e-15)
)

;;;=============================================================
;;; ����: �ڶ�����Բ����                                        
;;; ����: Phi �� k < 1                                          
;;; ���: ������Բ����ֵ                                        
;;;=============================================================
(defun Math:Elliptic_Integral_2 (phi kCoff / )
  (defun MATH::INT:func (x /)
    (setq x (* kCoff (sin x)))
    (sqrt (* (- 1 x) (1+ x)))
  )
  (MATH::INT:Romberg 0 phi 1e-15)
)

;;;=============================================================
;;; �׳�                                                        
;;;=============================================================
(defun Math::frac (n)
  (if (= n 0)
    1.
    (if	(= n 1)
      1.
      (* n (Math::frac (1- n)))
    )
  )
)

;;;=============================================================
;;; ٤�꺯��                                                    
;;;=============================================================
(defun Math::gammln (xx / x y tmp ser j cof)
  (setq	cof '(76.18009172947146		 -86.50532032941677
	      24.01409824083091		 -1.231739572450155
	      0.1208650973866179e-2	 -0.5395239384953e-5
	     )
  )
  (setq x xx)
  (setq y x)
  (setq tmp (+ x 5.5))
  (setq tmp (- tmp (* (+ 0.5 x) (log tmp))))
  (setq ser 1.000000000190015)
  (setq j 0)
  (repeat 6
    (setq y (1+ y))
    (setq ser (+ ser (/ (nth j cof) y)))
    (setq j (1+ j))
  )
  (- (log (/ (* 2.5066282746310005 ser) x)) tmp)
)

;;;=============================================================
;;; �κ������ڱȽ�����                                          
;;;=============================================================
(defun MATH::INT:funcSort (e1 e2)
  (< (car e1) (car e2))
)

;;;=============================================================
;;; �������ϵ��                                                
;;;=============================================================
(defun MATH::INT:Bind (xi wi)
  (setq xi (vl-sort xi 'MATH::INT:funcSort))
  (setq wi (vl-sort wi 'MATH::INT:funcSort))
  (setq xi (mapcar 'cdr xi))
  (setq wi (mapcar 'cdr wi))
  (mapcar 'cons xi wi)
)

;;;=============================================================
;;; ��������ַ�                                                
;;; Romberg Integration                                         
;;; ����: ������--foo (�÷��ű�ʾ��һ����ʽ�� (foo x a b c ...) 
;;;       ������--args ����ȥ�Ա���x�������Ĳ����б�            
;;;       ������--Ra                                            
;;;       ������--Rb                                            
;;;       �������--eps                                         
;;; ���: ������������εĻ���                                
;;;=============================================================
(defun MATH::INT:Romberg (a b eps / EP H I K M N P Q S X Y Y0)
  (setq h (- b a))
  (setq y nil)
  (setq i 0)
  (repeat 20
    (setq y (cons (cons i 0.0) y))
    (setq i (1+ i))
  )
  (setq y (reverse y))
  (setq y0 (* h (+ (MATH::INT:func a) (MATH::INT:func b)) 0.5))
  (setq y (cons (cons 0 y0) (cdr y)))
  (setq	m  1
	n  1
	ep (1+ eps)
  )
  (while (and (>= ep eps) (<= m 19))
    (setq p 0.0)
    (setq i 0)
    (repeat n
      (setq x (+ a (* (+ i 0.5) h)))
      (setq p (+ p (MATH::INT:func x)))
      (setq i (1+ i))
    )
    (setq p (/ (+ (cdar y) (* h p)) 2.0))
    (setq s 1.0)
    (setq k 1)
    (repeat m
      (setq s (+ s s s s))
      (setq q (/ (- (* s p) (cdr (assoc (1- k) y))) (1- s)))
      (setq y (subst (cons (1- k) p) (assoc (1- k) y) y))
      (setq p q)
      (setq k (1+ k))
    )
    (setq ep (abs (- q (cdr (assoc (1- m) y)))))
    (setq m (1+ m))
    (setq y (subst (cons (1- m) q) (assoc (1- m) y) y))
    (setq n (+ n n))
    (setq h (/ h 2.0))
  )
  q
)

;;;=============================================================
;;; ����ɭ���ַ�                                                
;;; Simpson Integration                                         
;;;=============================================================
(defun MATH::INT:Simpson (a b eps / EP H ITER K N P S1 S2 T1 T2 X)
  (setq n 1)
  (setq h (- b a))
  (setq t1 (* h (+ (MATH::INT:func a) (MATH::INT:func b)) 0.5))
  (setq s1 t1)
  (setq ep (1+ eps))
  (setq iter 0)
  (while (and (>= ep eps) (< iter 50))
    (setq p 0.0)
    (setq k 0)
    (repeat n
      (setq x (+ a (* (+ k 0.5) h)))
      (setq p (+ p (MATH::INT:func x)))
      (setq k (1+ k))
    )
    (setq t2 (/ (+ t1 (* h p)) 2.))
    (setq s2 (/ (- (* 4.0 t2) t1) 3.))
    (setq ep (abs (- s2 s1)))
    (setq t1 t2)
    (setq s1 s2)
    (setq n (+ n n))
    (setq h (/ h 2))
    (setq iter (1+ iter))
  )
  s2
)

;;;=============================================================
;;; �䲽����������ַ�                                          
;;; Trapezoidal Integration 1                                   
;;;=============================================================
(defun MATH::INT:Trapezia (a b eps / H K N P S T1 T2 X iter)
  (setq n 1)
  (setq h (- b a))
  (setq t1 (* h (+ (MATH::INT:func a) (MATH::INT:func b)) 0.5))
  (setq p (1+ eps))
  (setq iter 0)
  (while (and (>= p eps) (< iter 100))
    (setq s 0)
    (setq k 0)
    (repeat n
      (setq x (+ a (* (+ k 0.5) h)))
      (setq s (+ s (MATH::INT:func x)))
      (setq k (1+ k))
    )
    (setq t2 (/ (+ t1 (* h s)) 2.))
    (setq p (abs (- t1 t2)))
    (setq t1 t2)
    (setq n (+ n n))
    (setq h (/ h 2))
    (setq iter (1+ iter))
  )
  t2
)

;;;=============================================================
;;; �������ַ�                                                  
;;; Trapezoidal Integration 2                                   
;;;=============================================================
(defun MATH::INT:Trapzd (a b n / DEL IT SUM TNM X s)
  (if (= n 1)
    (setq s (* 0.5 (- b a) (+ (MATH::INT:func a) (MATH::INT:func b))))
    (progn
      (setq it 1)
      (repeat (- n 2)
	(setq it (lsh it 1))
      )
      (setq tnm it)
      (setq del (/ (- b a) tnm))
      (setq x (+ a (* 0.5 del)))
      (setq sum 0.0)
      (repeat it
	(setq sum (+ sum (MATH::INT:func x)))
	(setq x (+ x del))
      )
      (setq s (* 0.5 (+ s (/ (* (- b a) sum) tnm))))
    )
  )
)

;;;=============================================================
;;; ����Ӧ����ַ�                                              
;;; Self-adapting Trapezia Integration                          
;;;=============================================================
(defun MATH::INT:Atrapezia (a b eps / F0 F1 H S T0 TT d)
  (setq d 1e-4)
  (setq h (- b a))
  (setq TT '(0. . 0.))
  (setq f0 (MATH::INT:func a))
  (setq f1 (MATH::INT:func b))
  (setq t0 (* h (+ f0 f1) 0.5))
  (car (MATH::INT:ppp a b h f0 f1 t0 eps d tt))
)

(defun MATH::INT:PPP (x0 x1 h f0 f1 t0 eps d tt / EPS1 F G P T1 T2 T3 X X2)
  (setq x (+ x0 (* h 0.5)))
  (setq f (MATH::INT:func x))
  (setq t1 (* h (+ f0 f) 0.25))
  (setq t2 (* h (+ f1 f) 0.25))
  (setq p (abs (- t0 t1 t2)))
  (if (or (< p eps) (< (* 0.5 h) d))
    (cons (+ (car tt) t1 t2) (cdr tt))
    (progn
      (setq g (* h 0.5))
      (setq eps1 (/ eps 1.4))
      (setq t3 (MATH::INT:ppp x0 x g f0 f t1 eps1 d tt))
      (setq t3 (MATH::INT:ppp x x1 g f f1 t2 eps1 d t3))
    )
  )
)

;;;=============================================================
;;; ����Ի���                                                  
;;;=============================================================
(defun UTI:Inputbox (/ str wcs ret)
  (setq	str "Function GetNumbers()
  	     GetNumbers=inputbox(\"��������������,�м��ÿո����:\",\"�����\")
             End Function"
  )
  (if
    (or
      (setq wcs (vlax-create-object "Aec32BitAppServer.AecScriptControl.1"))
      (setq wcs (vlax-create-object "ScriptControl"))
    )
    (progn 
      (vlax-put-property wcs "language" "VBScript")
      (vlax-invoke wcs 'addcode str)
      (if (setq ret (vlax-invoke wcs 'run "GetNumbers"))
	(setq ret (strcat "(" ret ")")
	      ret (read ret)
	)
      )
      (vlax-release-object wcs)
      (if
	(and
	  (= 2 (length ret))
	  (or (= 'INT (type (car ret))) (= 'REAL (type (car ret))))
	  (or (= 'INT (type (cadr ret))) (= 'REAL (type (cadr ret))))
	)
	ret
      )
    )
  )
)

;;;=============================================================
;;; д�Ի����ļ����ڳ���                                      
;;;=============================================================
(defun MATH::INT:Write_Dcl (/ Dcl_File file str)
  (setq Dcl_File (vl-filename-mktemp nil nil ".Dcl"))
  (setq file (open Dcl_File "w"))
  (princ
    "dcl_Integration : dialog {
	label = \"��ֵ����LISP��  v1.2\";
	: boxed_column {
          width = 60;
	  fixed_width = true;
	  : edit_box {
	    key=\"F\";
	    label= \"����:\";
	  }
	  : row {
	    : edit_box {
	      key=\"A\";
	      label= \"����:\";
	    }
	    : edit_box {
	      key=\"B\";
	      label= \"����:\";
	    }      
	    : edit_box {
	      key=\"N\";
	      label = \"��ȷλ��:\";
	      value = 8;
	      edit_width = 2;
	      fixed_width = true;
	    }
	  }
	  spacer_1;
	}
	: row {
	  : boxed_column {
	    label = \"���㷽��:\";
	    : button {
	      key = \"K0\";
	      label = \"��������ַ�\";
	    }
	    : button {
	      key = \"K1\";
	      label = \"����ɭ���ַ�\";
	    }
	    : button {
	      key = \"K2\";
	      label = \"����Ӧ���ַ�\";
	    }
	    : button {
	      key = \"K3\";
	      label = \"�䲽�����η�\";
	    }
	    : button {
	      key = \"K4\";
	      label = \"��˹-���õ·�\";
	    }
	    : button {
	      key = \"K5\";
	      label = \"��˹-�б�ѩ��\";
	    }
	    : button {
	      key = \"K6\";
	      label = \"��˹-���Ƕ���\";
	    }
	    : button {
	      key = \"K7\";
	      label = \"��˹-�������ط�\";
	    }
	    : button {
	      key = \"K8\";
	      label = \"��˹-�ſ˱ȷ�\";
	    }
	    spacer;
	  }
	  : boxed_column {
	    width = 32;
	    fixed_width = true;
	    label = \"������:\";
	    : text {
	      key = \"R0\";
	    }
	    : text {
	      key = \"R1\";
	    }
	    : text {
	      key = \"R2\";
	    }
	    : text {
	      key = \"R3\";
	    }
	    : text {
	      key = \"R4\";
	    }
	    : text {
	      key = \"R5\";
	    }
	    : text {
	      key = \"R6\";
	    }
	    : text {
	      key = \"R7\";
	    }
	    : text {
	      key = \"R8\";
	    }
	    spacer;
	  }
	}
	ok_cancel_help;
	//ok_cancel_help_errtile;
	: text {
	  key = \"info\";
	  label = \"Copyright \\u+00A9 2007-2019 Highflybird. All rights reserved.\";
	  width = 20;
	}
    }  "
    file
  )
  (close file)
  Dcl_File
)

;;;=============================================================
;;; ����ľͲ��ý�����                                          
;;;=============================================================
(vl-load-com)
(if (= "CHS" (getvar "Locale"))
  (prompt "��������: JF")
  (prompt "Please enter: Quadrature")
)
(c:JF)
(princ)





