;|*************************************************************;
软件作者: Highflybird                                          ;
软件用途: 为AutoCAD 的LISP定制的一些算法和函数(数学：数值积分) ;
日期地点: 2019.07.14 深圳                                      ;
程序语言: AutoLISP,Visual LISP                                 ;
版本号:   Ver. 1.16.0714                                       ;
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

;;;=============================================================
;;; 用各种方法求积分的程序                                      
;;;=============================================================
(defun C:Quadrature (/ ID OK DCL_FILE)
  (setq id (load_dialog (setq Dcl_File (MATH::INT:Write_Dcl))))	;从对话框中得到表达式
  (vl-file-delete Dcl_File)					;删除临时对话框文件
  (setq ok 2)
  (if (new_dialog "dcl_Integration" id)
    (progn
      (VL-CATCH-ALL-APPLY 'MATH::INT:GetSettings)		;读取默认数据
      (action_tile "help" "(MATH::INT:Help 1)")			;帮助
      (foreach k '(0 1 2 3 4 5 6 7 8)
	(setq k (strcat "K" (itoa k)))
        (action_tile k "(MATH::INT:OnBtn $key)")                ;按钮动作，对应相对的积分方法
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
;;; 从环境变量读取上次数据                                      
;;;=============================================================
(defun MATH::INT:GetSettings (/ data)
  (if (setq Data (getenv "Intergration"))
    (foreach k (read data)
      (set_tile (car k) (cdr k))
    )
  )
)

;;;=============================================================
;;; 检查对话框输入                                              
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
;;; 按钮动作，对应相应的函数求积分                              
;;;=============================================================
(defun MATH::INT:OnBtn (key / DATA s N a b OldZIN m EPS RET tm0 map msg foo tmp idx)
  (setq m (VL-CATCH-ALL-APPLY 'MATH::INT:CheckInput '(s a b n)))
  (if (or (vl-catch-all-error-p m) (not m) (equal a b 1e-8))
    (if (vl-catch-all-error-p m) 
      (set_tile "info" (vl-catch-all-error-message m))
      (set_tile "info" "无效输入!")
    )
    (progn
      ;;如果精度过高，设置为15位的精度
      (if (> n 20)
	(setq n 15)
	(setq n (fix (abs n)))
      )
      ;;如果上区间小于下区间，则交换区间
      (if (< b a)
	(setq tmp a a b b tmp)
      )
      ;;记住对话框输入，用于下次
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
      ;;开始计算积分
      (setq eps (expt 0.1 n))
      (setq tm0 (getvar "TDUSRTIMER"))
      (setq map (MATH::INT:GetMethods))				;积分计算方法集
      (setq idx (atoi (substr key 2)))				
      (setq foo (nth idx map))					;获取计算积分的函数
      (setq ret (VL-CATCH-ALL-APPLY foo (list a b eps)))        ;获取积分值
      (if (vl-catch-all-error-p ret)
	(set_tile "info" (vl-catch-all-error-message ret))	;求解过程发生了错误
        (if (null ret)
	  (set_tile "info" "发生了错误，求值结果为空!")	
	  (progn
	    ;(MATH::INT:Bench 100 a b eps)
            (setq ret (rtos ret 2 20))
            (setq msg (get_attr key "label"))
            (setq msg (strcat msg "求的结果为:" ret))
            (set_tile (strcat "R" (itoa idx)) ret)             	;显示求解结果
            (set_tile "info" msg)				;显示求解结果
            (princ (strcat "\n" msg))				;打印求解结果
            (princ "\n费时:")
            (princ (* (- (getvar "TDUSRTIMER") tm0) 86400))
            (princ "秒.")
	  )
        )
      )
    )
  )
)

;;;=============================================================
;;; 各种积分测速                                                
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
;;; 积分方法集                                                  
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
;;; 表达式求值，也可以用cal函数                                 
;;;=============================================================
(defun MATH::INT:MyRead (str / e)
  (setq e (exp 1))
  (CAL:Expr2Value str)
)

;;;=============================================================
;;; 帮助和说明: help and instruction                            
;;;=============================================================
(defun MATH::INT:Help (n)
  (if (= n 1)
    (if	(= "CHS" (getvar "Locale"))
      (alert
	"函数式只接受符号x为变量,不规范很可能出错!
	\n函数可以LISP内置的数学函数，也可以自定义函数!
	\n指数用^表示，+-*/表示加减乘除，乘号不能省略。
	\n程序能采用多种方法求积,一般来说龙贝格积分法最快。
	\n有什么问题email: highflybird@qq.com
	\n作者: highflybird 日期2019.07"
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
    (set_tile "info" "表达式非法或者空输入.")
  )
)

;;;=============================================================
;;; 计算勒让德-高斯求积函数的系数 ，如下面的6次项系数           
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
;;; 输入: x1,x2 区间(一般来说是-1..1)，n 迭代次数,eps迭代精度   
;;; 输出: 勒让德-高斯求积函数的系数,用点表集表示                
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
;;; 勒让德-高斯求积函数                                         
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
;;; 勒让德-高斯求积函数(另一方法，慢些)                         
;;;=============================================================
(defun MATH::INT:Gauss-Legendre1 (a b eps / FLAG G H L N X Y)
  (setq n 1)                                                      
  (setq flag T)							;是否进行迭代
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
;;; 高斯-埃米尔特积分                                           
;;; 功能: 计算 e^(-x^2)*f(x)的广义积分(在区间-INF..INF上)       
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
;;; 获取埃米尔特系数                                            
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
;;; 高斯-雅克比积分                                             
;;; 功能: 计算 f(x)*((1-x)^a)*((1+x)^b)的积分(在区间-1..1上)    
;;;=============================================================
(defun MATH::INT:Gauss-Jacobi (a b eps / ALF ARGS BET G N X Y flag g0)
  (if (setq args (UTI:InputBox))
    (progn
      (setq flag T)						;是否进行迭代
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
;;; 高斯-雅克比积分                                             
;;; 功能: 计算高斯-雅克比积分系数项                             
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
;;; 高斯-拉盖尔积分                                             
;;; 功能: 计算 e^(-x)*f(x)的广义积分(在区间0..INF上)            
;;;=============================================================
(defun Math::INT:Gauss-Laguerre (a b eps / n flag L g h x y maxN)
  (setq n 2)
  (setq flag T)							;是否进行迭代
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
;;; 获取拉盖尔系数                                              
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
;;; 高斯-切比雪夫积分                                           
;;; 此方法用于针对 sqrt(1-x^2)*f(x)型的积分有很高的效率。       
;;; 输入: foo 函数名,arg 除自变量x外的参数列表, n迭代次数。     
;;; 输出: 此类积分的数值.                                       
;;; 说明: 此积分法效率较高，n取值一般10次左右就达到有效浮点精度.
;;; http://mathworld.wolfram.com/Chebyshev-GaussQuadrature.html 
;;;=============================================================
(defun Math::INT:Gauss-Chebyshev (a b eps / FI FLAG I N S0 SX WI XI)
  (setq n 3)							;一般来说迭代6-5次左右就可以达到浮点计算精度
  (setq flag T)							;是否进行迭代
  (while (and (< n 1000) flag)					;迭代次数不超过100
    (setq wi (/ pi n))						;所有的权值均为pi/n
    (setq sx 0)
    (setq i 1)
    (repeat n
      (setq xi (cos (/ (* pi (- i 0.5)) n)))			;x 项
      (setq fi (MATH::INT:func xi))				;x 项的函数值
      (setq sx (+ sx fi))
      (setq i  (1+ i))
    )
    (setq sx (* wi sx))						;统一乘以权值
    (if (equal sx s0 eps)					;是否满足精度要求
      (setq flag nil)						;是则不再迭代
      (setq n (+ n n)						;否则迭代次数倍增
	    s0 sx						;存储积分值
      )
    )
  )
  sx
)

;;;=============================================================
;;; 功能: 第一类椭圆积分                                        
;;; 输入: Phi 和 k < 1                                          
;;; 输出: 所求椭圆积分值                                        
;;;=============================================================
(defun Math:Elliptic_Integral_1 (phi kCoff / )
  (defun MATH::INT:func (x / s)
    (setq s (* kCoff (sin x)))
    (/ 1.0 (sqrt (* (- 1 s) (1+ s))))
  )
  (MATH::INT:Romberg 0 phi 1e-15)
)

;;;=============================================================
;;; 功能: 第二类椭圆积分                                        
;;; 输入: Phi 和 k < 1                                          
;;; 输出: 所求椭圆积分值                                        
;;;=============================================================
(defun Math:Elliptic_Integral_2 (phi kCoff / )
  (defun MATH::INT:func (x /)
    (setq x (* kCoff (sin x)))
    (sqrt (* (- 1 x) (1+ x)))
  )
  (MATH::INT:Romberg 0 phi 1e-15)
)

;;;=============================================================
;;; 阶乘                                                        
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
;;; 伽玛函数                                                    
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
;;; 次函数用于比较排序                                          
;;;=============================================================
(defun MATH::INT:funcSort (e1 e2)
  (< (car e1) (car e2))
)

;;;=============================================================
;;; 组合两个系数                                                
;;;=============================================================
(defun MATH::INT:Bind (xi wi)
  (setq xi (vl-sort xi 'MATH::INT:funcSort))
  (setq wi (vl-sort wi 'MATH::INT:funcSort))
  (setq xi (mapcar 'cdr xi))
  (setq wi (mapcar 'cdr wi))
  (mapcar 'cons xi wi)
)

;;;=============================================================
;;; 龙贝格积分法                                                
;;; Romberg Integration                                         
;;; 输入: 函数名--foo (用符号表示，一般形式是 (foo x a b c ...) 
;;;       参数表--args ，除去自变量x的其它的参数列表            
;;;       下区间--Ra                                            
;;;       上区间--Rb                                            
;;;       容许误差--eps                                         
;;; 输出: 所求函数在区间段的积分                                
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
;;; 辛普森积分法                                                
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
;;; 变步长梯形求积分法                                          
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
;;; 步长积分法                                                  
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
;;; 自适应求积分法                                              
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
;;; 输入对话框                                                  
;;;=============================================================
(defun UTI:Inputbox (/ str wcs ret)
  (setq	str "Function GetNumbers()
  	     GetNumbers=inputbox(\"请输入两个参数,中间用空格隔开:\",\"输入框\")
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
;;; 写对话框到文件用于程序                                      
;;;=============================================================
(defun MATH::INT:Write_Dcl (/ Dcl_File file str)
  (setq Dcl_File (vl-filename-mktemp nil nil ".Dcl"))
  (setq file (open Dcl_File "w"))
  (princ
    "dcl_Integration : dialog {
	label = \"数值积分LISP版  v1.2\";
	: boxed_column {
          width = 60;
	  fixed_width = true;
	  : edit_box {
	    key=\"F\";
	    label= \"函数:\";
	  }
	  : row {
	    : edit_box {
	      key=\"A\";
	      label= \"下限:\";
	    }
	    : edit_box {
	      key=\"B\";
	      label= \"上限:\";
	    }      
	    : edit_box {
	      key=\"N\";
	      label = \"精确位数:\";
	      value = 8;
	      edit_width = 2;
	      fixed_width = true;
	    }
	  }
	  spacer_1;
	}
	: row {
	  : boxed_column {
	    label = \"计算方法:\";
	    : button {
	      key = \"K0\";
	      label = \"龙贝格积分法\";
	    }
	    : button {
	      key = \"K1\";
	      label = \"辛普森积分法\";
	    }
	    : button {
	      key = \"K2\";
	      label = \"自适应积分法\";
	    }
	    : button {
	      key = \"K3\";
	      label = \"变步长梯形法\";
	    }
	    : button {
	      key = \"K4\";
	      label = \"高斯-勒让德法\";
	    }
	    : button {
	      key = \"K5\";
	      label = \"高斯-切比雪夫法\";
	    }
	    : button {
	      key = \"K6\";
	      label = \"高斯-拉盖尔法\";
	    }
	    : button {
	      key = \"K7\";
	      label = \"高斯-埃尔米特法\";
	    }
	    : button {
	      key = \"K8\";
	      label = \"高斯-雅克比法\";
	    }
	    spacer;
	  }
	  : boxed_column {
	    width = 32;
	    fixed_width = true;
	    label = \"计算结果:\";
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
;;; 下面的就不用介绍了                                          
;;;=============================================================
(vl-load-com)
(if (= "CHS" (getvar "Locale"))
  (prompt "输入命令: JF")
  (prompt "Please enter: Quadrature")
)
(c:JF)
(princ)





