;|*************************************************************;
软件作者: Highflybird                                          ;
软件用途: 为AutoCAD 的LISP定制的一些算法和函数(复数部分)       ;
日期地点: 2012.12.29 深圳                                      ;
修改时间: 2016.03.26 深圳                                      ;
程序语言: AutoLISP,Visual LISP                                 ;
版本号:   Ver. 1.0.16.0326                                     ;
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
;;;一元二次方程的解                                     	
;;;f(x) = a*x^2+b*x+c = 0                               	
;;;Input: the coefficients a, b, c are real numbers     	
;;;Output: when a /= 0,one or Two solutions,all of them like 	
;;;        this: (x y),means: x + y * i,if it's a real number,  
;;;        then y = 0.Otherwise ,  return a real number or nil  
;;;Ref: http://en.wikipedia.org/wiki/Quadratic_equation 	
;;;=============================================================
(defun Math:Quadratic_Equation (a b c / d e g)
  (if (zerop a)
    (if (not (zerop b))
      (list (list (/ (- c) (float b)) 0))
    )
    (progn
      (setq a (float a))
      (if (not (equal a 1 1e-14))
        (setq b (/ b a)
              c (/ c a)
        )
      )
      (setq d (- (* b b) (* 4 c)))
      (setq e (* b -0.5))
      (cond
        ( (equal d 0 1e-14)
          (list (list e 0) (list e 0))
        )
        ( (> d 0)
          (setq g (* (sqrt d) -0.5))
          (list (list (- e g) 0) (list (+ e g) 0))
        )
        ( (< d 0)
          (setq g (* (sqrt (- d)) -0.5))
          (list (list e (- g)) (list e g))
        )
      )
    )
  )
)


;;;=============================================================
;;;一元二次方程的解                                     	
;;;f(x) = a*x^2+b*x+c = 0                               	
;;;=============================================================
(defun Math:Quadratic_Equation_1 (a b c / d e g)
  (if (zerop a)
    (if	(not (zerop b))
      (list (/ (- c) (float b)))
    )
    (progn
      (setq a (float a))
      (if (not (equal a 1 1e-14))
	(setq b (/ b a)
	      c (/ c a)
	)
      )
      (setq d (- (* b b) (* 4 c)))
      (setq e (* b -0.5))
      (cond
	( (equal d 0 1e-14)
	  (list e)
	)
	( (> d 0)
	  (setq g (* (sqrt d) -0.5))
	  (list (- e g) (+ e g))
	)
      )
    )
  )
)

;;;=============================================================
;;;一元三次方程的解                                     	
;;;f(x) = a*x^3+b*x^2+c*x+d = 0                         	
;;;Input: the coefficients a, b, c, d are real numbers  	
;;;Output: when a /= 0,Three solutions,all of them like this: 	
;;;        (x y),means: x+y*i,if it's a real number,then y = 0; 
;;;        otherwise goto "Math:Quadratic_Equation"     	
;;;Ref: http://en.wikipedia.org/wiki/Cubic_function     	
;;;=============================================================
(defun Math:Cubic_Equation (a b c d / e f g h u w x y)
  (cond
    ( (zerop a)
      (Math:Quadratic_Equation b c d)
    )
    ( (zerop d)
      (cons '(0 0) (Math:Quadratic_Equation a b c))
    )
    (t
      (setq b (/ b a 3.0))
      (setq c (/ c a 6.0))
      (setq d (/ d a 2.0))
     
      (setq e (- (* b (- (+ c c c) (* b b))) d))        	;Alpha
      (setq f (- (* b b) c c))                          	;Beta
      (setq g (- (* e e) (* f f f)))                    	;Delta,The nature of the roots
      (cond
        ( (equal g 0 1e-14)
          (setq u (MATH:CubeRoot e))
          (list (list (- (+ u u) b) 0.0)
                (list (- (+ b u)) 0.0)
                (list (- (+ b u)) 0.0)
          )
        )
        ( (> g 0)
          (setq h (sqrt g))
          (setq u (MATH:CubeRoot (+ e h)))
          (setq w (MATH:CubeRoot (- e h)))
          (setq x (- (+ b (* (+ u w) 0.5))))
          (setq y (* (sqrt 3) (- u w) 0.5))
          (list (list (+ u w (- b)) 0)
                (list x y)
                (list x (- y))
          )
        )
        ( (< g 0)
          (setq x (/ e f (sqrt f)))
          (setq y (sqrt (abs (- 1 (* x x)))))

          (setq u (/ (atan y x) 3))
          (setq w (/ (+ pi pi) 3))
          (setq h (* 2 (sqrt f)))
          (list (list (- (* (cos u) h) b) 0)
                (list (- (* (cos (+ u w)) h) b) 0)
                (list (- (* (cos (- u w)) h) b) 0)
          )
        )
      )
    )
  )
)
;;;=============================================================
;;;一元三次方程的另外一种解法                           	
;;;=============================================================
(defun Math:Cubic_Equation1 (a b c d / 2A E F M N P Q X Y)
  (if (zerop a)
    (Math:Quadratic_Equation b c d)
    (progn 
      (setq a (float a))
      (setq m (- (* 36 a b c) (* 8 b b b) (* 108 a a d)))
      (setq f (- (* 4 b b) (* 12 a c)))
      (setq n (sqrt (- (* m m) (* f f f)))) 			;abs 有问题?
      (setq p (MATH:CubeRoot (+ m n)))
      (if (equal p 0 1e-8)
        (setq q (MATH:CubeRoot (- m n)))
        (setq q (/ f p))
      )
      (setq 2b (+ b b))
      (setq x (/ (+ p q 2b 2b) -12.0 a))
      (setq y (/ (* 0.144337567297406441127 (- p q)) a))     	;(sqrt 3)*(p-q)/12
      (list
        (list (/ (- (+ p q) 2b) 6.0 a) 0)
        (list x y)
        (list x (- y))
      )
    )
  )
)
;;;=============================================================
;;;一元四次方程的解                                     	
;;;f(x) = a*x^4+b*x^3+c*x^2+d*x+e= 0                    	
;;;Input: the coefficients a,b,c,d,e are real numbers.  	
;;;Output: when a /= 0,Three solutions,all of them like this: 	
;;;        (x y),means: x+y*i,if it's a real number,then y = 0, 
;;;        otherwise goto "Math:Quadratic_Equation".            
;;;Ref: http://en.wikipedia.org/wiki/Quartic_function   	
;;;=============================================================
(defun Math:Quartic_Equation (a b c d e / B2 B3 B4 EPS F G H P Q R V W X Y Z S)
  (setq eps 1e-8)
  (cond
    ( (equal a 0 eps)
      (Math:Cubic_Equation b c d e)
    )
    ( (equal e 0 eps)
      (cons '(0 0) (Math:Cubic_Equation a b c d))
    )
    ( (and (equal b 0 eps) (equal d 0 eps))
      (foreach x (Math:Quadratic_Equation a c e)
        (foreach y (Math:CSqrt x)
          (setq s (cons y s))
        )
      )
    )
    ( (and (equal b 0 eps) (equal c 0 1e-7)(equal d 0 1e-6))
      (setq f (/ e (float a)))
      (if (minusp f)
	(progn
	  (setq f (sqrt (sqrt (abs f))))
	  (list (list f 0) (list (- f) 0) (list 0 f) (list 0 (- f)))
	)
	(progn 
          (setq f (sqrt (* 0.5 (sqrt f))))
	  (list (list f f) (list (- f) f) (list f (- f)) (list (- f) (- f)))
	)
      )
    )
    ( (setq a (float a))
      (setq b (/ b a))
      (setq c (/ c a))
      (setq d (/ d a))
      (setq e (/ e a))
      (setq b2 (* b b))
      (setq b3 (* b2 b))
      (setq b4 (* b3 b))
     
      (setq f (+ (* b2 -0.375) c))                      	;alpha
      (setq g (+ (* b3 0.125) (* b c -0.5) d))          	;beta
      (setq h (+ (* -0.01171875 b4) (* 0.0625 c b2) (* -0.25 b d) e))

      (if (equal g 0 eps)
        (progn 
          (setq x (* b -0.25))
          (mapcar
            (function (lambda (e) (list (+ (car e) x) (cadr e))))
            (Math:Quartic_Equation 1 0 f 0 h)           	;Recursion 
          )
        )
        (progn 
          (setq p (- (* f f 2) h))
          (setq q (- (* f f f 0.5) (* f h 0.5) (* g g 0.125)))
          (setq r (Math:Cubic_Equation 1 (* 2.5 f) p q))
          (while (not (equal (cadar r) 0 eps))
            (setq r (cdr r))
          )
          (setq r (caar r))
          (setq w (sqrt (abs (+ f r r))))
          (foreach i (list + -)
            (foreach j (list + -)
              (setq x (i (* b -0.25) (* w 0.5)))
              (setq y (- (+ f f f r r (i (/ g 0.5 w)))))
              (if (< y 0)
                (setq z (list x (j (* (sqrt (- y)) 0.5))))
                (setq z (list (+ x (j (* (sqrt y) 0.5))) 0)) 
              )
              (setq S (cons z S))
            )
          )
        )
      )
    )
  )
)

(defun Math:Quartic_Equation1 (a b c d e / B2 B3 B4 EPS F G H P Q R V W X Y Z S)
  (setq eps 1e-8)
  (setq cc1 1.2599210498948731647672106072782);cuberoot 2
  (setq cc2 (/ cc1 3))
  (setq cc1 (* cc1 3))
  (cond
    ( (equal a 0 eps)
      (Math:Cubic_Equation b c d e)
    )
    ( (equal e 0 eps)
      (cons '(0 0) (Math:Cubic_Equation a b c d))
    )
    ( (and (equal b 0 eps) (equal d 0 eps))
      (foreach x (Math:Quadratic_Equation a c e)
        (foreach y (Math:CSqrt x)
          (setq s (cons y s))
        )
      )
    )
    ( (and (equal b 0 eps) (equal c 0 eps)(equal d 0 eps))
      (setq f (/ e (float a)))
      (if (minusp f)
	(progn
	  (setq f (sqrt (sqrt (abs f))))
	  (list (list f 0) (list (- f) 0) (list 0 f) (list 0 (- f)))
	)
	(progn 
          (setq f (sqrt (* 0.5 (sqrt f))))
	  (list (list f f) (list (- f) f) (list f (- f)) (list (- f) (- f)))
	)
      )
    )
    ( (setq a (float a))
      (setq b (/ b a))
      (setq c (/ c a))
      (setq d (/ d a))
      (setq e (/ e a))
      (setq b1 (+ (* c c) (* -3 b d) (* 12 e)))
      (setq b2 (+ (* 2 c c c) (* -9 b c d) (* 27 d d) (* 27 b b e) (* -72 c e)))
      (setq c1 (- (* 0.25 b b) (/ c 1.5)))
      (setq c2 (+ c1 c1))
      (setq d1 (- (* b2 b2) (* 4 b1 b1 b1)))
      (setq d2 (Math:CCSqrt (list d1 0)))
      (setq d3 (Math:C+C (list b2 0) d2))
      (setq d4 (MATH:CCRoot d3))
      (setq d5 (Math:c/c (list (* cc1 b1) 0) d4))
      (setq d6 (Math:c/c d4 (list cc2 0)))
      (setq dd (math:c+c d5 d6))
    )
  )
)

;;;=============================================================
;;;立方根(为解三次和四次方程编写，因expt函数第一个参数不能为负) 
;;;输入：x 实数                                                 
;;;输出：x 的立方根                                             
;;;=============================================================
(defun MATH:CubeRoot (x)
  (if (< x 0)
    (- (expt (- x) 0.33333333333333333333333))
    (expt x 0.33333333333333333333333)
  )
)

;;;=============================================================
;;;复数加复数                                                   
;;;输入：Z1--复数，Z2--复数                                     
;;;输出：复数跟实数相加的结果                                   
;;;=============================================================
(defun Math:C+C (z1 z2)
  (mapcar '+ z1 z2)
)

;;;=============================================================
;;;复数减复数                                                   
;;;输入：Z1--复数，Z2--复数                                     
;;;输出：复数跟实数相减的结果                                   
;;;=============================================================
(defun math:C-C (z1 z2)
  (mapcar '- Z1 Z2)
)

;;;=============================================================
;;;复数乘复数                                                   
;;;输入：Z1--复数，Z2--复数                             	
;;;输出：复数跟实数相乘的结果                           	
;;;=============================================================
(defun Math:C*C (Z1 Z2)
  (list
    (- (* (car Z1) (car z2)) (* (cadr z1) (cadr z2)))
    (+ (* (car Z1) (cadr Z2)) (* (cadr z1) (car Z2)))
  )
)

;;;=============================================================
;;;复数除以复数                                         	
;;;输入：Z1--复数，Z2--复数                             	
;;;输出：复数跟实数相除的结果                           	
;;;=============================================================
(defun Math:C/C (Z1 Z2 / a b c d N)
  (setq a (car  z1))
  (setq b (cadr z1))
  (setq c (car  z2))
  (setq d (cadr z2))
  (setq N (float (+ (* c c) (* d d))))
  (if (not (zerop N))
    (list
      (/ (+ (* a c) (* b d)) N)
      (/ (- (* b c) (* a d)) N)
    )
  )
)

;;;=============================================================
;;;复数加实数                                           	
;;;输入：Z --复数，R --实数                             	
;;;输出：复数跟实数相加的结果                           	
;;;=============================================================
(defun Math:C+R (Z R)
  (list (+ (car z) R) (cadr z))
)

;;;=============================================================
;;;复数减实数                                           	
;;;输入：Z --复数，R --实数                             	
;;;输出：复数跟实数相减的结果                           	
;;;=============================================================
(defun Math:C-R (Z R)
  (list (- (car z) R) (cadr z))
)

;;;=============================================================
;;;复数乘实数                                           	
;;;输入：Z --复数，R --实数                             	
;;;输出：复数跟实数相乘的结果                           	
;;;=============================================================
(defun Math:C*R (Z R)
  (list (* (car z) R) (* (cadr z) R))
)

;;;=============================================================
;;;复数除以实数                                         	
;;;输入：Z --复数，R --实数                             	
;;;输出：复数跟实数相除的结果                           	
;;;=============================================================
(defun Math:C/R (Z R)
  (if (not (zerop R))
    (list (/ (car z) R) (/ (cadr z) R))
  )
)

;;;=============================================================
;;;复数的模                                             	
;;;输入：Z --复数                                       	
;;;输出：复数的模，即复数代表的矢量长度                 	
;;;=============================================================
(defun MATH:CNormal (Z)
  (distance '(0 0) Z)
)

;;;=============================================================
;;;复数的平方根                                         	
;;;输入：Z --复数                                       	
;;;输出：复数的平方根，以复数形式表达，有两个解         	
;;;=============================================================
(defun Math:CSqrt (Z / x y a r)
  (setq x (car z))
  (setq y (cadr z))
  (if (equal y 0 1e-14)
    (if (> x 0)
      (list (list (setq x (sqrt x)) 0) (list (- x) 0))
      (list (list 0 (setq y (sqrt (- x)))) (list 0 (- y)))
    )
    (progn
      (setq a (* (atan y x) 0.5))
      (setq r (sqrt (distance '(0 0) Z)))
      (setq x (* r (cos a)))
      (setq y (* r (sin a)))
      (list (list x y) (list (- x) (- y)))
    )
  )
)

(defun Math:CCSqrt (Z / x y a r)
  (setq x (car z))
  (setq y (cadr z))
  (if (equal y 0 1e-14)
    (if (> x 0)
      (list (sqrt x) 0)  
      (list 0 (sqrt (- x)))
    )
    (progn
      (setq a (* (atan y x) 0.5))
      (setq r (sqrt (distance '(0 0) Z)))
      (setq x (* r (cos a)))
      (setq y (* r (sin a)))
      (list x y) 
    )
  )
)

;;;=============================================================
;;;复数的方根                                           	
;;;输入：Z --复数, n --正整数                           	
;;;输出：复数的n次方根，以复数形式表达，有n个解。       	
;;;=============================================================
(defun MATH:CRoot (Z n / r a b i s 2Pi)
  (if (and (= (type n) 'INT) (> n 0))
    (progn
      (setq r (expt (distance '(0 0) z) (/ 1. n)))
      (setq a (atan (cadr z) (car z)))
      (setq i 0)
      (setq 2Pi (+ pi pi))
      (repeat n
        (setq b (/ (+ a (* i 2Pi)) n))
        (setq s (cons (list (* r (cos b)) (* r (sin b))) s))
        (setq i (1+ i))
      )
      (reverse s)
    )
  )
)

(defun MATH:CCRoot (Z n / r a b i s 2Pi)
  (if (and (= (type n) 'INT) (> n 0))
    (progn
      (setq r (expt (distance '(0 0) z) (/ 1. n)))
      (setq a (atan (cadr z) (car z)))
      (setq b (/ a n))
      (list (* r (cos b)) (* r (sin b)))
    )
  )
)

;;;=============================================================
;;;复数的实幂指数                                       	
;;;输入：Z --复数, x -- 实数                            	
;;;输出：复数Z 的x次幂，以复数形式表达。                	
;;;=============================================================
(defun MATH:CRPower (Z x / a r)
  (setq a (atan (cadr Z) (car Z)))
  (setq r (expt (distance '(0 0) Z) x))
  (list (* r (cos (* a x))) (* r (sin (* a x))))
)

;;;=============================================================
;;;复数的复幂指数                                       	
;;;输入：Z1 --复数, Z2 -- 复数                          	
;;;输出：复数Z1的Z2次幂，以复数形式表达。               	
;;;=============================================================
(defun MATH:CZPower (Z1 Z2 / a r x y k)
  (if (> (setq r (distance '(0 0) Z1)) 1e-20)
    (progn 
      (setq a (atan (cadr Z1) (car Z1)))
      (setq x (car z2))
      (setq y (cadr z2))
      (setq k (log r))
      (setq r (exp (- (* k x) (* y a))))
      (setq a (+ (* k y) (* x a)))
      (list (* r (cos a)) (* r (sin a)))
    )
  )
)

;;;=============================================================
;;;复数的自然对数                                        	
;;;输入：Z --复数                                       	
;;;输出：复数Z 的自然对数，以复数形式表达。             	
;;;=============================================================
(defun MATH:CExp (Z / r)
  (if (> (setq r (distance '(0 0) Z)) 1e-20)
    (list (log r) (atan (cadr Z) (car Z)))
  )
)

;;;=============================================================
;;;复数的余弦                                           	
;;;输入：Z --复数                                          	
;;;输出：复数Z 的余弦，以复数形式表达。                 	
;;;=============================================================
(defun MATH:CCOS (Z / x y c s u v)
  (setq x (car z))
  (setq y (cadr z))
  (if (equal y 0 1e-20)
    (list (cos x) 0)
    (progn
      (setq c (* 0.5 (cos x)))
      (setq s (* 0.5 (sin x)))
      (setq u (exp y))
      (setq v (exp (- y)))
      (list (* c (+ v u)) (* s (- v u)))
    )
  )
)

;;;=============================================================
;;;复数的正弦                                          		
;;;输入：Z --复数                                       	
;;;输出：复数Z 的正弦，以复数形式表达。                 	
;;;=============================================================
(defun MATH:CSIN (Z / x y c s u v)
  (setq x (car z))
  (setq y (cadr z))
  (if (equal y 0 1e-20)
    (list (sin x) 0)
    (progn
      (setq s (* 0.5 (sin x)))
      (setq c (* 0.5 (cos x)))
      (setq u (exp (- y)))
      (setq v (exp y))
      (list (* s (+ v u)) (* c (- v u)))
    )
  )
)

;;;=============================================================
;;;复数的正切                                           	
;;;输入：Z --复数                                       	
;;;输出：复数Z 的正切，以复数形式表达。                 	
;;;=============================================================
(defun MATH:CTAN (Z / x y c s u v)
  (MATH:C/C (MATH:CSIN Z) (MATH:CCOS Z))
)

;;;=============================================================
;;;实系数的复数多项式运算                               	
;;;输入：Z --复数, RealNumbers --实系数列表             	
;;;输出：根据实系数多项式复数方程值，用复数表示。       	
;;;=============================================================
(defun MATH:CReal_Polynomial (Z RealNumbers / f)
  (cond
    ( (cdr RealNumbers) 
      (setq f (MATH:C*R Z (car RealNumbers)))
      (repeat (- (length RealNumbers) 2)
        (setq RealNumbers (cdr RealNumbers))
        (setq f (MATH:C+R f (car RealNumbers)))
        (setq f (MATH:C*C f Z))
      )
      (setq f (MATH:C+R f (cadr RealNumbers)))
    )
    ( (car RealNumbers) (list (car RealNumbers) 0))
  )
)

;;;=============================================================
;;;虚系数的复数多项式运算                               	
;;;输入：Z --复数, ImaginaryNumbers--复系数列表         	
;;;输出：根据复系数多项式复数方程值，用复数表示。       	
;;;=============================================================
(defun MATH:CImaginary_Polynomial (Z ImaginaryNumbers / f)
  (if (setq f (car ImaginaryNumbers))
    (progn
      (repeat (1- (length ImaginaryNumbers))
        (setq ImaginaryNumbers (cdr ImaginaryNumbers))
        (setq f (MATH:C*C f Z)) 
        (setq f (MATH:C+C f (car ImaginaryNumbers)))
      )
      f
    )
  )
)

;;|
;;;=============================================================
;;;以下程序为测试用:                                    	
;;;Test for "Math:Quartic_Equation"                     	
;;;If the difference between the Coefficients is big,it will get
;;;some float point error.                   			
;;;=============================================================
(defun C:t4 (/ a b c d e S z z1 z2 z3 z4)
  (initget 1)
  (setq a (getreal "\nCoefficient a:"))
  (initget 1)
  (setq b (getreal "\nCoefficient b:"))
  (initget 1)
  (setq c (getreal "\nCoefficient c:"))
  (initget 1)
  (setq d (getreal "\nCoefficient d:"))
  (initget 1)
  (setq e (getreal "\nCoefficient e:"))
  ;(MISC:test 1000 '((Math:Quartic_Equation a b c d e)))
  (setq S (Math:Quartic_Equation a b c d e))            	;get the solutions
  (foreach z S
    (princ "\n")
    (princ (mapcar '(lambda (x) (rtos x 2 20)) z))      	;print them.
  )
  (foreach z S                                          	;Check every solution
    (setq f (MATH:CReal_Polynomial z (list a b c d e)))
    (if (not (equal f '(0 0) 1e-6))                     	;if f(z) /= 0.0,maybe it's caused by floating point operation;
      (alert
        (strcat
          "Maybe it's a Wrong solution: f("
          (vl-princ-to-string z)
          ") = "
          (VL-PRINC-TO-STRING f)
        )
      )
    )
  )
  (princ)
)

(defun C:t3 (/ a b c d e S z z1 z2 z3 z4)
  (initget 1)
  (setq a (getreal "\nCoefficient a:"))
  (initget 1)
  (setq b (getreal "\nCoefficient b:"))
  (initget 1)
  (setq c (getreal "\nCoefficient c:"))
  (initget 1)
  (setq d (getreal "\nCoefficient d:"))
  (UTI:BENCH
    1
    (list
      (list 'Math:Cubic_Equation a b c d)
      (list 'Math:Cubic_Equation1 a b c d)
    )
  )
  (foreach n '(Math:Cubic_Equation Math:Cubic_Equation1)
    (setq S (apply n (list a b c d)))            		;get the solutions
    (foreach z S
      (princ "\n")
      (princ (mapcar '(lambda (x) (rtos x 2 20)) z))      	;print them.
    )
    (foreach z S                                          	;Check every solution
      (setq f (MATH:CReal_Polynomial z (list a b c d)))
      (if (not (equal f '(0 0) 1e-6))                     	;if f(z) /= 0.0,maybe it's caused by floating point operation;
        (alert
          (strcat
	    "Use "
	    (VL-PRINC-TO-STRING n)
            ",Maybe it's a Wrong solution: f("
            (vl-princ-to-string z)
            ") = "
            (VL-PRINC-TO-STRING f)
          )
        )
      )
    )
  )
  (princ)
)
(princ "\nThe command is : test.")
;;|;
(vl-load-com)
(princ)
