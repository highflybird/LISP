;|*************************************************************;
软件作者: Highflybird                                          ;
软件用途: 为AutoCAD 的LISP定制的一些算法和函数(计算部分)       ;
日期地点: 2019.07.03 深圳                                      ;
程序语言: AutoLISP,Visual LISP                                 ;
版本号:   Ver. 1.19.0703                                       ;
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
;;; 函数目的: 字符表达式转为函数，主要用于多次调用时提升速度    
;;; 输入: expr--字符表达式,sFunc--函数名,sArg--参数符号列表     
;;; 输出: 定义函数，并返回其名                                  
;;; 例子: (CAL:Expr2Func "sin(x)+20*y" 'test '(x y))            
;;; 结果: 定义了一个名为test的函数，参数符号为x y               
;;; 注意: 除法区分整数和浮点数，譬如"2/3"和"2/3.0"结果不同；    
;;;       可用自定义函数，前提是首先要加载；                    
;;;       可用科学计算法，但应满足LISP中的语法。建议用括号；    
;;;       表达式应满足语法要求，小数和乘号不能按省略写法。      
;;;=============================================================
(defun CAL:Expr2Func (expr sFunc sArgs / lst)	
  (setq lst (CAL:Separate expr))				;先按照括号空格和运算符分离字符
  (setq lst (CAL:Operators lst '((^ . expt)) ()))	        ;乘方（幂）最优先
  (setq lst (CAL:Operators lst '((* . *) (/ . /) (% . rem)) ()));其次乘除和求模运算
  (setq lst (CAL:Operators lst '((+ . +) (- . -)) ()))		;最后处理加减法运算
  (defun-q-list-set sFunc (cons sArgs lst))			;表达成函数
  sFunc
)

;;;=============================================================
;;; 函数目的: 字符表达式求值                                    
;;; 输入: expr--字符表达式                                      
;;; 输出: 计算表达式的结果                                      
;;; 例子: (CAL:Expr2Value "sin(1)+20*2")                        
;;; 结果: 40.8415                                               
;;;=============================================================
(defun CAL:Expr2Value (expr / lst)
  (setq lst (CAL:Separate expr))				;先按照括号空格和运算符分离字符
  (setq lst (CAL:Operators lst '((^ . expt)) ()))	        ;乘方（幂）最优先
  (setq lst (CAL:Operators lst '((* . *) (/ . /) (% . rem)) ()));其次乘除和求模运算
  (setq lst (CAL:Operators lst '((+ . +) (- . -)) ()))		;最后处理加减法运算
  (eval (car lst))						;求值
)

;;;=============================================================
;;; 函数目的: 先分离出函数和+-*/%^运算符，其余均视作变量或数值，
;;; 并简单检查括号匹配。                                        
;;; 输入: expr--字符表达式                                      
;;; 输出: 函数（包括运算符）和变量及数值的列表                  
;;;=============================================================
(defun CAL:Separate (expr / CHAR FUNS LASTCHAR LST Temp LBRACKET RBRACKET next)
  (setq expr (vl-string-translate "{[]}\t\n," "(())   " expr))  ;替换{[]}\t\n,字符
  (setq expr (strcase expr t))					;全部转为小写
  (setq funs '("+" "-" "*" "/" "^" "%" ))		        ;按照基本运算符分割字符
  (setq Temp "")
  (setq lst "(")
  (setq Lbracket 0)						;左括号计数器
  (setq Rbracket 0)						;右括号计数器
  (while (/= expr "")
    (setq char (substr expr 1 1))                               ;字符串的第一个字符
    (setq next (substr expr 2 1))				;字符串的第二个字符
    (if	(or (= char "(")
	    (= char ")")					;括号一定是分隔符
	    (and (= char " ") (/= next "(") (/= next " "))      ;如果不是连续的空格符
	    (and (member char funs)				;根据运算符进行分割
	         (not (CAL:isScientific temp lastchar char))    ;忽略科学计数法
	    )	       	                                                           
	)
      (progn
	(if (CAL:IsFunction (Read temp))			;如果为普通函数
	  (setq	lst	 (strcat lst "(" Temp " " )		;则把括号移至函数符号前
		Lbracket (1+ Lbracket)				;左括号计数器加1
	  )
	  (progn
	    (and (= char "(") (setq Lbracket (1+ Lbracket)))    ;左括号计数器加1
	    (and (= char ")") (setq Rbracket (1+ Rbracket)))	;右括号计数器加1
	    (setq lst (strcat lst Temp " " char " "))
	  )
	)
	(setq Temp "")                                          ;如果是函数或者括号空格之类，则在此处重新开始  
      )
      (setq Temp (strcat Temp char))                            ;否则连取这个字符
    )
    (setq expr (substr expr 2))					;字符串剩下的字符
    (setq lastchar char)
  )
  (if (/= Lbracket Rbracket)					;如果括号不平衡
    (alert "括号不匹配(Mismatched Brackets)!")			;警告信息
    (read (strcat lst Temp ")"))				;否则转为表
  )
)

;;;=============================================================
;;; 函数目的: 分析+-*/%^运算符，并组合到表中                    
;;; 输入: lst-已分割的表,funs-待分析的运算符,Recursive-是否递归 
;;; 输出: 函数（包括运算符）和变量及数值的列表                  
;;;=============================================================
(defun CAL:Operators (lst funs Recursive / fun L n)
  (foreach a lst
    (if	(listp a)
      (setq a (CAL:Operators a funs T))				;如果元素为表，则递归进去
    )
    (if (= (type a) 'INT)
      (setq a (float a))
    )
    (if	(setq fun (cdr (assoc (car L) funs)))                   ;前一个符号为+-*/%^运算符
      (if (or (null (setq n (cadr L)))                          ;前前一个符号为空
	      (and (VL-SYMBOLP n) (CAL:IsFunction n))           ;或者是函数符号
	  )
	(setq L (cons (list fun a) (cdr L)))                    ;无须交换位置
	(setq L (cons (list fun n a) (cddr L)))	                ;交换运算符和操作数位置
      )
      (setq L (cons a L))                                       ;其他的不做改变
    )                                            
  )
  (setq n (car L))
  (if (and Recursive (not (cadr L)) (or (listp n) (numberp n))) ;如果是递归的,而且只有一个元素，且这个元素为表或者数字
    n								;那么就只取这个元素，以防止多余括号出现
    (reverse L)							;cons运算后的反转表列
  )
)

;;;=============================================================
;;; 函数目的: 判断一个符号是不是普通函数（内部函数或自定义函数）
;;;=============================================================
(defun CAL:IsFunction (n / s)
  (setq s (type (eval n)))
  (and (or (= s 'SUBR) (= s 'USUBR)) (not (member n '(+ - * /))))
)

;;;=============================================================
;;; 函数目的: 检测一个字符串是否是科学计数法(是否有更好方法?)   
;;;=============================================================
(defun CAL:isScientific (temp lastchar char)
  (and (= lastchar "e") (numberp (read (strcat temp char "0"))))
)

;;;=============================================================
;;; 函数目的: 检查函数表达式转函数的结果                        
;;; 输入: lst,用cal:expr2func求得的表                           
;;; 输出: 如果表达式里有非参数且未赋值的变量符号则返回nil, 否则T
;;; 例子: (CAL:CheckFunc (CAL:Expr2func "sin(a)+20*2" 'fx '(x)))
;;; 结果: nil                                                   
;;;=============================================================
(defun CAL:CheckFunc (lst / isOK CAL:TempSym Args)
  (setq IsOK T)
  (setq Args (car lst))
  (while (setq lst (cdr lst))                                   
    (setq CAL:TempSym (car lst))                                ;对表中的每个元素
    (if	(listp CAL:TempSym)					;如果这个元素为表
      (if CAL:TempSym
	(setq IsOk (CAL:CheckFunc (cons Args CAL:TempSym)))	;且不为空则递归进去
	(setq IsOk nil)                                         ;否则检测结果为假
      )
      (if (and (vl-symbolp CAL:TempSym)                         ;如果是一个符号
	       (not (member CAL:TempSym Args))			;且不为参数表中的符号
	       (not (vl-symbol-value CAL:TempSym))              ;且未赋值
	  )
	(setq IsOk nil)						;则检测结果为假
      )
    )
    (if	(null IsOK)
      (setq lst nil)
    )
  )
  IsOK
)

;;;=============================================================
;;;以下函数为自定义的一些简单的数学函数                         
;;;=============================================================
(defun r2d (x) (* 57.2957795130823208768 x))                    ;弧度转度
(defun d2r (x) (* 0.01745329251994329577 x))                    ;度转弧度
(defun int (x) (atoi (rtos x 2 0)))                             ;四舍五入取整函数
(defun ceil (x) (1+ (fix x)))                                   ;天花板函数
(defun ln (x) (log x))            				;以e为底的对数函数
(defun log10 (x) (* (log x) 0.43429448190325182765))            ;以10为底的对数函数
(defun exp10 (x) (expt 10 x))					;以10为底的指数函数
(defun pow (x y) (expt x y))                                    ;指数函数
(defun tan (x) (/ (sin x) (cos x)))				;正切函数
(defun cot (x) (/ (cos x) (sin x)))				;余切函数
(defun sec (x) (/ 1 (cos x)))                                   ;正割函数
(defun csc (x) (/ 1 (sin x)))					;余割函数
(defun asin (x) (atan x (sqrt (- 1 (* x x)))))                  ;反正弦函数
(defun acos (x) (atan (sqrt (- 1 (* x x))) x))			;反余弦函数
(defun sinh (x) (* 0.5 (- (exp x) (exp (- x)))))		;双曲正弦函数
(defun cosh (x) (* 0.5 (+ (exp x) (exp (- x)))))		;双曲余弦函数
(defun tanh (x) (- 1 (/ 2 (1+ (exp (+ x x)))))) 		;双曲正切函数
(defun coth (x) (/ 1 (tanh x)))					;双曲余切函数
(defun sech (x) (/ 1 (cosh x)))					;双曲正割函数
(defun csch (x) (/ 1 (sinh x)))					;双曲余割函数
(defun asinh (x) (log (+ x (sqrt (1+ (* x x))))))		;反双曲正弦函数=log(x+sqrt(x*x+1))
(defun acosh (x) (log (+ x (sqrt (1- (* x x))))))       	;反双曲余弦函数=log(x+sqrt(x*x-1))
(defun atanh (x) (log (sqrt (/ (+ 1 x)(- 1 x)))))		;反双曲正切函数=log(sqrt((1+x)/(1-x)))
(defun revSign (x) (- x))					;反号函数
(defun reciprocal (x) (/ 1.0 x))				;倒数
(defun sqr (x) (* x x))						;平方函数
(defun cube (x) (* x x x))					;立方函数
(defun cuberoot	(x)						;立方根函数
  (if (minusp x)
    (- (expt (- x) 0.333333333333333333333))
    (expt x 0.333333333333333333333)
  )
)
(defun round (x / y)						;四舍五入函数
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
;;; 例子:
;;; (CAL:Separate "(sin(-x)-cos(-x+(1+8*(2/7))+2^4-5))*0.5-0.5e-20+20*cos(x)+20")
;;; 结果: ((SIN - X) - (COS - X + (1 + 8 * (2 / 7)) + 2 ^ 4 - 5))
;;; (CAL:Expr2Func "(sin(+x)-cos(-x+(1+8*(2/7))+(2^4)-5))*0.5-0.5e-20+20*cos(x)+20" 'test '(x))
;;; 结果: 定义了一个名为test的函数，参数符号为x
;;; (CAL:Expr2Value "(sin(+0.5)-cos(-pi+(1+8*(2/7))+(2^4)-5))*0.5-0.5e-20+20*cos(pi/2)+20")
;;; 结果: 20.6616
;;; 以下是关于这个程序的其他方法：
;;; 方法一:用cal函数计算
;;; 如:(cal "1+4+5*2+(5+5)/2+((6+6)/2+(5+5)/2)") 
;;; 优点：CAD内置函数。
;;; 缺点：这个函数要求先要加载cal函数.并且三角函数会自动把变量或者数值理解为角度。
;;; 方法二:wcs脚本语言法,无痕提出的一种方法
;;; (setq wcs (vla-GetInterfaceObject (vlax-get-acad-object) "ScriptControl"))
;;; (vlax-put-property wcs "language" "vbs")
;;; (vla-eval wcs "1+4+5*2+(5+5)/2+((6+6)/2+(5+5)/2)")  ;返回 ->31.0
;;; 优点：能按照vb的语法直接计算。
;;; 缺点：难以定义表达式为函数，不能利用自定义函数，在64位CAD上此法行不通，因为不能创建脚本对象。
;;; 下面例子为在CAD中绘制函数图像
(defun c:test1(/ expr a b d x y e pts)
  (setq expr (getstring "\n请输入表达式:"))
  (initget 1)
  (setq a (getreal "\n上届:"))
  (initget 1)
  (setq b (getreal "\n下届:"))
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
;;; 在CAD中绘制参数曲线
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
;;; 定义为函数后，明显速度快多了
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
表达式一般来说有三种：前缀表达式、中缀表达式、后缀表达式，其中后缀表达式又叫做逆波兰表达式。
中缀表达式是最符合人们思维方式的一种表达式，顾名思义，就是操作符在操作数的中间。
而前缀表达式和后缀表达式中操作符分别在操作数的前面和操作数的后面。
在写表达式时候，我们习惯用中缀表达式，譬如 1+2*3-4/5。并且按照操作符的优先级进行计算。
然而LISP语言是一种前缀表达式，为了把表达式转为LISP函数或者求值，需要进行翻译，添加大量的括号和修改函数的顺序。
这个程序的目的就是使得这一工作变简单。
当然，CAD里面本身也有几种种方式能完成这个，但它们的优缺点容我后面讨论。
程序借鉴了飞诗的一些代码，在此深表感谢。
;;|;