;|*************************************************************;
软件作者: Highflybird                                          ;
软件用途: 为AutoCAD 的LISP定制的一些算法和函数(FFT算法)        ;
日期地点: 2022.07.08 深圳                                      ;
程序语言: AutoLISP,Visual LISP                                 ;
版本号:   Ver. 1.0.22.0708                                     ;
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
;;; FFT（快速傅里叶变换）和IFFT（FFT逆变换）代码的终极实现      
;;; 参数: S=1为FFT,lst为系数列表; S=-1为IFFT,lst为要还原的复数表
;;; 返回: FFT返回复数表，IFFT返回多项式系数表                   
;;; 说明: 设多项式P系数为A0,A1,...,An-1,指数从低到高            
;;;-------------------------------------------------------------
;;; A little test 小测试:                                       
;;; (FFT 1 '(1 -6 2 4))                                         
;;; => ((1.0 0.0) (-1.0 10.0) (5.0 0.0) (-1.0 -10.0))           
;;; (FFT -1 '((1.0 0.0) (-1.0 10.0) (5.0 0.0) (-1.0 -10.0)))    
;;; => (1.0 -6.0 2.0 4.0)                                       
;;; 可见很好地还原了系数。                                      
;;; 代码稍加修改可进行复系数的FFT和IFFT变换。                   
;;;=============================================================
(defun FFT (s lst / len new add)
  (if (and lst (listp lst))
    (progn
      ;;把实系数转成复系数 
      (setq lst	(mapcar (function (lambda (x) (if (listp x) x (list x 0)))) lst))
      (setq len (length lst))
      ;;如果列表长度不满足2^N，则补0
      (if (/= 0 (logand len (1- len)))
	(progn
	  (setq new (expt 2 (1+ (fix (/ (log len) (log 2))))))
	  (setq lst (appendZero lst (- new len)))
	  (setq len new)
	)
      )
      ;;S为1进行FFT，否则进行IFFT并除以新的表长。
      (if (> s 0)
	(RFFT 1 lst)
	(mapcar
	  (function (lambda (x) (/ (car x) len)))
	  (RFFT -1 lst)
	)
      )
    )
  )
)

;;;=============================================================
;;; 主函数                                                      
;;; 参数: S=1为FFT,lst为系数列表; S=-1为IFFT,lst为要还原的复数表
;;; 返回: FFT返回复数表，IFFT返回多项式系数表                   
;;;=============================================================
(defun RFFT (s lst / E K N O L0 L1)
  (setq N (length lst))
  (if (> N 2)
    (progn
      (setq k 0)
      (while (cadr lst)
	(setq L0 (cons (car lst) L0))
	(setq L1 (cons (cadr lst) L1))
	(setq lst (cddr lst))
      )
      (mapcar
	(function
	  (lambda (x y / w)
	    (setq w (phase s k N))
	    (setq y (CMP::MUL w y))
	    (setq e (cons (CMP::ADD x y) e))
	    (setq o (cons (CMP::SUB x y) o))
	    (setq k (1+ k))
	  )
	)
	(RFFT s (reverse L0))
	(RFFT s (reverse L1))
      )
      (reverse (append o e))
    )
    (if	(= N 2);减少递归深度。
      (list
	(apply 'CMP::ADD lst)
	(apply 'CMP::SUB lst)
      )
      (if (= N 1);一般到不了这步
	lst
      )
    )
  )
)

;;;=============================================================
;;; 获取偶数项                                                  
;;;=============================================================
(defun evens (f)
  (if f
    (cons (car f) (evens (cddr f)))
  )
)

;;;=============================================================
;;; 获取奇数项                                                  
;;;=============================================================
(defun odds (f)
  (if f
    (cons (cadr f) (odds (cddr f)))
  )
)

;;;=============================================================
;;; 求w(利用欧拉公式求系数)                                     
;;;=============================================================
(defun phase (s k n / x)
  (if (> s 0)
    (setq x (/ (* -6.283185307179586476925286766559 k) N))
    (setq x (/ (* +6.283185307179586476925286766559 k) N))
  )
  (list (cos x) (sin x))
)

;;;=============================================================
;;; 复数相加                                                    
;;;=============================================================
(defun CMP::ADD (z1 z2)
  (mapcar '+ z1 z2)
)

;;;=============================================================
;;; 复数相减                                                    
;;;=============================================================
(defun CMP::SUB (z1 z2)
  (mapcar '- z1 z2)
)

;;;=============================================================
;;; 复数相乘                                                    
;;;=============================================================
(defun CMP::MUL (z1 z2)
  (list
    (- (* (car z1) (car z2)) (* (cadr z1) (cadr z2)))
    (+ (* (car z1) (cadr z2)) (* (car z2) (cadr z1)))
  )
)

;;;=============================================================
;;; 实数转复数                                                  
;;;=============================================================
(defun CMP::R2C (x)
  (if (listp x) x (list x 0))
)

;;;=============================================================
;;; 字符串是否只包含数字                                        
;;;=============================================================
(defun isNumber (a)
  (vl-every
    (function (lambda (n) (< 47 n 58)))
    (vl-string->list a)
  )
)

;;;=============================================================
;;; 字符串转数字表                                              
;;; 例如："12312" => '(1 2 3 1 2)                               
;;;=============================================================
(defun str->num (a / l)
  (setq l (vl-string->list a))
  (if (vl-every (function (lambda (n) (< 47 n 58))) l)
    (mapcar (function (lambda (n) (- n 48))) l)
  )
)

;;;=============================================================
;;; 末尾补零，使得表长满足需要长度（一般来说是2^n幕）           
;;;=============================================================
(defun AppendZero (lst len / add)
  (repeat len
    (setq add (cons '(0 0) add))
  )
  (setq lst (append lst add))
)

;;;=============================================================
;;; 高精度乘法字符串形式 void multiply                          
;;; 例如：(gjds "1234567890987654321" "9876543210123456789")    
;;; =>"12193263121170553265523548251112635269"                  
;;;=============================================================
(defun GJDS (s1 s2 / a1 a2)
  (if (and (setq a1 (str->num S1)) (setq a2 (str->num S2)))
    (GJD a1 a2)
  )
)

;;;=============================================================
;;; 高精度乘法表形式 void multiply                              
;;; 例如：(gjd '(1 2 3 4 5)  '(9 8 7 6 5))                      
;;; =>"1219253925"                                              
;;;=============================================================
(defun GJD (a1 a2 / FA LEN LEN1 LEN2 LL N0 N1)
  ;;倒排表（因为十进制数是从高位到低位），并转换为复数
  (setq a1 (mapcar 'CMP::R2C (reverse a1)))
  (setq a2 (mapcar 'CMP::R2C (reverse a2)))
  ;;计算需要补零个数
  (setq len1 (length a1))
  (setq len2 (length a2))
  (setq len (max len1 len2))
  (if (zerop (logand len (1- len)))
    (setq len (* len 2))
    (setq len (expt 2 (+ 2 (fix (/ (log len) (log 2))))))
  )
  ;;补零
  (setq a1 (AppendZero a1 (- len len1)))
  (setq a2 (AppendZero a2 (- len len2)))
  ;;利用FFT把两大数相乘转为多项式乘法
  (setq a1 (RFFT 1 a1))
  (setq a2 (RFFT 1 a2))
  (setq fa (RFFT -1 (mapcar 'cmp::mul a1 a2)))
  ;;小数转整并消除多余的零
  (setq fa (mapcar (function (lambda (x) (fix (+ 0.5 (/ (car x) len))))) fa))
  (setq fa (reverse fa))
  (while (zerop (car fa))
    (setq fa (cdr fa))
  )
  (setq fa (reverse fa))
  ;;进位
  (setq n0 0)
  (foreach n fa
    (setq n1 (+ n n0))
    (setq n0 (/ n1 10))
    (setq ll (cons (rem n1 10) ll))
  )
  (if (/= n0 0)
    (setq ll (cons n0 ll))
  )
  ;;转字符串
  (vl-list->string (mapcar (function (lambda (n) (+ n 48))) ll))
)


(defun c:ttt()
  (setq rndObj (vlax-create-object "System.Random"))
  (setq x (vlax-invoke rndobj 'nextdouble))
  (setq y (vlax-invoke rndobj 'next))
 
  (defun Rnd10 (rndobj)
    (fix (* (vlax-invoke rndobj 'nextdouble) 10))
  )
  (setq N 10000)
  (setq A nil)
  (setq B nil)
  (repeat N
    (setq A (cons (rnd10 rndobj) A))
    (setq B (cons (rnd10 rndobj) B))
  )
  (princ "\n")
  (princ (apply 'strcat (mapcar 'itoa A)))
  (princ "\nX\n")
  (princ (apply 'strcat (mapcar 'itoa B)))
  (princ "\n=\n")
  (princ (GJD A B))
  (princ)
)

(defun bitrp ( v n / A B I J P)
  (setq p (fix (/ (log n) (log 2))))
;;;  (setq i 1 p 0)
;;;  (while (< i n)
;;;    (setq i (lsh i 1))
;;;    (setq p (1+ p))
;;;  )
  (setq i 0)
  (repeat n
    (setq a i)
    (setq b 0)
    (setq j 0)
    (repeat p
      (setq b (logior (lsh b 1) (logand a 1)))
      (setq a (lsh a -1))
      (setq j (1+ j))
    )
    (if (> b i)
      (vector-swap v i b)
    )
    (setq i (1+ i))
  )
)

(defun VFFT (v N)
  (setq arg (/ pi n -0.5))
  (setq treal (cos arg))
  (setq timag (sin arg))
)
  

;;;(defun reverse1	()
;;;  (setq v (make-vector 64 0))
;;;  (setq	v (list->vector
;;;	    '(1	 3  2  5  8  4	1  3  2	 5  8  4  1  3	2  5  8	 4  1
;;;	      3	 2  5  8  4  1	3  2  5	 8  4  1  3  2	5  8  4	 1  3
;;;	      2	 5  8  4  1  3	2  5  8	 4  1  3  2  5	8  4  1	 3  2
;;;	      5	 8  4  1  3  2	5
;;;	     )
;;;	  )
;;;  )
;;;  (setq i 0)
;;;  (repeat 64
;;;    (setq k i)
;;;    (setq j 0)
;;;    (repeat 6
;;;      (setq j (lsh j 1))
;;;      (setq j (logior j (logand k 1)))
;;;      (setq k (lsh k -1))
;;;    )
;;;    (if	(> j i)
;;;      (vector-swap v i j)
;;;    )
;;;    (setq i (1+ i))
;;;  )
;;;)