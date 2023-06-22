;|*************************************************************;
软件作者: Highflybird                                          ;
软件用途: 为AutoCAD 的LISP定制的一些算法和函数(线性代数部分)   ;
日期地点: 2012.12.12 深圳                                      ;
修改时间: 2019.04.18 深圳                                      ;
程序语言: AutoLISP,Visual LISP                                 ;
版本号:   Ver. 1.0.19.0418                                     ;
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

(defun MatLibSymbols ()
       '(MAT:v+v           MAT:v-v           MAT:v*v
         MAT:v/v           MAT:vxs           MAT:v/s
	 MAT:Dot           MAT:vxv           MAT:SxVs
	 MAT:mxv           MAT:mxp           MAT:mxs
	 MAT:m+m           MAT:m-m           MAT:mxm
	 MAT:norm          MAT:Norm3D        MAT:Unitization
	 MAT:unit          MAT:Det2          MAT:Det3
	 MAT:Det2V         MAT:Rot90         MAT:Rot2D
	 MAT:TransU2W      MAT:TransW2U      MAT:trp
	 MAT:Translation   MAT:TranslateBy2P MAT:Scaling
	 MAT:Rotation      MAT:Rotation3D    MAT:RotateBy2P
	 MAT:Reflect       MAT:Reflect3D     MAT:TransNested
	 MAT:RefGeom       MAT:RevRefGeom    MAT:AttGeom
	 Mat:DispToMatrix  MAT:Trans         MAT:u2w
	 MAT:w2u           MAT:Align         MAT:2VMatrix
	 Mat:3PMatrix      Mat:EntityMatrix  MAT:ISO
	 Mat:OcsMatrix     MAT:TransList     Mat:3VLE
	 Mat:Detm          Mat:3dPolarArray  Mat:Normal_Origin
         MAT:General       MAt:GetMat        VEC:Angle
	 Mat:Verify        Mat:o2w           Mat:w2o  
	 Mat:AffineTrans   Mat:AffineBy2P    Mat:TriangularForm   
	 Mat:vxm           MAT:det3P         Mat:Inverse_Quadratic                                                      
	 Mat:LUDcmp        Mat:Gauss_Equations
	 Mat:RandomMatrix  Mat:Gauss_Elimination
	 LM:Rotate3D       LM:RotateByMatrix LM:ReflectByMatrix
	 LM:Reflect3D      LM:TranslateByMatrix
         LM:ScaleByMatrix  LM:ApplyMatrixTransformation
        )
)

;;;=============================================================
;;;符号保护                                                     
;;;=============================================================
(defun protect-assign (syms)
  (eval
    (list
      'pragma
      (list 'quote (list (cons 'protect-assign syms)))
    )
  )
)

;;;=============================================================
;;;符号解除保护                                                 
;;;=============================================================
(defun unprotect-assign	(syms)
  (eval
    (list
      'pragma
      (list 'quote (list (cons 'unprotect-assign syms)))
    )
  )
)

(unProtect-assign (MatLibSymbols))

;;;*************************************************************
;;; 矩阵部分                                                    
;;;*************************************************************

;;;=============================================================
;;; 两向量相加 addition                                         
;;; Input: v1,v2 -vectors in R^n                                
;;; OutPut: A vector                                            
;;;=============================================================
(defun MAT:v+v (v1 v2)
  (mapcar '+ v1 v2)
)

;;;=============================================================
;;; 两向量相减  subtraction                                     
;;; Input: v1,v2 -vectors in R^n                                
;;; OutPut: A vector                                            
;;;=============================================================
(defun MAT:v-v (v1 v2)
  (mapcar '- v1 v2)
)

;;;=============================================================
;;; 两向量相乘  multiplication                                  
;;; Input: v1,v2 -vectors in R^n                                
;;; OutPut: A vector                                            
;;;=============================================================
(defun MAT:v*v (v1 v2)
  (mapcar '* v1 v2)
)

;;;=============================================================
;;; 两向量相除  division                                        
;;; Input: v1,v2 -vectors in R^n                                
;;; OutPut: A vector                                            
;;;=============================================================
(defun MAT:v/v (v1 v2)
  (mapcar '/ v1 v2)
)

;;;=============================================================
;;; 向量乘标量(系数)                                            
;;; Vector x Scalar - Lee Mac                                   
;;; Args: v - vector in R^n, s - real scalar                    
;;;=============================================================
(defun MAT:vxs ( v s )
  (mapcar (function (lambda ( n ) (* n s))) v)
)

;;;=============================================================
;;; 向量除以标量(系数)                                          
;;; Vector x Scalar - Lee Mac                                   
;;; Args: v - vector in R^n, s - real scalar                    
;;;=============================================================
(defun MAT:v/s ( v s )
  (if (not (zerop s))
    (mat:vxs v (/ 1.0 s))
  )
)

;;;=============================================================
;;; 两向量的点积                                                
;;; Vector Dot Product                                          
;;; Input: v1,v2 -vectors in R^n                                
;;;=============================================================
(defun MAT:Dot (v1 v2)
  (apply '+ (mapcar '* v1 v2))
)

;;;=============================================================
;;; 两向量的叉积                                                
;;; Vector Cross Product                                        
;;; Args: u,v - vectors in R^3                                  
;;;=============================================================
(defun MAT:vxv ( u v )
  (list
    (- (* (cadr u) (caddr v)) (* (cadr v) (caddr u)))
    (- (* (car  v) (caddr u)) (* (car  u) (caddr v)))
    (- (* (car  u) (cadr  v)) (* (car  v) (cadr  u)))
  )
)

;;;=============================================================
;;; 两向量的夹角度                                              
;;; The angle of two Vectors                                    
;;; Args: u,v - vectors in R^3                                  
;;;=============================================================
(defun VEC:Angle ( u v / w)
  (setq w (trans V 0 U))
  (atan (distance '(0 0) (list (car w) (cadr w))) (caddr w))
)

;;;=============================================================
;;; 线性组合  标量组乘向量组                                    
;;; Linear combination - highflybird                            
;;; Input: Vectors - vectors, Scalars, - a real number list     
;;; Output: a vector                                            
;;;=============================================================
(defun MAT:SxVs (Vectors Scalars)
  (apply 'mapcar (cons '+ (mapcar 'MAT:vxs Vectors Scalars)))
)

;;;=============================================================
;;; 向量的模(长度)                                              
;;; Vector Norm - Lee Mac                                       
;;; Args: v - vector in R^n                                     
;;;=============================================================
(defun MAT:norm ( v )
  (sqrt (apply '+ (mapcar '* v v)))
)

;;;=============================================================
;;; 向量的模(长度)                                              
;;; Vector Norm - highflybird                                   
;;; Args: v - vector in R^3                                     
;;;=============================================================
(defun MAT:Norm3D ( v )
  (distance '(0 0 0) v)
)

;;;=============================================================
;;; 单位向量                                                    
;;; Unit Vector - Lee Mac                                       
;;; Args: v - vector in R^n                                     
;;;=============================================================
(defun MAT:Unitization (v)
  ( (lambda (n)
      (if (equal 0.0 n 1e-14)
        nil
        (MAT:vxs v (/ 1.0 n))
      )
    )
    (MAT:norm v)
  )
)

;;;=============================================================
;;; 单位向量                                                    
;;; Unit Vector - highflybird                                   
;;; Args: v - vector in R^3                                     
;;;=============================================================
(defun MAT:unit ( v / l)
  (cond
    ( (equal (setq l (distance '(0 0 0) v)) 1.0 1e-8) v)
    ( (> l 1e-14) (mapcar '/ v (list l l l)))
  )
)                   

;;;=============================================================
;;; 两个2d向量的叉积的数值                                      
;;; 输入: 两个点（或者两个向量）                                
;;; 输出: 一个数值.如果为正则是逆时针,两向量形成的平面法线向量  
;;;       向上，为负则是顺时针，为零则两向量共线或平行。        
;;;=============================================================
(defun MAT:Det2V (v1 v2)
  (- (* (car v1) (cadr v2)) (* (car v2) (cadr v1)))
)

;;;=============================================================
;;; 2d行列式 determinant in R^2                                 
;;; Args: 4 numbers                                             
;;;=============================================================
(defun MAT:Det2 (x1 y1 x2 y2)
  (- (* x1 y2) (* x2 y1))
)

;;;=============================================================
;;; 功能: 定义三点的行列式，即三点之倍面积                      
;;; 参数: 三点P1, P2, P3                                        
;;; 返回: 此三点的行列式值。                                    
;;;=============================================================
(defun MAT:det3P (p1 p2 p3 /)
  (- (* (- (car p2) (car p3)) (- (cadr p2) (cadr p1)))
     (* (- (car p2) (car p1)) (- (cadr p2) (cadr p3)))
  )
)

;;;=============================================================
;;; 3d行列式  determinant in R^3                                
;;; Args: 9 numbers                                             
;;;=============================================================
(defun MAT:Det3 (a1 b1 c1 a2 b2 c2 a3 b3 c3)
  (+ (* a1 (- (* b2 c3) (* b3 c2)))
     (* a2 (- (* b3 c1) (* b1 c3)))
     (* a3 (- (* b1 c2) (* b2 c1)))
  )
)

;;;=============================================================
;;; n阶行列式  determinant in R^n                               
;;; Args: n*n 矩阵                                              
;;; Matrix Determinant  -  ElpanovEvgeniy                       
;;; Last edit 2013.11.13                                        
;;; Args: m - nxn matrix                                        
;;; (mat:detm '((0 1) (1 0)))                                   
;;;=============================================================
(defun mat:detm (m / r i)
  (cond
    ((null m) 1.0)
    ((and
       (zerop (setq i (caar m)))
       (setq r (car (vl-member-if-not
                      (function (lambda (a) (zerop (car a))))
                      (cdr m)
                    )
               )
       )
     )
     (mat:detm (cons (mapcar '+ (car m) r) (cdr m)))
    )
    ((zerop i) 0)
    ((setq i (float i))
     (setq r (cdar m))
     (* i
        (mat:detm
          (mapcar
            (function
              (lambda (a / d)
                (setq d (/ (car a) i))
                (mapcar
                  (function (lambda (b c) (- b (* c d))))
                  (cdr a)
                  r
                )
              )
            )
            (cdr m)
          )
        )
      )
    )
  )
)


;;;=============================================================
;;;三元一次方程组的解                                           
;;;The solution of a Three-variable linear equations            
;;;=============================================================
(defun Mat:3VLE (a b c d e f g h i j k l / d1)
  (setq d1 (float (Mat:DET3 a b c d e f g h i)))
  (if (/= d1 0.0)
    (list
      (/ (Mat:DET3 j b c k e f l h i) d1)
      (/ (Mat:DET3 a j c d k f g l i) d1)
      (/ (Mat:DET3 a b j d e k g h l) d1)
    )
  )
)

;;;=============================================================
;;; 旋转一个向量或者点90度                                      
;;; 输入: 一个向量                                              
;;; 输出: 被旋转90度后的向量                                    
;;;=============================================================
(defun MAT:Rot90 (vec)
  (vl-list* (- (cadr vec)) (car vec) (cddr vec))
)

;;;=============================================================
;;; 旋转向量到指定角度                                          
;;; 输入: 一个向量和指定的角度                                  
;;; 输出: 被旋转后的向量                                        
;;;=============================================================
(defun MAT:Rot2D (v a / c s x y)
  (setq c (cos a) s (sin a))
  (setq x (car v) y (cadr v))
  (list (- (* x c) (* y s)) (+ (* x s) (* y c)))
)

;;;=============================================================
;;; 矩阵转置                                                    
;;; MAT:trp Transpose a matrix -Doug Wilson-                    
;;; 输入：矩阵                                                  
;;; 输出：转置后的矩阵                                          
;;;=============================================================
(defun MAT:trp (m)
  (apply 'mapcar (cons 'list m))
)

;;;=============================================================
;;; 矩阵相加                                                    
;;; Matrix + Matrix - Lee Mac                                   
;;; Args: m,n - nxn matrices                                    
;;;=============================================================
(defun MAT:m+m ( m n )
  (mapcar '(lambda ( r s ) (mapcar '+ r s)) m n)
)

;;;=============================================================
;;; 矩阵相减                                                    
;;; Matrix - Matrix - Lee Mac                                   
;;; Args: m,n - nxn matrices                                    
;;;=============================================================
(defun MAT:m-m ( m n )
  (mapcar '(lambda ( r s ) (mapcar '- r s)) m n)
)

;;;=============================================================
;;; 矩阵相乘                                                    
;;; MAT:mxm Multiply two matrices -Vladimir Nesterovsky-        
;;;=============================================================
(defun MAT:mxm (m q)
  (mapcar (function (lambda (r) (MAT:mxv (MAT:trp q) r))) m)
)

;;;=============================================================
;;; 向量或点的矩阵变换(向量乘矩阵)                              
;;; Matrix x Vector - Vladimir Nesterovsky                      
;;; Args: m - nxn matrix, v - vector in R^n                     
;;;=============================================================
(defun MAT:mxv (m v)
  (mapcar (function (lambda (r) (apply '+ (mapcar '* r v)))) m)
)

;;;=============================================================
;;; 向量或点的矩阵变换(向量乘矩阵)                              
;;; Matrix x Vector - Vladimir Nesterovsky                      
;;; Args: m - nxn matrix, v - vector in R^n                     
;;;=============================================================
(defun MAT:vxm (v m)
  (mat:mxv (mat:trp m) v)
)

;;;=============================================================
;;; 点的矩阵(4x4 matrix) 变换                                   
;;; 输入：矩阵m和一个三维点p                                    
;;; 输出：点变换后的位置                                        
;;;=============================================================
(defun MAT:mxp (m p)
  (reverse (cdr (reverse (MAT:mxv m (append p '(1.0))))))
)

;;;=============================================================
;;; 矩阵乘标量                                                  
;;; Matrix x Scalar - Lee Mac                                   
;;; Args: m - nxn matrix, n - real scalar                       
;;;=============================================================
(defun MAT:mxs ( m s )
  (mapcar (function (lambda ( v )(MAT:VxS v s))) m)
)

;;;=============================================================
;;; 判断一个数在误差范围内是否为0                               
;;;=============================================================
(defun Mat:Zerop (x)
  (equal x 0 1e-14)
)

;;;=============================================================
;;; 判断某一行是否要加入高斯消元法的递归表中                    
;;;=============================================================
(defun Mat:Join_Gauss (n l / y)
  (setq n (cdr n))
  (setq y (cdr (reverse n)))
  (if (or (null y) (vl-every 'Mat:Zerop y))			;系数如果全为0，则不做改变
    l
    (cons n l)						        ;系数如果不全为0
  )
)

;;;=============================================================
;;; 高斯消元法之矩阵分解                                        
;;; 根据网上程序做了修改                                        
;;;=============================================================
(defun Mat:Gauss (m / A B L R x y)
  (if (car m)
    (progn
      (setq r (car m))
      (setq x (abs (car r)))
      (foreach n (cdr m)
        (if (> (setq y (abs (car n))) x)
          (setq r n x y)					;获取其首元素绝对值取最大值所在项
        )
      )				
      (setq a (float (car r)))					;需转化为浮点数，防止整除
      (if (equal a 0 1e-14)
	(Mat:Gauss (mapcar 'cdr m))                 		;去掉全部为零的列
	(progn 
	  (setq r (mat:vxs r (/ 1.0 a)))			;归一化
	  (foreach n m
	    (if (equal (setq b (car n)) 0 1e-14)                ;是否首元素为0
	      (setq l (Mat:Join_Gauss n l))
	      (setq n (mapcar '- n (mat:vxs r b))               ;消元法
	            l (Mat:Join_Gauss n l)        		;不相同则加入进去
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
;;;高斯消元法之矩阵分解                                         
;;;Mat:Gauss_Elimination, use Safearray to express a matrix     
;;;=============================================================
(defun Mat:Gauss_Elimination (m / r v i a)
  (if (car m)
    (progn 
      (setq v (mapcar (function (lambda (x) (abs (car x)))) m)) ;首列的绝对值
      (setq i (vl-position (apply 'max v) v))                   ;其最大值所在的行号
      (setq r (nth i m))                                        ;得到绝对值的最大值所在的行
      (setq a (float (car r)))                                  ;需转化为浮点数，防止整除
      (if (equal a 0 1e-14)
        (Mat:Gauss_Elimination (mapcar 'cdr m))                 ;去掉全部为零的列
        (progn
          (setq m (vl-remove r m))                              ;去掉相同的行
          (setq r (mat:vxs r (/ 1.0 a)))                        ;归一化
          (setq m
            (mapcar
              (function
                (lambda (z)
                  (mapcar
                    (function
                      (lambda (x y)
                        (- x (* (car z) y))                     ;消元
                      )
                    )
                    z
                    r
                  )
                )
              )
              m                         
            )
          )
          (cons r (Mat:Gauss_Elimination (mapcar 'cdr m)))
        )
      )
    )
  )
)

;;;=============================================================
;;;LU三角形分解，没考虑除零情况                                 
;;;=============================================================
(defun Mat:LUDcmp (mat / A I J K M N SUM)
  (setq m (length mat))
  (setq n (length (car mat)))
  (setq a (vlax-make-safearray 5 (cons 0 (1- m)) (cons 0 (1- n))))
  (vlax-safearray-fill a mat)
  
  (setq j 0)
  (repeat n
    (setq i 0)
    (repeat m
      (if (<= i j)
        (progn
          (setq sum 0)
          (setq k 0)
          (repeat i
            (setq sum (+ sum (* (vlax-safearray-get-element a i k) (vlax-safearray-get-element a k j))))
            (setq k (1+ k))
          )
          (vlax-safearray-put-element a i j (- (vlax-safearray-get-element a i j) sum))
        )
        (progn
          (setq sum 0)
          (setq k 0)
          (repeat j
            (setq sum (+ sum (* (vlax-safearray-get-element a i k) (vlax-safearray-get-element a k j))))
            (setq k (1+ k))
          )
          (vlax-safearray-put-element a i j
            (/ (- (vlax-safearray-get-element a i j) sum) (float (vlax-safearray-get-element a j j)))
          )
        )
      )
      (setq i (1+ i))
    )
    (setq j (1+ j))
  )
  (vlax-safearray->list a)
)

;;;=============================================================
;;;从高斯消元法得到的三角形矩阵回代解方程                       
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
;;;解一次方程组                                                 
;;;=============================================================
(defun Mat:Gauss_Equations (mat)
  (Mat:TriangularForm (Mat:Gauss mat))
)

;;;=============================================================
;;;解一次方程组                                                 
;;;=============================================================
(defun Mat:GaussElimination_Equations (mat)
  (Mat:TriangularForm (Mat:Gauss_Elimination mat))
)

;;;=============================================================
;;;验证解                                                       
;;;=============================================================
(defun Mat:Verify (mat ans fuzz)
  (vl-every
    (function (lambda (x) (equal (mat:dot x ans) (last x) fuzz)))
    mat 
  )
)

;;;=============================================================
;;;验证解1                                                      
;;;=============================================================
(defun Mat:Verify1 (mat ans fuzz)
  (equal (mat:mxv mat ans) (mapcar 'last mat) fuzz)
)

;;;=============================================================
;;;随机函数                                                     
;;;=============================================================
(defun Misc:Rand (nMin nMax / seed)
  (setq seed (getvar "USERR4"))
  (if (= seed 0.)
    (setq seed (getvar "TDUSRTIMER")
          seed (- seed (fix seed))
          seed (rem (* seed 86400) 1)
    )
  )
  (setq seed (rem (+ (* seed 15625.7) 0.21137152) 1))
  (setvar "USERR4" seed)
  (+ nMin (* seed (- nMax nMin)))
)

;;;=============================================================
;;;随机矩阵                                                     
;;;=============================================================
(defun Mat:RandomMatrix (m n rMin rMax / x y)
  (repeat m 
    (setq y nil)
    (repeat n
      (setq y (cons (Misc:Rand rMin rMax) y))
    )
    (setq x (cons y x))
  )
)

;;;=============================================================
;;; 解方程之测试部分                                            
;;;=============================================================
(defun c:GaussTest (/ n x m r)
  (initget 7)
  (setq n (getint "\n请未知数个数:"))
  (initget 1)
  (setq x (getreal "\n请输入范围:"))
  (setq x (abs x))
  (setq m (Mat:RandomMatrix n (1+ n) (- x) x))
  (princ "\n方程是:")
  (princ m)
  (if (setq r (Mat:Gauss_Equations m))
    (progn
      (princ "\n解是:")
      (princ r)
      (if (Mat:Verify1 m r 1e-8)
        (princ "\n对的!")
        (princ "\n错了!")
      )
    )
    (princ "\n无解或者多解!")
  )
  (princ)
)

;;;*************************************************************
;;;矩阵之变换部分                                               
;;;*************************************************************

;;;=============================================================
;;; 功能: 二阶平移变换矩阵                                      
;;; 输入: 二维平移矢量                                          
;;; 输出: 返回二阶平移变换矩阵                                  
;;;=============================================================
(defun MAT:Translation2D ( v )
  (list
    (list 1. 0. (car v))
    (list 0. 1. (cadr v))
    (list 0. 0. 1.)
  )
)

;;;=============================================================
;;; 二维比例缩放矩阵                                            
;;; 参数:                                                       
;;; base   - 基点                                               
;;; scale - 缩放比例                                            
;;;=============================================================
;;;-----------------=={ Scale by Matrix }==-------------------;;
;;;                                                             
;;; Scaling Matrix                                              
;;;=============================================================
;;; Author: highflybird, Copyright ? 2012                       
;;;=============================================================
;;; Arguments:                                                  
;;; base   - Base Point for Scaling Transformation              
;;; scale  - Scale Factor by which to scale object              
;;;=============================================================
(defun MAT:Scaling2D ( base scale / s)
  (setq s (- 1 scale)) 
  (list
    (list scale 0. (* s (car base)))
    (list 0. scale (* s (cadr base)))
    '(0. 0. 1.)
  )
)

;;;=============================================================
;;; 二维旋转变换矩阵                                            
;;; 参数:                                                       
;;; Cen - 基点                                                  
;;; ang - 旋转角度                                              
;;;=============================================================
;;;-----------------=={ Rotate by Matrix }==------------------;;
;;;                                                             
;;; Rotation Matrix                                             
;;;=============================================================
;;; Author: highflybird, Copyright ? 2012                       
;;;=============================================================
;;; Arguments:                                                  
;;; Cen    - Base Point for Rotation Transformation             
;;; ang    - Angle through which to rotate object               
;;;=============================================================
(defun MAT:Rotation2D ( Cen ang / c s x y)
  (setq c (cos ang) s (sin ang))
  (setq x (car Cen) y (cadr Cen))
  (list
    (list c (- s) (- x (- (* c x) (* s y))))
    (list s    c  (- y (+ (* s x) (* c y))))
    '(0. 0. 1.)
  )
)

;;;=============================================================
;;; 功能: 简易的相对坐标到世界坐标转换，比trans应该快一些       
;;; 输入: 要变换的点p，坐标原点p0，角度an                       
;;; 输出: 变换后的点坐标。                                      
;;;=============================================================
(defun Mat:o2w (p p0 an / cx sx)
  (setq cx (cos an))
  (setq sx (sin an))
  (list 
    (+ (car p0) (- (* (car p) cx) (* (cadr p) sx)))
    (+ (cadr p0) (+ (* (car p) sx) (* (cadr p) cx)))
  )
)

;;;=============================================================
;;; 功能: 简易的世界坐标到相对坐标转换，比trans应该快一些       
;;; 输入: 要变换的点p，坐标原点p0，角度an                       
;;; 输出: 变换后的点坐标。                                      
;;;=============================================================
(defun Mat:w2o (p p0 an / J L)
  (setq J (- (angle p0 p) an))
  (setq L (distance p0 p))
  (list (* L (cos J)) (* L (sin J)))
)

;;;=============================================================
;;; 功能: 根据三点求仿射变换及其逆                              
;;; 输入: 三对变换的原点和对应的变换后的点，其中第一对为平移变换
;;; 输出: 返回此仿射变换矩阵及其逆矩阵(为一个2X3矩阵)           
;;;=============================================================
(defun Mat:AffineTrans (p0 q0 p1 q1 p2 q2 / M1 M2 V1 V2)
  (setq p1 (mapcar '- p1 p0))
  (setq p2 (mapcar '- p2 p0))
  (setq q1 (mapcar '- q1 q0))
  (setq q2 (mapcar '- q2 q0))
  (setq m1 (Mat:AffineBy2P p1 q1 p2 q2))
  (setq m2 (cadr m1))
  (setq m1 (car m1))
  (setq v1 (mapcar '+ q0 (mat:mxv m1 (mapcar '- p0))))
  (setq v2 (mapcar '+ p0 (mat:mxv m2 (mapcar '- q0))))
  (list
    (mapcar 'append m1 (mapcar 'list v1))
    (mapcar 'append m2 (mapcar 'list v2))
  )
)

;;;=============================================================
;;; 功能: 根据两点求仿射变换及其逆                              
;;; 输入: 两对变换的原点和对应的变换后的点                      
;;; 输出: 返回此仿射变换矩阵及其逆矩阵                          
;;;=============================================================
(defun Mat:AffineBy2P (p1 q1 p2 q2 / A B C D DET PX1 PX2 PY1 PY2 QX1 QX2 QY1 QY2)
  (mapcar 'set '(px1 px2 qx1 qx2) (mapcar 'car (list p1 p2 q1 q2)))
  (mapcar 'set '(py1 py2 qy1 qy2) (mapcar 'cadr (list p1 p2 q1 q2)))
  (setq det (MAT:Det2 px1 py1 px2 py2))
  (setq a (/ (mat:det2 qx1 py1 qx2 py2) det))
  (setq b (/ (mat:det2 px1 qx1 px2 qx2) det))
  (setq c (/ (mat:det2 qy1 py1 qy2 py2) det))
  (setq d (/ (mat:det2 px1 qy1 px2 qy2) det))
  (list
    (list (list a b) (list c d))
    (Mat:Inverse_Quadratic a b c d)
  )
)

;;;=============================================================
;;; 另外一方法                                                  
;;;=============================================================
(defun Mat:AffineBy2P-1 (p1 q1 p2 q2 / m1 m2 PX1 PX2 PY1 PY2 QX1 QX2 QY1 QY2)
  (setq px1 (car p1))
  (setq py1 (cadr p1))
  (setq qx1 (car q1))
  (setq qy1 (cadr q1))
  (setq px2 (car p2))
  (setq py2 (cadr p2))
  (setq qx2 (car q2))
  (setq qy2 (cadr q2))
  (setq	m1 (list
	     (list px1 py1 0 0 qx1)
	     (list 0 0 px1 py1 qy1)
	     (list px2 py2 0 0 qx2)
	     (list 0 0 px2 py2 qy2)
	   )
  )
  (setq	m2 (list
	     (list qx1 qy1 0 0 px1)
	     (list 0 0 qx1 qy1 py1)
	     (list qx2 qy2 0 0 px2)
	     (list 0 0 qx2 qy2 py2)
	   )
  )
  (mapcar 'Mat:Gauss_Equations (list m1 m2))
)

;;;=============================================================
;;; 功能: 求二阶矩阵的逆                                        
;;; 输入: 二阶矩阵的元素a,b,c,d==>'((a b)(c d))                 
;;; 输出: 返回此二阶矩阵的逆，不存在则返回nil                   
;;;=============================================================
(defun Mat:Inverse_Quadratic (a b c d / e)
  (setq E (- (* a d) (* b c)))
  (setq e (float E))
  (if (/= e 0)
    (list
      (list (/ d e) (- (/ b e)))
      (list (- (/ c e)) (/ a e))
    )
  )
)

;;;=============================================================
;;; 平移变换矩阵方式1                                           
;;; 参数:                                                       
;;; v - 位移矢量                                                
;;;=============================================================
;;;---------------=={ Translate by Matrix }==-----------------;;
;;;                                                             
;;; Translation Matrix                                          
;;;=============================================================
;;; Author: highflybird, Copyright ? 2012                       
;;;=============================================================
;;; Arguments:                                                  
;;; v  - Displacement vector by which to translate              
;;;=============================================================
(defun MAT:Translation ( v )
  (list
    (list 1. 0. 0. (car v))
    (list 0. 1. 0. (cadr v))
    (list 0. 0. 1. (caddr v))
    (list 0. 0. 0. 1.)
  )
)

;;;=============================================================
;;; 平移变换矩阵方式2                                           
;;; 参数:                                                       
;;; p1 - 基点                                                   
;;; p2 - 目标点                                                 
;;;=============================================================
;;;---------------=={ Translate by Matrix }==-----------------;;
;;;                                                             
;;; Translation Matrix                                          
;;;=============================================================
;;; Author: highflybird, Copyright ? 2012                       
;;;=============================================================
;;; Arguments:                                                  
;;; p1, p2 - Points representing vector by which to translate   
;;;=============================================================
(defun MAT:TranslateBy2P ( p1 p2 )
  (MAT:Translation (mapcar '- p2 p1))
)

;;;=============================================================
;;; 比例缩放矩阵                                                
;;; 参数:                                                       
;;; Cen   - 基点                                                
;;; scale - 缩放比例                                            
;;;=============================================================
;;;-----------------=={ Scale by Matrix }==-------------------;;
;;;                                                             
;;; Scaling Matrix                                              
;;;=============================================================
;;; Author: highflybird, Copyright ? 2012                       
;;;=============================================================
;;; Arguments:                                                  
;;; Cen    - Base Point for Scaling Transformation              
;;; scale  - Scale Factor by which to scale object              
;;;=============================================================
(defun MAT:Scaling ( Cen scale / s)
  (setq s (- 1 scale)) 
  (list
    (list scale 0. 0. (* s (car Cen)))
    (list 0. scale 0. (* s (cadr Cen)))
    (list 0. 0. scale (* s (caddr Cen)))
    '(0. 0. 0. 1.)
  )
)

;;;=============================================================
;;; 二维旋转变换矩阵                                            
;;; 参数:                                                       
;;; Cen - 基点                                                  
;;; ang - 旋转角度                                              
;;;=============================================================
;;;-----------------=={ Rotate by Matrix }==------------------;;
;;;                                                             
;;; Rotation Matrix                                             
;;;=============================================================
;;; Author: highflybird, Copyright ? 2012                       
;;;=============================================================
;;; Arguments:                                                  
;;; Cen    - Base Point for Rotation Transformation             
;;; ang    - Angle through which to rotate object               
;;;=============================================================
(defun MAT:Rotation ( Cen ang / c s x y)
  (setq c (cos ang) s (sin ang))
  (setq x (car Cen) y (cadr Cen))
  (list
    (list c (- s) 0. (- x (- (* c x) (* s y))))
    (list s    c  0. (- y (+ (* s x) (* c y))))
    '(0. 0. 1. 0.)
    '(0. 0. 0. 1.)
  )
)

;;;=============================================================
;;; 三维旋转变换矩阵                                            
;;; 参数:                                                       
;;; Cen  - 基点                                                 
;;; Axis - 旋转轴                                               
;;; ang  - 旋转角                                               
;;;=============================================================
;;;---------------=={ 3D Rotate by Matrix }==-----------------;;
;;; Author: highflybird.                                        
;;; Arguments:                                                  
;;; Cen ---Input origin point of rotation                       
;;; Axis---Input axis vector of rotation                        
;;; Ang ---Input angle of rotation                              
;;;=============================================================
(defun MAT:Rotation3D (Cen Axis Ang / A B C D M N P x y z)
  (setq D (distance '(0 0 0) Axis))
  (if (or (< D 1e-8) (zerop ang))
    '((1. 0. 0. 0.) (0. 1. 0. 0.) (0. 0. 1. 0.) (0. 0. 0. 1.))
    (setq N (mapcar '/ Axis (list D D D))
          x (car N)
          y (cadr N)
          z (caddr N)
          A (cos Ang)
          B (sin Ang)
          C (- 1 A)
          M (list (list (+ A (* x x C))
                        (- (* x y C) (* z B))
                        (+ (* y B) (* x z C))
                  )
                  (list (+ (* z B) (* x y C))
                        (+ A (* y y C))
                        (- (* y z C) (* x B))
                  )
                  (list (- (* x z C) (* y B))
                        (+ (* x B) (* y z C))
                        (+ A (* z z C))
                  )
            )
          p (mapcar '- Cen (Mat:mxv M Cen))
          M (Mat:DispToMatrix M p)
    )
  )
)

;;;=============================================================
;;; 三维旋转变换矩阵（通过两点和旋转角）                        
;;; 参数:                                                       
;;; p1,p2  - 两点定义的旋转轴                                   
;;; ang    - 旋转角度                                           
;;;=============================================================
;;;---------------=={ 3D Rotate by Matrix }==-----------------;;
;;; Rotation matrix                                             
;;;=============================================================
;;; Author: highflybird, Copyright ? 2012                       
;;;=============================================================
;;; Arguments:                                                  
;;; p1,p2  - Two 3D points defining the axis of rotation        
;;; ang    - Rotation Angle                                     
;;;=============================================================
(defun MAT:RotateBy2P ( p1 p2 ang )
   (MAT:Rotation3D P1 (mapcar '- p2 p1) ang)
)

;;;=============================================================
;;; 二维镜像变换矩阵                                            
;;; 参数:                                                       
;;; p1     - 镜像向量第一点                                     
;;; p2     - 镜像向量第二点                                     
;;;=============================================================
;;;----------------=={ Reflect by Matrix }==------------------;;
;;;                                                             
;;; Reflects a VLA-Object or Point List using a                 
;;; Transformation Matrix                                       
;;;=============================================================
;;; Author: Lee Mac, Copyright ? 2010 - www.lee-mac.com         
;;;=============================================================
;;; Arguments:                                                  
;;; target - VLA-Object or Point List to transform              
;;; p1, p2 - Points representing vector in which to reflect     
;;;=============================================================
(defun MAT:Reflect ( p1 p2 / a c s x y)
  (setq a (angle p1 p2) a (+ a a))
  (setq c (cos a) s (sin a))
  (setq x (car p1) y (cadr p1))
  (list
    (list c    s  0. (- x (+ (* c x) (* s y))))
    (list s (- c) 0. (- y (- (* s x) (* c y))))
    '(0. 0. 1. 0.)
    '(0. 0. 0. 1.)
  )
)

;;;=============================================================
;;; 三维镜像变换矩阵                                            
;;; 参数:                                                       
;;; p1,p2,p3 - 三点定义的镜像平面                               
;;;=============================================================
;;;---------------=={ 3D Reflect by Matrix }==----------------;;
;;;                                                             
;;; Reflection matrix                                           
;;;=============================================================
;;; Author: highflybird, Copyright ? 2012-                      
;;;=============================================================
;;; Arguments:                                                  
;;; p1,p2,p3 - Three 3D points defining the reflection plane    
;;;=============================================================
(defun MAT:Reflect3D (p1 p2 p3 / m ux uy uz)
  (mapcar
    'set
    '(ux uy uz)
    (MAT:unit (MAT:vxv (mapcar '- p2 p1) (mapcar '- p3 p1)))
  )
  (setq m (list (list (- 1. (* 2. ux ux)) (* -2. uy ux) (* -2. ux uz))
                (list (* -2. ux uy) (- 1. (* 2. uy uy)) (* -2. uy uz))
                (list (* -2. ux uz) (* -2. uy uz) (- 1. (* 2. uz uz)))
          )
  )
  (Mat:DispToMatrix m (mapcar '- p1 (MAT:mxv m p1)))
)


;;;=============================================================
;;; 块参照的变换矩阵和逆矩阵                                    
;;;=============================================================

;;;=============================================================
;;; 功能： 某点在块内坐标系统和世界或者用户坐标系统的转换       
;;; 参数： pt 要变换的点。                                      
;;;        rlst 用 nentselp或者nentsel得到的表的最后一项        
;;;        from  坐标系：0,WCS; 1,当前UCS; 2,块参照坐标系RCS    
;;;        to    坐标系：0,WCS; 1,当前UCS; 2,块参照坐标系RCS    
;;;=============================================================
;;; MAT:TransNested (gile)                                      
;;; Translates a point coordinates from WCS or UCS to RCS       
;;; -coordinates system of a                                    
;;; reference (xref or block) whatever its nested level-        
;;;                                                             
;;; Arguments                                                   
;;; pt : the point to translate                                 
;;; rlst : the parents entities list from the deepest nested    
;;;        to the one inserted in current space -same as        
;;;        (last (nentsel)) or (last (nentselp))                
;;; from to : as with trans function: 0.WCS, 1.UCS, 2.RCS       
;;;=============================================================

(defun MAT:TransNested (pt rlst from to / GEOM)
  (and (= 1 from) (setq pt (trans pt 1 0)))
  (and (= 2 to) (setq rlst (reverse rlst)))
  (and (or (= 2 from) (= 2 to))
       (while rlst
        (setq geom (if  (= 2 to)
                      (MAT:RevRefGeom (car rlst))
                      (MAT:RefGeom (car rlst))
                    )
               rlst (cdr rlst)
               pt   (mapcar '+ (MAT:mxv (car geom) pt) (cadr geom))
        )
       )
  )
  (if (= 1 to)
    (trans pt 0 1)
    pt
  )
)

;;;=============================================================
;;; 功能：图块的变换矩阵                                        
;;; 输入：块参照的图元名                                        
;;; 输出：块参照的变换矩阵                                      
;;;=============================================================
;;; MAT:RefGeom (gile)                                          
;;; Returns a list which first item is a 3x3 transformation     
;;; matrix(rotation,scales normal) and second item the object   
;;; insertion point in its parent(xref, bloc or space)          
;;;                                                             
;;; Argument : an ename                                         
;;;=============================================================

(defun MAT:RefGeom (ename / elst ang norm mat)
  (setq elst (entget ename)
        ang  (cdr (assoc 50 elst))
        norm (cdr (assoc 210 elst))
  )
  (list
    (setq mat
           (MAT:mxm
             (mapcar (function (lambda (v) (trans v 0 norm T)))
                     '((1.0 0.0 0.0) (0.0 1.0 0.0) (0.0 0.0 1.0))
             )
             (MAT:mxm
               (list (list (cos ang) (- (sin ang)) 0.0)
                     (list (sin ang) (cos ang) 0.0)
                     '(0.0 0.0 1.0)
               )
               (list (list (cdr (assoc 41 elst)) 0.0 0.0)
                     (list 0.0 (cdr (assoc 42 elst)) 0.0)
                     (list 0.0 0.0 (cdr (assoc 43 elst)))
               )
             )
           )
    )
    (mapcar
      '-
      (trans (cdr (assoc 10 elst)) norm 0)
      (MAT:mxv mat
           (cdr (assoc 10 (tblsearch "BLOCK" (cdr (assoc 2 elst)))))
      )
    )
  )
)

;;;=============================================================
;;; 功能：图块的变换矩阵的逆矩阵                                
;;;=============================================================
;;; MAT:RevRefGeom (gile)                                       
;;; MAT:RefGeom inverse function                                
;;; 输入：块参照的图元名                                        
;;; 输出：块参照的变换矩阵的逆矩阵                              
;;;=============================================================
(defun MAT:RevRefGeom (ename / entData ang norm mat)
  (setq entData (entget ename)
        ang     (- (cdr (assoc 50 entData)))
        norm    (cdr (assoc 210 entData))
  )
  (list
    (setq mat
           (MAT:mxm
             (list (list (/ 1 (cdr (assoc 41 entData))) 0.0 0.0)
                   (list 0.0 (/ 1 (cdr (assoc 42 entData))) 0.0)
                   (list 0.0 0.0 (/ 1 (cdr (assoc 43 entData))))
             )
             (MAT:mxm
               (list (list (cos ang) (- (sin ang)) 0.0)
                     (list (sin ang) (cos ang) 0.0)
                     '(0.0 0.0 1.0)
               )
               (mapcar (function (lambda (v) (trans v norm 0 T)))
                       '((1.0 0.0 0.0) (0.0 1.0 0.0) (0.0 0.0 1.0))
               )
             )
           )
    )
    (mapcar '-
            (cdr (assoc 10 (tblsearch "BLOCK" (cdr (assoc 2 entData)))))
            (MAT:mxv mat (trans (cdr (assoc 10 entData)) norm 0))
    )
  )
)

;;;=============================================================
;;; 属性的变换矩阵Attrib Transformation Matrix. -highflybird    
;;; 输入：Ename 属性的图元名                                    
;;; 输出：属性的变换矩阵                                        
;;;=============================================================
(defun MAT:AttGeom (ename / ang norm mat elst)
  (setq elst (entget ename)
        ang  (cdr (assoc 50 elst))
        norm (cdr (assoc 210 elst))
  )
  (list
    (setq mat
           (mxm
             (mapcar (function (lambda (v) (trans v 0 norm T)))
                     '((1.0 0.0 0.0) (0.0 1.0 0.0) (0.0 0.0 1.0))
             )
             (list (list (cos ang) (- (sin ang)) 0.0)
                   (list (sin ang) (cos ang) 0.0)
                   '(0.0 0.0 1.0)
             )
           )
    )
    (trans (cdr (assoc 10 elst)) norm 0)
  )
)

;;;=============================================================
;;; Append displacement vector to a matrix      -Highflybird-   
;;; 把位移矢量添加到矩阵中                                      
;;; 输入：mat -- 矩阵(3x3)，disp -- 位移矢量                    
;;; 输出：一个4X4的变换CAD的标准变换矩阵                        
;;;=============================================================
(defun Mat:DispToMatrix (mat disp)
  (append
    (mapcar 'append mat (mapcar 'list disp))
    '((0. 0. 0. 1.))
  )
)

;;;=============================================================
;;; 从一个坐标系统到另一个坐标系统的变换矩阵                    
;;; 输入：from - 源坐标系；to - 目的坐标系                      
;;; 输出：一个4X4的CAD变换矩阵                                  
;;;=============================================================
(defun MAT:Trans (from to)
  (append
    (MAT:trp 
      (list
        (trans '(1 0 0) from to t)
        (trans '(0 1 0) from to t)
        (trans '(0 0 1) from to t)
        (trans '(0 0 0) from to nil)
      )
    )
    '((0. 0. 0. 1.))
  )
)

;;;=============================================================
;;; ucs到wcs矩阵，也可称UCS的变换矩阵                           
;;;=============================================================
(defun MAT:u2w () (MAT:Trans 1 0))

;;;=============================================================
;;; wcs到ucs矩阵，也可称UCS的逆变换矩阵                         
;;;=============================================================
(defun MAT:w2u () (MAT:Trans 0 1))

;;;=============================================================
;;; OCS的变换矩阵，或叫法线矢量的变换矩阵                       
;;;=============================================================
(defun Mat:OcsMatrix (zdir / xdir) 
  (or (equal 1.0 (distance '(0 0 0) zdir) 1e-8)     
    (setq zdir (Mat:Unit zdir))                                 ; 先把矢量单位化。  
  )  
  (if (and (< (abs (car zdir)) 0.015625)                        ; 如果(abs (Nx) < 1/64)           
           (< (abs (cadr zdir)) 0.015625)                       ; 且 (abs (Ny) < 1/64)
      )    
    (setq xdir (Mat:Unit (Mat:vxv '(0 1 0) zdir)))              ; Ax = Wy X N   （叉积）
    (setq xdir (Mat:Unit (Mat:vxv '(0 0 1) zdir)))              ;否则 Ax = Wz X N。
  )  
  (list xdir (Mat:Unit (Mat:vxv zdir xdir)) zdir)               ;Y方向满足右手型坐标系统
)

;;;=============================================================
;;; 通用变换矩阵 by highflybird                                 
;;; 输入：from - 原坐标系，                                     
;;;       to   - 目的坐标系，                                   
;;;       Org  - 目的坐标系的原点相对原坐标系的位置             
;;;       Ang  - 相对于原坐标系的旋转角度                       
;;; 输出：两个矩阵，一个是从原坐标系变换到目的坐标系的变换矩阵;;
;;;       一个是从目的坐标系变换到原坐标系的变换矩阵            
;;;=============================================================
(defun MAT:Align (from to Org Ang / Mat Rot Inv Cen)
  (setq Mat (mapcar (function (lambda (v) (trans v from to T)))
                    '((1. 0. 0.) (0. 1. 0.) (0. 0. 1.))
            )
  )
  (if (not (equal ang 0 1e-14))
    (setq Rot (list (list (cos ang) (- (sin ang)) 0.)
                    (list (sin ang) (cos ang) 0.)
                    (list 0. 0. 1.)
              )
          mat (MAT:mxm mat Rot)
    )
  )
  (setq Cen (trans Org to from))
  (setq Inv (mat:trp mat))
  (list
    (Mat:DispToMatrix mat Cen)                                  ;from->to
    (Mat:DispToMatrix Inv (mat:mxv mat (mapcar '- Cen)))        ;to->from
  )
)

;;;=============================================================
;;; 通用变换矩阵 by highflybird                                 
;;; 输入：from - 原坐标系，                                     
;;;       to   - 目的坐标系，                                   
;;;       Org  - 目的坐标系的原点相对原坐标系的位置             
;;;       Ang  - 相对于原坐标系的旋转角度                       
;;; 输出：两个矩阵，一个是从原坐标系变换到目的坐标系的变换矩阵;;
;;;       一个是从目的坐标系变换到原坐标系的变换矩阵            
;;;=============================================================
(defun MAT:General (Factor Rotation Normal disp / A B C mat)
  (setq A   (* (cos Rotation) Factor)
	B   (* (sin Rotation) Factor)
	C   (- B)
	mat (list
	      (list A C 0 0)
	      (list B A 0 0)
	      (list 0 0 Factor 0)
	      (list 0 0 0 1)
	    )
  )
  (mat:mxm
    (mat:translation (trans disp 0 normal   T))
    (Mat:mxm (MAT:Trans normal 0) (Mat:mxm mat (MAT:Trans 0 normal)))
  )
)

(defun MAt:GetMat (From To Factor Rotation  / A B C Mat X Y)
  (setq	X   (car From)
	Y   (cadr from)
	A   (* (cos Rotation) Factor)
	B   (* (sin Rotation) Factor)
	C   (- B)
	mat (list
	      (list A C 0 (- (car To) (* X A) (* Y C)))
	      (list B A 0 (- (cadr To) (* X B) (* Y A)))
	      (list 0 0 Factor (- (caddr To) (* (caddr From) Factor)))
	      (list 0 0 0 1)
	    )
  )
  (Mat:mxm (mat:U2w) (Mat:mxm mat (Mat:W2u)))
)

;;;=============================================================
;;; 通过两个坐标轴和坐标原点定义的变换矩阵  -by highflybird     
;;; 输入：Org  - 坐标系原点，                                   
;;;       Vx   - 坐标系X 方向，                                 
;;;       Vy   - 坐标系y 方向                                   
;;; 输出：两个矩阵，一个是该坐标系的变换矩阵，一个是其逆矩阵    
;;;=============================================================
(defun MAT:2VMatrix (Org Vx Vy / Vz Rot)
  (if (or (equal Vx '(0 0 0) 1e-14) (equal Vy '(0 0 0) 1e-14))
    '((1. 0. 0. 0.) (0. 1. 0. 0.) (0. 0. 1. 0.) (0. 0. 0. 1.))
    (progn 
      (setq Vx  (Mat:Unit Vx))                                  ;X Axis
      (setq Vy  (Mat:Unit Vy))                                  ;Y Axis
      (setq Vz  (Mat:unit (MAT:vxv Vx Vy)))                     ;Z Axis
      (setq Vy  (Mat:unit (MAT:vxv Vz Vx)))                     ;Y Axis
      (setq Rot (list Vx Vy Vz))                                ;Rotation matrix
      (list                                                     ;Inverse Rotation matrix
        (Mat:DispToMatrix (MAT:trp Rot) Org)                    ;The transformation matrix
        (Mat:DispToMatrix Rot (MAT:mxv Rot (mapcar '- Org)))    ;The Inverse matrix
      )
    )
  )
)

;;;=============================================================
;;; Mat:3PMatrix  -Highflybird-                                 
;;; 通过两个坐标轴和坐标原点定义的变换矩阵  -by highflybird     
;;; 输入：P1 - 坐标系原点，                                     
;;;       P2 - 坐标系的第2点                                    
;;;       P3 - 坐标系的第3点                                    
;;; 输出：两个矩阵，一个是该坐标系的变换矩阵，一个是其逆矩阵    
;;;=============================================================
(defun Mat:3PMatrix (p1 p2 p3 / v1 v2 v3)
  (MAT:2VMatrix P1 (mapcar '- p2 p1) (mapcar '- p3 p1))
)

;;;=============================================================
;;; 平齐实体的变换矩阵  -by highflybird                         
;;; 输入：Ent - 实体名                                          
;;; 输出：平齐这个实体的变换矩阵和它的逆矩阵                    
;;;=============================================================
(defun Mat:EntityMatrix (Ent / z dxf Cen obj an m1 mat Inv org)
  (setq dxf (entget ent))
  (if (setq Cen (cdr (assoc 10 dxf)))                           ;Insertpoint,center or startpoint,etc.
    (if (null (caddr Cen))
      (setq Cen (append Cen '(0.0)))
    )
    (setq Cen '(0 0 0))
  )
  (setq obj (vlax-ename->vla-object Ent))                       
  (if (and (vlax-property-available-p obj 'elevation)           ;If it has elevation value.
           (wcmatch (vla-get-objectname obj) "*Polyline")       ;It's a "AcDb2dPolyline" or "AcDbPolyline" object
      )
    (setq z   (vla-get-elevation obj)
          Cen (list (car Cen) (cadr Cen) (+ (caddr Cen) z))     ;add elevation value
    )
  )
  (if (vlax-property-available-p obj 'rotation)                 ;if it has a rotaion angle
    (setq an (vla-get-rotation obj))
    (setq an 0)
  )
  (MAT:Align 0 Ent Cen an)                                      ;return two matrices, the first is WCS->OCS,the second is OCS->WCS
)

;;;=============================================================
;;;通用的轴测变换矩阵     highflybird  2012.12                  
;;;Axonometric projections Rotation matrices                    
;;;Isometric projection: a = (/ pi 4),b = (atan (- (sqrt 2)))   
;;;Input: a - Rotation angle about the vertical axis            
;;;       b - Rotation angle about the horizontal axis          
;;;Output: transforamtion matrix of this projection             
;;;=============================================================
(defun MAT:ISO (a b / ca sa cb sb)
  (setq ca (cos a))
  (setq sa (sin a))
  (setq cb (cos b))
  (setq sb (sin b))
  (list (list ca        (- sa)    0      0)
        (list (* sa cb) (* ca cb) (- sb) 0)
        (list (* sa sb) (* ca sb) cb     0)
        (list 0 0 0 1)
  )
)

;;;=============================================================
;;; 点集变换                                                    
;;; 输入: 要变换的点集                                          
;;; 输出: 变换后的点集                                          
;;;=============================================================
(defun MAT:TransList (points from to Disp)
  (mapcar (function (lambda (p) (trans p from to Disp))) points)
)
          
;;;=============================================================
;;; 点变换1                                                     
;;; 输入: 要变换的点和原点及变换向量                            
;;; 输出: 点变换后的位置                                        
;;;=============================================================
(defun MAT:TransU2W (p p0 v / d x0 y0 x1 y1 dv rt)
  (setq d (distance '(0 0) v))
  (if (equal d 1e-14)
    P0
    (setq x1 (car  p)
          y1 (cadr p)
          x0 (car  v)
          y0 (cadr v)
          dv (list (/ (- (* x1 x0) (* y1 y0)) d)
                   (/ (+ (* y1 x0) (* x1 y0)) d)
             )
          rt (mapcar '+ P0 dv)
    )
  )
)

;;;=============================================================
;;; 点变换2                                                     
;;; 输入: 要变换的点和原点及变换向量                            
;;; 输出: 点变换后的位移向量                                    
;;;=============================================================
(defun MAT:TransW2U (p p0 v / d x0 y0 x1 y1 dv)
  (setq d (distance '(0 0) v))
  (if (equal d 1e-14)
    (list 0 0)
    (setq x1 (- (car p) (car p0))
          y1 (- (cadr p) (cadr p0))
          x0 (car v)
          y0 (cadr v)
          dv (list (/ (+ (* x1 x0) (* y1 y0)) d)
                   (/ (- (* y1 x0) (* x1 y0)) d)
             )
    )
  )
)

;;;=============================================================
;;; 三维环形阵列                                                
;;; 输入：Objlst    -- 物体集                                   
;;;       Number    -- 要阵列的个数（包含自身在内）             
;;;       FillAngle -- 旋转角度                                 
;;;       IsCCW     -- 是否逆时针                               
;;;       P1        -- 阵列中心点                               
;;;       P2        -- 阵列轴线的另一点                         
;;; 输出：阵列的物体列表                                        
;;;=============================================================
(defun Mat:3dPolarArray (Objlst Number FillAngle IsCCW P1 P2 / lst1 lst2 ANG MAT NEW)
  (if (and (= (type number) 'INT) (> number 1))
    (progn  
      (if IsCCW
        (setq FillAngle (float FillAngle))
        (setq FillAngle (- FillAngle pi pi))
      )
      (setq ang (/ FillAngle (1- Number)))
      (setq mat (vlax-tmatrix (MAT:RotateBy2P P1 P2 ang)))
      (repeat (1- Number)
        (setq lst1 nil)
        (foreach obj ObjLst 
          (setq new (vla-copy obj))
          (vla-transformby new mat)
          (setq lst1 (cons new lst1))
        )
        (setq objLst (reverse lst1))
        (setq lst2 (cons objLst lst2))
      )
      (reverse lst2)
    )
    (list ObjLst)
  )
)

;;;=============================================================
;;; 已知法线和原点的平面变换矩阵                                
;;; 输入：Normal    -- 法线                                     
;;;       Origin    -- 原点                                     
;;; 输出：阵列的物体列表                                        
;;;=============================================================
(defun Mat:Normal_Origin_1 (Normal Origin / mat)
  (setq mat (MAT:OCSMATRIX Normal))
  (list
    (Mat:DispToMatrix (mat:trp mat) origin)
    (mat:DispToMatrix mat (mat:mxv mat (mapcar '- Origin)))
  )   
)

(defun Mat:Normal_Origin (Normal Origin / mat rev xdir ydir zdir)
  (setq xdir (trans '(1 0 0) 0 Normal T))
  (setq ydir (trans '(0 1 0) 0 Normal T))
  (setq zdir (trans '(0 0 1) 0 Normal T))
  (setq mat  (list xdir ydir zdir))
  (setq rev  (Mat:trp mat))
  (list
    (MAT:DISPTOMATRIX mat Origin)
    (MAT:DISPTOMATRIX rev (mat:mxv rev (mapcar '- Origin)))
  )
)

;;;=============================================================
;;; 选择集的包围盒                                              
;;;=============================================================
(defun ENT:SelBox (sel / i ent obj MinPt MaxPt MinPts MaxPts objs)
  (setq i 0)
  (repeat (sslength sel)
    (setq ent (ssname sel i))
    (setq obj (vlax-ename->vla-object ent))
    (setq objs (cons obj objs))
    (vla-getboundingbox obj 'MinPt 'MaxPt)
    (setq MinPts (cons (vlax-safearray->list minPt) MinPts))
    (setq MaxPts (cons (vlax-safearray->list maxPt) MaxPts))
    (setq i (1+ i))
  )
  (list (reverse objs)
        (list (apply 'mapcar (cons 'min MinPts))
              (apply 'mapcar (cons 'max MaxPts)) 
        )
  )
)

;;;=============================================================
;;;获取物体Objects                                              
;;;=============================================================
(defun Ent:Ents->Objs (sel / i e o l)
  (setq i (sslength sel))
  (repeat i
    (setq e (ssname sel (setq i (1- i))))
    (setq o (vlax-ename->vla-object e))
    (setq l (cons o l))
  )
)

;;;=============================================================
;;; 以下为矩阵的伴随矩阵以及求逆等线性代数算法，来自gile等      
;;;=============================================================

;;;=============================================================
;;; REMOVE-I (gile)                                             
;;; Returns the list but item at specified index(first item = 0)
;;;                                                             
;;; Arguments : a list and the index of item to be remove       
;;;=============================================================
(defun Mat:remove-i (Index lst)
  (if (or (zerop Index) (null lst))
    (cdr lst)
    (cons (car lst) (Mat:remove-i (1- Index) (cdr lst)))
  )
)

;;;=============================================================
;;; COFACT (gile)                                               
;;; Returns the cofactor associated to ij item of a matrix      
;;;                                                             
;;; Arguments                                                   
;;; i = row index (first row = 1)                               
;;; j = column index (first column = 1)                         
;;; m = a matrix                                                
;;;=============================================================
(defun Mat:Cofactor (i j m)
  (*
    (Mat:determ
      (Mat:remove-i
	(1- i)
	(mapcar (function (lambda (x) (Mat:remove-i (1- j) x))) m)
      )
    )
    (expt -1 (+ i j))
  )
)

;;;=============================================================
;;; DETERM (gile)                                               
;;; Returns the déterminant of a matrix                         
;;;                                                             
;;; Argument : a matrix                                         
;;;=============================================================
(defun Mat:determ (m)
  (if (= 2 (length m))
    (- (* (caar m) (cadadr m)) (* (caadr m) (cadar m)))
    ( (lambda (r n)
        (apply
	  '+
	  (mapcar
	    (function
	      (lambda (x) (* x (Mat:Cofactor 1 (setq n (1+ n)) m)))
	    )
	    r
	  )
        )
      )
      (car m)
      0
    )
  )
)

;;;=============================================================
;;; ADJ-MAT (gile)                                              
;;; Returns the adjugate matrix                                 
;;;                                                             
;;; Argument : a matrix                                         
;;;=============================================================
(defun Mat:adjoint (m / i)
  (setq i 0)
  (if (= (length m) 2)
    (list
      (list (cadadr m) (- (caadr m)))
      (list (- (caadr m)) (cadadr m))
    )
    (Mat:trp
      (mapcar
	(function
	  (lambda (v / j)
	    (setq i (1+ i)
		  j 0
	    )
	    (mapcar
	      (function (lambda (x) (Mat:Cofactor i (setq j (1+ j)) m)))
	      v
	    )
	  )
	)
	m
      )
    )
  )
)

;;;=============================================================
;;; INV-MAT (gile)                                              
;;; Inverse a matrix                                            
;;;                                                             
;;; Argument : a matrix                                         
;;;=============================================================
(defun Mat:Inverse (m / d)
  (if (/= 0 (setq d (Mat:determ m)))
    (mapcar
      (function
	(lambda	(v)
	  (mapcar (function (lambda (x) (* (/ 1.0 d) x))) v)
	)
      )
      (Mat:adjoint m)
    )
  )
)
    
;;;*************************************************************
;;; 矩阵测试部分                                                
;;;*************************************************************
;|
;;;测试两向量的夹角
(defun c:ttt()
  (setq p1 (getpoint "\n1:"))
  (setq p2 (getpoint "\n2:"))
  (setq p3 (getpoint "\n2:"))
  (setq u  (mapcar '- p2 p1))
  (setq v (mapcar '- p3 p1))
  (setq s (vec:angle u v))
  (ent:make_line p1 p2)
  (ent:make_line p1 p3)
  (princ s)
  (princ)
)
Test for Mat:Normal_Origin
;;;法线原点矩阵测试
(defun c:tt ()
  (setq e (car (entsel)))
  (setq o (vlax-ename->vla-object e))
  (setq Normal (trans '(0 0 1) 1 0 T))
  (setq origin (getvar 'ucsorg))
  (command "undo" "be")
  (setq mat (mat:normal_origin Normal origin))
  (vla-transformby o (vlax-tmatrix (cadr mat)))
  (command "undo" "e")
  (Princ)
)

;;;3d环形阵列测试
(defun C:PolarArrayTest (/ ss N P1 P2 ActDoc ObjLst)                     
  (setq ss (ssget))
  (initget 7)
  (setq N (getInt "\n数量:"))
  (initget 9)
  (setq P1 (getpoint "\n中心点:"))
  (initget 9)
  (setq p2 (getpoint P1 "\n另一点:"))
  (if ss
    (progn
      (setq ActDoc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (vla-StartUndoMark ActDoc)
      (setq Ojblst (Ent:Ents->Objs ss))
      (Mat:3dPolarArray ojblst N (/ pi 2) nil (trans p1 1 0) (trans p2 1 0))
      (vla-EndUndoMark ActDoc)
      (vlax-release-object ActDoc)
      (princ)
    )
  )
)

;;;Align a 3dSolid to the WCS view
;;;轴测矩阵的测试
(defun C:Test (/ sel ent obj i an new MAT)
  (setq sel (ssget "_+.:E:S:L" '((0 . "3DSOLID"))))
  (if sel
    (progn
      (setq ent (ssname sel 0))
      (setq obj (vlax-ename->vla-object ent))
      (setq i 0)
      (setq an (atan (- (sqrt 2))))
      (foreach f '(0.25 0.75 1.25 1.75)                         ;Southwest,Northwest,Northeast,Southeast Isometric projection
        (setq mat (MAT:ISO (* f pi) an))
        (setq new (vla-copy obj))
        (vla-put-color new (setq i (1+ i)))                     
        (vla-transformby new (vlax-tmatrix mat))                ;transformate the object by matrix
      ) 
    )
  )
  (princ)
)

;;;一些矩阵函数的测试
(defun c:ccc (/ DXF E ENT I INS MAT0 MAT1 MAT2 MAT3 MAT4 MAT5 MAT6 MAT7 MAT8 MAT9 O ORG SEL VX VY)
  (if (setq ent (car (entsel "\n要平齐的对象:")))               ;(setq sel (ssget ":S" '((0 . "CIRCLE"))))
    (progn
      (setq dxf  (entget ent))
      (setq ins  (cdr (assoc 10 dxf)))
      (setq vx   (getvar 'ucsxdir))
      (setq vy   (getvar 'ucsydir))
      (setq org  (getvar 'ucsorg))
      (setq Mat0 (Mat:EntityMatrix ent))                        
      (setq mat1 (cadr mat0))                                   ;OCS->WCS
      (setq mat0 (car mat0))                                    ;WCS->OCS(trans Pt WCS OCS)
                        
      (setq mat2 (Mat:u2w))                                     ;UCS的变换矩阵
      (setq mat3 (Mat:w2u))                                     ;UCS的变换矩阵的逆矩阵

      (setq Mat4 (MAT:2VMatrix org vx vy))                      ;UCS的变换矩阵
      (setq mat5 (cadr mat4))                                   ;WCS->UCS
      (setq mat4 (car mat4))                                    ;UCS->WCS(trans Pt UCS WCS)
      
      (setq mat6 (Mat:trans 1 0))                               ;UCS的变换矩阵
      (setq mat7 (mat:trans 0 1))                               ;UCS的变换矩阵的逆矩阵

      (setq i -1)
      (if (setq sel (ssget))
        (progn
          (command "undo" "be")
          (repeat (sslength sel)
            (setq e (ssname sel (setq i (1+ i))))
            (setq o (vlax-ename->vla-object e))
            (vla-transformby o (vlax-tmatrix mat0))
          )
          (command "undo" "e")
        )
      )
    )
  )
  (princ)
)

;;;比例缩放矩阵的测试
(defun c:test1 (/ ENT I MAT OBJ PT SC SS)
  (initget 1)
  (setq Pt (getpoint "\n比例缩放基点:"))
  (initget 7)
  (setq sc (getreal "\n缩放倍数:")) 
  (setq mat (MAT:Trans 0 (list 0 0 sc)))
  (setq mat (vlax-tmatrix mat))
  (setq i -1)
  (if (setq ss (ssget))
    (repeat (sslength ss)
      (setq ent (ssname ss (setq i (1+ i))))
      (setq obj (vlax-ename->vla-object ent))
      (MAT:ScaleByMatrix obj mat)
    )
  )
)

;;;镜像，旋转和3点矩阵的测试
(defun c:test2 (/ AN ENT I MAT OBJ P0 P1 P2 SS)
  (initget 1)
  (setq P1 (getpoint "\n 点1:"))
  (initget 1)
  (setq P2 (getpoint p1 "\n 点2:"))
  ;(initget 1)
  ;(setq P3 (getpoint p1 "\n 点2:"))
  
  (grdraw p1 p2 1)
  ;(grdraw p2 p3 1)
  ;(grdraw p3 p1 1)
  (setq P1 (trans p1 1 0))
  (setq P2 (trans p2 1 0))
  ;(setq P3 (trans p3 1 0))
  (initget 1)
  (setq an (getangle "\n旋转角度:"))
  (initget 7)
  (setq sc (getreal "\n缩放倍数:")) 

  (setq p0 '(2.3 1.3 -1.2))
;;;  (setq s 
;;;  (Misc:test 10000
;;;    '( 
;;;      (MAT:Reflect p1 p2)
;;;    )
;;;  ))
 
  (setq mat (MAT:RotateBy2P P1 P2 an))
  (setq mat (vlax-tmatrix mat))
  (setq i -1)
  (command "undo" "be")
  (if (setq ss (ssget))
    (repeat (sslength ss)
      (setq ent (ssname ss (setq i (1+ i))))
      (setq obj (vlax-ename->vla-object ent))
      (setq obj (vla-copy obj))
      (vla-transformby obj mat)
    )
  )
  (command "undo" "e")
  (princ)
)

;;; 测试仿射变换
(defun c:fsbh ()
  (initget 9)
  (setq p0 (getpoint "\n原点0:"))
  (initget 9)
  (setq q0 (getpoint "\n像点0:"))
  (initget 9)
  (setq p1 (getpoint "\n原点1:"))
  (setq p1 (list (car p1) (cadr p1)))
  (initget 9)
  (setq q1 (getpoint "\n像点1:"))
  (setq q1 (list (car q1) (cadr q1)))
  (initget 9)
  (setq p2 (getpoint "\n原点2:"))
  (setq p2 (list (car p2) (cadr p2)))
  (initget 9)
  (setq q2 (getpoint "\n像点2:"))
  (setq q2 (list (car q2) (cadr q2)))
  
  (setq mat1 (Mat:AffineTrans p0 q0 p1 q1 p2 q2))
  (setq mat2 (cadr mat1))
  (setq mat1 (car mat1))
  (while (setq p (getpoint "\n要变换的点:"))
    (setq p (list (car p) (cadr p) 1.0))
    (setq q (Mat:mxv mat2 p))
    (setq j (mat:mxv mat1 (append q '(1))))
    (ent:make_point (trans q 1 0))
    (ent:make_Point (trans j 1 0))
  )
)

;;;=============================================================
;;;镜像，旋转和缩放的变换矩阵的测试                             
;;;=============================================================
;;;以下例子演示：                                               
;;;把选择集的所有物体，从指定的基点移动到目标点，并根据目标点 ;;
;;;旋转45度，然后再以目标点放大2倍.固然，这个程序完全可以用命 ;;
;;;令方式或者vla方式来完成。此处仅仅说明如何运用矩阵。          
;;;注意：CAD的矩阵和OpenGL或其他的语言的矩阵有区别:             
;;;      1.它们的矩阵是互为转置的。                             
;;;      2.它们的矩阵相乘也是顺序相反的。                       
;;;=============================================================
(defun c:test (/ ss p1 p2 mat1 mat2 mat3 i e o)
  (if (setq ss (ssget))                                         ;选择物体
    (progn
      (initget 1)
      (setq P1 (getpoint "\n基点:"))                            ;指定基点
      (initget 1)
      (setq P2 (getpoint P1 "\n目标点:"))                       ;指定目标点
      (grvecs (list 1 p1 p2))                                   ;红线标识位移
      (setq p1 (trans p1 1 0))                                  ;把输入得到的点转化为世界坐标系的点
      (setq p2 (trans p2 1 0))                                  ;把输入得到的点转化为世界坐标系的点
 
      (setq mat1 (MAT:TRANSLATEBY2P P1 p2))                     ;从P1位移到P2的位移矩阵
      (setq mat2 (MAT:ROTATION p2 (* pi 0.25)))                 ;以P2为基点旋转45度的变换矩阵
      (setq mat3 (MAT:SCALING p2 2.0))                          ;以P2为基点放大2倍变换矩阵
      (setq mat  (MAT:mxm mat3 (MAT:mxm mat2 mat1)))            ;须按照先后顺序从里到外这样相乘
      (setq mat  (vlax-tmatrix mat))                            ;用vlax-tmatrix把变换矩阵从表转化为ActiveX数组表达的矩阵
      (command "undo" "be")
      (setq i 0)
      (repeat (sslength ss)                                     
        (setq e (ssname ss i))                                  ;获得图元名
        (setq o (vlax-ename->vla-object e))                     ;获得ActiveX对象
        (vla-transformby o mat)                                 ;用vla-transformby函数对之变换
        (setq i (1+ i))
      )
      (command "undo" "e")
    )
  )
  (princ)
)                                 
;;;http://bbs.mjtd.com/forum.php?mod=viewthread&tid=91331
;;|;
(protect-assign (MatLibSymbols))
(princ)
 
