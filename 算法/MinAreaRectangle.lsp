;;;此程序跟tryhi的做比较:
(defun c:test (/ ss ls)
  (setq ss (ssget))
  (uti:bench 1
    (list
      (cons 'try-getbox-min (list ss))
      (cons 'ALG:MinBox (list ss 20))
    )
  )
  (setq ls (try-getbox-min ss))
  (command "pline" "_non"(car ls)"_non"(cadr ls)"_non"(caddr ls)"_non"(cadddr ls) "c")
  (make-poly (ALG:MinBox ss 20) 1)
) 
  

;;;The procedure for Test
(defun C:test3 (/ PP PTLIST SEL T0 n)
  (princ "\n命令是test(The command is Test).")
  (princ "\n请选择点、曲线类物体或者图块")
  (setq sel (ssget '((0 . "POINT,*LINE,ARC,CIRCLE,ELLIPSE,INSERT"))))  ;select curve or point
  (initget 7)
  (setq n (getint "\n取样精度(100-2000):"))
  (if sel                                                         
    (progn
      (setq ptlist (getpt sel n))                                       ;construct the set of points
      (setq ptlist (ALG:GrahamScan ptlist))                             ;construct the CCW Hull of this set.
      (if (<= (det (car ptlist) (cadr ptlist) (caddr ptlist)) 0.0)      ;ensure the hull is CCW.
        (setq ptlist (reverse ptlist))                                  ;if it isn't CCW,then reverse it
      )
      (make-poly ptlist 5)
      (setq t0 (getvar "TDUSRTIMER"))                                   ;The start time of this algorithm
      (setq pp (car (MinAreaRectangle ptlist)))                         ;start calculating
      (princ "\nIt takes :")                                            
      (princ (* (- (getvar "TDUSRTIMER") t0) 86400))                    ;The End time
      (princ "seconds")
      (if pp
        (make-poly pp 1)                                                ;draw rectangle.
      )
    )
  )
  (princ)
)

(defun ALG:MinBox (sel n / ptlist pp)
  (setq ptlist (getpt sel n))					;construct the set of points
  (setq ptlist (ALG:GrahamScan ptlist))				;construct the CCW Hull of this set.
  (if (<= (det (car ptlist) (cadr ptlist) (caddr ptlist)) 0.0)	;ensure the hull is CCW.
    (setq ptlist (reverse ptlist))				;if it isn't CCW,then reverse it
  )		 
  (setq pp (car (MinAreaRectangle ptlist))) 			;start calculating
)

;;;=============================================================
;;;Function : Find the minimum area of encasing rectangle.      
;;;Arguments : A CCW HULL                                       
;;;Return: The Four points of Rectangle and its Area            
;;;=============================================================
(defun MinAreaRectangle (ptlist      /     AA    AI    BB    D1
                         D2    EDGE  I     I1X   I1Y   I2X   I2Y
                         IL    INF   IX    IY    J1    J2    MINA
                         MINH  MINW  NORH  NORM  PI1   PI2   PTI0
                         PTI1  PTI2  PTJ1  PTK1  PTM1  PTS1  PTS2
                         PTS3  PTS4  REC1  REC2  REC3  REC4  RECT
                         VECH  VECL  VJ12  VM12
                        )
  (setq INF 1e309)                                                      
  (setq minA INF)                                               ;Initiating the Minimum area is infinite
  (setq pti0 (car ptlist))                                      ;the first point of Hull.
  (setq pts1 (append ptlist (list pti0)))                       ;add the first point at back of Hull
  (setq pts2 (cdr (append ptlist ptlist (list pti0))))          ;Construct a loop for the hull
  (setq i 0)                                                            

  ;;Find area of encasing rectangle anchored on each edge.
  (repeat (length ptlist)
    (setq pi1 (car   pts1)                                              
          pi2 (cadr  pts1)
          i1x (car   pi1)
          i1y (cadr  pi1)
          i2x (car   pi2)
          i2y (cadr  pi2)
          ix  (- i2x i1x)
          iy  (- i2y i1y)
          il  (distance (list ix iy) '(0.0 0.0))
    )

    ;;寻找最左点
    ;;Find a vertex on on first perpendicular line of support
    (while (> (DOTPR ix iy pts2) 0.0)
      (setq pts2 (cdr pts2))
    )

    ;;寻找最上点
    ;;Find a vertex on second perpendicular line of suppoer
    (if (= i 0)
      (setq pts3 pts2)
    )
    (while (> (CROSSPR ix iy pts3) 0.0)
      (setq pts3 (cdr pts3))
    )

    ;;寻找最右点
    ;;Find a vertex on second perpendicular line of suppoer
    (if (= i 0)
      (setq pts4 pts3)
    )
    (while (< (DOTPR ix iy pts4) 0.0)
      (setq pts4 (cdr pts4))
    )

    ;;得出了每边的矩形
    ;;Find distances between parallel and perpendicular lines of support
    (cond
      ((equal i1x i2x 1e-4)                                     ;如果边两点的X值相同
       (setq d1 (- (caar pts3) i1x)                             ;那么矩形的高就是最上点与边的X的差值
             d2 (- (cadar pts4) (cadar pts2))                   ;矩形的宽就是最左和最右的Y的差值
       )
      )
      ((equal i1y i2y 1e-4)                                     ;如果边两点的Y值相同
       (setq d1 (- (cadar pts3) i1y)                            ;那么矩形的高就是最上点与边的Y的差值
             d2 (- (caar pts4) (caar pts2))                     ;矩形的宽就是最左和最右的X的差值
       )
      )

      (T
       (setq aa (det pi1 pi2 (car pts3)))                       ;否则计算边和最上点构成的面积的二倍(det)
       (setq d1 (/ aa il))                                      ;高就是det值除以边长
       (setq j1 (car pts2))                                     ;最右边点
       (setq j2 (list (- (car j1) iy) (+ (cadr j1) ix)))        ;通过最右边点的垂直边的点
       (setq bb (det j1 j2 (car pts4)))                         ;最右边点，上面的点和最左边的点
       (setq d2 (/ bb il))                                      ;这三点的det除以边长就是宽
      )
    )

    ;;计算矩形的面积，必要时更新最小面积
    ;;Compute area of encasing rectangle anchored on current edge.
    ;;if the area is smaller than the old Minimum area,then update,and record the width,height and five points.
    (setq Ai (abs (* d1 d2)))                                   ;面积就是高和宽的积
    (if (< Ai MinA)                                             ;如果面积小于先前的最小面积，则记录：
      (setq MinA Ai                                             ;更新最小面积
            MinH d1                                             ;最小面积的高
            MinW d2                                             ;最小面积的宽
            pti1 pi1                                            ;最小面积的边的第一个端点
            pti2 pi2                                            ;最小面积的边的第二个端点
            ptj1 (car pts2)                                     ;最右边的点
            ptk1 (car pts3)                                     ;最上面的点
            ptm1 (car pts4)                                     ;最左边的点
      )
    )
    (setq pts1 (cdr pts1))                                      ;检测下一条边
    (setq i (1+ i))                                             ;计数器加一
  )

  ;;according to the result ,draw the Minimum Area Rectangle
  (setq edge (mapcar '- pti2 pti1))                             ;最小面积的边对应的向量
  (setq VecL (distance edge '(0.0 0.0)))                        ;最小面积的边的长度
  (setq NorH (abs (/ MinH VecL)))                               ;这边的法线
  
  (setq Norm (list (- (cadr edge)) (car edge)))                 ;这边的垂直向量
  (setq vj12 (mapcar '+ ptj1 Norm))                             ;通过最右点的垂直向量
  (setq vm12 (mapcar '+ ptm1 Norm))                             ;通过最左点的垂直向量
  (setq vecH (mapcar '* (list NorH NorH) Norm))                         

  (setq rec1 (inters pti1 pti2 ptj1 vj12 nil))                  ;矩形的第一点
  (setq rec4 (inters pti1 pti2 ptm1 vm12 nil))                  ;矩形的第四点
  (setq rec2 (mapcar '+ rec1 vecH))                             ;矩形的第二点
  (setq rec3 (mapcar '+ rec4 vecH))                             ;矩形的第三点
  (setq rect (list Rec1 rec2 rec3 rec4))                        ;矩形的点表
  (cons rect MinA)                                              ;返回这个矩形的点表和最大距离
)

;;;=============================================================
;;;求凸壳的直径的程序                                           
;;;参数：逆时针的凸壳 H-------注意逆时针!!!                     
;;;返回值: 直径的两个端点和直径 Pair . MaxD                     
;;;=============================================================
(defun ALG:MaxCaliber (H / D M MAXD P PAIR Q U V W)
  (setq Q (cdr (append H H (list (car H)))))                    ;构造一个首尾循环的凸集,且起始点为凸壳的第二点
  (setq MaxD 0.0)                                               ;初始化最小距离为0
  (foreach U H                                                  ;依次检查凸壳的边
    (setq V (car Q))                                            ;循环集的第一点
    (setq W (cadr Q))                                           ;循环集的第二点
    (setq M (GEO:Midpoint V W))                                       ;这两点的中点
    (while (> (dot M U V) 0.0)                                  ;如果夹角小于90度（即点积大于0）
      (setq Q (cdr Q))                                          ;循环集推进
      (setq V (car Q))                                          ;取下一点
      (setq W (cadr Q))                                         ;下下一点
      (setq M (GEO:Midpoint V W))                                     ;这两点的中点
    )
    (setq D (distance U V))                                     ;计算这时的最大距离
    (if (> D MaxD)                                              ;如果大于前面的最大距离
      (setq MaxD D                                              ;就替换前面的最大距离
            Pair (list U V)                                     ;并记录这对点
      )
    )
  )
  (cons Pair MaxD)                                              ;返回这对点和最大距离
)

;;;=============================================================
;;; 功能: 用Gramham扫描法构造凸包                               
;;; 输入: 二维点集                                              
;;; 输出: 点集的凸包                                            
;;;=============================================================
(defun ALG:GrahamScan (pts / pMax Hull P Q)
  (if (< (length pts) 3)                                        ;3点以下
    pts                                                      	;是本集合
    (progn
      (setq pMax (assoc (apply 'max (mapcar 'car pts)) pts))    ;最右边的点
      (setq Pts  (ALG:SortByAngle pts pMax))                    ;分类点集
      (setq Hull (list (cadr Pts) pMax))                        ;开始的两点
      (foreach n (cddr Pts)                                     ;从第3点开始
        (setq Hull (cons n Hull))                               ;把Pi加入到凸集
        (setq P (cadr Hull))                                    ;Pi-1
        (setq Q (caddr Hull))                                   ;Pi-2
        (while (and Q (> (det n P Q) -1e-6))                    ;如果左转
          (setq Hull (cons n (cddr Hull)))                      ;删除Pi-1点
          (setq P (cadr Hull))                                  ;得到新的Pi-1点
          (setq Q (caddr Hull))                                 ;得到新的Pi-2点
        )
      )
      (reverse Hull)                                            ;返回凸集
    )
  )
)

;;;=============================================================
;;; 以某点为基点，按照角度和距离分类点集                        
;;;=============================================================
(defun ALG:SortByAngle (lst p /)
  (vl-sort
    lst
    (function
      (lambda (e1 e2 / a1 a2)
        (if (= (setq a1 (angle p e1)) (setq a2 (angle p e2)))
          (< (distance p e1) (distance p e2))
          (< a1 a2)
        )
      )
    )
  )
)

;;;=============================================================
;;; 中点函数                                                    
;;;=============================================================
(defun GEO:Midpoint (p1 p2)
  (list
    (* (+ (car p1) (car p2)) 0.5)
    (* (+ (cadr p1) (cadr p2)) 0.5)
  )
)

;;;-------------------------------------------------------------
;;; 点积= x1*x2 + y1*y2                                         
;;;-------------------------------------------------------------
(defun DOTPR (ix iy pts / pt1 pt2)
  (setq pt1 (car pts))
  (setq pt2 (cadr pts))
  (+ (* ix (- (car pt2) (car pt1)))
     (* iy (- (cadr pt2) (cadr pt1)))
  )
)

;;;-------------------------------------------------------------
;;; 叉积= x1*y2 - x2*y1                                         
;;;-------------------------------------------------------------
(defun CROSSPR (ix iy pts / pt1 pt2)
  (setq pt1 (car pts))
  (setq pt2 (cadr pts))
  (- (* ix (- (cadr pt2) (cadr pt1)))
     (* iy (- (car pt2) (car pt1)))
  )
)

;;;-------------------------------------------------------------
;;; 定义三点的行列式,即三点之倍面积                             
;;;-------------------------------------------------------------
(defun det (p1 p2 p3 / x2 y2)
  (setq x2 (car p2)
        y2 (cadr p2)
  )
  (- (* (- x2 (car p3)) (- y2 (cadr p1)))
     (* (- x2 (car p1)) (- y2 (cadr p3)))
  )
)

;;;-------------------------------------------------------------
;;; 定义向量P1P2,P1P3的点积函数                                 
;;;-------------------------------------------------------------
(defun dot (p1 p2 p3 / x1 y1)
  (setq x1 (car p1)
        y1 (cadr p1)
  )
  (+ (* (- (car p2) x1) (- (car p3) x1))
     (* (- (cadr p2) y1) (- (cadr p3) y1))
  )
)

;;;-------------------------------------------------------------
;;; 取点函数2                                                   
;;;-------------------------------------------------------------
(defun getpt (ss num / i E O N C S)
  (repeat (setq i (sslength ss))
    (setq e (ssname ss (setq i (1- i))))
    (setq O (vlax-ename->vla-object E))
    (setq N (vla-get-ObjectName O))
    (cond
      ( (= N "AcDbPolyline")
        (setq c (get-pline-vertexs E num))
        (setq s (append c s))
      )
      ( (= N "AcDbPoint")
        (setq s (cons (vlax-get O 'Coordinates) s))
      )
      ( (= N "AcDbLine")
        (setq s (cons (vlax-get O 'startpoint) s))
        (setq s (cons (vlax-get O 'Endpoint) s))
      )
      ( (= 0 (cdr (assoc 75 (entget E))))
        (setq C (get-3dpolyline-vertexs E))
        (setq s (append C s))
      )
      (t
        (setq c (get-spline-vertexs E num))
        (setq s (append c s))
      )
    )
  )
)

;;;-------------------------------------------------------------
;;; 取得轻多边形顶点                                            
;;;-------------------------------------------------------------
(defun get-LWpolyline-vertexs (DXF / lst)
  (foreach n DXF
    (if (= (car n) 10)
      (setq lst (cons (cdr n) lst))
    )
  )
  (reverse lst)
)

;;;-------------------------------------------------------------
;;; 取无拟合的POLYLINE(2d或3d)顶点                              
;;;-------------------------------------------------------------
(defun get-3dpolyline-vertexs (poly / vtx DXF lst)
  (setq vtx (entnext poly))
  (while (/= (cdr (assoc 0 (setq DXF (entget vtx)))) "SEQEND")
    (setq lst (cons (cdr (assoc 10 DXF)) lst)
	  vtx (entnext vtx)
    )
  )
  (reverse lst)
)

;;;-------------------------------------------------------------
;;; 取得样条曲线的点                                            
;;;-------------------------------------------------------------
(defun get-spline-vertexs (ent n / DIST ENDPAR I LEN OBJ PT PTS SEG)
  (setq obj (vlax-ename->vla-object ent))
  (setq len (vlax-curve-getDistAtParam ent (vlax-curve-getEndParam ent)))
  (setq name (vla-get-objectname obj))
  (if (= name (vla-get-objectname obj) "AcDbSpline")
    (setq n (* n (vla-get-NumberOfControlPoints obj)))
    (if (or (= name "AcDb3dPolyline") (= name "AcDb2dPolyline"))
      (setq n (* n (/ (length (vlax-get obj 'Coordinates)) 3)))
    )
  )
  (setq seg (/ len n))
  (setq dist 0)
  (while (< dist len)   
    (setq pt (vlax-curve-getPointAtDist ent dist))
    (setq pts (cons pt pts))
    (setq dist (+ seg dist))   
  )
  (if (not (vlax-curve-isClosed ent))
    (setq pt (vlax-curve-getEndPoint ent)
          pts (cons pt pts)
    )
  )
  (reverse pts)
)

;;;-------------------------------------------------------------
;;; 取得轻多段线的点                                            
;;;-------------------------------------------------------------
(defun get-pline-vertexs (ent n / BLG DIST ENDPAR I L1 L2 L3 LI OBJ PT PTS VEXNUM)
  (setq obj (vlax-ename->vla-object ent))
  (setq endpar (vlax-curve-getEndParam ent))
  (setq vexNum (fix endPar))
  (setq pts nil)
  (setq i 0)
  (repeat vexNum
    (setq pt (vlax-curve-getPointAtParam ent i))
    (setq pts (cons pt pts))
    (setq blg (vla-getbulge obj i))
    (if (/= blg 0.0)
      (progn
        (setq l1 (vlax-curve-getDistAtParam ent i))
        (setq l2 (vlax-curve-getDistAtParam ent (1+ i)))
        (setq l3 (- l2 l1))
        (setq li (/ l3 n))
        (setq dist l1)
        (repeat (1- n)
          (setq dist (+ dist li))
          (setq pt (vlax-curve-getPointAtDist ent dist))
          (setq pts (cons pt pts))
        )
      )
    )
    (setq i (1+ i))
  )
  (if (not (vlax-curve-isClosed ent))
    (setq pt (vlax-curve-getEndPoint ent)
          pts (cons pt pts)
    )
  )
  pts
)

;;;-------------------------------------------------------------
;;; 绘制多段线                                                  
;;;-------------------------------------------------------------
(defun Make-Poly (pp closed / x)
  (entmakeX                                                      
    (vl-list*
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbPolyline")
      '(62 . 1)							;红色的
      (cons 90 (length pp))                                     ;顶点个数
      (cons 70 closed)						;1闭合，0不闭合
      (mapcar (function (lambda (x) (cons 10 x))) pp)           ;多段线顶点
    )
  )
)

;;;-------------------------------------------------------------
;;; 测试用函数(benchMark function)                              
;;;-------------------------------------------------------------
(defun UTI:Bench (Times Expressions / s fName TIME0 TIME1 Value Speed)
  (defun Princ-Column (l value / s)
    (setq s (vl-princ-to-string value))
    (princ s)
    (repeat (- l (strlen s))
      (princ " ")
    )
  )
  (defun Print-Result (lst)
    (princ "\n")
    (mapcar 'princ-Column '(19 9 14 16) lst)
  )
  
  (foreach Func Expressions
    (setq fName (car Func))
    (setq TIME0 (getvar "millisecs"))
    (repeat times
      (setq Value (apply fName (cdr func)))
    )
    (setq TIME1 (getvar "millisecs"))
    (setq TIME1 (- TIME1 TIME0 0.0))
    (setq Speed (/ TIME1 times))
    (setq S (cons (list fName times TIME1 Speed Value) S))
  )
  
  (princ "\nStatement          Times    Elapse(ms)    Average(ms/time)")
  (princ "\n----------------------------------------------------------")
  (setq s (vl-sort s (function (lambda (a b) (< (caddr a) (caddr b))))))
  (mapcar 'Print-Result s) 
  (gc)
  s
)
