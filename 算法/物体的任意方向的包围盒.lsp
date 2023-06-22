;|*************************************************************;
软件作者: Highflybird                                          ;
软件用途: 获取在某个角度下或者UCS下的物体BOX   　　　　　　　　;
日期地点: 2021.08.30 深圳                                      ;
程序语言: AutoLISP,Visual LISP                                 ;
版本号:   Ver. 1.0.21.0830                                     ;
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
      (setq rec (GEO:GetBoxBySet rec ang))			;仅仅为测试用.
      (Ent:Make_LWPoly rec T)					;仅仅为测试用.
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
;;; 功能: 沿某一个方向的曲线类物体的包围盒点                    
;;; 输入: ent--实体图元名                                       
;;;       ang--方向与X轴夹角                                    
;;; 输出: 包围盒点                                              
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
;;; 不考虑倾角的text的四个角点                                  
;;;-------------------------------------------------------------
(defun Ent:TextBox (ent / dxf rec ins rot pt1 pt2 pt3 pt4)
  (setq dxf (entget ent))					;;从选取的文本对象的获取一些属性
  (setq rec (textbox dxf))					;文本的包围矩形
  (setq ins (cdr (assoc 10 dxf)))                               ;插入点
  (setq rot (cdr (assoc 50 dxf)))                               ;旋转角
  (setq pt1 (car rec))
  (setq pt2 (cadr rec)) 
  (setq pt3 (list (car pt2) (cadr pt1)))
  (setq pt4 (list (car pt1) (cadr pt2)))
  (if (not (equal rot 0 1e-6))                                  ;如果有旋转角
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
;;; 考虑倾角的text的四个角点                                    
;;;-------------------------------------------------------------
(defun Ent:TextBox1 (ent / DXF INS ISX LEN OBL PT1 PT2 PT3 PT4 REC ROT)
  (setq dxf (entget ent))					;;从选取的文本对象的获取一些属性
  (setq rec (textbox dxf))					;文本的包围矩形
  (setq ins (cdr (assoc 10 dxf)))                               ;插入点
  (setq rot (cdr (assoc 50 dxf)))                               ;旋转角
  (setq obl (cdr (assoc 51 dxf)))                               ;倾斜角
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
;;; 测试单行文本程序                                            
;;;-------------------------------------------------------------
(defun c:tt ()
  (if (setq ent (car (entsel "\n请选取单行文本: "))) 
    (Ent:Make_LWPoly (Ent:TextBox ent) T)
  )
)
 
;;;-------------------------------------------------------------
;;; 功能: 沿某一个方向的点集的包围盒                            
;;; 输入: pts--点集                                             
;;;       ang--方向与X轴夹角                                    
;;; 输出: 包围盒点(WCS)                                         
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
;;; 功能: 沿UCS方向的点集的包围盒                               
;;; 输入: pts--点集                                             
;;; 输出: 包围盒点(WCS)                                         
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
;;; 对一个点集施加矩阵变换                                      
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
;;; 对一个点集施加旋转变换                                      
;;;-------------------------------------------------------------
(defun GEO:RotatePoints (pts ang)
  (mapcar (function (lambda (p) (GEO:RotByAngle p ang))) pts)
)
 
;;;-------------------------------------------------------------
;;; 功能：图块的变换矩阵                                        
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
;;; 功能：图块的变换矩阵的逆矩阵,                               
;;; 输入：块参照的图元名,                                       
;;; 输出：块参照的变换矩阵的逆矩阵                              
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
;;; 功能: 两点之中点                                            
;;; 输入: 两点p1,P2                                             
;;; 输出: 中点位置                                              
;;;-------------------------------------------------------------
(defun GEO:Midpoint (p1 p2)
  (mapcar (function (lambda (e1 e2) (* (+ e1 e2) 0.5))) p1 p2)
)

;;;-------------------------------------------------------------
;;; 功能: 位移点                                                
;;; 输入: 点P，位移量v                                          
;;; 输出: 位移后点位置                                          
;;;-------------------------------------------------------------
(defun Geo:displacement (p v)
  (mapcar '+ p v)
)

;;;-------------------------------------------------------------
;;; 功能: 旋转点                                                
;;; 输入: 点P，角度ang                                          
;;; 输出: 中点位置                                              
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
;;;创建轻多段线                                         	
;;;输入: 二维的点集                                     	
;;;输出: 轻多段线实体名                                 	
;;;-------------------------------------------------------------
(defun Ent:Make_LWPoly (pts closed /)
  (entmakeX                                              
    (VL-LIST*
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbPolyline")
      (cons 90 (length pts))                      	        ;顶点个数
      (cons 70 (if closed 1 0))                          	;闭合的
      (mapcar (function (lambda (x) (cons 10 x))) pts)  	;多段线顶点
    )
  )
)

;;;-------------------------------------------------------------
;;;创建一条直线段                                       	
;;;输入: 两个三维或者二维的点                           	
;;;输出: 线段实体的图元名                               	
;;;-------------------------------------------------------------
(defun Ent:Make_Line (p q)
  (entmakeX (list '(0 . "LINE") (cons 10 p) (cons 11 q)))
)

;;;-------------------------------------------------------------
;;;创建矩形                                         	        
;;;输入: 矩形的两个角点                                    	
;;;输出: 矩形的实体名                                  	        
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