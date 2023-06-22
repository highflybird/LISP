(defun c:ttt(/ ent i sel pts)
  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ang (angle '(0 0 0) (getvar "ucsxdir")))
  (setq Pts nil)
  (if (setq sel (ssget '((0 . "*LINE,ARC,ELLIPSE,CIRCLE"))))
    (repeat (setq i (sslength sel))
      (setq ent (ssname sel (setq i (1- i))))
      (setq rec (Ent:BoxByAngle ent ang))
      (setq pts (cons rec pts))
      (setq rec (GEO:GetBoxBySet rec ang))
      (Ent:Make_LWPoly rec T)
      ;(Ent:Make_LWPoly pts T)
      ;(setq rec (GEO:GetBoxBySet pts ang))
      ;(setq pts (ENT:BoundingBoxUCS ent))
      ;(Ent:Make_LWPoly rec T)
      ;(apply 'Ent:Make_Rectangle pts)
      ;(setq pts (SPL:BoundingBox1 ent))
      ;(apply 'Ent:Make_Rectangle pts)
    )
  )
  (setq pts (apply 'append pts))
  (setq box (GEO:GetBoxBySet pts ang))
  (Ent:Make_LWPoly box T)
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)

;;;方法1 最快
(defun SPL:BoundingBoxWCS (ent / obj ll ur l q)
  (foreach n '((1. 0. 0.) (0. 1. 0.))
    (foreach p '((-1e12 -1e12 -1e12) (1e12 1e12 1e12))
      (setq p (vlax-curve-getClosestPointToProjection ent p n))
      (setq l (cons p l))
    )
  )
  (list
    (apply 'mapcar (cons 'min l))
    (apply 'mapcar (cons 'max l))
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
;;; 功能: 沿某一个方向的曲线类物体的包围盒点                    
;;; 输入: ent--实体图元名                                       
;;;       ang--方向与X轴夹角                                    
;;; 输出: 包围盒点                                              
;;;-------------------------------------------------------------
(defun Ent:BoxByAngle (ent ang / ll ur R90 cen L LEN P Vx Vy)
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
    (setq l (cons p l))
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
(defun GEO:GetBoxBySet (pts / pMin pMax)
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

(defun ENT:BoundingBoxUCS (ent / l)
  (setq x (getvar "ucsxdir"))
  (setq y (getvar "ucsydir"))
  (setq ang (angle '(0 0 0) x))
  (setq obj (vlax-ename->vla-object ent))
  (vla-GetBoundingBox obj 'll 'ur)
  (setq ll (vlax-safearray->list ll))
  (setq ur (vlax-safearray->list ur))
  (setq cen (GEO:Midpoint ll ur))
  (setq R90 (* 0.5 pi))
  (setq len (* 10000 (distance ll ur)))
  (setq xdir (list (cos ang) (sin ang) 0))
  (setq ydir (list (- (sin ang)) (cos ang) 0))
  (foreach n (list 0 R90 PI (+ R90 PI))
    (setq p (polar cen (+ ang n) len))
    (if (zerop (rem n pi))
      (setq p (vlax-curve-getClosestPointToProjection ent p ydir))
      (setq p (vlax-curve-getClosestPointToProjection ent p xdir))
    )
    (setq l (cons (trans p 0 1) l))
  )
  (setq pMin (apply 'mapcar (cons 'min l)))
  (setq pMax (apply 'mapcar (cons 'max l)))
  (mapcar
    '(lambda (pt) (trans pt 1 0))
    (list
      (list (car pMin) (cadr pMin))
      (list (car pMax) (cadr pMin))
      (list (car pMax) (cadr pMax))
      (list (car pMin) (cadr pMax))
    )
  )
)

(defun SPL:BoundingBoxUCS (ent / obj ll ur l q x y)
  (setq x (getvar "ucsxdir"))
  (setq y (getvar "ucsydir"))
  (setq ang (angle '(0 0 0) x))
  (setq obj (vlax-ename->vla-object ent))
  (vla-GetBoundingBox obj 'll 'ur)
  (setq ll (vlax-safearray->list ll))
  (setq ur (vlax-safearray->list ur))
  (setq mp (GEO:Midpoint ll ur))
  (setq cd (* (distance ll ur) 10000))
  (foreach n (list x y)
    (setq a (+ (angle '(0 0 0) n) (* 0.5 PI)))
    (foreach s (LIST 0 pi)
      (setq p (polar mp (+ a S) cd)) 
      (setq p (vlax-curve-getClosestPointToProjection ent p n))
      (SETQ P (TRANS p 0 1))
      (setq l (cons p l))
    )
  )
  ;(Ent:Make_LWPoly L T)
  (setq pMin (apply 'mapcar (cons 'min l)))
  (setq pMax (apply 'mapcar (cons 'max l)))
  (mapcar
    '(lambda (pt) (trans pt 1 0))
    (list
      (list (car pMin) (cadr pMin))
      (list (car pMax) (cadr pMin))
      (list (car pMax) (cadr pMax))
      (list (car pMin) (cadr pMax))
    )
  )
)

;;;方法2 最慢
(defun SPL:BoundingBox1 (ent / obj ll ur l q)
  (setq obj (vlax-ename->vla-object ent))
  (vla-GetBoundingBox obj 'll 'ur)
  (foreach p (list ll ur)
    (setq p (vlax-safearray->list p))
    (foreach n '((1. 0. 0.) (0. 1. 0.))
      (foreach s '(+ -)
	(setq q (mapcar s p))
	(setq q (vlax-curve-getClosestPointToProjection ent q n))
        ;(setq q (vlax-curve-getClosestPointToProjection ent p n T))
        (setq l (cons q l))
      )	
    )
  )
  (list
    (apply 'mapcar (cons 'min l))
    (apply 'mapcar (cons 'max l))
  )
)

(defun test (c)
  ;;(test (car(entsel)))
  ;; insufficiently tested!!!
  (mapcar (function (lambda (p)
                      (list (car (vlax-curve-getClosestPointToProjection c p '(0 1 0) t))
                            (cadr (vlax-curve-getClosestPointToProjection c p '(1 0 0) t))
                            (caddr (vlax-curve-getClosestPointToProjection c (reverse p) '(0 1 0) t))
                      )
                    )
          )
          '((-1e12 -1e12 0) (1e12 1e12 0))
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