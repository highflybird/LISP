;|*************************************************************;
软件作者: Highflybird                                          ;
软件用途: 一个几何作图题的LISP程序解                           ;
日期地点: 2020.06.12 深圳                                      ;
程序语言: AutoLISP,Visual LISP                                 ;
版本号:   Ver. 1.0.20.0612                                     ;
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

;;;-------------------------------------------------------------
;;; 测试主程序。                                                
;;; 输入: 无。                                                  
;;; 输出: 无。                                                  
;;;-------------------------------------------------------------
(defun c:tt(/ CEN CIR DXF P RAD S X)
  (if (setq cir (car (entsel "\n选择圆: ")))
    (progn 
      (setq dxf (entget cir))
      (setq cen (cdr (assoc 10 dxf)))
      (setq rad (cdr (assoc 40 dxf)))
      (repeat 4
        (initget 9)
        (setq p (getpoint "\n点取: "))
        (setq p (trans p 1 0))
        (ent:make_point p)
        (setq s (cons p s))
      )
      (setq s (cons Rad s))
      (setq s (cons Cen S))
      (setq x (apply 'CIR:Inscribed_Quadrilateral s))
      (foreach v X
        (Ent:Make_LWPoly v T)
      )
    )
  )
  (princ)
)

;;;-------------------------------------------------------------
;;; 测试反演。                                                  
;;; 输入: 无。                                                  
;;; 输出: 无。                                                  
;;;-------------------------------------------------------------
(defun c:qq (/ CEN CIR DXF P RAD q)
  (setq cir (car (entsel "\n选择圆: ")))
  (setq dxf (entget cir))
  (setq cen (cdr (assoc 10 dxf)))
  (setq rad (cdr (assoc 40 dxf)))
  (while (setq p (getpoint "\n点取: "))
    (setq p (trans p 1 0))
    (setq q (CIR:Inversion cen rad p))
    (ent:make_point p)
    (ent:make_point q)
  )
  (princ)
)

;;;-------------------------------------------------------------
;;; 已知四点（无三点共线）和一个圆，求圆内接四边形，并满足四边依
;;; 次通过这四点。                                              
;;; 输入: 圆心，半径和四点(四点不应交错，否则结果也会交错）     
;;; 输出: 圆形内接四边形                                        
;;; 此题由QJChen出，见http://bbs.mjtd.com/thread-181642-1-1.html
;;;-------------------------------------------------------------
(defun CIR:Inscribed_Quadrilateral (C R Pa Pb Pc Pd / p1 p2 p3 q1 q2 q3 s E F G H)
  (setq p1 (CIR:Inversion2 C R Pa Pb))
  (setq p2 (CIR:InverSion2 C R P1 Pc))
  (setq p3 (CIR:InverSion2 C R P2 Pd))
  (setq q1 (CIR:inversion2 C R pd pc))
  (setq q2 (CIR:inversion2 C R q1 pb))
  (setq q3 (CIR:inversion2 C R q2 pa))
  (foreach E (CIR:Inters_Circle_Line C R p3 q3)
    (setq F (CIR:Inversion2 C R E Pa))
    (setq G (CIR:Inversion2 C R F Pb))
    (setq H (CIR:Inversion2 C R G Pc))
    (setq s (cons (list E F G H) s))
  )
)

;;;-------------------------------------------------------------
;;;卡斯蒂郎求解                                         	
;;;Castillon's Problem                                  	
;;;输入: 一给定圆（圆心半径表示）和不重合的三定点。     	
;;;输出：圆上三点，使得三已知点分别通过这三点形成的边   	
;;;-------------------------------------------------------------
(defun CIR:Castillon (C R p1 p2 p3 / A B E F P Q S y)
  (setq S nil)
  (foreach v (list (list R 0) (list 0 R) (list (- R) 0))
    (setq p (mapcar '+ C v))
    (setq q (CIR:Inversion2 C R p p1))
    (setq E (CIR:Inversion2 C R p p2))
    (setq F (CIR:Inversion2 C R q p3))
    (setq s (cons (list E F) S))
  )
  (setq A (inters (caar s) (cadadr s) (cadar s) (caadr s) nil))
  (setq B (inters (caar s) (cadr (caddr s)) (cadar s) (caaddr s) nil))
  (foreach x (CIR:Inters_Circle_Line C R A B)
    (setq p (CIR:Inversion2 C R x P3))
    (setq q (CIR:Inversion2 C R x P2))
    (setq y (cons (list p q x) y))
  )
)

;;;-------------------------------------------------------------
;;; 测试卡斯蒂郎程序。                                          
;;; 输入: 无。                                                  
;;; 输出: 无。                                                  
;;;-------------------------------------------------------------
(defun c:ttt (/ CEN CIR DXF P RAD S X)
  (if (setq cir (car (entsel "\n选择圆: ")))
    (progn 
      (setq dxf (entget cir))
      (setq cen (cdr (assoc 10 dxf)))
      (setq rad (cdr (assoc 40 dxf)))
      (repeat 3
        (initget 9)
        (setq p (getpoint "\n点取: "))
        (setq p (trans p 1 0))
        (ent:make_point p)
        (setq s (cons p s))
      )
      (setq s (cons Rad s))
      (setq s (cons Cen S))
      (setq x (apply 'CIR:Castillon s))
      (foreach v X
        (Ent:Make_LWPoly v T)
      )
    )
  )
  (princ)
)

;;;-------------------------------------------------------------
;;; 求反形点                                             	
;;; 输入: 圆心，半径和两点                              	
;;; 输出: 反形点                                          	
;;;-------------------------------------------------------------
;;;-------------------------------------------------------------
;;; 已知圆上一点P1和非圆上一点P2，求P1P2与圆的另一个交点        
;;; 输入: 圆心，半径和两点P1,P2                              	
;;; 输出: P1P2与圆的另一个交点                                  
;;;-------------------------------------------------------------
(defun CIR:Inversion2 (C R p1 p2 / d1 d2)
  (setq d1 (distance p2 C))
  (setq d2 (distance p2 p1))
  (polar p1 (angle p1 p2) (- d2 (/ (* (+ d1 r) (- d1 r)) D2)))
)

;;;-------------------------------------------------------------
;;; 点的反演                                             	
;;; 输入: 圆心，半径和一点                              	
;;; 输出: nil或者反演点                                  	
;;;-------------------------------------------------------------
(defun CIR:Inversion (c r p / d)
  (setq d (distance c p))
  (if (equal d 0 1e-8)
    nil                                                 	;圆心处没有反演点
    (polar c (angle c p) (/ (* r r) d))                 	;根据反演公式计算
  )
)

;;;-------------------------------------------------------------
;;; 直线和圆的交点                               	        
;;; 输入：圆心，半径，直线的两个端点                     	
;;; 输出：直线与圆的交点集(nil,一个点或者两个点)         	
;;;-------------------------------------------------------------
(defun CIR:Inters_Circle_Line (cen rad P1 P2 / y d a p l s)
  (setq y (car (trans (mapcar '- cen p1) 0 (mapcar '- p2 p1))))
  (setq d (abs y))
  (setq a (angle p1 p2))
  (if (equal d Rad 1e-6)
    (list (polar cen (- a (* pi 0.5)) y))           		;相切
    (if (< d Rad)
      (setq p (polar cen (- a (* pi 0.5)) y)        		;垂足
            L (sqrt (* (+ rad d)(- rad d)))             	;半弦长
            S (list (polar p a (- L))
                    (polar p a L)
              )
      )
    )
  )
)
 
;;;-------------------------------------------------------------
;;; 创建一个点                                           	
;;; 输入: 一个三维或者二维的点                           	
;;; 输出: 点实体的图元名                                 	
;;;-------------------------------------------------------------
(defun Ent:Make_Point (p)
  (entmakex (list '(0 . "POINT") (cons 10 p)))
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