;;返回一个选择集的最小矩形包围盒
;;参数1：选择集
(defun try-getbox-min (ss / ang0 ang1 angs angx ba0 ba1 bax bb0 bb1 bbx cn cv i k l ro temp)
  (if ss
    (progn
			(setq i -1 
				l(repeat (sslength ss)
					 (setq l(cons (vlax-ename->vla-object(ssname ss(setq i(1+ i)))) l))
				 ));选择集转对象表
			(setq
				bb1(try-GetBox l);包围盒
				cn (try-mid(car bb1)(cadr bb1));中心点
				cv (vlax-3D-point cn);创建3维点结构
				ba1 (apply '*(mapcar '- (car bb1)(cadr bb1)));包围盒面积
      )
			(setq ang1 0.0)
			(setq ang0(* 0.25 pi)angs ang0);转45度
			(foreach n l (vla-rotate n cv ang0))
			(setq bb0 (try-GetBox l);取得包围盒
				ba0 (apply '*(mapcar '- (car bb0)(cadr bb0)));包围盒面积
			)
			(if (> ba0 ba1);对换
				(setq 
					temp ang0
					ang0 ang1
				 	ang1 temp
					temp ba0
					ba0 ba1
					ba1 temp
					temp bb0
					bb0 bb1
					bb1 temp	
				)
			);0小的面积
			(repeat 2
				(while (> (abs(- ang1 ang0))(* 0.000001 pi))
					(if k (setq angx ang1 k nil);二次循环
						(setq angx(* 0.5(+ ang0 ang1)));中间角度
					)
					(setq ro(- angx angs)angs angx)
					(foreach n l (vla-rotate n cv ro))
					(setq bbx (try-GetBox l);取得包围盒
						bax (apply '*(mapcar '- (car bbx)(cadr bbx)));包围盒面积
					)
					(if (< bax ba0);对换
						(setq 
							ba1 ba0 ang1 ang0 bb1 bb0
							ba0 bax ang0 angx bb0 bbx)
						(setq ang1 angx ba1 bax bb1 bbx)
					)
				)
				(foreach n l (vla-rotate n cv (- ang0)))
				(setq ang1 (- ang0(* (if (/= 0.0 ang0)(- 0.25)0.25) pi))angs 0.0 k t)
			)
      (try-pts-rotate (try-pt2-to-pt4 (car bb0)(cadr bb0)) cn (- ang0))
    )
  )
)
;;取两点的中点(二维)
;;例子(try-mid 点1 点2)
(defun try-mid(p1 p2)(mapcar '(lambda (x y) (* 0.5 (+ x y))) p1 p2))
;;pt点围绕p0点旋转ang弧度
(defun try-pt-rotate (pt p0 ang)
(polar p0 (+ (angle p0 pt) ang) (distance pt p0))
)
;;点表围绕p0点旋转ang弧度
(defun try-pts-rotate(lst p0 ang)
(mapcar '(lambda(x)(try-pt-rotate x p0 ang))lst)
)
;;根据两点坐标返回4个点坐标
(defun try-pt2-to-pt4 (pt1 pt2)
	(list (list (car pt1)(cadr pt1)) (list(car pt1)(cadr pt2))(list (car pt2)(cadr pt2))(list (car pt2)(cadr pt1)))
)




;;================
;取得对象最小包围盒；参数 1、对象(表)/图元名(表)/选择集
;;注意，当选择集有大量图元时速度较慢，1万个图元可能接近1秒
(defun try-getbox (e / en i l max1 min1 pt1 pt2 sn tx1 tx2 ty1 ty2 tye x1 x2 y1 y2)
	(setq tye(type e))
	(cond 
		((= 'VLA-object tye)
			(vla-GetBoundingBox e 'p1 'p2);取得包容图元的最大点和最小点
			(setq min1 (vlax-safearray->list p1));把变体数据转化为表
			(setq max1 (vlax-safearray->list p2));把变体数据转化为表
			(list min1 max1)
		)
		((= 'ENAME tye)
			(try-GetBox (vlax-ename->vla-object e))
		)
		((or(= 'LIST tye)(= 'PICKSET tye))
			(if (= 'PICKSET tye)(setq e (repeat (sslength ss)
					 (setq l(cons (vlax-ename->vla-object(ssname ss(setq i(1+ i)))) l))
				 )))
			(if (= ENAME(type(car e)))(setq e(mapcar 'vlax-ename->vla-object e)))
			
			
			(vla-getboundingbox (car e) 'pt1 'pt2);获取单个图元包盒
			(setq 
				pt1(vlax-safearray->list pt1)
				pt2 (vlax-safearray->list pt2)
				tx1 (car pt1)
				ty1 (cadr pt1)
				tx2 (car pt2)
				ty2 (cadr pt2)
			)
			(setq i 0)
			(setq sn (length e))
			(repeat (1- sn)
				(setq en (nth (setq i (1+ i))e))
				(vla-getboundingbox en 'pt1 'pt2);获取单个图元包盒
				(setq 
					pt1(vlax-safearray->list pt1)
					pt2 (vlax-safearray->list pt2)
					x1 (car pt1)
					y1 (cadr pt1)
					x2 (car pt2)
					y2 (cadr pt2))
				(if (> tx1 x1)(setq tx1 x1))
				(if (> ty1 y1)(setq ty1 y1))
				(if (< tx2 x2)(setq tx2 x2))
				(if (< ty2 y2)(setq ty2 y2))
			)
			(list (list tx1 ty1) (list tx2 ty2))
		)
	)
)


;;以下是应用例子
(defun c:tt (/ ls)
	(setq ls(try-getbox-min (ssget)))
	(command "pline" "_non"(car ls)"_non"(cadr ls)"_non"(caddr ls)"_non"(cadddr ls) "c")
)


(defun c:vv ()
  (setq ss (ssget))
  (uti:bench 10
    (list
      (cons 'try-getbox-min (list ss))
      (cons 'ALG:MinBox (list ss 20))
    )
  )
  (setq ls (try-getbox-min ss))
  (command "pline" "_non"(car ls)"_non"(cadr ls)"_non"(caddr ls)"_non"(cadddr ls) "c")
  (make-poly (ALG:MinBox ss 20) 1)
) 
  