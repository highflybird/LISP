;;����һ��ѡ�񼯵���С���ΰ�Χ��
;;����1��ѡ��
(defun try-getbox-min (ss / ang0 ang1 angs angx ba0 ba1 bax bb0 bb1 bbx cn cv i k l ro temp)
  (if ss
    (progn
			(setq i -1 
				l(repeat (sslength ss)
					 (setq l(cons (vlax-ename->vla-object(ssname ss(setq i(1+ i)))) l))
				 ));ѡ��ת�����
			(setq
				bb1(try-GetBox l);��Χ��
				cn (try-mid(car bb1)(cadr bb1));���ĵ�
				cv (vlax-3D-point cn);����3ά��ṹ
				ba1 (apply '*(mapcar '- (car bb1)(cadr bb1)));��Χ�����
      )
			(setq ang1 0.0)
			(setq ang0(* 0.25 pi)angs ang0);ת45��
			(foreach n l (vla-rotate n cv ang0))
			(setq bb0 (try-GetBox l);ȡ�ð�Χ��
				ba0 (apply '*(mapcar '- (car bb0)(cadr bb0)));��Χ�����
			)
			(if (> ba0 ba1);�Ի�
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
			);0С�����
			(repeat 2
				(while (> (abs(- ang1 ang0))(* 0.000001 pi))
					(if k (setq angx ang1 k nil);����ѭ��
						(setq angx(* 0.5(+ ang0 ang1)));�м�Ƕ�
					)
					(setq ro(- angx angs)angs angx)
					(foreach n l (vla-rotate n cv ro))
					(setq bbx (try-GetBox l);ȡ�ð�Χ��
						bax (apply '*(mapcar '- (car bbx)(cadr bbx)));��Χ�����
					)
					(if (< bax ba0);�Ի�
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
;;ȡ������е�(��ά)
;;����(try-mid ��1 ��2)
(defun try-mid(p1 p2)(mapcar '(lambda (x y) (* 0.5 (+ x y))) p1 p2))
;;pt��Χ��p0����תang����
(defun try-pt-rotate (pt p0 ang)
(polar p0 (+ (angle p0 pt) ang) (distance pt p0))
)
;;���Χ��p0����תang����
(defun try-pts-rotate(lst p0 ang)
(mapcar '(lambda(x)(try-pt-rotate x p0 ang))lst)
)
;;�����������귵��4��������
(defun try-pt2-to-pt4 (pt1 pt2)
	(list (list (car pt1)(cadr pt1)) (list(car pt1)(cadr pt2))(list (car pt2)(cadr pt2))(list (car pt2)(cadr pt1)))
)




;;================
;ȡ�ö�����С��Χ�У����� 1������(��)/ͼԪ��(��)/ѡ��
;;ע�⣬��ѡ���д���ͼԪʱ�ٶȽ�����1���ͼԪ���ܽӽ�1��
(defun try-getbox (e / en i l max1 min1 pt1 pt2 sn tx1 tx2 ty1 ty2 tye x1 x2 y1 y2)
	(setq tye(type e))
	(cond 
		((= 'VLA-object tye)
			(vla-GetBoundingBox e 'p1 'p2);ȡ�ð���ͼԪ���������С��
			(setq min1 (vlax-safearray->list p1));�ѱ�������ת��Ϊ��
			(setq max1 (vlax-safearray->list p2));�ѱ�������ת��Ϊ��
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
			
			
			(vla-getboundingbox (car e) 'pt1 'pt2);��ȡ����ͼԪ����
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
				(vla-getboundingbox en 'pt1 'pt2);��ȡ����ͼԪ����
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


;;������Ӧ������
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
  