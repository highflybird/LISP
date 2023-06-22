;;;************************************************************************
;;;һ����㼯�ϵ�͹����lisp����--------------------------------------------
;;;------���õ��㷨Ϊ��Ʒ������--------------------------------------------
;;;����Ϊ���Ҷ˵ĵ㿪ʼ�������õ���Ϊ͹���߽�ĵ�һ����P1��������Ĵ�ֱ��
;;;������P1˳ʱ����ת��ֱ��������һ��P2�����͹���߽�ĵڶ�����P2����������
;;;p2���p3......ֱ�������»ص�p1���Ѿ������˸����˻�����͸������㣬���㷨
;;;ʱ�䲻����O(n.h),����h��͹���ĸ��Ӷȣ�ʱ�仹�Ǻܿ�ġ���Ҳ�����֤��    
;;;�ο�����<<���㼸��-�㷨����Ӧ��>>(�ڶ���),�Լ��ο���������վ��һЩԴ����
;;;------------------------------------------------------------------------
;;;���г��������Ǻ����㷨�������ĸ��ӳ���Ϊȡ�õ㼯����͹���߽��ߣ����Դ���
;;;�㼯�������������ѵ�ʱ�䡣----------------------------------------------
;;;�÷�: ����lisp����testѡȡ�㣬ֱ�߶Σ��������(ȫ��ֱ�߶����)���ɡ�----
;;;************************************************************************
(defun C:test (/ olderr en errmsg oldmode oce sl ss t0 ptlist pp)
  ;;�����������Ԥ����--------------------
  (setvar "errno" 0)
  (setq olderr *error*)
  (defun *error* (msg)
    (setq en (getvar "errno"))
    (setq errmsg (strcat "errno=" (itoa en) "\nError:" msg))
    (alert errmsg)
    (setq *error* olderr)
  )
  (graphscr)
  (setq oldmode (getvar "osmode"))
  (setq oce (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (command ".ucs" "W")
  ;;Ҳ������������ʽȡ�õ㼯----------------
  ;;ȡ�㣬���ߣ����Ժ�����ʱ����------------
  (setq	sl '((-4 . "<OR")
	     (0 . "POINT")
	     (0 . "LINE")
	     (0 . "POLYLINE")
	     (0 . "LWPOLYLINE")
	     (-4 . "OR>")
	    )
  )
  (setq ss (ssget sl))
  (setq ptlist (getpt ss))
  (setq t0 (getvar "TDUSRTIMER"))
  (setq pp (hull ptlist))
  (princ "\n��ʱ")
  (princ (* (- (getvar "TDUSRTIMER") t0) 86400))
  (princ "��")
  (if (= nil pp)
    (progn
      (alert "�����Ч��Ŀ̫С������������!")
      (command ".ucs" "p")
      (setvar "osmode" oldmode)
      (setvar "cmdecho" oce)
      (princ)
    )
    (progn
      ;;��͹���߽���------------------------
      (setvar "osmode" 0)
      (entmake
	(append
	  '((0 . "lwpolyline")
	    (100 . "AcDbEntity")
	    (100 . "AcDbPolyline")
	   )
	  (list (cons 90 (length pp)))
	  (mapcar '(lambda (x) (cons 10 (list (car x) (cadr x)))) pp)
	  (list (cons 70 1))
	  (list (cons 62 1))
	)
      )
      (command ".ucs" "P")
      (setvar "osmode" oldmode)
      (setvar "cmdecho" oce)
      (princ)
    )
  )
)
;;;*****************************************
;;;*****************************************
;;;�������Σ����Ե�����Ϊ����---------------
(defun hull (ptlist / pfirst p0 p1 p2 pp)
  (cond
    ( (= (length ptlist) 0)
      nil
    )
    ( (or nil (= (length ptlist) 1) (= (length ptlist) 2))
      (progn
        (alert "������ĵ�Ϊ�����һ��!")
        ptlist
      )
    )
    ( t
      (progn
        ;;����--------------------------------
        (setq pfirst (maxium ptlist))
        (setq p1 pfirst
	      p0 (list (car pfirst) (+ 1.0 (cadr pfirst)))
        )
        (setq p2 (angmax ptlist p0 p1))
        (setq pp (cons p2 (list p1)))
        (while (not (equal pfirst p2 1e-8))
	  (setq pp (cons p2 pp))
	  (setq p0 p1
	        p1 p2
	        p2 (angmax ptlist p0 p1)
	  )
        )pp     
      )
    )
  )
)
;;;�������ν���-----------------------------
;;;*****************************************
;;;*****************************************
;;����������վ�Ĵ����д���ɵ�ȡ�㺯��------
(defun getpt (ss / i listpp a b c d)
  (setq	i 0
	listpp nil
  )
  (if ss
    (repeat (sslength ss)
      (setq a (ssname ss i))
      (setq b (entget a))
      (setq ename (cdr (assoc 0 b)))
      (cond
	((or nil (= ename "POLYLINE") (= ename "LWPOLYLINE"))
	 (progn
	   (setq c (xdl-pl-vertexs a))
	   (setq listpp (append c listpp))
	 )
	)
	((= ename "LINE")
	 (progn
	   (setq c (cdr (assoc 10 b)))
	   (setq d (cdr (assoc 11 b)))
	   (setq listpp (cons c listpp))
	   (setq listpp (cons d listpp))
	 )
	)
	((= ename "POINT")
	 (progn
	   (setq c (cdr (assoc 10 b)))
	   (setq listpp (cons c listpp))
	 )
	)
      )
      (setq i (1+ i))
    )
  )
  listpp
)
;;����˳ʱ�뷽��ļн�Ϊ��ֵ����֮Ϊ��
(defun ang (p1 p0 p2 / x)
  (setq x (- (angle p1 p2) (angle p1 p0)))
  (cond
    ((equal p1 p2 1e-8) 0)
    ((<= (abs (- x 1e-8)) Pi) x)
    (t (- x (* (/ x (abs x)) 2 Pi)))
  )
)
;;��㼯��˳ʱ�뷽��ļнǵ����ֵ�ĵ�
(defun angmax (ptlist p0 p1 / ppp)
  (setq ppp (mapcar '(lambda (x) (ang p1 p0 x)) ptlist))
  (nth (vl-position (apply 'max ppp) ppp) ptlist)
)
;;������----------------------------
(defun maxium (ptlist)
  (car
    (vl-sort ptlist
	     '(lambda (e1 e2)
		(if (equal (car e1) (car e2) 1e-8)
		  (> (cadr e1) (cadr e2))
		  (> (car e1) (car e2))
		)
	      )
    )
  )
)
;;ȡ�ö���ζ���------------------��лeachy!
(defun xdl-pl-vertexs (e / n lst)
  (if (= e nil)
    nil
    (progn
      (setq lst
	     (repeat (setq n (fix (1+ (vlax-curve-getendparam e))))
	       (setq lst
		      (cons (vlax-curve-getpointatparam e (setq n (1- n))) lst)
	       )
	     )
      )
      (if (= 0 (cdr (assoc 70 (entget e))))
	lst
	(cdr lst)
      )
    )
  )
)