(defun C:test (/ olderr en errmsg oce sl ss t0 pp
                 ptlist ptlst1 ptlst2 ppup ppdw)
  ;;�����������Ԥ����
  (setvar "errno" 0)
  (setq olderr *error*)
  (defun *error* (msg)
    (setq en (getvar "errno"))
    (setq errmsg (strcat "errno=" (itoa en) "\nError:" msg))
    (alert errmsg)
    (setq *error* olderr)
  )
  (graphscr)
  (setq oce (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (command ".ucs" "W")
  ;;Ҳ������������ʽȡ�õ㼯
  (setq sl '((0 . "POINT")))
  (setq ss (ssget sl))
  (if (= nil ss)
    (progn
      (alert "������ĵ���Ŀ̫С!")
      (command ".ucs" "p")
      (setvar "cmdecho" oce)
      (princ)
    )
    (progn
      (setq ptlist (getpt ss))
      ;;����͹����ʱ
      (setq t0 (getvar "TDUSRTIMER"))
      ;;����
      (setq ptlist (XYsort ptlist))
      ;;�ְ�
      (setq ptlist (divide ptlist))
      (setq ptlst1 (car ptlist) ptlst2 (cadr ptlist))
      ;;�ֱ������͹������͹��
      (setq ppup (cdr (hull1 ptlst1)))
      (setq ppdw (cdr (hull1 ptlst2)))
      ;;�ϲ�͹��
      (setq pp (append ppup ppdw))
      (princ "\n����͹����ʱ")
      (princ (* (- (getvar "TDUSRTIMER") t0) 86400))
      (princ "��")
      ;;��͹��
      (entmake
	(append
	  '((0 . "lwpolyline")(100 . "AcDbEntity")(100 . "AcDbPolyline"))
	  (list (cons 90 (length pp)))
	  (mapcar '(lambda (x) (cons 10 (list (car x) (cadr x)))) pp)
	  (list (cons 70 1))
	  (list (cons 62 1))
	)
      )
      (command ".ucs" "P")
      (setvar "cmdecho" oce)
      (gc)
      (princ)
    )
  )
)
;;ȡ�㺯��
(defun getpt (ss / i listpp a b c) 
  (setq i 0 listpp nil ) 
  (if ss 
    (repeat (sslength ss) 
      (setq a (ssname ss i)) 
      (setq b (entget a)) 
      (setq c (cdr (assoc 10 b)))
      (setq c (list (car c) (cadr c)))
      (setq listpp (cons c listpp)) 
      (setq i (1+ i))  
    ) 
  ) 
  (reverse listpp)
)
;;����ʸ��֮���,����������ʽֵ֮
(defun det2 (p1 p2)
  (- (* (car p1) (cadr p2)) (* (car p2) (cadr p1)))
)
;;�������������ʽ,������֮�����
(defun det (p1 p2 p3)
  (+ (det2 p1 p2) (det2 p2 p3) (det2 p3 p1))
)	
;;�����б���
(defun judge (p1 p2 p3 / x)
  (setq x (det p1 p2 p3))
  (if (> x 0) t nil)
)
;;����������
(defun XYsort (ptlist)
  (vl-sort ptlist
	   '(lambda (e1 e2)
	      (if (equal (car e1) (car e2) 1e-8)
		(< (cadr e1) (cadr e2))
		(< (car  e1) (car  e2))
	      )
	    )
  )
)
;;;************************************
;;;���������--------------------------
;;;��͹������--------------------------
(defun Hull1 (ptlist / l p1 p2 p3 ppp pp1 pp2) 
  (setq l (length ptlist))
  (if (<= l 3)
    ptlist
    (progn 
      (setq p1 (car  ptlist));;��˵�
      (setq p2 (last ptlist));;�Ҷ˵�
      (setq ppp (mapcar '(lambda (x) (det x p1 p2)) ptlist))
      (setq p3 (nth (vl-position (apply 'max ppp) ppp) ptlist));;��������
      (foreach n ptlist
        (if (and (judge p1 p3 n) (judge p3 n p2))
	  (setq pp1 (cons n pp1))
        )
        (if (and (judge p1 n p3) (judge n p3 p2))
	  (setq pp2 (cons n pp2))
        )
      )
      (setq pp1 (reverse pp1) pp1 (cons p1 pp1) pp1 (append pp1 (list p3)))
      (setq pp2 (reverse pp2) pp2 (cons p3 pp2) pp2 (append pp2 (list p2)))
      (setq pp1 (hull1 pp1) pp2 (hull1 pp2));;�ݹ�(recursion)
      (append pp1 (vl-remove p3 pp2))
    )
  )
)
;;;�ְ�����----------------------------
(defun divide (ptlist / p1 p2 ptlist1 ptlist2)
  (setq p1 (car ptlist))
  (setq p2 (last ptlist))
  (setq	ptlist1
    (vl-remove-if
      (function (lambda (x)(< (- (angle p2 p1) (angle p2 x)) 0))) ptlist
    )
  )
  (setq	ptlist2
    (vl-remove-if
      (function (lambda (x)(> (- (angle p2 p1) (angle p2 x)) 0))) ptlist
    )
  )
  (setq ptlist1 (append (cons p1 ptlist1) (list p2)))
  (setq ptlist2 (append (cons p1 ptlist2) (list p2)))
  (list ptlist1 (reverse ptlist2))
)
;;;���ν���****************************
;;;************************************
