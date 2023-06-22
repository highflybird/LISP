;;;************************************
;;;����С��ΧԲ��lisp����--------------
;;;���㷨Ϊ�μ����й�����--------------
;;;�����㷨���˻������ص�������Ҳ��ȷ
;;;���г��������Ǻ����㷨�������ĸ��ӳ�
;;;��Ϊȡ�㣬���㣬��Բ�Ͱ뾶����������
;;;************************************
(defun C:test (/ )
  ;;ȡ�㣬���㣬���Ժ�����ʱ����-------
  (setq ss (ssget '((0 . "POINT,LINE,POLYLINE,LWPOLYLINE"))))
  (setq pts (ssgetpoint ss))
  (setq t0 (getvar "TDUSRTIMER"))
  ;;(setq x (mincir pts))
  (setq x (mdesc pts nil))
  (princ "\n��ʱ")
  (princ (* (- (getvar "TDUSRTIMER") t0) 86400)) ;������ʱ
  (princ "��")
  (if (null x)
    (alert "�����Ч��Ŀ̫С������������!")
    (progn
      (setq cen	  (car x)
	    rad   (cdr x)
	    ;;rad	  (cadr x)
	    ;;ptmax (caddr x)
      )
      ;;��Բ���뾶���г�Բ��Բ�İ뾶ֵ
      (make-circle cen rad)
      ;;(make-line cen ptmax)
    )
  )
  (princ)
)
;;;************************************
;;;����С��ΧԲ�ĺ������ռ����ؿռ�����
;;;�򷵻���СԲ��Բ�ģ��뾶��Բ�ϵ�һ��
;;;���ǳ��������----------------------
;;;************************************
(defun mincir (ptlist / CEN CEN_R P1 P2 P3 PTMAX R rad X i)
  ;;�ж���Ч�����---------------------
  (cond
    ((= (length ptlist) 0)
     nil
    )
    ((= (length ptlist) 1)
     (alert "�㼯Ϊһ��,��СԲ�뾶Ϊ0")
     (list (car ptlist) 0 (car ptlist))
    )
    ((= (length ptlist) 2)
     (alert "�㼯Ϊ���㣬��СԲΪ�������Բ")
     (setq cen (mid (car ptlist) (cadr ptlist))
	   rad (/ (distance (car ptlist) (cadr ptlist)) 2)
     )
     (list cen rad (car ptlist))
    )
    (t
     ;;��ʼ�ݹ�����----------------------------
     (setq p1 (car ptlist)
	   p2 (cadr ptlist)
	   p3 (caddr ptlist)
     )
     (setq cen_r (3pc p1 p2 p3))
     (setq ptmax (maxd-cir ptlist (car cen_r)))
     (setq i 0)
     (while (null (in1 ptmax (car cen_r) (cadr cen_r)))
       (setq cen_r (4pc p1 p2 p3 ptmax))
       (setq p1	(car (caddr cen_r))
	     p2	(cadr (caddr cen_r))
	     p3	(caddr (caddr cen_r))
       )
       (setq ptmax (maxd-cir ptlist (car cen_r)))
       (setq i (1+ i))
     )
     (list (car cen_r) (cadr cen_r) ptmax)
    )
  )
)
(defun make-circle (cen rad)
  (entmake
    (list
      '(0 . "circle")
      (cons 10 cen)
      (cons 40 rad)
      (cons 62 1)
    )
  )
)
(defun make-line (p q)
  (entmake
    (list
      '(0 . "LINE")
      (cons 10 p)
      (cons 11 q)
    )
  )
)
;;���´�����������
;;����ȡ�㺯��----
(defun ssgetpoint (ss / i l a b c)
  (setq i 0)
  (if ss
    (repeat (sslength ss)
      (setq a (ssname ss i))
      (setq i (1+ i))
      (setq b (entget a))
      (setq c (cdr (assoc 10 b)))
      (setq l (cons c l))
    )
  )
  (reverse l)
)
;;;
(defun mid (p1 p2)
  (list
    (* (+ (car p1) (car p2)) 0.5)
    (* (+ (cadr p1) (cadr p2)) 0.5)
    (* (+ (caddr p1) (caddr p2)) 0.5)
  )
)
;;;�жϵ��Ƿ���Բ��------------------------
(defun in1 (pt cen r)
  (< (- (distance pt cen) r) 1e-8)
)
;;;�жϵ㼯�Ƿ���Բ��----------------------
(defun in2 (ptl cen r / pts pt)
  (setq pts ptl)
  (while (and (setq pt (car pts))
	      (in1 pt cen r)
	 )
    (setq pts (cdr pts))
  )
  (null pt)
)
;;;����������СԲԲ�ļ���뾶�������������
;;;�Σ�����������Բ�������������ߵ�ֱ��Բ
(defun 3pc (pa pb pc / D MIDPT)
  (cond
    ( (in1 pc (setq midpt (mid pa pb)) (setq d (/ (distance pa pb) 2)))
      (list midpt d (list pa pb pc))
    )
    ( (in1 pa (setq midpt (mid pb pc)) (setq d (/ (distance pb pc) 2)))
      (list midpt d (list pb pc pa))
    )
    ( (in1 pb (setq midpt (mid pc pa)) (setq d (/ (distance pc pa) 2)))
      (list midpt d (list pc pa pb))
    )
    (t
      (3pcircle pa pb pc)
    )
  )
)
;;; ����Բ����
(defun 3PCirCle(P0 P1 P2 / X0 Y0 X1 Y1 X2 Y2 DX1 DY1 DX2 DY2 D 2D C1 C2 CE)
  (setq	X0  (car P0)
	Y0  (cadr P0)
	X1  (car P1)
	Y1  (cadr P1)
	X2  (car P2)
	Y2  (cadr P2)
	DX1 (- X1 X0)
	DY1 (- Y1 Y0)
	DX2 (- X2 X0)
	DY2 (- Y2 Y0)
  )
  (setq D (- (* DX1 DY2) (* DX2 DY1)))
  (if (/= D 0.0)
    (progn
      (setq 2D (+ D D)
	    C1 (+ (* DX1 (+ X0 X1)) (* DY1 (+ Y0 Y1)))
	    C2 (+ (* DX2 (+ X0 X2)) (* DY2 (+ Y0 Y2)))
	    CE (List (/ (- (* C1 DY2) (* C2 DY1)) 2D)
		     (/ (- (* C2 DX1) (* C1 DX2)) 2D)
	       )
      )
      (list CE (distance CE P0) (list p0 p1 p2))
    )
  )
)
;;;�����ĵ����СԲԲ�İ뾶����������������
(defun 4pc (p1 p2 p3 ptmax / pts mind minr r 4ps)
  (setq	pts (list (3pc p1 p2 ptmax)
		  (3pc p1 p3 ptmax)
		  (3pc p2 p3 ptmax)
	    )
  )
  (setq 4ps (list p1 p2 p3 ptmax))
  (setq minr 1e308)
  (foreach n pts
    (setq r (cadr n))
    (if	(and (< r minr)
	     (in2 4ps (car n) r)
	)
      (setq mind n)
    )
  )
  mind
)

;;������㼯����Բ����Զ�ĵ�ĺ���--------
(defun maxd-cir	(ptl cen / pmax dmax d)
  (setq dmax 0.0)
  (foreach pt ptl
    (if	(> (setq d (distance pt cen)) dmax)
      (setq dmax d
	    pmax pt
      )
    )
  )
  pmax
)
;;;���������
(defun Mdesc (pts sup / s p c)
  (if (setq s pts)
    (progn
      ;;(setq p (GetRandomElementOf pts))
      (setq p (car s))
      (setq s (cdr s))
      (setq c (mdesc s sup))
      (if (Inside p c)
	C
	(mdesc s (cons p sup))
      )
    )
    (Circle Sup)
  )
)
;;;���������
(defun Circle (Sup / n p1 p2 p3 CR)
  (setq n (length sup))
  (cond
    ( (= n 1)
      (cons (car sup) 0)
    ) 
    ( (= n 2)
      (setq p1 (car sup))
      (setq p2 (cadr sup))
      (cons (mid p1 p2) (/ (distance p1 p2) 2))
    )
    ( (= n 3)
      (setq p1 (car sup))
      (setq p2 (cadr sup))
      (setq p3 (caddr sup))
      (setq CR (3pCirCle p1 p2 p3))
      (cons (car CR) (cadr CR))
    )
    (t nil) 
  )
)
(defun Inside (p C)
  (if c 
    (< (- (distance p (car C)) (cdr C)) 1e-8)
  )  
)  
      