;;;************************************************************************
;;;һ����㼯�ϵ�͹����lisp����--------------------------------------------
;;;���õ��㷨ΪGrahamɨ�跨,���巽����ע��---------------------------------
;;;�ο�����<<���㼸��-�㷨����Ӧ��>>(�ڶ���),�Լ��ο���������վ��һЩԴ����
;;;�÷�: �������г����ѡȡ�㣬ֱ�߶Σ��������(ȫ��ֱ�߶����)���ɡ�----
;;;************************************************************************
(defun C:test1 (/ fil sel t0 ptlist pp 2Pi)
  (setq fil '((0 . "POINT,LINE,POLYLINE,LWPOLYLINE")))
  (setq sel (ssget fil))                ;ѡ��㼯
  (setq ptlist (getpt sel))             ;����㼯
  (setq t0 (getvar "TDUSRTIMER"))       ;��ʼ��ʱ
  (setq pp (Graham-scan ptlist))        ;��͹��
  (princ "\n��ʱ")
  (princ (* (- (getvar "TDUSRTIMER") t0) 86400)) ;������ʱ
  (princ "��")
  (if (null pp)
    (alert "�����Ч��Ŀ̫С������������!")
    (MAKE-POLY PP)
  )
  (gc)
  (princ)
)
;;;==========================
;;;�������Σ����Ե�����Ϊ����
;;;==========================
(defun Graham-scan (ptlist / hullpt maxXpt sortPt P Q)
  (if (< (length ptlist) 3)             ;3������
    ptlist                              ;�Ǳ�����
    (progn
      (setq maxXpt (assoc (apply 'max (mapcar 'car ptlist)) ptlist))    ;���ұߵĵ�
      (setq sortPt (sort-by-angle-distance ptlist maxXpt))              ;����㼯
      (setq hullPt (list (cadr sortPt) maxXpt))                         ;��ʼ������      
      (foreach n (cddr sortPt)                                          ;�ӵ�3�㿪ʼ
        (setq hullPt (cons n HullPt))                                   ;��Pi���뵽͹��
        (setq P (cadr hullPt))                                          ;Pi-1
        (setq Q (caddr hullPt))                                         ;Pi-2
        (while (and q (> (det n P Q) -1e-6))                            ;�����ת
          (setq hullPt (cons n (cddr hullPt)))                          ;ɾ��Pi-1��
          (setq P (cadr hullPt))                                        ;�õ��µ�Pi-1��
          (setq Q (caddr hullPt))                                       ;�õ��µ�Pi-2��
        )
      )
      (reverse hullpt)                                                  ;����͹��
    )
  )
)
;;;��������ĵ�Ϊ���㣬���սǶȺ;������㼯
(defun sort-by-angle-distance (ptlist pt / Ang1 Ang2)
  (vl-sort ptlist
           (function
             (lambda (e1 e2)
               (setq ang1 (angle pt e1))
               (setq ang2 (angle pt e2))
               (if (= ang1 ang2)
                 (< (distance pt e1) (distance pt e2))
                 (< ang1 ang2)
               )
             )
           )
  )
)
(defun sort-by-angle (ptlist pt / Ang1 Ang2)
  (vl-sort ptlist
           (function
             (lambda (e1 e2) (< (angle pt e1) (angle pt e2)))
           )
  )
)
(defun sort-XY (ptlist)
  (vl-sort ptlist
           (function
             (lambda (e1 e2)
               (if (equal (cadr e1) (cadr e2) 1e-8)
                 (> (car e1) (car e2))
                 (< (cadr e1) (cadr e2))
               )
             )
           )
  )
)
;;�������������ʽ,������֮�����
(defun det (p1 p2 p3 / x2 y2)
  (setq x2 (car p2)
        y2 (cadr p2)
  )
  (- (* (- x2 (car p3)) (- y2 (cadr p1)))
     (* (- x2 (car p1)) (- y2 (cadr p3)))
  )
)
;;;============
;;;�������ν���
;;;============

;;;ȡ�㺯��1
(defun getpt1 (ss / i listpp a b c d)
  (setq i 0)
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
  listpp
)
;;;ȡ�㺯��2
(defun getpt (ss / i listpp a b c d)
  (setq i 0)
  (if ss
    (repeat (sslength ss)
      (setq a (ssname ss i))
      (setq b (entget a))
      (setq ename (cdr (assoc 0 b)))
      (cond
        ((= ename "LWPOLYLINE")
         (setq c (get-LWpolyline-vertexs b))
         (setq listpp (append c listpp))
        )
        ((= ename "LINE")
         (setq c (cdr (assoc 10 b)))
         (setq d (cdr (assoc 11 b)))
         (setq c (list (car c) (cadr c)))
         (setq d (list (car d) (cadr d)))
         (setq listpp (cons c listpp))
         (setq listpp (cons d listpp))
        )
        ((= ename "POINT")
         (setq c (cdr (assoc 10 b)))
         (setq c (list (car c) (cadr c)))
         (setq listpp (cons c listpp))
        )
      )
      (setq i (1+ i))
    )
  )
  listpp
)
(DEFUN make-poly (pp / X)
  (entmake                              ;��͹��
    (append
      '((0 . "LWPOLYLINE")
        (100 . "AcDbEntity")
        (100 . "AcDbPolyline")
       )
      (list (cons 90 (length pp)))      ;�������
      (mapcar
        (function (lambda (x) (cons 10 x)))
        pp
      )                                 ;����߶���
      (list (cons 70 1))                ;�պϵ�
      (list (cons 62 1))                ;��ɫ��
    )
  )
)
;;ȡ�ö���ζ���
(defun get-LWpolyline-vertexs (entlst / n lst)
  (foreach n entlst
    (if (= (car n) 10)
      (setq lst (cons (cdr n) lst))
    )
  )
  (reverse lst)
)
