;;;************************************************************************
;;;一个求点集合的凸包的lisp程序--------------------------------------------
;;;采用的算法为Graham扫描法,具体方法见注释---------------------------------
;;;参考文献<<计算几何-算法及其应用>>(第二版),以及参考了其他网站的一些源代码
;;;用法: 加载运行程序后，选取点，直线段，或多义线(全是直线段组成)即可。----
;;;************************************************************************
(defun C:test1 (/ fil sel t0 ptlist pp 2Pi)
  (setq fil '((0 . "POINT,LINE,POLYLINE,LWPOLYLINE")))
  (setq sel (ssget fil))                ;选择点集
  (setq ptlist (getpt sel))             ;构造点集
  (setq t0 (getvar "TDUSRTIMER"))       ;开始计时
  (setq pp (Graham-scan ptlist))        ;求凸包
  (princ "\n用时")
  (princ (* (- (getvar "TDUSRTIMER") t0) 86400)) ;结束计时
  (princ "秒")
  (if (null pp)
    (alert "点的有效数目太小，请重新输入!")
    (MAKE-POLY PP)
  )
  (gc)
  (princ)
)
;;;==========================
;;;程序主段，可以单独成为函数
;;;==========================
(defun Graham-scan (ptlist / hullpt maxXpt sortPt P Q)
  (if (< (length ptlist) 3)             ;3点以下
    ptlist                              ;是本集合
    (progn
      (setq maxXpt (assoc (apply 'max (mapcar 'car ptlist)) ptlist))    ;最右边的点
      (setq sortPt (sort-by-angle-distance ptlist maxXpt))              ;分类点集
      (setq hullPt (list (cadr sortPt) maxXpt))                         ;开始的两点      
      (foreach n (cddr sortPt)                                          ;从第3点开始
        (setq hullPt (cons n HullPt))                                   ;把Pi加入到凸集
        (setq P (cadr hullPt))                                          ;Pi-1
        (setq Q (caddr hullPt))                                         ;Pi-2
        (while (and q (> (det n P Q) -1e-6))                            ;如果左转
          (setq hullPt (cons n (cddr hullPt)))                          ;删除Pi-1点
          (setq P (cadr hullPt))                                        ;得到新的Pi-1点
          (setq Q (caddr hullPt))                                       ;得到新的Pi-2点
        )
      )
      (reverse hullpt)                                                  ;返回凸集
    )
  )
)
;;;以最下面的点为基点，按照角度和距离分类点集
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
;;定义三点的行列式,即三点之倍面积
(defun det (p1 p2 p3 / x2 y2)
  (setq x2 (car p2)
        y2 (cadr p2)
  )
  (- (* (- x2 (car p3)) (- y2 (cadr p1)))
     (* (- x2 (car p1)) (- y2 (cadr p3)))
  )
)
;;;============
;;;程序主段结束
;;;============

;;;取点函数1
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
;;;取点函数2
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
  (entmake                              ;画凸包
    (append
      '((0 . "LWPOLYLINE")
        (100 . "AcDbEntity")
        (100 . "AcDbPolyline")
       )
      (list (cons 90 (length pp)))      ;顶点个数
      (mapcar
        (function (lambda (x) (cons 10 x)))
        pp
      )                                 ;多段线顶点
      (list (cons 70 1))                ;闭合的
      (list (cons 62 1))                ;红色的
    )
  )
)
;;取得多边形顶点
(defun get-LWpolyline-vertexs (entlst / n lst)
  (foreach n entlst
    (if (= (car n) 10)
      (setq lst (cons (cdr n) lst))
    )
  )
  (reverse lst)
)
