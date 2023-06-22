
;;; 前段时间编程遇到要将首尾相连的直线从选择集中挑选出来的功能。用region方法虽然能实现但是运行速度较慢，
;;; 明经找的首尾相连函数又不怎么理想，只好自己动手了，没想到还挺复杂，循环套循环的绕的人头晕，现在好像调
;;; 试通过了，纯lisp方法，运行速度还算比较快，发出来共享，给有需要的朋友。

;;; [函数]取得选择集中首尾相连的直线
;;; 函数(lineclose ss p ),ss为直线选择集，
;;; p为真时，返回封闭直线端点列表。
;;; p为假时，返回封闭直线图元名列表。
(defun lineclose (ss p / ent i loop lsar lst lst0 lst00 lst001 lst01 lst1 lstn name pd pt3 pt4 ptn ptsar)
  (setq lst '())
  (repeat (setq i (sslength ss))
    (setq name (ssname ss (setq i (1- i))) ent (entget name))
    (if (= (cdr (assoc 0 ent)) "LINE")
      (setq lst (cons (list name (cdr (assoc 10 ent)) (cdr (assoc 11 ent))) lst))))
  (setq lst0 '()lst01 '())
  (while (setq lsar (car lst))
    (setq lst (cdr lst) lst1 lst ptsar (cadr lsar) ptn (last lsar))
    (setq lst00 (list (last lsar)) lst001 (list (car lsar)) loop t)
    (while loop
      (setq pd nil)
      (repeat (setq i (length lst1))
        (setq lstn (nth (setq i (1- i))        lst1)  pt3 (cadr lstn)  pt4 (last lstn))
        (if (equal pt3 ptn 0.00001)
          (setq ptn pt4        lst00 (cons pt4 lst00)        lst001 (cons (car lstn) lst001)
                lst1 (vl-remove lstn lst1) pd t)
          (if (equal pt4 ptn 0.00001)
            (setq ptn pt3 lst00 (cons pt3 lst00) lst001 (cons (car lstn) lst001)
                  lst1 (vl-remove lstn lst1)  pd t )))
        (if (equal ptn ptsar 0.00001)
           (setq loop nil lst0 (cons lst00 lst0)  lst01 (cons lst001 lst01))))
      (if (= pd nil)(setq loop nil))))
  (if p lst0 lst01 )
)

;;; 示例1：首尾相连直线生成多段线
(defun c:test1 (/ lst lst1 pt ss x)
  (setq ss (ssget (list '(0 . "line"))))
  (setq lst (lineclose ss t))
  (foreach x lst
    (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 90 (length x)) '(70 . 1))
               (mapcar '(lambda (pt) (cons 10 pt)) x ))))
  (princ)
)

;;; 示例2：首尾相连直线亮显
(defun c:test2 (/ lst name ss x)
  (setq ss (ssget (list '(0 . "line"))))
  (setq lst (lineclose ss nil))
  (foreach x lst  (foreach name x (redraw name 3)))
  (princ)
)