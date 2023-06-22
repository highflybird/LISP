(defun c:mp (/)				;ss e dxf pt1 pt2 a b)
  (setq ss (ssget '((0 . "LINE"))))
  (if ss
    (setq ent (ssname ss 0)
	  dxf (entget ent)
	  pt1 (cdr (assoc 10 dxf))
	  pt2 (cdr (assoc 11 dxf))
    )
  )
  (initget 9)
  (setq W (getdist pt1 "\n距离1："))
  (initget 9)
  (setq H (getdist pt1 "\n距离2："))

  (setq vz (mapcar '- pt2 pt1))
  (setq dx (abs (car vz)))
  (setq dy (abs (cadr vz)))

  (princ (strcat "\ndx is:"
		 (rtos dx)
		 ", dy is:"
		 (rtos dy)
		 ", W  is:"
		 (rtos W)
		 ", H  is:"
		 (rtos H)
	 )
  )

  (setq f (* (/ W H) (/ dy dx)))
  ;(if (> f 1) (setq f (/ 1.0 f)))
     
  (setq k (- f (fix f)))
  (setq k0 k)
  (setq n 1.0)
  (setq x (/ n k))
  (setq d (abs (- (fix x) x)))
  
  (princ "\n此时的k数值是：")
  (princ k)
  (princ "\n此时的x数值是：")
  (princ x)
  (princ "\t此时的d数值是：")
  (princ d)

  (while (not (or (<= d 1e-5) (equal d 1 1e-5)))
    (setq n (1+ n))
    (setq x (/ n k))
    ;(setq d (abs (- (fix x) x)))
    (setq d (abs (- f (/ n (fix x)))))
    (princ "\n此时的x数值是：")
    (princ x)
    (princ "\t此时的d数值是：")
    (princ d)
  )
  ;;m代表重复的次数，那么到了这个次数才可能重合
  (princ "\n最终M是: ")
  (princ (fix x))
  (princ "\n最终N是: ")
  (setq n (fix (* x f)))
  (princ n)
  (princ)
)