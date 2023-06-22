 
 ;_2004.01.09.11.40___________________________________
 ;_曲线间最小距离及最近点__BY__WKAI__晓东CAD论坛__
 ;________________________________________________________
 ;________________________________________________________
(defun c:tt (/ pts min_distacne vlo1 vlo2 st_par ed_par ent n m min_pt str)
  (setvar "osmode" 0)
  (setq n (getint "\n叠代次数:"))
  (if n
    ()
    (setq n 2)
  )
  (setq	m   1000
	str ""
  )
  (setq vlo_object1 (vlax-ename->vla-object (car (entsel "\n曲线一："))))
  (setq vlo_object2 (vlax-ename->vla-object (car (entsel "\n曲线二："))))
  (setq tt1 (t2 vlo_object1 vlo_object2 n m))
  (setq tt2 (t2 vlo_object2 vlo_object1 n m))
  (setq	tt (if (> (car tt1) (car tt2))
	     tt2
	     tt1
	   )
  )
  (command "ucs" "w")
  (command "line" (cadr tt) (caddr tt) "")
  (command "ucs" "p")
  (princ (strcat "\n叠代次数为" (itoa n) ",最小距离为" (rtos (car tt) 2 20)))
  (princ "\n最近点1:")
  (princ (cadr tt))
  (princ "   最近点2:")
  (princ (caddr tt))
  (princ)
)
;________________________________________________________________________
;________________________________________________________________________
;________________________________________________________________________
(defun t2 (vlo1 vlo2 rep rod /)
  (setq	st_par	     (vlax-curve-getstartparam vlo1)
	ed_par	     (vlax-curve-getendparam vlo1)
	pts	     (list (vlax-curve-getstartpoint vlo1))
	min_distacne (/ (vlax-curve-getdistatparam vlo1 ed_par) rod)
  )
  (repeat rep
    (setq pts (list (vlax-curve-getpointatparam vlo1 st_par)))
    (t1 vlo1 st_par ed_par min_distacne)
    (setq min_pt (car (vl-sort pts
			       '(lambda	(s1 s2)
				  (< (distance s1 (vlax-curve-getClosestPointTo vlo2 s1))
				     (distance s2 (vlax-curve-getClosestPointTo vlo2 s2))
				  )
				)
		      )
		 )
    )
    (setq position (vl-position min_pt pts))
    (if	(= (1+ position) (length pts))
      (setq ed_par (vlax-curve-getendparam vlo1))
      (setq ed_par (if (vlax-curve-getparamatpoint vlo1 (nth (1+ position) pts))
		     (vlax-curve-getparamatpoint vlo1 (nth (1+ position) pts))
		     (vlax-curve-getendparam vlo1)
		   )
      )
    )
    (if	(= position 0)
      (setq st_par (vlax-curve-getstartparam vlo1))
      (setq st_par (vlax-curve-getparamatpoint vlo1 (nth (1- position) pts)))
    )
    (setq min_distacne
	   (/ (- (vlax-curve-getdistatparam vlo1 ed_par)
		 (vlax-curve-getdistatparam vlo1 st_par)
	      )
	      rod
	   )
    )
  )
  (list	(distance min_pt (vlax-curve-getClosestPointTo vlo2 min_pt))
	min_pt
	(vlax-curve-getClosestPointTo vlo2 min_pt)
  )
)
;________________________________________________________________________
;________________________________________________________________________
(defun t1 (vl_obj start_par end_par min_dis / dis_m_m ed_p ed_pt mid_p mid_pt mid_pt_vlo st_p st_pt)
  (setq	st_p	   start_par
	ed_p	   end_par
	st_pt	   (vlax-curve-getpointatparam vl_obj st_p)
	ed_pt	   (if (vlax-curve-getpointatparam vl_obj ed_p)
		     (vlax-curve-getpointatparam vl_obj ed_p)
		     (vlax-curve-getendpoint vl_obj)
		   )
	mid_pt	   (mapcar '(lambda (x y) (/ (+ x y) 2.0)) st_pt ed_pt)
	mid_pt_vlo (vlax-curve-getpointatparam vl_obj (/ (+ st_p ed_p) 2.0))
	mid_p	   (vlax-curve-getparamatpoint vl_obj mid_pt_vlo)
	dis_m_m	   (distance mid_pt_vlo mid_pt)
  )
  (if (> dis_m_m min_distacne)
    (progn
      (t1 vl_obj st_p mid_p min_dis)
      (t1 vl_obj mid_p ed_p min_dis)
    )
    (setq pts (append pts (list mid_pt_vlo) (list ed_pt)))
  )
)
;________________________________________________________________________
;________________________________________________________________________
;________________________________________________________________________
(princ "\n两条曲线最近距离及最近点__BY__WKAI__晓东CAD论坛")
(princ "\n命令:tt")
