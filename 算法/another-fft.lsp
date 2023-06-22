;;;以下代码来自chlh_jd

(defun fourier (pl / xl yl N i j k kl)
  (setq	xl (mapcar 'car pl)
	yl (mapcar 'cadr pl)
	N  (length xl)
	i  0
  )
  (repeat N
    (setq j -1)
    (setq k (apply
	      'mapcar
	      (cons
		'+
		(mapcar	(function
			  (lambda (x y)
			    (setq j (1+ j))
			    (list (+ (* x (cos (/ (* 2. pi i j) N)))
				     (* y (sin (/ (* 2. pi i j) N)))
				  )
				  (- (* y (cos (/ (* 2. pi i j) N)))
				     (* x (sin (/ (* 2. pi i j) N)))
				  )
			    )
			  )
			)
			xl
			yl
		)
	      )
	    )
	  k (* pt k (/ 1. N))
    )
    (setq kl (cons k kl)
	  i  (1+ i)
    )
  )
  (reverse kl)
)

(defun c:ttt (/ pl kl N i j p)
  (setq	pl (LM:Entity->PointList
	     (car (entsel "\nSelect a closed curve :"))
	   )
  )
  (setq kl (fourier pl))
  (princ kl)
  (setq	N  (length pl)
	i  0
	pl nil
  )
  ;; rebuild curve
  (repeat N
    (setq j -1)
    (setq p
	   (apply
	     'mapcar
	     (cons
	       '+
	       (mapcar (function
			 (lambda (k)
			   (setq j (1+ j))
			   (ixi	k
				(list (cos (/ (* 2. pi i j) N))
				      (- (sin (/ (* 2. pi i j) N)))
				)
			   )
			 )
		       )
		       kl
	       )
	     )
	   )
    )
    (setq pl (cons p pl))
    (setq i (1+ i))
  )
  (setq pl (reverse pl))
  (entmakex
    (append
      (list (cons 0 "LWPOLYLINE")
	    (cons 100 "AcDbEntity")
	    (cons 100 "AcDbPolyline")
	    (cons 90 (length pl))
      )
      (mapcar (function (lambda (x) (cons 10 x))) pl)
      (list (cons 70 1) (cons 62 1))
    )
  )
  (princ)
)

;;-------------------------------------------------------------------------
;; &#22797;&#25968;&#30456;&#20056;  Plural Multiplication
(defun ixi (a b)
  (list	(- (* (car a) (car b)) (* (cadr a) (cadr b)))
	(+ (* (car a) (cadr b)) (* (car b) (cadr a)))
  )
)
;;&#27431;&#25289;&#20844;&#24335; Euler's formula &#65306; e^±ix=cosx±isinx  e^i&#960;+1=0
(defun eix (x)
  (list (cos x) (sin x))
)

;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright ?2011 - www.lee-mac.com        ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  ent - Entity for which to return Point List.              ;;
;;------------------------------------------------------------;;
;;  Returns:  List of Points describing/approximating entity  ;;
;;------------------------------------------------------------;;
(defun LM:Entity->PointList (ent / der di1 di2 di3 elst fun inc lst par rad)
  (setq elst (entget ent))
  (cond
    ((eq "POINT" (cdr (assoc 0 elst)))
     (list (cdr (assoc 10 elst)))
    )
    ((eq "LINE" (cdr (assoc 0 elst)))
     (list (cdr (assoc 10 elst)) (cdr (assoc 11 elst)))
    )
    ((member (cdr (assoc 0 elst)) (list "CIRCLE" "ARC"))
     (setq di1 0.0
	   di2 (vlax-curve-getdistatparam ent (vlax-curve-getendparam ent))
	   inc (/ di2
		  (1+ (fix (* 35.0 (/ di2 (cdr (assoc 40 elst)) (+ pi pi)))))
	       )
	   fun (if (vlax-curve-isclosed ent)
		 <
		 <=
	       )
     )
     (while (fun di1 di2)
       (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
	     di1 (+ di1 inc)
       )
     )
     lst
    )
    ((or (eq (cdr (assoc 0 elst)) "LWPOLYLINE")
	 (and (eq (cdr (assoc 0 elst)) "POLYLINE")
	      (zerop (logand (cdr (assoc 70 elst)) 80))
	 )
     )
     (setq par 0)
     (repeat (fix (1+ (vlax-curve-getendparam ent)))
       (if (setq der (vlax-curve-getsecondderiv ent par))
	 (if (equal der (list 0.0 0.0 0.0) 1e-8)
	   (setq lst (cons (vlax-curve-getpointatparam ent par) lst))
	   (if (setq rad (distance (list 0.0 0.0)
				   (vlax-curve-getfirstderiv ent par)
			 )
		     di1 (vlax-curve-getdistatparam ent par)
		     di2 (vlax-curve-getdistatparam ent (1+ par))
	       )
	     (progn
	       (setq inc
		      (/ (- di2 di1)
			 (1+ (fix (* 35.0 (/ (- di2 di1) rad (+ pi pi)))))
		      )
	       )
	       (while (< di1 di2)
		 (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
		       di1 (+ di1 inc)
		 )
	       )
	     )
	   )
	 )
       )
       (setq par (1+ par))
     )
     (if (or (vlax-curve-isclosed ent)
	     (equal (list 0.0 0.0 0.0) der 1e-8)
	 )
       lst
       (cons (vlax-curve-getendpoint ent) lst)
     )
    )
    ((eq (cdr (assoc 0 elst)) "ELLIPSE")
     (setq di1 (vlax-curve-getdistatparam
		 ent
		 (vlax-curve-getstartparam ent)
	       )
	   di2 (vlax-curve-getdistatparam ent (vlax-curve-getendparam ent))
	   di3 (* di2
		  (/ (+ pi pi)
		     (abs (- (vlax-curve-getendparam ent)
			     (vlax-curve-getstartparam ent)
			  )
		     )
		  )
	       )
     )
     (while (< di1 di2)
       (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
	     der (distance (list 0.0 0.0)
			   (vlax-curve-getsecondderiv
			     ent
			     (vlax-curve-getparamatdist ent di1)
			   )
		 )
	     di1 (+ di1 (/ di3 (1+ (fix (/ 35.0 (/ di3 der (+ pi pi)))))))
       )
     )
     (if (vlax-curve-isclosed ent)
       lst
       (cons (vlax-curve-getendpoint ent) lst)
     )
    )
    ((eq (cdr (assoc 0 elst)) "SPLINE")
     (setq di1 (vlax-curve-getdistatparam
		 ent
		 (vlax-curve-getstartparam ent)
	       )
	   di2 (vlax-curve-getdistatparam ent (vlax-curve-getendparam ent))
	   inc (/ di2 25.0)
     )
     (while (< di1 di2)
       (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
	     der (/ (distance (list 0.0 0.0)
			      (vlax-curve-getsecondderiv
				ent
				(vlax-curve-getparamatdist ent di1)
			      )
		    )
		    inc
		 )
	     di1 (+ di1
		    (if	(equal 0.0 der 1e-10)
		      inc
		      (min inc (/ 1.0 der (* 10. inc)))
		    )
		 )
       )
     )
     (if (vlax-curve-isclosed ent)
       lst
       (cons (vlax-curve-getendpoint ent) lst)
     )
    )
  )
)