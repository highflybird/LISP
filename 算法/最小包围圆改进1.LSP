;;;******************************************************
;;; A routine for finding a smallest enclosing circle for
;;; a set of points (SEC problem)                        
;;; command: test                                        
;;; usage:  select some points ,then you will get the SEC
;;;******************************************************
(defun C:test (/ ss pts t0 x cen rad ptmax)
  ;; select the points
  (setq ss (ssget '((0 . "POINT"))))
  (setq pts (ssgetpoint ss))
  ;; get started
  (setq t0 (getvar "TDUSRTIMER"))
  (setq x (mincir pts))
  (princ "\nIt takes")
  (princ (* (- (getvar "TDUSRTIMER") t0) 86400)) 
  (princ "seconds")
  (if (null x)
    (alert "Invalid input!")
    (progn
      (setq cen	  (car x)
	    rad	  (cadr x)
	    ptmax (caddr x)
      )
      ;;draw the smallest circle and the radiu.
      (make-circle cen rad)
      (make-line cen ptmax)
    )
  )
  (princ)
)
;;;*****************************************************
;;;Function : find the SEC with a set of points         
;;;Arguments: pts ,the set of points                    
;;;Return: the center and radius of the SEC             
;;;*****************************************************
(defun mincir (pts / CEN CEN_R P1 P2 P3 PTMAX R rad X i)
  (cond
    ;;where it works
    ( (setq p3 (caddr pts))
      (setq p2 (cadr pts))
      (setq p1 (car pts))   
      (setq cen_r (3pc p1 p2 p3))
      (setq ptmax (maxd-cir pts (car cen_r)))
      (setq i 0)
      (while (null (in1 ptmax (car cen_r) (cadr cen_r)))
        (setq cen_r (4pc p1 p2 p3 ptmax))
        (setq p1 (car (caddr cen_r))
	      p2 (cadr (caddr cen_r))
	      p3 (caddr (caddr cen_r))
        )
        (setq ptmax (maxd-cir pts (car cen_r)))
        (setq i (1+ i))
      )
      (list (car cen_r) (cadr cen_r) ptmax)
    )
    ( (cdr pts) 
      (alert "Two points,radius is the half distance of these points.")
      (setq cen (mid (car pts) (cadr pts))
	    rad (/ (distance (car pts) (cadr pts)) 2)
      )
      (list cen rad (car pts))
    )
    ( (car pts) 
      (alert "One point,radius is zero.")
      (list (car pts) 0 (car pts))
    )
  )
)

;;; the midpoint of two points
(defun mid (p1 p2)
  (list
    (* (+ (car p1) (car p2)) 0.5)
    (* (+ (cadr p1) (cadr p2)) 0.5)
  )
)

;;; whether a point is inside a circle or not
(defun in1 (pt cen r)
  (< (- (distance pt cen) r) 1e-8)
)

;;; whether some points is is inside a circle or not
(defun in2 (ptl cen r / pt)
  (while (and (setq pt (car ptl)) (in1 pt cen r))
    (setq ptl (cdr ptl))
  )
  (null pt)
)

;;判断点集是否在圆内----------------------
;;;改为递归写法
(defun in3 (ptl cen r)
  (cond
    ( (null (cadr ptl))
      (in1 (car ptl) cen r)
    )
    ( (in1 (car ptl) cen r)
      (in3 (cdr ptl) cen r)
    )
  )
)

;;; Function: get the SEC with 3 points
;;; Arguments: pa pb pc ,the 3 points
;;; Return: the SEC for 3 points.
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

;;; Function: Get the circle of a triangle
;;; Arguments: three points of this triangle
;;; Return: the center and radius
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

;;; Function: get the SEC with 4 points
;;; Arguments: pa pb pc ptmax,the 4 points
;;; Return: the SEC for 4 points.
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
    (if	(and (< r minr) (in2 4ps (car n) r))
      (setq mind n
	    minr r
      )
    )
  )
  mind
)

;;; Function: Get the farthest point from a center.
;;; Arguments: ptl,the points
;;;            cen,the center
;;; Return: the farthest point
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

;;; draw a circle
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
;;; draw a line
(defun make-line (p q)
  (entmake
    (list
      '(0 . "LINE")
      (cons 10 p)
      (cons 11 q)
    )
  )
)

;;; Gather the coordinates of these points
(defun ssgetpoint (ss / i l a b c)
  (setq i 0)
  (if ss
    (repeat (sslength ss)
      (setq a (ssname ss i))
      (setq b (entget a))
      (setq c (cdr (assoc 10 b)))
      (setq l (cons c l))
      (setq i (1+ i))
    )
  )
  (reverse l)
)