;;;Minimization or Maximization of Functions
;;;Executive routine
;;;I got the thought from the book "NUMERICAL RECIPES",translated some C++ codes to AutoLISP.
(vl-load-com)
;;(arxload "geomcal.arx")
(prompt "\nPlease enter:Min.")
(defun C:Min (/ F ID N OK X1 X2 file wcs)
  (setq id (load_dialog (setq Dcl_File (Write_Dcl))))		;load DCL
  (vl-file-delete Dcl_File)					;delete temp dcl after load it.
  (setq ok 2)
  (if (setq file (findfile "mycal.lsp"))
    (load file)
  )
  (if (new_dialog "dcl_Minimization" id)
    (progn
      (action_tile "F" "(setq f $value)") 			;Get the expression
      (action_tile "X1" "(setq x1 (CAL:EvalExpress $value))")	;Get the lower bound
      (action_tile "X2" "(setq x2 (CAL:EvalExpress $value))")	;Get the upper bound
      (action_tile "N" "(setq n (CAL:EvalExpress $value))") 	;Get the pression
      (action_tile "help" "(choose 1)")				;help 
      (action_tile "S1" "(Read_DLg_Data x1 x2 n f \"S1\" wcs)")
      (action_tile "S2" "(Read_DLg_Data x1 x2 n f \"S2\" wcs)")
      (setq ok (start_dialog))
    )
    (alert "Can't load the dialoag!")
  )
  (unload_dialog ID)
  (princ)
)
(defun StrReplace (newstr pattern string / string1)
  (if (/= string (setq string1 (vl-string-subst newstr pattern string)))
    (StrReplace newstr pattern string1)
    string1
  )
)
  

;;;Read data and solve it.
(defun Read_DLg_Data (x1 x2 n f key wcs / EPS RET T0 E str)
  (if (and x1 x2 n f)
    (progn
      (if (> n 20)
	(setq n 20)
      )
      (setq e (exp 1))
      (setq f (strReplace "x" "X" f))
      (CAL:Expr2Func f 'func1 '(x))
      (defun func (x) (list (func1 x) x))
      (setq eps (expt 0.1 n))
      (setq t0 (getvar "TDUSRTIMER"))
      (setq ub (mnbrak func x1 x2 eps))
      (setq ax (caar ub))
      (setq bx (caadr ub))
      (setq cx (caaddr ub))
      (cond
	((= key "S1")
	 (setq ret (Brent func ax bx cx eps))
	 (setq str (strcat  "When Xmin="
			    (rtos (car ret) 2 20)
			    ",Ymin="
			    (rtos (caadr ret) 2 20)
		   )
	 )	
	 (set_tile "R1" str)
	 (princ "\nThe solution Used by Brent's Method is:\n")
	)
	((= key "S2")
	 (setq ret (Golden func ax bx cx eps))
	 (setq str (strcat  "When Xmin="
			    (rtos (car ret) 2 20)
			    ",Ymin="
			    (rtos (caadr ret) 2 20)
		   )
	 )	
	 (set_tile "R2" str)
	 (princ "\nThe solution Used by Gold Section Search is:\n")
	)
      )
      (princ str)
      (princ "\nIt takes:")
      (princ (* (- (getvar "TDUSRTIMER") t0) 86400))
      (princ "seconds.")
      (princ)
    )
    (alert "Illegal expression or invalid input!")
  )
)

;;;help and instruction
(defun choose (n)
  (if (= n 1)
    (alert
      "Standard expression only accepts \"x\" as a variale!
           \nYou can use two ways to get  a minimum value of a function.
      	   \nThe better way is Brent's method,but it may be a little slower.
           \nActually, you can use this routine for get maximum value of a function.
           \nAlso You can replace the Func(x) with a customize function,even for CAD.
           \nIt won't work very well on some circumstances.
	   \nIt's an Open Source Software. Thanks for your advice or bug reports.
	   \nAuthor: highflybird  Email: highflybird@qq.com   Date:2010.09."
    )
    (set_tile "error" "Illegal expression or invalid input.")
  )
)
;;;带return的apply
(defun Fsxm-Apply ($Sym $Lst / $$ return $rt)
  (defun Return (var) (setq Return nil) (setq $$ var) (exit))
  (setq $rt (vl-catch-all-apply $Sym $Lst))
  (if Return
    $rt
    $$
  )
)
;;;符号判断
(defun sign (a b)
  (if (>= b 0)
    (abs a)
    (- (abs a))
  )
)
;;;确定搜索区间
(defun mnbrak (func a b tol / AX BX CX DUM FA FB FC FU GLIMIT GOLD I	Q R TINY U ULIM)
  (setq TINY 1e-20)
  (setq GLIMIT 100)
  (setq GOLD 1.618)
  (setq ax a)
  (setq bx b)
  (setq fa (func ax))
  (setq fb (func bx))
  (if (> (car fb) (car fa))
    (setq dum ax
	  ax  bx
	  bx  dum
	  dum fb
	  fb  fa
	  fa  dum
    )
  )
  (setq cx (+ bx (* GOLD (- bx ax))))
  (setq fc (func cx))
  (setq i 0)
  (while (and (> (car fb) (car fc)) (< i 500))
    (setq r (* (- bx ax) (- (car fb) (car fc))))
    (setq q (* (- bx cx) (- (car fb) (car fa))))
    (setq u (- bx
	       (/ (- (* (- bx cx) q) (* (- bx ax) r))
		  (* 2 (sign (max (abs (- q r)) TINY) (- q r)))
	       )
	    )
    )
    (setq ulim (+ bx (* GLIMIT (- cx bx))))
    (if	(> (* (- bx u) (- u cx)) 0.0)
      (progn
	(setq fu (func u))
	(if (< (car fu) (car fc))
	  (progn
	    (setq ax bx
		  bx u
		  fa fb
		  fb fu
	    )
	    (return
	      (list
		(list ax fa)
		(list bx fb)
		(list cx fc)
	      )
	    )
	  )
	  (if (> (car fu) (car fb))
	    (progn
	      (setq cx u
		    fc fu
	      )
	      (return
		(list
		  (list ax fa)
		  (list bx fb)
		  (list cx fc)
		)
	      )
	    )
	  )
	)
	(setq u  (+ cx (* GOLD (- cx bx)))
	      fu (func u)
	)
      )
      (if (> (* (- cx u) (- u ulim)) 0.0)
	(progn
	  (setq fu (func u))
	  (if (< (car fu) (car fc))
	    (setq bx cx
		  cx u
		  u  (+ u (* GOLD (- u cx)))
		  fb fc
		  fc fu
		  fu (func u)
	    )
	  )
	)
	(if (>= (* (- u ulim) (- ulim cx)) 0.0)
	  (setq	u  ulim
		fu (func u)
	  )
	  (setq	u  (+ cx (* GOLD (- cx bx)))
		fu (func u)
	  )
	)
      )
    )
    (setq ax bx
	  bx cx
	  cx u
	  fa fb
	  fb fc
	  fc fu
    )
    (setq i (1+ i))
  )
  (list	(list ax fa)
	(list bx fb)
	(list cx fc)
  )
)
;;;一维Brent方法求函数极小值
;;;Brent's mothed in One Dimension
(defun Brent (func ax   bx	cx   tol  /    A    B	 CGOLD	   D	E
	      ETEMP	FU   FV	  FW   FX   ITER ITMAX	   P	Q
	      R	   TOL1	TOL2 U	  V    W    X	 XM   XM-X XMIN	ZEPS
	     )
  (setq ITMAX 100)
  (setq CGOLD 0.3819660)
  (setq ZEPS 1e-10)
  (setq d 0.0)
  (setq e 0.0)
  (setq a (min ax cx))
  (setq b (max ax cx))

  (setq	v bx
	w v
	x w
  )
  (setq	fx (func x)
	fv fx
	fw fv
  )
  (setq iter 0)
  (while (< iter ITMAX)
    (setq xm (* 0.5 (+ a b)))
    (setq tol1 (+ (* (abs x) tol) ZEPS))
    (setq tol2 (+ tol1 tol1))
    (if	(<= (abs (- x xm)) (- tol2 (* 0.5 (- b a))))
      (setq xmin x
	    iter iTMAX
      )
      (progn
	(if (> (abs e) tol1)
	  (progn
	    (setq r (* (- x w) (- (car fx) (car fv))))
	    (setq q (* (- x v) (- (car fx) (car fw))))
	    (setq p (- (* (- x v) q) (* (- x w) r)))
	    (setq q (* 2.0 (- q r)))
	    (if (> q 0.0)
	      (setq p (- p))
	    )
	    (setq q (abs q))
	    (setq etemp e)
	    (setq e d)
	    (if	(or (>= (abs p) (abs (* 0.5 q etemp)))
		    (<= p (* q (- a x)))
		    (>= p (* q (- b x)))
		)
	      (setq e (if (>= x xm)
			(- a x)
			(- b x)
		      )
		    d (* CGOLD e)
	      )
	      ;;上述条件决定抛物线内插是否合适。在较长区间段内采用黄金分割法
	      (progn
		(setq d	(/ p q)
		      u	(+ x d)
		)
		(if (or	(< (- u a) tol2)
			(< (- b u) tol2)
		    )
		  (setq d (sign tol1 (- xm x)))
		)
	      )
	    )
	  )
	  (setq	e (if (>= x xm)
		    (- a x)
		    (- b x)
		  )
		d (* CGOLD e)
	  )
	)
	(setq u	 (if (>= (abs d) tol1)
		   (+ x d)
		   (+ x (sign tol1 d))
		 )
	      fu (func u)
	)
	;;每次迭代在此计算一次函数值
	(if (<= (car fu) (car fx))
	  (progn
	    (if	(>= u x)
	      (setq a x)
	      (setq b x)
	    )
	    (setq v  w
		  w  x
		  x  u
		  fv fw
		  fw fx
		  fx fu
	    )
	  )
	  (progn
	    (if	(< u x)
	      (setq a u)
	      (setq b u)
	    )
	    (if	(or (<= (car fu) (car fw)) (= w x))
	      (setq v  w
		    w  u
		    fv fw
		    fw fu
	      )
	      (if (or (<= (car fu) (car fv)) (= v x) (= v w))
		(setq v	 u
		      fv fu
		)
	      )
	    )
	  )
	)
      )
    )
    (setq iter (1+ iter))
  )
  (list xmin fx)
)

;;;黄金搜索法求函数的极小值
;;;Golden Section Search in One Dimension
(defun Golden (func ax bx cx tol / C F1 F2 I R RET X0 X1 X2 X3)
  (setq R (- (sqrt 1.25) 0.5))
  (setq C (- 1.5 (sqrt 1.25)))
  (setq x0 ax)
  (setq x3 cx)
  (if (> (abs (- bx cx)) (abs (- bx ax)))
    (setq x1 bx
	  x2 (+ bx (* C (- cx bx)))
    )
    (setq x2 bx
	  x1 (- bx (* C (- bx ax)))
    )
  )
  (setq f1 (func x1))
  (setq f2 (func x2))
  (setq i 0)
  (while (and (> (abs (- x3 x0)) (* tol (+ (abs x1) (abs x2))))
	      (< i 500)
	 )
    (if	(< (car f2) (car f1))
      (setq x0 x1
	    x1 x2
	    x2 (+ (* R x1) (* C x3))
	    f1 f2
	    f2 (func x2)
      )
      (setq x3 x2
	    x2 x1
	    x1 (+ (* R x2) (* C x0))
	    f2 f1
	    f1 (func x1)
      )
    )
    (if	(< (car f1) (car f2))
      (setq ret (List x1 f1))
      (setq ret (list x2 f2))
    )
    (setq i (1+ i))
  )
  ret
)

;;; for DCL
(defun Write_Dcl (/ Dcl_File file str)
  (setq Dcl_File (vl-filename-mktemp nil nil ".Dcl"))
  (setq file (open Dcl_File "w"))
  (foreach str
	   '(
	     "//Minimization.dcl
  dcl_Minimization : dialog {
  label = \"Minimization of Functions\";
  : boxed_column {
    : row {
      : edit_box {
        key=\"F\";
        label= \"Expression:\";
      }
    }
    : row {
      : edit_box {
        key=\"X1\";
        label= \"Lower bound:\";
        edit_width = 10;
      }
      : edit_box {
        key=\"X2\";
        label= \"Upper bound:\";
        edit_width = 10;
      }      
      : edit_box {
        key=\"N\";
        label= \"Precision digits:\";
        edit_width = 2;
      }
    }
    spacer_1;
  }
  : boxed_column {
    :row {
      : button {
        key = \"S1\";
        label = \"Brent's Method\";
        width = 25;
        fixed_width = true;
      }
      : text {
        key = \"R1\";
        alignment = left;
        width = 60;
        fixed_width = true;
      }
    }
    :row {
      : button {
        key = \"S2\";
        label = \"Golden Search\";
        width = 25;
        fixed_width = true;
      }
      : text {
        key = \"R2\";
        alignment = left;
        width = 60;
        fixed_width = true;
      }
    }
    spacer_1;
  }
  ok_cancel_help;
  //errtile;
  }  "
	    )
    (write-line str file)
  )
  (close file)
  Dcl_File
)