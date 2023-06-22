(vl-load-com)
;;;主程序
(prompt "请输入solve命令!")

(defun C:solve (/ f x1 x2 n id Dcl_File)
  (setq id (load_dialog (setq Dcl_File (Write_Dcl))))		;装入对话框
  (vl-file-delete Dcl_File)					;删除临时文件
  (if (new_dialog "dcl_solve" id)
    (progn
      (action_tile "EXP" "(SOLVE:GetFunc (setq f $value))") 	;从对话框中得到表达式
      (action_tile "MIN" "(setq x1 $value)") 			;从对话框中得到下届
      (action_tile "MAX" "(setq x2 $value)") 			;从对话框中得到上届
      (action_tile "PRC" "(setq n $value)") 			;从对话框中得到精度
      (action_tile "ANS" "(GetAnswer f x1 x2 n)")               ;求解
      (action_tile "help" "(choose 1)")				;帮助
      (start_dialog)
      (unload_dialog id)
      (princ (GetAnswer f x1 x2 n))
    )
  )
  (princ)
)

(defun SOLVE:GetFunc (f)
  (CAL:Expr2Func f 'func '(x))
)

;;;*********************************************
;;;从对话框得到方程参数并求解和显示结果         
;;;*********************************************
(defun GetAnswer (f x1 x2 n / str1 str2 str3 str4 str5 ret test fra_x int_x result func)
  (if (and x1 x2 n f)
    (progn
      (setq x1 (atof x1))
      (setq x2 (atof x2))
      (setq n  (abs (atoi n)))
      (if (> n 20)
         (setq n 20)
      )
      
      ;;表达式求值
      (SOLVE:GetFunc f)
      
      ;(setq ret (vl-catch-all-apply 'zbrent (list func x1 x2 n)))
      (setq ret (vl-catch-all-apply 'ZRidder (list func x1 x2 n)))
      
      (if (vl-catch-all-error-p ret)
	(setq str5 (strcat "出错: " (vl-catch-all-error-message ret)))
	(progn
	  (setq test (func ret))
	  (setq fra_x (rtos (- (abs ret) (fix (abs ret))) 2 n))
	  (setq fra_x (vl-string-left-trim "0" fra_x))
	  (setq int_x (itoa (fix ret)))
	  (if (equal test 0 1)
	    (setq result (strcat "方程" f "=0" "的解为:" (rtos ret 2 n))
	          result (list result (rtos ret 2 n))
	    )
	    (setq result (strcat "方程" f "=0" "在此区间无解")
		  result (list result (rtos ret 2 n))
	    )
	  )
	  (setq	str1 (car result)
		str2 "\n下面是求解验证结果:"
		str3 (cadr result)
		str4 (rtos test 2 n)
		str5 (strcat str1 str2 "\nf(" str3 ")=" str4)
	  )
	)
      )
      (set_tile "error" str5)
      str5
    )
    (set_tile "error" "输入有误!")
  )
)

(defun Solve:Sign (x y)
  (if (>= y 0)
    (if (>= x 0)
      x
      (- x)
    )
    (if (>= x 0)
      (- x)
      x
    )
  )
)

(defun Sign (x)
  (if (> x 0)
    1.0
    (if	(zerop x)
      0.0
      -1.0
    )
  )
)
;;;*********************************************
;;; 用Ridders方法求解一元方程的根               
;;;*********************************************
(defun ZRidder (func x1 x2 n / ans EPS FH FL FM FNEW ITR J S TOL XH XL XM XNEW XXX b)
  (setq tol (expt 0.1 n))
  (setq itr 1000)
  (setq EPS 1e-16)
  (setq xxx -1.11e100)
  (setq fl (func x1))
  (setq fh (func x2))
  (if (or (and (> fl 0.0) (< fh 0.0)) (and (< fl 0.0) (> fh 0.0)))
    (progn
      (setq xl x1)
      (setq xh x2)
      (setq ans xxx)
      (setq j 0)
      (setq b T)
      (while (and b (< j itr))
	(setq xm (* 0.5 (+ xl xh)))
	(setq fm (func xm))
	(setq s (sqrt (- (* fm fm) (* fl fh))))  
	(if (equal s 0 eps)
	  (setq b nil)
	  (progn
	    (setq xnew (+ xm (/ (* (- xm xl) (if (>= fl fh) 1 -1) fm) s)))
	    (if (equal xnew ans tol)
	      (setq b nil)
	      (progn
		(setq ans xnew)
		(setq fnew (func ans))
		(if (equal fnew 0.0 tol)
		  (setq b nil)
		  (progn
		    (if (/= (solve:sign fm fnew) fm)
		      (setq xl xm
			    fl fm
			    xh ans
			    fh fnew
		      )
		      (if (/= (solve:sign fl fnew) fl)
			(setq xh ans
			      fh fnew
			)
			(if (/= (solve:sign fh fnew) fh)
			  (setq xl ans
				fl fnew
			  )
			  (princ "\nNever get here!")
			)
		      )
		    )
		    (if (equal xh xl tol)
		      (setq b nil)
		    )
		  )
		)
              )
	    )
	  )
	)
	(setq j (1+ j))
      )
      ;(princ "\nIterations: ")
      ;(princ j)
      (if (>= j itr)
	(princ "\nzriddr exceed maximum iterations.")
      )
    )
    (progn
      (if (equal fl 0 tol)
	(setq ans x1)
	(if (equal fh 0 tol)
	  (setq ans x2)
	  (progn
	    (setq ans (zbrent func x1 x2 n))
	    ;(setq ans 0)
	    ;(princ "\nroot must be bracketed in zriddr.")
	  )
	)
      )
    )
  )
  ans
)
		  	    
;;;*********************************************
;;; 用Van Wijingaarden-Dekker-Brent方法求根     
;;;*********************************************
(defun zbrent (func x1 x2 n / a b c d e i p q r s xm fa fb fc EPS Itr tol tol1 min1 min2)
  (setq tol (expt 0.1 n))					;容差
  (setq Itr 10000)						;最大迭代次数
  (setq EPS 1e-16)						;计算机有效精度
  (if (> x1 x2)							;如果下届大于上届
    (setq a x2
	  b x1
	  c x1
    )								;则交换上下界
    (setq a x1
	  b x2
	  c x2
    )
  )
  (setq fa (func a))
  (setq fb (func b))
  (if (or (and (> fa 0) (> fb 0)) (and (< fa 0) (< fb 0)))	;上下界函数值同号
    (princ "\nRoot must be bracketed in zbrent!")
  )
  (setq fc fb)
  (setq i 1)
  (while (<= i Itr)
    (if	(or (and (> fb 0) (> fc 0))
	    (and (< fb 0) (< fc 0))
	)
      (setq c  a
	    fc fa
	    d  (- b a)
	    e  d
      )								;对a,b,c更名并调整解区间d
    )
    (if	(< (abs fc) (abs fb))
      (setq a  b
	    b  c
	    c  a
	    fa fb
	    fb fc
	    fc fa
      )
    )
    (setq tol1 (+ (* 2.0 EPS (abs b)) (* 0.5 tol))) 		;收敛性检查
    (setq xm (* 0.5 (- c b)))
    (if	(or (<= (abs xm) tol1) (equal fb 0 1e-16)) 		;跳出循环
      (setq i Itr)
      (progn
	(if (and (>= (abs e) tol1)
		 (> (abs fa) (abs fb))
	    )
	  (progn
	    (setq s (/ fb fa))					;将进行第二次反插
	    (if	(equal a c 1e-16)
	      (setq p (* 2.0 xm s)
		    q (- 1.0 s)
	      )
	      (setq q (/ fa fc)
		    r (/ fb fc)
		    p (* s (- (* 2.0 xm q (- q r)) (* (- b a) (1- r))))
		    q (* (1- q) (1- r) (1- s))
	      )
	    )
	    (if	(> p 0)
	      (setq q (- q))
	    )							;检查是否在解区间内
	    (setq p (abs p))
	    (setq min1 (- (* 3.0 xm q) (abs (* tol1 q))))
	    (setq min2 (abs (* e q)))
	    (if	(< (* 2.0 p)
		   (if (< min1 min2)
		     min1
		     min2
		   )
		)
	      (setq e d
		    d (/ p q)
	      )							;可以进行内插
	      (setq d xm
		    e d
	      )							;不能进行内插，改用二分法
	    )
	  )
	  (setq	d xm
		e d
	  )							;届下降速度太慢，改用二分法
	)
	(setq a b)						;将最新估值赋给a
	(setq fa fb)
	(if (> (abs d) tol1)					;计算新的实验解
	  (setq b (+ b d))
	  (setq	b (+ b
		     (if (>= xm 0)
		       (abs tol1)
		       (- (abs tol1))
		     )
		  )
	  )
	)
	(setq fb (func b))
      )
    )
    (setq i (1+ i))
  )
  b
)


;;;帮助说明函数
(defun choose (n)
  (if (= n 1)
    (alert
      "方程式只接受x(小写)为变量,不规范很可能出错!
           \n无须写等式,例如求解x^2-2=0写作x^2-2.
           \n程序采用brent方法求解,不保证每个方程都有效!
	   \n有什么问题email: highflybird@qq.com"
    )
    (set_tile
      "error"
      "方程式只接受x(小写)为变量,无须写等式,例如求解x^2-2=0写作x^2-2."
    )
  )
)

;;; for DCL
(defun Write_Dcl (/ Dcl_File file str)
  (setq Dcl_File (vl-filename-mktemp nil nil ".DCL"))
  (setq file (open Dcl_File "w"))
  (princ
    "dcl_solve : dialog {
      label = \"一元方程求解程序\";
      : boxed_column {
        : edit_box {
          key=\"EXP\";
          label= \"一元方程:\";
        }
        : row {
          : edit_box {
            key=\"MIN\";
	    label= \"区间下届:\";
            edit_width = 8;
          }
          : edit_box {
            key=\"MAX\";
            label= \"区间上届:\";
            edit_width = 8;
          }
          : edit_box {
            key=\"PRC\";
            label= \"精确位数:\";
            edit_width = 2;
          }
        }
        spacer_1;
      }
      : row {
        : button {
          key=\"ANS\";
          label=\"求解\";
        }
        ok_cancel_help;
      }
      errtile;
    }"
    file
  )
  (close file)
  Dcl_File
)






