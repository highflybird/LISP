(vl-load-com)
;;;������
(prompt "������solve����!")

(defun C:solve (/ f x1 x2 n id Dcl_File)
  (setq id (load_dialog (setq Dcl_File (Write_Dcl))))		;װ��Ի���
  (vl-file-delete Dcl_File)					;ɾ����ʱ�ļ�
  (if (new_dialog "dcl_solve" id)
    (progn
      (action_tile "EXP" "(SOLVE:GetFunc (setq f $value))") 	;�ӶԻ����еõ����ʽ
      (action_tile "MIN" "(setq x1 $value)") 			;�ӶԻ����еõ��½�
      (action_tile "MAX" "(setq x2 $value)") 			;�ӶԻ����еõ��Ͻ�
      (action_tile "PRC" "(setq n $value)") 			;�ӶԻ����еõ�����
      (action_tile "ANS" "(GetAnswer f x1 x2 n)")               ;���
      (action_tile "help" "(choose 1)")				;����
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
;;;�ӶԻ���õ����̲�����������ʾ���         
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
      
      ;;���ʽ��ֵ
      (SOLVE:GetFunc f)
      
      ;(setq ret (vl-catch-all-apply 'zbrent (list func x1 x2 n)))
      (setq ret (vl-catch-all-apply 'ZRidder (list func x1 x2 n)))
      
      (if (vl-catch-all-error-p ret)
	(setq str5 (strcat "����: " (vl-catch-all-error-message ret)))
	(progn
	  (setq test (func ret))
	  (setq fra_x (rtos (- (abs ret) (fix (abs ret))) 2 n))
	  (setq fra_x (vl-string-left-trim "0" fra_x))
	  (setq int_x (itoa (fix ret)))
	  (if (equal test 0 1)
	    (setq result (strcat "����" f "=0" "�Ľ�Ϊ:" (rtos ret 2 n))
	          result (list result (rtos ret 2 n))
	    )
	    (setq result (strcat "����" f "=0" "�ڴ������޽�")
		  result (list result (rtos ret 2 n))
	    )
	  )
	  (setq	str1 (car result)
		str2 "\n�����������֤���:"
		str3 (cadr result)
		str4 (rtos test 2 n)
		str5 (strcat str1 str2 "\nf(" str3 ")=" str4)
	  )
	)
      )
      (set_tile "error" str5)
      str5
    )
    (set_tile "error" "��������!")
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
;;; ��Ridders�������һԪ���̵ĸ�               
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
;;; ��Van Wijingaarden-Dekker-Brent�������     
;;;*********************************************
(defun zbrent (func x1 x2 n / a b c d e i p q r s xm fa fb fc EPS Itr tol tol1 min1 min2)
  (setq tol (expt 0.1 n))					;�ݲ�
  (setq Itr 10000)						;����������
  (setq EPS 1e-16)						;�������Ч����
  (if (> x1 x2)							;����½�����Ͻ�
    (setq a x2
	  b x1
	  c x1
    )								;�򽻻����½�
    (setq a x1
	  b x2
	  c x2
    )
  )
  (setq fa (func a))
  (setq fb (func b))
  (if (or (and (> fa 0) (> fb 0)) (and (< fa 0) (< fb 0)))	;���½纯��ֵͬ��
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
      )								;��a,b,c����������������d
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
    (setq tol1 (+ (* 2.0 EPS (abs b)) (* 0.5 tol))) 		;�����Լ��
    (setq xm (* 0.5 (- c b)))
    (if	(or (<= (abs xm) tol1) (equal fb 0 1e-16)) 		;����ѭ��
      (setq i Itr)
      (progn
	(if (and (>= (abs e) tol1)
		 (> (abs fa) (abs fb))
	    )
	  (progn
	    (setq s (/ fb fa))					;�����еڶ��η���
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
	    )							;����Ƿ��ڽ�������
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
	      )							;���Խ����ڲ�
	      (setq d xm
		    e d
	      )							;���ܽ����ڲ壬���ö��ַ�
	    )
	  )
	  (setq	d xm
		e d
	  )							;���½��ٶ�̫�������ö��ַ�
	)
	(setq a b)						;�����¹�ֵ����a
	(setq fa fb)
	(if (> (abs d) tol1)					;�����µ�ʵ���
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


;;;����˵������
(defun choose (n)
  (if (= n 1)
    (alert
      "����ʽֻ����x(Сд)Ϊ����,���淶�ܿ��ܳ���!
           \n����д��ʽ,�������x^2-2=0д��x^2-2.
           \n�������brent�������,����֤ÿ�����̶���Ч!
	   \n��ʲô����email: highflybird@qq.com"
    )
    (set_tile
      "error"
      "����ʽֻ����x(Сд)Ϊ����,����д��ʽ,�������x^2-2=0д��x^2-2."
    )
  )
)

;;; for DCL
(defun Write_Dcl (/ Dcl_File file str)
  (setq Dcl_File (vl-filename-mktemp nil nil ".DCL"))
  (setq file (open Dcl_File "w"))
  (princ
    "dcl_solve : dialog {
      label = \"һԪ����������\";
      : boxed_column {
        : edit_box {
          key=\"EXP\";
          label= \"һԪ����:\";
        }
        : row {
          : edit_box {
            key=\"MIN\";
	    label= \"�����½�:\";
            edit_width = 8;
          }
          : edit_box {
            key=\"MAX\";
            label= \"�����Ͻ�:\";
            edit_width = 8;
          }
          : edit_box {
            key=\"PRC\";
            label= \"��ȷλ��:\";
            edit_width = 2;
          }
        }
        spacer_1;
      }
      : row {
        : button {
          key=\"ANS\";
          label=\"���\";
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






