;;;=============================================================
;;; 寻找内部函数的小李飞刀                                      
;;;-------------------------------------------------------------

;;;-------------------------------------------------------------
;;; 表版（放到表中，可以排序）                                  
;;;-------------------------------------------------------------
(defun GetLst (s L1 L2 L3 / val)
  (if (listp s)                                                 ;如果是表（包括点对）
    (if	(list-length s)                                         ;如果是表，遍历
      (foreach i s                                 
	(GetLst i L1 L2 L3)                                     ;递归进去
      )
      (progn                                                    
	(GetLst (car s) L1 L2 L3)                               ;递归car
	(GetLst (cdr s) L1 L2 L3)                               ;递归cdr
      )
    )
    (progn 
      (set L1 (cons s (vl-symbol-value L1)))			;全部的函数和符号
      (if (getintern s)			                        ;内部和普通函数及符号
        (set L3 (cons s (vl-symbol-value L3)))                  ;普通、内部函数及符号
	(set L2 (cons s (vl-symbol-value L2)))                  ;用户或者外部函数及符号
      )
    )
  )
)

(defun GetLst1 (p s L1 L2 L3 / val)
  (if (listp s)                                                 ;如果是表（包括点对）
    (if	(list-length s)                                         ;如果是表，遍历
      (foreach i s                                 
	(GetLst1 p i L1 L2 L3)                                  ;递归进去
      )
      (progn                                                    
	(GetLst1 p (car s) L1 L2 L3)                            ;递归car
	(GetLst1 p (cdr s) L1 L2 L3)                            ;递归cdr
      )
    )
    (progn 
      (set L1 (cons s (vl-symbol-value L1)))			;全部的函数和符号
      (if (eval (intern s p))			                ;内部和普通函数及符号
        (set L3 (cons s (vl-symbol-value L3)))                  ;普通、内部函数及符号
	(set L2 (cons s (vl-symbol-value L2)))                  ;用户或者外部函数及符号
      )
    )
  )
)


;;;-------------------------------------------------------------
;;; 把消重和排序好的打印到文件                                  
;;;-------------------------------------------------------------
(defun C:PrintFunc (/ F LST S1 S2 S3 S4 S5 S6 RemoveDup)
  (or tranf (HFB-LOAD-TRANF))
  (tranf ':lpp)
  (tranf ':autolisp)
  (tranf 'list-length)
  (tranf 'vector->list)
  (tranf '_package-vector)
  ;; 你还可以深挖更多
  (foreach n (vector->list (_package-vector :lpp))              
    (GetLst1 :lpp n 'S1 'S2 'S3)
  )
  (foreach n (vector->list (_package-vector :autolisp))              
    (GetLst1 :autolisp n 'S4 'S5 'S6)
  )
  (defun RemoveDup (lst / n ret)
    (setq lst (mapcar 'VL-PRINC-TO-STRING lst))
    (setq lst (vl-sort lst '<))
    (while lst
      (setq Ret (cons (setq n (car lst)) Ret)
	    lst (cdr lst)
      )
      (while (and lst (= (car lst) n))
        (setq lst (cdr lst))
      )
    )
    (reverse Ret)
  )
  ;(init:AUTOLISP-package)                                      ;恢复原状态
  (setq lst (list (cons s1 "d:/all1.lsp")                       ;全部函数和符号
		  (cons s2 "d:/user1.lsp")                      ;外部函数及符号
		  (cons s3 "d:/available1.lsp")                 ;普通及内部函数及符号
		  (cons s4 "d:/all2.lsp")                       ;全部函数和符号
		  (cons s5 "d:/user2.lsp")                      ;外部函数及符号
		  (cons s6 "d:/available2.lsp")                 ;普通及内部函数及符号
	    )
  )
  (foreach n lst
    (setq f (open (cdr n) "w"))                                 ;打开文件写数据
    (foreach s (removeDup (car n))                              ;去重
      (write-line s f) 
    )
    (close f)
  )

  (princ)
)

;;;-------------------------------------------------------------
;;; 注意：下面程序要等第一个运行完毕，关掉CAD或新开图后再运行.  
;;;-------------------------------------------------------------
(defun c:findintern(/ F F1 F2 L1 L2 L3 S)
  (or tranf (HFB-LOAD-TRANF))
  (tranf 'AL-symbol)
  (setq f (open "d:\\available.lsp" "r"))
  (setq l1 nil)
  (setq l2 nil)
  (while (setq s (read-line f))
    (if (vl-symbol-value (al-symbol s))
      (setq l1 (cons s l1));普通函数
      (setq l2 (cons s l2));内部函数
    )
  )
  (close f)
  ;l3是程序运行时候产生的内部函数
  (setq l3 '("al-add-subr-name" "lpp-symfun->al" "intern"
	     ":autolisp" "list-length" "vector->list"
	     "_package-vector" "AL-symbol"  ":lpp"))
  (setq l2 (append l3 l2))
  (foreach n l3
    (setq l1 (vl-remove n l1))
  )
  (setq l1 (cons "nil" l1))
  (foreach n '((l1 . "d:\\Normal.lsp") (l2 . "d:\\Intern.lsp"))
    (setq f (open (cdr n) "w"))
    (foreach s (vl-sort (vl-symbol-value (car n)) '<)
      (write-line s f)
    )
    (close f)
  )	  
  (princ)
)

;;;-------------------------------------------------------------
;;; 打印版（打印到文件，不排序）                                
;;;-------------------------------------------------------------
(defun PrintList (l f1 f2)
  (if (listp l)
    (if	(list-length l)
      (foreach i l
	(PrintList i f1 f2)
      )
      (progn
	(PrintList (car l) f1 f2)
	(PrintList (cdr l) f1 f2)
      )
    )
    (progn
      (princ l f1)			                        ;全部的
      (princ "\n" f1)
      (if (tranf l)			                        ;可用的保留函数
	(progn
	  (princ l f2)
	  (princ "\n" f2)
	)
      )
    )
  )
)

(defun C:PrintFunc2 (/ F LST S1 S2 S3 S4 S5 S6 RemoveDup)
  (or tranf (HFB-LOAD-TRANF))
  (tranf ':lpp)
  (tranf ':autolisp)
  (tranf 'list-length)
  (tranf 'vector->list)
  (tranf '_package-vector)
  ;; 你还可以深挖更多
  (foreach n (vector->list (_package-vector :lpp))              
    (GetLst1 :lpp n 'S1 'S2 'S3)
  )
  (foreach n (vector->list (_package-vector :autolisp))              
    (GetLst1 :autolisp n 'S4 'S5 'S6)
  )
  (defun RemoveDup (lst / n ret)
    (setq lst (mapcar 'VL-PRINC-TO-STRING lst))
    (setq lst (vl-sort lst '<))
    (while lst
      (setq Ret (cons (setq n (car lst)) Ret)
	    lst (cdr lst)
      )
      (while (and lst (= (car lst) n))
        (setq lst (cdr lst))
      )
    )
    (reverse Ret)
  )
  ;(init:AUTOLISP-package)                                      ;恢复原状态
  (setq lst (list (cons s1 "d:/all1.lsp")                       ;全部函数和符号
		  (cons s2 "d:/user1.lsp")                      ;外部函数及符号
		  (cons s3 "d:/available1.lsp")                 ;普通及内部函数及符号
		  (cons s4 "d:/all2.lsp")                       ;全部函数和符号
		  (cons s5 "d:/user2.lsp")                      ;外部函数及符号
		  (cons s6 "d:/available2.lsp")                 ;普通及内部函数及符号
	    )
  )
  (foreach n lst
    (setq f (open (cdr n) "w"))                                 ;打开文件写数据
    (foreach s (removeDup (car n))                              ;去重
      (write-line s f) 
    )
    (close f)
  )
  
  (foreach s s4
    (if (member s s1)
      (setq s4 (vl-remove s s4))
    )
  )
  (foreach s s5
    (if (member s s2)
      (setq s5 (vl-remove s s5))
    )
  )
  (foreach s s6
    (if (member s s3)
      (setq s6 (vl-remove s s6))
    )
  )
  (setq f1 (open "d:/diff1.lsp" "w"))
  (foreach n s4
    (write-line (vl-princ-to-string n) f1)
  )
  (close f1)

  (setq f2 (open "d:/diff2.lsp" "w"))
  (foreach n s5
    (write-line (vl-princ-to-string n) f2)
  )
  (close f2)

  (setq f3 (open "d:/diff3.lsp" "w"))
  (foreach n s6
    (write-line (vl-princ-to-string n) f3)
  )
  (close f3)

  (list s4 s5 s6)
  (princ)
)

(defun c:ppp ()
  (tranf ':lpp)
  (tranf ':autolisp)
  (setq f1 (open "d:/lp.txt" "w"))
  (setq f2 (open "d:/al.txt" "w"))
  (setq lp (vector->list (_package-vector :LPP)))
  (setq al (vector->list (_package-vector :autolisp)))
  (foreach n lp
    (princ n f1)
    (princ "\n")
  )
  (foreach n lp
    (princ n f2)
    (princ "\n")
  )
  (close f1)
  (close f2)
  (princ)
)

(defun C:PrintFunc1 (/ f1 f2)
  (or :lpp (HFB-LOAD-TRANF))
  (tranf ':lpp)
  (tranf ':autolisp)
  (tranf 'list-length)
  (tranf 'vector->list)
  (tranf '_package-vector)
  (setq f1 (open "d:/all.lsp" "w"))                             ;全部的
  (setq f2 (open "d:/available.lsp" "w"))                       ;可用的保留函数
  (foreach s (list :lpp :autolisp)                              ;你还可以深挖更多
    (foreach n (vector->list (_package-vector s))
      (PrintList n f1 f2)
    )
  )
  (close f1)
  (close f2)
  (init:AUTOLISP-package)
)

(princ "运行命令是:PrintFunc.查找内部函数是:FindIntern.\n")
(princ)