;;;-------------------------------------------------------------
;;; 功能：查找系统中包含的函数及符号并消重和排序打印到文件      
;;; 作者：highflybird                                           
;;; 日期：2022.8.21首发，2022.8.27修改。                        
;;; 说明：用前确保vlisp已经打开，并保证没加载其它程序以保准确性.
;;;-------------------------------------------------------------
;;;(c:vlide)                                                       ;确保vlisp打开
(defun C:PrintFunc (/ F LST S1 S2 S3)
  (or tranf (HFB-LOAD-TRANF))
  (tranf ':lpp)
  (tranf ':autolisp)
  (tranf 'list-length)
  (tranf 'vector->list)
  (tranf '_package-vector)
  (tranf 'al-symbol)
  (tranf 'init:AUTOLISP-package)
  (tranf '_subrp)
  (foreach p (list :lpp :autolisp)                              ;你还可以深挖更多
    (foreach s (vector->list (_package-vector p))              
      (GetLst s 'S1 'S2 'S3 'S4)
    )
  )

;;;  ;;这个foreach是不需要加的，仅仅为测试用
;;;  (foreach s S3
;;;    (if (_subrp s)
;;;      (i2n s)                
;;;      (tranf s)
;;;    )
;;;  )
;;;
;;;  (init:AUTOLISP-package)

  (defun RemoveDup (lst / n ret)                                ;消重
    (setq lst (mapcar 'VL-PRINC-TO-STRING lst))                 ;转成字符
    (setq lst (mapcar 'strcase lst))                            ;全部大写
    (setq lst (vl-sort lst '<))                                 ;排序
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
  (setq lst (list (cons s1 "d:/all.lsp")                        ;全部函数和符号
		  (cons s2 "d:/user.lsp")                       ;外部函数及符号
		  (cons s3 "d:/normal.lsp")                     ;普通函数及符号
		  (cons s4 "d:/intern.lsp")                     ;内部函数及符号
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

;;;=============================================================
;;; 寻找内部函数的小李飞刀                                      
;;;-------------------------------------------------------------
(if (null GetIntern)
  (defun GetIntern (s / f)
    (if (setq f (vl-symbol-value (intern s :lpp)))
      f
      (vl-symbol-value (intern s :autolisp))
    )
  )
)

;;;-------------------------------------------------------------
;;; 表版（放到表中，可以排序）                                  
;;;-------------------------------------------------------------
(defun GetLst (s L1 L2 L3 L4 / name x)
  (if (listp s)                                                 ;如果是表（包括点对）
    (if	(list-length s)                                         ;如果是表，遍历
      (foreach i s                                 
	(GetLst i L1 L2 L3 L4)                                  ;递归进去
      )
      (progn                                                    
	(GetLst (car s) L1 L2 L3 L4)                            ;递归car
	(GetLst (cdr s) L1 L2 L3 L4)                            ;递归cdr
      )
    )
    (progn 
      (set L1 (cons s (vl-symbol-value L1)))			;全部的函数和符号
      (if (getintern s)			                        ;内部和普通函数及符号，还是和tranf有区别
	(progn	  
	  (setq name (vl-symbol-name s))
 	  (if (vl-symbol-value (setq x (al-symbol name)))
	    (if (not (member x (DefunSelf)))                    ;排除本程序定义的函数和符号
	      (if (member x (InternSelf))
		(set L4 (cons s (vl-symbol-value L4)))          ;已经变为普通的
	        (set L3 (cons s (vl-symbol-value L3)))          ;普通、内部函数及符号
	      )
	    )
	    (set L4 (cons s (vl-symbol-value L4)))              ;内部函数及符号
	  )
	)
	(set L2 (cons s (vl-symbol-value L2)))                  ;用户或者外部函数及符号
      )
    )
  )
)

;;;-------------------------------------------------------------
;;; 排除程序自身影响                                            
;;;-------------------------------------------------------------
(defun DefunSelf ()
  '(L1 L2 L3 L4 P S S1 S2 S3 S4 I2N HFB-LOAD-TRANF 
    GETLST DEFUNSELF GETINTERN TRANF INTERNSELF X
  )
)

;;;-------------------------------------------------------------
;;; 已转化为普通的                                              
;;;-------------------------------------------------------------
(defun InternSelf ()
  '(al-add-subr-name lpp-symfun->al intern :autolisp :lpp
    list-length vector->list _package-vector al-symbol
  )
)

(princ "运行命令是:PrintFunc.\n")
(princ)