;;;=============================================================
;;; Ѱ���ڲ�������С��ɵ�                                      
;;;-------------------------------------------------------------

;;;-------------------------------------------------------------
;;; ��棨�ŵ����У���������                                  
;;;-------------------------------------------------------------
(defun GetLst (s L1 L2 L3 / val)
  (if (listp s)                                                 ;����Ǳ�������ԣ�
    (if	(list-length s)                                         ;����Ǳ�����
      (foreach i s                                 
	(GetLst i L1 L2 L3)                                     ;�ݹ��ȥ
      )
      (progn                                                    
	(GetLst (car s) L1 L2 L3)                               ;�ݹ�car
	(GetLst (cdr s) L1 L2 L3)                               ;�ݹ�cdr
      )
    )
    (progn 
      (set L1 (cons s (vl-symbol-value L1)))			;ȫ���ĺ����ͷ���
      (if (getintern s)			                        ;�ڲ�����ͨ����������
        (set L3 (cons s (vl-symbol-value L3)))                  ;��ͨ���ڲ�����������
	(set L2 (cons s (vl-symbol-value L2)))                  ;�û������ⲿ����������
      )
    )
  )
)

(defun GetLst1 (p s L1 L2 L3 / val)
  (if (listp s)                                                 ;����Ǳ�������ԣ�
    (if	(list-length s)                                         ;����Ǳ�����
      (foreach i s                                 
	(GetLst1 p i L1 L2 L3)                                  ;�ݹ��ȥ
      )
      (progn                                                    
	(GetLst1 p (car s) L1 L2 L3)                            ;�ݹ�car
	(GetLst1 p (cdr s) L1 L2 L3)                            ;�ݹ�cdr
      )
    )
    (progn 
      (set L1 (cons s (vl-symbol-value L1)))			;ȫ���ĺ����ͷ���
      (if (eval (intern s p))			                ;�ڲ�����ͨ����������
        (set L3 (cons s (vl-symbol-value L3)))                  ;��ͨ���ڲ�����������
	(set L2 (cons s (vl-symbol-value L2)))                  ;�û������ⲿ����������
      )
    )
  )
)


;;;-------------------------------------------------------------
;;; �����غ�����õĴ�ӡ���ļ�                                  
;;;-------------------------------------------------------------
(defun C:PrintFunc (/ F LST S1 S2 S3 S4 S5 S6 RemoveDup)
  (or tranf (HFB-LOAD-TRANF))
  (tranf ':lpp)
  (tranf ':autolisp)
  (tranf 'list-length)
  (tranf 'vector->list)
  (tranf '_package-vector)
  ;; �㻹�������ڸ���
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
  ;(init:AUTOLISP-package)                                      ;�ָ�ԭ״̬
  (setq lst (list (cons s1 "d:/all1.lsp")                       ;ȫ�������ͷ���
		  (cons s2 "d:/user1.lsp")                      ;�ⲿ����������
		  (cons s3 "d:/available1.lsp")                 ;��ͨ���ڲ�����������
		  (cons s4 "d:/all2.lsp")                       ;ȫ�������ͷ���
		  (cons s5 "d:/user2.lsp")                      ;�ⲿ����������
		  (cons s6 "d:/available2.lsp")                 ;��ͨ���ڲ�����������
	    )
  )
  (foreach n lst
    (setq f (open (cdr n) "w"))                                 ;���ļ�д����
    (foreach s (removeDup (car n))                              ;ȥ��
      (write-line s f) 
    )
    (close f)
  )

  (princ)
)

;;;-------------------------------------------------------------
;;; ע�⣺�������Ҫ�ȵ�һ��������ϣ��ص�CAD���¿�ͼ��������.  
;;;-------------------------------------------------------------
(defun c:findintern(/ F F1 F2 L1 L2 L3 S)
  (or tranf (HFB-LOAD-TRANF))
  (tranf 'AL-symbol)
  (setq f (open "d:\\available.lsp" "r"))
  (setq l1 nil)
  (setq l2 nil)
  (while (setq s (read-line f))
    (if (vl-symbol-value (al-symbol s))
      (setq l1 (cons s l1));��ͨ����
      (setq l2 (cons s l2));�ڲ�����
    )
  )
  (close f)
  ;l3�ǳ�������ʱ��������ڲ�����
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
;;; ��ӡ�棨��ӡ���ļ���������                                
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
      (princ l f1)			                        ;ȫ����
      (princ "\n" f1)
      (if (tranf l)			                        ;���õı�������
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
  ;; �㻹�������ڸ���
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
  ;(init:AUTOLISP-package)                                      ;�ָ�ԭ״̬
  (setq lst (list (cons s1 "d:/all1.lsp")                       ;ȫ�������ͷ���
		  (cons s2 "d:/user1.lsp")                      ;�ⲿ����������
		  (cons s3 "d:/available1.lsp")                 ;��ͨ���ڲ�����������
		  (cons s4 "d:/all2.lsp")                       ;ȫ�������ͷ���
		  (cons s5 "d:/user2.lsp")                      ;�ⲿ����������
		  (cons s6 "d:/available2.lsp")                 ;��ͨ���ڲ�����������
	    )
  )
  (foreach n lst
    (setq f (open (cdr n) "w"))                                 ;���ļ�д����
    (foreach s (removeDup (car n))                              ;ȥ��
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
  (setq f1 (open "d:/all.lsp" "w"))                             ;ȫ����
  (setq f2 (open "d:/available.lsp" "w"))                       ;���õı�������
  (foreach s (list :lpp :autolisp)                              ;�㻹�������ڸ���
    (foreach n (vector->list (_package-vector s))
      (PrintList n f1 f2)
    )
  )
  (close f1)
  (close f2)
  (init:AUTOLISP-package)
)

(princ "����������:PrintFunc.�����ڲ�������:FindIntern.\n")
(princ)