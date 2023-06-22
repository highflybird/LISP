;;;-------------------------------------------------------------
;;; ���ܣ�����ϵͳ�а����ĺ��������Ų����غ������ӡ���ļ�      
;;; ���ߣ�highflybird                                           
;;; ���ڣ�2022.8.21�׷���2022.8.27�޸ġ�                        
;;; ˵������ǰȷ��vlisp�Ѿ��򿪣�����֤û�������������Ա�׼ȷ��.
;;;-------------------------------------------------------------
;;;(c:vlide)                                                       ;ȷ��vlisp��
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
  (foreach p (list :lpp :autolisp)                              ;�㻹�������ڸ���
    (foreach s (vector->list (_package-vector p))              
      (GetLst s 'S1 'S2 'S3 'S4)
    )
  )

;;;  ;;���foreach�ǲ���Ҫ�ӵģ�����Ϊ������
;;;  (foreach s S3
;;;    (if (_subrp s)
;;;      (i2n s)                
;;;      (tranf s)
;;;    )
;;;  )
;;;
;;;  (init:AUTOLISP-package)

  (defun RemoveDup (lst / n ret)                                ;����
    (setq lst (mapcar 'VL-PRINC-TO-STRING lst))                 ;ת���ַ�
    (setq lst (mapcar 'strcase lst))                            ;ȫ����д
    (setq lst (vl-sort lst '<))                                 ;����
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
  (setq lst (list (cons s1 "d:/all.lsp")                        ;ȫ�������ͷ���
		  (cons s2 "d:/user.lsp")                       ;�ⲿ����������
		  (cons s3 "d:/normal.lsp")                     ;��ͨ����������
		  (cons s4 "d:/intern.lsp")                     ;�ڲ�����������
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

;;;=============================================================
;;; Ѱ���ڲ�������С��ɵ�                                      
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
;;; ��棨�ŵ����У���������                                  
;;;-------------------------------------------------------------
(defun GetLst (s L1 L2 L3 L4 / name x)
  (if (listp s)                                                 ;����Ǳ�������ԣ�
    (if	(list-length s)                                         ;����Ǳ�����
      (foreach i s                                 
	(GetLst i L1 L2 L3 L4)                                  ;�ݹ��ȥ
      )
      (progn                                                    
	(GetLst (car s) L1 L2 L3 L4)                            ;�ݹ�car
	(GetLst (cdr s) L1 L2 L3 L4)                            ;�ݹ�cdr
      )
    )
    (progn 
      (set L1 (cons s (vl-symbol-value L1)))			;ȫ���ĺ����ͷ���
      (if (getintern s)			                        ;�ڲ�����ͨ���������ţ����Ǻ�tranf������
	(progn	  
	  (setq name (vl-symbol-name s))
 	  (if (vl-symbol-value (setq x (al-symbol name)))
	    (if (not (member x (DefunSelf)))                    ;�ų���������ĺ����ͷ���
	      (if (member x (InternSelf))
		(set L4 (cons s (vl-symbol-value L4)))          ;�Ѿ���Ϊ��ͨ��
	        (set L3 (cons s (vl-symbol-value L3)))          ;��ͨ���ڲ�����������
	      )
	    )
	    (set L4 (cons s (vl-symbol-value L4)))              ;�ڲ�����������
	  )
	)
	(set L2 (cons s (vl-symbol-value L2)))                  ;�û������ⲿ����������
      )
    )
  )
)

;;;-------------------------------------------------------------
;;; �ų���������Ӱ��                                            
;;;-------------------------------------------------------------
(defun DefunSelf ()
  '(L1 L2 L3 L4 P S S1 S2 S3 S4 I2N HFB-LOAD-TRANF 
    GETLST DEFUNSELF GETINTERN TRANF INTERNSELF X
  )
)

;;;-------------------------------------------------------------
;;; ��ת��Ϊ��ͨ��                                              
;;;-------------------------------------------------------------
(defun InternSelf ()
  '(al-add-subr-name lpp-symfun->al intern :autolisp :lpp
    list-length vector->list _package-vector al-symbol
  )
)

(princ "����������:PrintFunc.\n")
(princ)