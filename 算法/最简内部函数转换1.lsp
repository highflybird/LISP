;;;=============================================================
;;; ������غ󣬾Ϳ�����tranf������һ��ϵͳ���صĺ���           
;;; �÷���(tranf "�ڲ�������")                                  
;;; ���������������ַ���                                        
;;; ���أ�T���ڻ����ǿ��õ��ڲ�������nil�����ڻ�����Ч          
;;; ���ߣ�highflybird                                           
;;; ���ӣ�(tranf "get-logical-drives")                          
;;; �˳���õ�����baitang36�Ĵ����������ش���л��               
;;; �������˲���è��tryhi�Ĵ��룬�ڴ�һ����л��               
;;;-------------------------------------------------------------
(defun HFB-LOAD-TRANF (/ f o a b c)
  (setq o (strcat (getenv "UserProfile") "\\Intern.fas"))
  (if (findfile o)
    (vl-file-delete o)
  )
  (setq f (open o "w"))
  (foreach a '(13   70	 65   83   52	45   70	  73   76   69	 32
	       59   13	 49   13   49	32   36	  1    36   13	 54
	       55   32	 54   32   36	20   1	  1    1    256	 91
	       65   256	 66   256  67	256  256  86   108  112	 112
	       45   115	 121  109  102	117  110  45   62   97	 108
	       256  105	 110  116  101	114  110  256  58   76	 80
	       80   256	 256  1	   67	256  256  6    256  3	 5
	       256  6	 2    256  3	4    256  6    1    256	 3
	       3    256	 6    256  256	22   36
	      )
    (write-char a f)
  )
  (close f)
  (load o)
  (vl-file-delete o)
  (foreach s '(al-add-subr-name lpp-symfun->al intern :lpp)
    (a (b s c))
    (or (eq s ':lpp) (al-add-subr-name s))
  )
  (if intern
    (princ "\n�Ѵ��ڲ�����ת��ͨ��������.\n")
    (princ "\n�����ڲ�����ת��ͨ����ʧ��!\n")
  )
)

(if (null :lpp)
  (progn
    (HFB-LOAD-TRANF)
    (defun tranf (s)
      (lpp-symfun->al (intern s :lpp))
    )
  )
)

(princ "�Ѿ������ڲ�����ת��ͨ����TRANF.\n  ���ߣ�highflybird\n")
(princ)

 