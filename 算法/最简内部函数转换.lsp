;;;=============================================================
;;; ������غ󣬾Ϳ�����tranf������һ��ϵͳ���صĺ���           
;;; �÷���(tranf "�ڲ�������")                                  
;;; ���������������ַ���                                        
;;; ���أ�T���ڻ����ǿ��õ��ڲ�������nil�����ڻ�����Ч          
;;; ���ߣ�highflybird                                           
;;; ���ӣ�(tranf "get-logical-drives")                          
;;; �˳���õ�����baitang36�Ͳ���è�Ĵ����������ش���л��       
;;; ��������tryhi���������ѵĴ��룬�ڴ�һ����л��             
;;;-------------------------------------------------------------
(defun HFB-LOAD-TRANF (/ f o l)
  (setq o (strcat (getenv "UserProfile") "\\Intern.fas"))
  (if (findfile o)
    (vl-file-delete o)
  )
  (setq f (open o "w"))
  (foreach a '(70  65  83  52  45  70  73  76  69  13  49  13  48  32
	       36  22  36  49  54  32  48  32  36  86 105 110 116 101
	      114 110 256  58 108 112 112 256 256  42  22  36)
    (write-char a f)
  )
  (close f)
  (setq l (load o))
  (setq intern (eval (car l)))
  (setq :lpp (eval (cdr l)))
  (vl-file-delete o)
  (foreach s '(lpp-symfun->al :autolisp al-add-subr-name)
    (set s (eval (intern s :lpp)))
  )
  
  ;;�˺���û�и����ã��õ�һ����������ŵĵ�ַ�����ã����Ƽ�ʹ��
  ;;���ӣ�(setq type-of (getintern 'type-of)) (type-of 2324)    
  (defun GetIntern (s / f)
    (if (setq f (vl-symbol-value (intern s :lpp)))
      f
      (vl-symbol-value (intern s :autolisp))    
    )
  )

  (defun I2N (s / p)
    (if (vl-symbol-value (setq p (intern s :lpp)))
      (al-add-subr-name p)
      (if (vl-symbol-value (setq p (intern s :autolisp)))
	(al-add-subr-name p)
      )
    )
  )
  
  ;;�˺������ڲ�תΪ��ͨ����ͬ����ֵ���ı���ɫ���ܱ������и�����
  ;;���ӣ�(tranf 'type-of) (type-of 2324)                       
  (defun tranf (s)
    (or
      (lpp-symfun->al (intern s :lpp))
      (lpp-symfun->al (intern s :autolisp))
    )
  )
  
  (mapcar
    'al-add-subr-name
    '(al-add-subr-name lpp-symfun->al intern tranf GetIntern I2N)
  )
  (if lpp-symfun->al
    (princ "\n�Ѵ��ڲ�����ת��ͨ��������.\n")
    (princ "\n�����ڲ�����ת��ͨ����ʧ��!\n")
  )
)
(or lpp-symfun->al (HFB-LOAD-TRANF))

(princ "�Ѿ������ڲ�����ת��ͨ����TRANF.\n  ���ߣ�highflybird\n")
(princ)