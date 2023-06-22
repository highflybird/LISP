;;;=============================================================
;;; ������غ󣬾Ϳ�����tranf������һ��ϵͳ���صĺ���           
;;; �÷���(tranf "�ڲ�������")                                  
;;; ���������������ַ���                                        
;;; ���أ�T���ڻ����ǿ��õ��ڲ�������nil�����ڻ�����Ч          
;;; ���ߣ�highflybird                                           
;;; ���ӣ�(tranf "get-logical-drives")                          
;;; �˳���õ�����baitang36�Ĵ����������ش���л��               
;;; �������˲���è��tryhi�Ĵ��룬�ڴ�һ����л��               
(vl-load-com)
(defun HFB-LOAD-TRANF (/ f o l b s)
  (setq o (strcat (getenv "UserProfile") "\\Intern.fas"))
  (if (findfile o)
    (vl-file-delete o)
  )
  (setq l '(70  65  83  52  45  70  73  76  69  13  49  13  48  32
	    36  22  36  49  54  32  48  32  36  86 105 110 116 101
	   114 110   0  58 108 112 112   0   0  42  22  36))
  (setq b (vlax-make-safearray 17 (cons 0 (1- (length l)))))
  (vlax-safearray-fill b l)
  (setq s (vlax-create-object "ADODB.Stream"))
  (vlax-put s 'type 1) 			
  (vlax-invoke s'open)		
  (vlax-invoke-method s 'Write b)
  (vlax-invoke s 'saveToFile o 2)
  (vlax-invoke s 'close)
  (vlax-release-object s)
  (setq f (load o))
  (setq intern (eval (car f)))
  (setq :lpp (eval (cdr f)))
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

;;;=============================================================
;;; ��������ϵͳ���صĺ���                                      
;;; �������������һ���ھ�                                      
;;;-------------------------------------------------------------
(defun ActiveIntern ( )
  (foreach symbol 
    '(al-add-subr-name                                          ;������תΪ�ڲ���������д����
      get-logical-drives                                        ;��ȡϵͳ�����̷����޲�����
      write-byte                                                ;��һ���ֽ�д���ļ����÷���write-char��ͬ�����Ǵ��ֽ�д�룬�ǳ�����
      _read-nb                                                  ;���ֽڣ������ƣ�
      _WRITE-NB-STR			                        ;д�ֽڣ������ƣ�
      vrtlib-list                                               ;��ȡһ��VLX�ļ�������Ϊ�ļ����������ļ��ṹ
      VRTLIB-GET			                        ;�����÷�δ֪
      beep                                                      ;��������
      LOAD-FILE                                                 ;�����ļ�
      load-library                                              ;���ؿ��ļ�
      LOAD-DLL                                                  ;���ض�̬���ӿ�
      _run-dll	                                                ;�ǺǺ�!
      PUT                                                       ;�����������
      GET                                                       ;��ȡ��������
      SYMBOL-PLIST                                              ;��ȡ�������Ա�
      SYMBOL-PLIST<-                                            ;�޸ķ������Ա�
      lpp-symfun->al                                            ;�ڲ�����ת��ͨ����
      intern                                                    ;�ڲ�����
      make-symbol                                               ;���ɷ���
      find-symbol                                               ;���ҷ���
      symbol-name                                               ;������
      gensym                                                    ;���ɷ��Ŵ���׺��������׺
      nthcdr                                                    ;n��cdr
      nth<-                                                     ;�޸ĵ�N��Ԫ��
      format                                                    ;��ʽ���ַ���
      rassoc                                                    ;��assoc����,����ȡcdr
      vector                                                    ;��������
      vectorp                                                   ;�ж��Ƿ�����
      vector-length                                             ;��������
      vector-elt                                                ;��ȡ����Ԫ��
      vector-elt&                                               ;��ȡ����Ԫ��location
      vector-elt<-                                              ;�޸�����Ԫ��
      vector-swap                                               ;����Ԫ�ؽ���
      vector-append                                             ;�����������
      vector-fill                                               ;�������
      vector->list                                              ;����תΪ��
      vector-position                                           ;Ԫ����������λ��
      make-vector                                               ;��������
      copy-vector                                               ;��������
      list->vector                                              ;��תΪ����
      list-elt                                                  ;��ȡ��Ԫ��
      list-elt<-                                                ;�޸ı�Ԫ��
      copy-list                                                 ;���Ʊ�
      make-list                                                 ;���ɱ�
      string                                                    ;�����ַ���
      string<                                                   ;�ж����ַ�����С
      string=                                                   ;���ַ��Ƿ����
      string-capitalize                                         ;����ĸ��д
      string-downcase                                           ;�ַ���Сд
      string-equal                                              ;���ַ��Ƿ����
      string-left-trim                                          ;�ַ�����ض�
      string-lessp                                              ;�ַ����Ƚϴ�С
      stringp                                                   ;�Ƿ�Ϊ�ַ���
      string-right-trim                                         ;�ַ����ҽض� 
      string-trim                                               ;�ַ����ض� 
      string-upcase                                             ;�ַ�����д
      string-fill                                               ;����ַ���
      string-by-char-to-list                                    ;�ַ����ָ�ɱ�
      string-elt<-                                              ;�޸��ַ�����ĳ���ַ�
      copy-string                                               ;�����ַ���
      make-string                                               ;�����ַ���
      _byte@                                                    ;��ȡ�ڴ��ֽ�
      _byte@<-                                                  ;�޸��ڴ��ֽ�
      _word@                                                    ;��ȡ�ڴ���
      _word@<-                                                  ;�޸��ڴ���
      _ptr@                                                     ;��ȡ�ڴ��ַ
      _ptr@<-                                                   ;�޸��ڴ��ַ
      _addr-of                                                  ;��ȡ��ַ
      _type-id                                                  ;����ID
      type-of                                                   ;����
      ptoa                                                      ;��ַת�ַ���
      ftoa                                                      ;������ת�ַ�
      RANDOM&                                                   ;�������
      sort                                                      ;������
      al-defun-hook-proc                                        ;���ӣ�̽����
      ADSi-REGFUN-HOOK                                          ;���ӣ�̽����
      _get-windows-directory                                    ;��ȡWindowsĿ¼
      Get-Obj-From-Dll					        ;��GUID��ȡObject
      _al-bind-alist                                            ;��������ļ��ϲ�
      _package-vector                                           ;�����:autolisp,:lpp 
      _package-vector<-                                         ;�޸İ���
      al-fas-load                                               ;װ��fas�ļ�
      dcl-call-back                                             ;̽����
      make-string-input-stream                                  ;���ַ�����ȡ��
      load-stream                                               ;�����м���
      al-load-stream
      funcall                                                   ;ͬapply,�����Ǳ�
      choose-file-dialog                                        ;ѡ���ļ��Ի���
      choose-editor-file-dialog                                 ;ѡ��༭�ļ��Ի���
      directory-browse-files                                    ;�൱��VL-DIRECTORY-FILES
      sh-browse-for-folder                                      ;����ļ��в����·��
      get-string-dialog                                         ;getstring�Ի����
      _subr-entry-addr
    )
    (i2n symbol)                                                ;��tranf+al-add-subr-name��
  )
  (foreach symbol '(itoa atoi)                                  ;�ַ���������ת��������ʵ�ֽ���ת��
    (tranf symbol)
    (al-add-subr-name
      (cons (strcat "hfb-" (symbol-name symbol)) symbol)        ;Ϊ��ֹ������ͻ������ǰ׺
    )     
  )
  (princ "������ת��һЩϵͳ�ڲ�����!  ���ߣ�highflybird\n")
)
(or _addr-of (ActiveIntern))
;;;=============================================================
;;; �������غ���                                                
;;;-------------------------------------------------------------
(defun c:findfun (/ l f s)
  (setq l nil)
  (setq f (open "c:\\common-lisp.txt" "R"))
  (while (setq s (read-line f))
    (if (not (vl-symbol-value (read s)))
      (if (tranf s)
        (setq l (cons s l))
      )
    )
  )
  (close f)
  (setq f (open "c:\\1.txt" "W"))
  (foreach s (reverse l)
    (write-line s f)
  )
  (close f)
  (princ)
)

;;;=============================================================
;;; ������������                                                
;;;-------------------------------------------------------------
(defun swap (symbol-A symbol-B / temp)
  (setq temp (vl-symbol-value symbol-A))
  (set symbol-A (vl-symbol-value symbol-B))
  (set symbol-B temp)
)

;;;=============================================================
;;; ÿ�ֽڶ�ȡ�ڴ����ݵ���                                      
;;;-------------------------------------------------------------
(defun readbyte (PEntry Offset Number / codes)
  (while (> Number 0)
    (setq codes  (cons (_byte@ PEntry offset) codes))
    (setq offset (1+ offset))
    (setq Number (1- Number))
  )
  (reverse codes)
)

(defun read64 (PEntry offset Number)
  (mapcar
    (function
      (lambda (x)
	(substr (ptoa x) 15 2)
      )
    )
    (readbyte PEntry offset Number)
  )
)

(defun read32 (PEntry offset Number)
  (mapcar
    (function
      (lambda (x)
	(substr (ptoa x) 7 2)
      )
    )
    (readbyte PEntry offset Number)
  )
)


;;;=============================================================
;;; ÿ�ֶ�ȡ�ڴ����ݵ���                                        
;;;-------------------------------------------------------------
(defun readword (PEntry Offset Number / codes)
  (while (> Number 0)
    (setq codes  (cons (_word@ PEntry offset) codes))
    (setq offset (1+ offset))
    (setq Number (1- Number))
  )
  (reverse codes)
)

;;;=============================================================
;;; ÿ˫�ֶ�ȡ�ڴ����ݵ���                                      
;;;-------------------------------------------------------------
(defun readptr (PEntry Offset Number / codes)
  (while (> Number 0)
    (setq codes  (cons (_ptr@ PEntry offset) codes))
    (setq offset (1+ offset))
    (setq Number (1- Number))
  )
  (reverse codes)
)

;;;=============================================================
;;; ��ȡ���س���·��                                            
;;;-------------------------------------------------------------
(defun syz-vlx-path ()
  (tranf "*current-lisplet*")
  (tranf "slot-value")
  (tranf "Resource-add-file")
  (tranf "Resource-find-dcl")
  (tranf "Resource-find-fas")
  (tranf "Resource-find")
  (get
    (find-symbol
      (string (string-upcase "syz-vlx-path"))
      (slot-value (car *current-lisplet*) 'Resource-table)
    ) ;_ find-symbol
    1330
  ) ;_ get
) ;_ defun

;;;ͨ��Fas����ֱֵ�ӻ�ȡ�ڲ�������ʵ����С�ֽ����ɡ�
(defun getInternalFunc (FuncStr / file f l);byè��ʦ
	(setq file (strcat (getenv "UserProfile") "\\�ڲ�����.fas"))
	(if (findfile file) (vl-file-delete file))
	(setq f (open file "w"))
        (setq l (append 
		(list 70 65 83 52 45 70 73 76 69 13 49 13 48 32 36 1 36)
		(vl-string->list (itoa (+ 4 (strlen FuncStr))))
		(list 32 48 32 36 86 )
		(vl-string->list FuncStr)
		(list 256 256 22 36)
		))
	(foreach x l
		
		(write-char x f)
	)
	(close f)
	(car (list (eval (load file)) (vl-file-delete file)))
)

;;;�����������ֽ�����һ�����ڴ����fas�ļ�������
(defun getInternalFuncStream (FuncStr);byè��ʦ
	(eval (al-fas-load (make-string-input-stream (vl-list->string (append 
		(list 70 65 83 52 45 70 73 76 69 13 49 13 48 32 36 1 36)
		(vl-string->list (itoa (+ 4 (strlen FuncStr))))
		(list 32 48 32 36 86 )
		(vl-string->list FuncStr)
		(list 0 0 22 36)
	)))))
)

;;;���Ҫһ����ͬʱ��������������������һ�����أ�Ȼ��ӱ�����ȡ��Ӧ���ڲ�������
;;;�������Լ��ٴ���ͷ��������ʴ������ٶȸ��졣
(defun getInternalFuncStream-s (FuncStrList / re len l);byè��ʦ
	(setq len (length FuncStrList))
	(if (> len 65535)
		(list (alert "һ�����ɲ�����ô��!")(exit))
	)
	(setq re (list 86))
	(foreach x FuncStrList
		(setq re (append re (vl-string->list x) (list 0)))
	)
        (setq l (append 
		(list 70 65 83 52 45 70 73 76 69 13 49 13 48 32 36 1 36)
		(vl-string->list (itoa (+ 6 len (strlen (apply 'strcat FuncStrList)))))
		(list 32 48 32 36 )
		re
		(list 0 57)
		(list (rem len 256) (/ len 256))
		(list 22 36)
	))
	(al-fas-load (make-string-input-stream (vl-list->string l)))
)
;(setq re (getInternalFuncStream-s (list "printf" "shell")))
;(foreach x re (print (eval x)))

(defun loadstream (s)
  (al-load-stream
    (make-string-input-stream  s )
  )
)

(princ)

