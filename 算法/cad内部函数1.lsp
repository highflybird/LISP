;;;=============================================================
;;; ����һ��ϵͳ���صĺ���                                      
;;; ����Ϊһ�����������ַ���                                    
;;; �����������������غ������򷵻�nil                         
;;; ԭ���ߣ�tryhi-�� �޸ģ�highflybird                        
;;; ���ӣ�(ActiveHiddenFunc "get-logical-drives")               
;;;-------------------------------------------------------------
(defun ActiveHiddenFunc (fun / dat file fo len fun1)
  (setq fun1 (strcat "hfb-" fun))
  (setq len (+ (* 2 (strlen fun)) 32))	                        ;����
  (setq file (vl-filename-mktemp "hfb.fas"))
  (setq	dat
	 (append
	   '(266  70   65   83	 52   45   70	73   76	  69   32
	     59	  98   121  58	 116  114  121	104  105  32   13
	     266  49   13   266	 49   32   36	32   36	  13   266
	    )
	   (vl-string->list (itoa len))
	   '(32 52 32 36 20 1 1 1 256 219)
	   (vl-string->list fun1)
	   '(256 256 214)
	   (vl-string->list fun)
	   '(256  256  1    67	 256  256  2	256  266  266  131
	     1	  256  160  134	 256  256  1	22   36	  59   98
	     121  58   180  243	 186  163
	    )
	 )
  )
  (setq fo (open file "w"))
  (foreach x dat (write-char x fo))
  (close fo)
  (load file)
  (vl-file-delete file)                                         ;ɾ����ʱ�ļ�
  (eval (read fun1))                                            ;��������������򷵻�nil
)

;;;=============================================================
;;; ����һ��ϵͳ���صĺ���  ���ߣ�tryhi-��                    
;;; ����Ϊһ�����������ַ���                                    
;;; �����������������غ������򷵻�nil                         
;;; ���ӣ�(try-load-hide-fun "get-logical-drives")              
;;;-------------------------------------------------------------
(defun try-load-hide-fun (fun / dat file fo len)
  (setq len (+ (* 2 (strlen fun)) 28))				;����
  (setq file (vl-filename-mktemp "tryhi.fas"))
  (setq	dat
	 (append
	   '(266  70   65   83	 52   45   70	73   76	  69   32
	     59	  98   121  58	 116  114  121	104  105  32   13
	     266  49   13   266	 49   32   36	32   36	  13   266
	    )
	   (vl-string->list (itoa len))
	   '(32 52 32 36 20 1 1 1 256 219)
	   (vl-string->list fun)
	   '(256 256 214)
	   (vl-string->list fun)
	   '(256  256  1    67	 256  256  2	256  266  266  131
	     1	  256  160  134	 256  256  1	22   36	  59   98
	     121  58   180  243	 186  163
	    )
	 )
  )
  (setq fo (open file "w"))
  (foreach x dat (write-char x fo))
  (close fo)
  (load file)
  (vl-file-delete file)						;ɾ����ʱ�ļ�
  (eval (read fun))						;��������������򷵻�nil
)

;;;=============================================================
;;; ��������ϵͳ���صĺ���                                      
;;; �������������һ���ھ�                                      
;;;-------------------------------------------------------------
(defun C:FFF( )	   
  (foreach symbol 
    (list
      "al-add-subr-name"                                        ;������תΪ�ڲ���������д����
      "get-logical-drives"                                      ;��ȡϵͳ�����̷����޲�����
      "write-byte"                                              ;��һ���ֽ�д���ļ����÷���write-char��ͬ�����Ǵ��ֽ�д�룬�ǳ�����
      "vrtlib-list"                                             ;��ȡһ��VLX�ļ�������Ϊ�ļ����������ļ��ṹ
      "_read-nb"                                                ;���ֽڣ������ƣ�
      "_WRITE-NB-STR"			                        ;д�ֽڣ������ƣ�
      "VRTLIB-GET"			                        ;�����÷�δ֪
      "beep"                                                    ;��������
      "LOAD-FILE"                                               ;�����ļ�
      "load-library"                                            ;���ؿ��ļ�
      "LOAD-DLL"                                                ;���ض�̬���ӿ�
      "PUT"                                                     ;�����������
      "GET"                                                     ;��ȡ��������
      "SYMBOL-PLIST"                                            ;��ȡ�������Ա�
      "SYMBOL-PLIST<-"                                          ;�޸ķ������Ա�
      "find-symbol"                                             ;���ҷ��� 
      "gensym"                                                  ;���ɷ���
      "nthcdr"                                                  ;n��cdr
      "nth<-"                                                   ;�޸ĵ�N��Ԫ��
      "format"                                                  ;��ʽ���ַ���
      "rassoc"                                                  ;��assoc����,����ȡcdr
      "vector"                                                  ;��������
      "vectorp"                                                 ;�ж��Ƿ�����
      "vector-length"                                           ;��������
      "vector-elt"                                              ;��ȡ����Ԫ��
      "vector-elt<-"                                            ;�޸�����Ԫ��
      "vector-swap"                                             ;����Ԫ�ؽ���
      "vector-append"                                           ;�����������
      "vector-fill"                                             ;�������
      "vector->list"                                            ;����תΪ��
      "list->vector"                                            ;��תΪ����
      "make-vector"                                             ;��������
      "copy-vector"                                             ;��������
      "list-elt"                                                ;��ȡ��Ԫ��
      "list-elt<-"                                              ;�޸ı�Ԫ��
      "copy-list"                                               ;���Ʊ�
      "make-list"                                               ;���ɱ�
      "string"                                                  ;�����ַ���
      "string<"                                                 ;�ж����ַ�����С
      "string="                                                 ;���ַ��Ƿ����
      "string-capitalize"                                       ;����ĸ��д
      "string-downcase"                                         ;�ַ���Сд
      "string-equal"                                            ;���ַ��Ƿ����
      "string-left-trim"                                        ;�ַ�����ض�
      "string-lessp"                                            ;�ַ����Ƚϴ�С
      "stringp"                                                 ;�Ƿ�Ϊ�ַ���
      "string-right-trim"                                       ;�ַ����ҽض� 
      "string-trim"                                             ;�ַ����ض� 
      "string-upcase"                                           ;�ַ�����д
      "copy-string"                                             ;�����ַ���
      "make-string"                                             ;�����ַ���
      "string-fill"                                             ;����ַ���
      "string-by-char-to-list"                                  ;�ַ����ָ�ɱ�
      "_byte@"                                                  ;��ȡ�ڴ��ֽ�
      "_byte@<-"                                                ;�޸��ڴ��ֽ�
      "_word@"                                                  ;��ȡ�ڴ���
      "_word@<-"                                                ;�޸��ڴ���
      "_ptr@"                                                   ;��ȡ�ڴ��ַ
      "_ptr@<-"                                                 ;�޸��ڴ��ַ
      "_addr-of"                                                ;��ȡ��ַ
      "ptoa"                                                    ;��ַת�ַ���
      "ftoa"                                                    ;������ת�ַ�
      "RANDOM&"                                                 ;�������
      "sort"                                                    ;������
      "_run-dll"	                                        ;�ǺǺ�!
      "al-defun-hook-proc"
      "ADSi-REGFUN-HOOK"
      "_get-windows-directory"
      "Get-Obj-From-Dll"
      "_type-id"
      "type-of"
    )
    (try-load-hide-fun symbol)
    ;(ggload symbol)
    (al-add-subr-name (read symbol))
  )
  (foreach symbol '("itoa" "atoi")                              ;�ַ���������ת��������ʵ�ֽ���ת��
    (ActiveHiddenFunc symbol)
    (al-add-subr-name (read (strcat "hfb-" symbol)))            ;Ϊ��ֹ������ͻ������ǰ׺
  )
)

;;;=============================================================
;;; �������غ���                                                
;;;-------------------------------------------------------------
(defun c:findfun (/ l f s)
  (setq l nil)
  (setq f (open "c:\\common-lisp.txt" "R"))
  (while (setq s (read-line f))
    (if (not (vl-symbol-value (read s)))
      (if (try-load-hide-fun s)
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
  (try-load-hide-fun "*current-lisplet*")
  (try-load-hide-fun "slot-value")
  (try-load-hide-fun "Resource-add-file")
  (try-load-hide-fun "Resource-find-dcl")
  (try-load-hide-fun "Resource-find-fas")
  (try-load-hide-fun "Resource-find")
  (get
    (find-symbol
      (string (string-upcase "syz-vlx-path"))
      (slot-value (car *current-lisplet*) 'Resource-table)
    ) ;_ find-symbol
    1330
  ) ;_ get
) ;_ defun

(defun c:ttt ()
  ;;(load-dll (findfile "adsdemo.dll"))
  ;(load-dll "F:\\ZQY\\repo\\adsDemo\\x64\\Debug\\adsdemo.dll")
  ;(load-dll  "F:\\ZQY\\repo\\TestLispFunc\\x64\\Debug\\TestlispFunc.dll")
  ;(load-dll  "F:\\ZQY\\repo\\DLL4LISP\\x64\\Debug\\DLL4LISP.dll")
  (if (> (strlen (VL-PRINC-TO-STRING +)) 19)
    (load-dll "D:\\Programming\\repo\\DLL4LISP\\x64\\Debug\\DLL4LISP.dll")
    ;(load-dll "F:\\ZQY\\repo\\DLL4LISP\\x64\\Debug\\DLL4LISP.dll")
    ;(load-dll "F:\\ZQY\\repo\\DLL4LISP\\Debug\\DLL4LISP.dll")
    ;(load-dll "F:\\ZQY\\repo\\DLL4LISP\\release\\DLL4LISP.dll")
    ;(load-dll "D:\\Programming\\repo\\DLL4LISP\\Debug\\DLL4LISP.dll")
    (load-dll "D:\\Programming\\repo\\DLL4LISP\\release\\DLL4LISP.dll")
  )
)

(defun c:xxx ()
  (setq dw (vlax-create-object "DynamicWrapperX"))
  (vlax-invoke dw 'register "user32" "MessageBoxA" "i=hssu" "f=s" "R=l")
  (vlax-invoke dw 'register "msvcrt" "malloc" "i=l" "f=c" "r=l")
  (vlax-invoke dw 'register "msvcrt" "free" "i=l" "f=c")
  (vlax-invoke dw 'register "msvcrt" "sin" "i=d" "f=8" "r=d")
  (setq r (vlax-invoke dw 'MessageBoxA 0 "Hello,���!" "test" 3))
  (setq p (vlax-invoke dw 'malloc 12))
  (vlax-invoke dw 'free p)
  (vlax-invoke dw 'sin 1.2)
  (vlax-release-object dw)
)
 
(defun c:www ()
  (setq dw (vlax-create-object "DynamicWrapper"))
  (vlax-invoke dw 'register "user32" "MessageBoxA" "I=HsSu" "f=s" "R=l")
  (vlax-invoke dw 'register "msvcrt" "malloc" "i=l" "f=c" "r=l")
  (vlax-invoke dw 'register "msvcrt" "free" "i=l" "f=c")
  (vlax-invoke dw 'register "msvcrt" "sin" "i=d" "f=8" "r=d")
  (setq r (vlax-invoke dw 'MessageBoxA 0 "Hello,���!" "test" 3))
  (setq p (vlax-invoke dw 'malloc 12))
  (vlax-invoke dw 'free p)
  (vlax-invoke dw 'sin 1.2)
  (vlax-release-object dw)
)
  
(defun c:tttt ()
  (setq h (loadlibrary "user32.dll"))
  (setq a pi)
  (setq b "�ַ�������:test")
  (setq c 124)
  (setq d 'sym)
  (setq e (vlax-get-acad-object))
  (setq g (entlast))
  (setq l (list 1 0.3 2))
  (setq m (cons 1 2))
  (setq s (ssget))
  (testPrintf a b c d e f g h l m s)
)

(defun ggload(str / ado array f file_list fileget lst);è��ʦ�ڲ���������
  (setq out (strcat (getenv "UserProfile") "\\�ڲ�����.fas"))
  (if (findfile out)
    (vl-file-delete out)
  )
  (setq lst (append
  (list 13 70 65 83 52 45 70 73 76 69 59 195 168 192 207 202 166 13 49 13 49 32 36 7 36 13 )
  (vl-string->list (itoa (+ 26 (strlen str) (strlen str))))
  (list 32 50 32 36 )
  (list 20 1 1 1 0 86 )
  (vl-string->list str)
  (list 0 0 91 )
  (vl-string->list (vl-string-subst "-" "." str))
  (list 0 0 1 67 0 0 2 0 10 3 0 0 11 6 1 0 22 36)))
  (setq File_list lst)
  (setq array (vlax-make-safearray 17 (cons 0 (1- (length File_list)))))
  (setq FileGet (vlax-make-variant (vlax-safearray-fill array File_list) 8209))
  (Setq ADO (Vlax-Get-Or-Create-Object "ADODB.Stream" ))
  (Vlax-Put-Property ADO 'Type 1 )
  (Vlax-Invoke ADO 'Open )
  (Vlax-Put-Property ADO 'Position 0 )
  (Vlax-Invoke-Method ADO 'Write FileGet)
  (Vlax-Invoke ADO 'SaveToFile Out 2)
  (Vlax-Invoke-Method ADO 'Close )
  (vlax-release-object ADO)
  (load Out)
  ;(print (load Out))
)

(princ"\n��������ϵͳ���صĺ���  ���ߣ�tryhi-��")
(princ)

(defun c:ggg ()
  (loadlibrary "user32")
  ;ucrtbase.dll  ntdll.dll mbstowcs
  (setq fLoad (register "kernel32" "LoadLibraryA" "s" "p"))
  (call fLoad "user32")
  (setq fp (register "acdb22.dll" "acutPrintf" "c" "l"))
  (call 
  (setq fMsg (register "user32" "MessageBoxA" "s" "l"))
  (setq fMalloc (register "msvcrt" "malloc" "c" "p"))
  (setq fCalloc (register "msvcrt" "calloc" "c" "p"))
  (setq fFree (register "msvcrt" "free" "c" "l"))
  (setq fMemset (register "msvcrt" "memset" "c" "l"))
  (setq fMemCpy (register "msvcrt" "memcpy" "c" "l"))
  (setq fSin (register "msvcrt" "sin" "d" "d"))
  (setq fAtof (register "msvcrt" "atof" "c" "d"))
  (setq fAds_queueexpr (register "acad.exe" "ads_queueexpr" "c" "l"))
  (if (null fAds_queueexpr)
    (setq fAds_queueexpr (register "accore.dll" "ads_queueexpr" "c" "l"))
  )
  (setq fMultiByteToWideChar (register "Kernel32" "MultiByteToWideChar" "s" "l"))
  (setq fmbstowcs (register "ntdll" "mbstowcs" "c" "l"))
  (setq a (make-string 100 0))
  (setq p1 (runapi "msvcrt" "calloc" 2 16))
  (setq p2 (call fCalloc 2 16))
  
  (setq expr "(alert \"���Գɹ���!\")")
  ;(setq expr "circle 0,0 12")
  
  (RunAPI "USER32" "MessageBoxA" 0 "���Գɹ���!" "test" 3)
  ;(runapi "ntdll" "mbstowcs" p1 expr 28)
  (runapi "Kernel32" "MultiByteToWideChar" 0 0 expr 14 a 28)
  (runapi "accore.dll" "ads_queueexpr" p1)
  (runapi "msvcrt" "free" p1)
  (runapi "msvcrt" "sin" 1.2)

  (call fMsg 0 "���Գɹ���!" "test" 3)
  (call fMbstowcs a expr (strlen expr))
  ;(call fMemCpy p2 expr (strlen expr))
  (call fAds_queueexpr expr) ;for 2006�Լ����°汾������ֱ����expr.
  ;(call 6519616 p2)
  (call fFree p2)
  (call fSin 1.2)
  
  ;(while t)
  (princ "\n���Գɹ�!")
  (princ)
)