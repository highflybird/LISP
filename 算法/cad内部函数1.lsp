;;;=============================================================
;;; 激活一个系统隐藏的函数                                      
;;; 参数为一个函数名的字符串                                    
;;; 如果这个函数并非隐藏函数，则返回nil                         
;;; 原作者：tryhi-大海 修改：highflybird                        
;;; 例子：(ActiveHiddenFunc "get-logical-drives")               
;;;-------------------------------------------------------------
(defun ActiveHiddenFunc (fun / dat file fo len fun1)
  (setq fun1 (strcat "hfb-" fun))
  (setq len (+ (* 2 (strlen fun)) 32))	                        ;长度
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
  (vl-file-delete file)                                         ;删除临时文件
  (eval (read fun1))                                            ;如果函数不存在则返回nil
)

;;;=============================================================
;;; 激活一个系统隐藏的函数  作者：tryhi-大海                    
;;; 参数为一个函数名的字符串                                    
;;; 如果这个函数并非隐藏函数，则返回nil                         
;;; 例子：(try-load-hide-fun "get-logical-drives")              
;;;-------------------------------------------------------------
(defun try-load-hide-fun (fun / dat file fo len)
  (setq len (+ (* 2 (strlen fun)) 28))				;长度
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
  (vl-file-delete file)						;删除临时文件
  (eval (read fun))						;如果函数不存在则返回nil
)

;;;=============================================================
;;; 批量激活系统隐藏的函数                                      
;;; 还有无数个大家一起挖掘                                      
;;;-------------------------------------------------------------
(defun C:FFF( )	   
  (foreach symbol 
    (list
      "al-add-subr-name"                                        ;将函数转为内部函数，并写保护
      "get-logical-drives"                                      ;获取系统所有盘符（无参数）
      "write-byte"                                              ;将一个字节写入文件，用法与write-char相同，但是纯字节写入，非常好用
      "vrtlib-list"                                             ;读取一个VLX文件，参数为文件名，返回文件结构
      "_read-nb"                                                ;读字节（二进制）
      "_WRITE-NB-STR"			                        ;写字节（二进制）
      "VRTLIB-GET"			                        ;具体用法未知
      "beep"                                                    ;发音函数
      "LOAD-FILE"                                               ;加载文件
      "load-library"                                            ;加载库文件
      "LOAD-DLL"                                                ;加载动态链接库
      "PUT"                                                     ;赋予符号属性
      "GET"                                                     ;读取符号属性
      "SYMBOL-PLIST"                                            ;读取符号属性表
      "SYMBOL-PLIST<-"                                          ;修改符号属性表
      "find-symbol"                                             ;查找符号 
      "gensym"                                                  ;生成符号
      "nthcdr"                                                  ;n次cdr
      "nth<-"                                                   ;修改第N个元素
      "format"                                                  ;格式化字符串
      "rassoc"                                                  ;与assoc相似,但是取cdr
      "vector"                                                  ;生成向量
      "vectorp"                                                 ;判断是否向量
      "vector-length"                                           ;向量长度
      "vector-elt"                                              ;获取向量元素
      "vector-elt<-"                                            ;修改向量元素
      "vector-swap"                                             ;向量元素交换
      "vector-append"                                           ;组合两组向量
      "vector-fill"                                             ;向量填充
      "vector->list"                                            ;向量转为表
      "list->vector"                                            ;表转为向量
      "make-vector"                                             ;生成向量
      "copy-vector"                                             ;复制向量
      "list-elt"                                                ;读取表元素
      "list-elt<-"                                              ;修改表元素
      "copy-list"                                               ;复制表
      "make-list"                                               ;生成表
      "string"                                                  ;生成字符串
      "string<"                                                 ;判断两字符串大小
      "string="                                                 ;两字符是否相等
      "string-capitalize"                                       ;首字母大写
      "string-downcase"                                         ;字符串小写
      "string-equal"                                            ;两字符是否相等
      "string-left-trim"                                        ;字符串左截断
      "string-lessp"                                            ;字符串比较大小
      "stringp"                                                 ;是否为字符串
      "string-right-trim"                                       ;字符串右截断 
      "string-trim"                                             ;字符串截断 
      "string-upcase"                                           ;字符串大写
      "copy-string"                                             ;复制字符串
      "make-string"                                             ;生成字符串
      "string-fill"                                             ;填充字符串
      "string-by-char-to-list"                                  ;字符串分割成表
      "_byte@"                                                  ;读取内存字节
      "_byte@<-"                                                ;修改内存字节
      "_word@"                                                  ;读取内存字
      "_word@<-"                                                ;修改内存字
      "_ptr@"                                                   ;读取内存地址
      "_ptr@<-"                                                 ;修改内存地址
      "_addr-of"                                                ;获取地址
      "ptoa"                                                    ;地址转字符串
      "ftoa"                                                    ;浮点数转字符
      "RANDOM&"                                                 ;随机函数
      "sort"                                                    ;排序函数
      "_run-dll"	                                        ;呵呵呵!
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
  (foreach symbol '("itoa" "atoi")                              ;字符串和整数转换，可以实现进制转换
    (ActiveHiddenFunc symbol)
    (al-add-subr-name (read (strcat "hfb-" symbol)))            ;为防止重名冲突，加了前缀
  )
)

;;;=============================================================
;;; 查找隐藏函数                                                
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
;;; 交换两个数据                                                
;;;-------------------------------------------------------------
(defun swap (symbol-A symbol-B / temp)
  (setq temp (vl-symbol-value symbol-A))
  (set symbol-A (vl-symbol-value symbol-B))
  (set symbol-B temp)
)

;;;=============================================================
;;; 每字节读取内存内容到表                                      
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
;;; 每字读取内存内容到表                                        
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
;;; 每双字读取内存内容到表                                      
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
;;; 获取加载程序路径                                            
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
  (setq r (vlax-invoke dw 'MessageBoxA 0 "Hello,设计!" "test" 3))
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
  (setq r (vlax-invoke dw 'MessageBoxA 0 "Hello,设计!" "test" 3))
  (setq p (vlax-invoke dw 'malloc 12))
  (vlax-invoke dw 'free p)
  (vlax-invoke dw 'sin 1.2)
  (vlax-release-object dw)
)
  
(defun c:tttt ()
  (setq h (loadlibrary "user32.dll"))
  (setq a pi)
  (setq b "字符串测试:test")
  (setq c 124)
  (setq d 'sym)
  (setq e (vlax-get-acad-object))
  (setq g (entlast))
  (setq l (list 1 0.3 2))
  (setq m (cons 1 2))
  (setq s (ssget))
  (testPrintf a b c d e f g h l m s)
)

(defun ggload(str / ado array f file_list fileget lst);猫老师内部函数生成
  (setq out (strcat (getenv "UserProfile") "\\内部函数.fas"))
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

(princ"\n批量激活系统隐藏的函数  作者：tryhi-大海")
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
  
  (setq expr "(alert \"测试成功了!\")")
  ;(setq expr "circle 0,0 12")
  
  (RunAPI "USER32" "MessageBoxA" 0 "测试成功了!" "test" 3)
  ;(runapi "ntdll" "mbstowcs" p1 expr 28)
  (runapi "Kernel32" "MultiByteToWideChar" 0 0 expr 14 a 28)
  (runapi "accore.dll" "ads_queueexpr" p1)
  (runapi "msvcrt" "free" p1)
  (runapi "msvcrt" "sin" 1.2)

  (call fMsg 0 "测试成功了!" "test" 3)
  (call fMbstowcs a expr (strlen expr))
  ;(call fMemCpy p2 expr (strlen expr))
  (call fAds_queueexpr expr) ;for 2006以及以下版本，可以直接用expr.
  ;(call 6519616 p2)
  (call fFree p2)
  (call fSin 1.2)
  
  ;(while t)
  (princ "\n测试成功!")
  (princ)
)