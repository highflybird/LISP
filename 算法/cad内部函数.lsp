;;;=============================================================
;;; 程序加载后，就可以用tranf来激活一个系统隐藏的函数           
;;; 用法：(tranf "内部函数名")                                  
;;; 参数：函数名的字符串                                        
;;; 返回：T存在或者是可用的内部函数，nil不存在或则无效          
;;; 作者：highflybird                                           
;;; 例子：(tranf "get-logical-drives")                          
;;; 此程序得到网友baitang36的大力帮助，特此致谢！               
;;; 另外借鉴了不死猫和tryhi的代码，在此一并感谢！               
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
  
  ;;此函数没有副作用，得到一个函数或符号的地址（引用），推荐使用
  ;;例子：(setq type-of (getintern 'type-of)) (type-of 2324)    
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
  
  ;;此函数将内部转为普通，且同名赋值，改变颜色，受保护。有副作用
  ;;例子：(tranf 'type-of) (type-of 2324)                       
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
    (princ "\n已打开内部函数转普通函数大门.\n")
    (princ "\n激活内部函数转普通函数失败!\n")
  )
)
(or lpp-symfun->al (HFB-LOAD-TRANF))

;;;=============================================================
;;; 批量激活系统隐藏的函数                                      
;;; 还有无数个大家一起挖掘                                      
;;;-------------------------------------------------------------
(defun ActiveIntern ( )
  (foreach symbol 
    '(al-add-subr-name                                          ;将函数转为内部函数，并写保护
      get-logical-drives                                        ;获取系统所有盘符（无参数）
      write-byte                                                ;将一个字节写入文件，用法与write-char相同，但是纯字节写入，非常好用
      _read-nb                                                  ;读字节（二进制）
      _WRITE-NB-STR			                        ;写字节（二进制）
      vrtlib-list                                               ;读取一个VLX文件，参数为文件名，返回文件结构
      VRTLIB-GET			                        ;具体用法未知
      beep                                                      ;发音函数
      LOAD-FILE                                                 ;加载文件
      load-library                                              ;加载库文件
      LOAD-DLL                                                  ;加载动态链接库
      _run-dll	                                                ;呵呵呵!
      PUT                                                       ;赋予符号属性
      GET                                                       ;读取符号属性
      SYMBOL-PLIST                                              ;读取符号属性表
      SYMBOL-PLIST<-                                            ;修改符号属性表
      lpp-symfun->al                                            ;内部函数转普通函数
      intern                                                    ;内部函数
      make-symbol                                               ;生成符号
      find-symbol                                               ;查找符号
      symbol-name                                               ;符号名
      gensym                                                    ;生成符号带后缀并递增后缀
      nthcdr                                                    ;n次cdr
      nth<-                                                     ;修改第N个元素
      format                                                    ;格式化字符串
      rassoc                                                    ;与assoc相似,但是取cdr
      vector                                                    ;生成向量
      vectorp                                                   ;判断是否向量
      vector-length                                             ;向量长度
      vector-elt                                                ;获取向量元素
      vector-elt&                                               ;获取向量元素location
      vector-elt<-                                              ;修改向量元素
      vector-swap                                               ;向量元素交换
      vector-append                                             ;组合两组向量
      vector-fill                                               ;向量填充
      vector->list                                              ;向量转为表
      vector-position                                           ;元素在向量的位置
      make-vector                                               ;生成向量
      copy-vector                                               ;复制向量
      list->vector                                              ;表转为向量
      list-elt                                                  ;读取表元素
      list-elt<-                                                ;修改表元素
      copy-list                                                 ;复制表
      make-list                                                 ;生成表
      string                                                    ;生成字符串
      string<                                                   ;判断两字符串大小
      string=                                                   ;两字符是否相等
      string-capitalize                                         ;首字母大写
      string-downcase                                           ;字符串小写
      string-equal                                              ;两字符是否相等
      string-left-trim                                          ;字符串左截断
      string-lessp                                              ;字符串比较大小
      stringp                                                   ;是否为字符串
      string-right-trim                                         ;字符串右截断 
      string-trim                                               ;字符串截断 
      string-upcase                                             ;字符串大写
      string-fill                                               ;填充字符串
      string-by-char-to-list                                    ;字符串分割成表
      string-elt<-                                              ;修改字符串的某个字符
      copy-string                                               ;复制字符串
      make-string                                               ;生成字符串
      _byte@                                                    ;读取内存字节
      _byte@<-                                                  ;修改内存字节
      _word@                                                    ;读取内存字
      _word@<-                                                  ;修改内存字
      _ptr@                                                     ;读取内存地址
      _ptr@<-                                                   ;修改内存地址
      _addr-of                                                  ;获取地址
      _type-id                                                  ;类型ID
      type-of                                                   ;类型
      ptoa                                                      ;地址转字符串
      ftoa                                                      ;浮点数转字符
      RANDOM&                                                   ;随机函数
      sort                                                      ;排序函数
      al-defun-hook-proc                                        ;钩子？探索中
      ADSi-REGFUN-HOOK                                          ;钩子？探索中
      _get-windows-directory                                    ;获取Windows目录
      Get-Obj-From-Dll					        ;从GUID获取Object
      _al-bind-alist                                            ;不清楚？文件合并
      _package-vector                                           ;两大包:autolisp,:lpp 
      _package-vector<-                                         ;修改包？
      al-fas-load                                               ;装载fas文件
      dcl-call-back                                             ;探索中
      make-string-input-stream                                  ;从字符串获取流
      load-stream                                               ;从流中加载
      al-load-stream
      funcall                                                   ;同apply,但不是表
      choose-file-dialog                                        ;选择文件对话框
      choose-editor-file-dialog                                 ;选择编辑文件对话框
      directory-browse-files                                    ;相当于VL-DIRECTORY-FILES
      sh-browse-for-folder                                      ;浏览文件夹并获得路径
      get-string-dialog                                         ;getstring对话框版
      _subr-entry-addr
    )
    (i2n symbol)                                                ;比tranf+al-add-subr-name好
  )
  (foreach symbol '(itoa atoi)                                  ;字符串和整数转换，可以实现进制转换
    (tranf symbol)
    (al-add-subr-name
      (cons (strcat "hfb-" (symbol-name symbol)) symbol)        ;为防止重名冲突，加了前缀
    )     
  )
  (princ "已批量转换一些系统内部函数!  作者：highflybird\n")
)
(or _addr-of (ActiveIntern))
;;;=============================================================
;;; 查找隐藏函数                                                
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

;;;通过Fas返回值直接获取内部函数，实现最小字节生成。
(defun getInternalFunc (FuncStr / file f l);by猫老师
	(setq file (strcat (getenv "UserProfile") "\\内部函数.fas"))
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

;;;另外用最少字节做了一个在内存加载fas文件流函数
(defun getInternalFuncStream (FuncStr);by猫老师
	(eval (al-fas-load (make-string-input-stream (vl-list->string (append 
		(list 70 65 83 52 45 70 73 76 69 13 49 13 48 32 36 1 36)
		(vl-string->list (itoa (+ 4 (strlen FuncStr))))
		(list 32 48 32 36 86 )
		(vl-string->list FuncStr)
		(list 0 0 22 36)
	)))))
)

;;;如果要一次性同时激活多个函数，可以做成一个表返回，然后从表中提取对应的内部函数。
;;;这样可以减少带宽和服务器访问次数，速度更快。
(defun getInternalFuncStream-s (FuncStrList / re len l);by猫老师
	(setq len (length FuncStrList))
	(if (> len 65535)
		(list (alert "一次生成不了这么多!")(exit))
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

