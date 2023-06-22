;;以下程序由网友收集可用来学习，请勿拿 它来做不正当商业使用V1.01 2014-8-1
;;涉密文件，对FAS文件还原解码工具，测试32位CAD的使用正常，64位CAD不成功 建议用CAD 2004-2006
;;2014-9-5优化速度 将append改为cons， 改nth为car cdr 组合
;;速度提高100倍 1M的FAS文件 都是在10秒搞定

;;修正高版本64位转二进制错误,CAD2011X64测试通过 by edata @2017-9-15

(vl-load-com)
;读取文件转成10进制_BY不死猫
;;读取fas文件返回表
(defun dec(fname /)
  (Setq ADO (Vlax-Get-Or-Create-Object "ADODB.Stream"))
  (Vlax-Put-Property ADO 'Type 1)
  (Vlax-Invoke ADO 'Open)
  (Vlax-Invoke-Method ADO 'LoadFromFile fname)
  (Vlax-Put-Property ADO 'Position 0)
  (setq FileGet (Vlax-Invoke-Method ADO 'Read (Vlax-Get ADO 'Size)))
  (Vlax-Invoke-Method ADO 'Close)
  (setq File_list (vlax-safearray->list (vlax-variant-value FileGet)))
  (vlax-release-object ADO)
  file_list
)

;BY 不死猫
;;读取表写出fas文件
(defun Text (File_list Out)
  (setq array (vlax-make-safearray 17 (cons 0 (1- (length File_list)))))
  (setq	FileGet	(vlax-make-variant
		  (vlax-safearray-fill array File_list)
		  8209
		)
  )
  (Setq ADO (Vlax-Get-Or-Create-Object "ADODB.Stream"))
  (Vlax-Put-Property ADO 'Type 1)
  (Vlax-Invoke ADO 'Open)
  (Vlax-Put-Property ADO 'Position 0)
  (Vlax-Invoke-Method ADO 'Write FileGet)
  (Vlax-Invoke ADO 'SaveToFile Out 2)
  (Vlax-Invoke-Method ADO 'Close)
  (vlax-release-object ADO)
)

(defun c:q1(/ aa bb toupl $1 toupl ee1 $1sta de13 da0lst num $1end $2 strdat0 tmpl
	    $3 tmpl1 de32 da1lst num1 $3end strdat1 keylen keyend keylst lastlst zjlst
	    nstrlst0 nstrlst1 fpl fpl1 dfg ee tmp tlst err)
  (setq t0 (gettime));设置起点
  (setq f (getfiled "请选择文件" "c:/" "" 8))
  (setq bb (dec f))  ;"d:\\a1.fas"
  ;;;;;;;;;;;;;;;;;;
  (if (> (atoi (getvar "acadver"))16)
    (progn
      (setq decint (- (car bb) 13))
      (setq bb (mapcar '(lambda(x) (- x decint)) bb))
      )
    )
  ;;;;;;;;;修正高版本64位转二进制错误,CAD2011X64测试通过 by edata @2017-9-15
  ;;;;;;;;;;;;;;;;
  (setq $1 (vl-position 36 bb))
  (setq tmp (tiqulst bb 34 $1))
  (setq ee (vl-position 13 tmp))
  (setq tlst (tiqulst tmp 0 ee))
  (setq dfg (dec-fix tlst))
  (setq toupl (tiqulst bb 0 (+ $1 1)))
  (if (/= dfg 1)
    (progn
      (setq ee1 (tiqulst bb 34 $1))
      (setq $1sta (1+ $1));第一段字符流开始
      (setq de13 (vl-position 13 ee1));查找位置
      (setq da0lst (tiqulst ee1 0 de13));得到第一段字符长度表
      (setq num (dec-fix da0lst));;得到第一段长度
      (setq $1end (+ $1sta num));第一段字符流结束
      (setq $2 (1+ $1end));第二个$
      (setq strdat0 (tiqulst bb $1sta $1end));取得第一段字符流
      (setq tmpl (tiqulst bb $2 (+ $2 50)));临时设置有50个字符
      (setq $3 (+ $2 1 (vl-position 36 tmpl)));取得第三个$位置
      (setq tmpl1 (cddr (tiqulst bb $2 $3)));做一个临时表，为取得第二段长度做前提
      (setq de32 (vl-position 32 tmpl1));找到位置
      (setq da1lst (tiqulst tmpl1 0 de32));得到第二段字符长度表
      (setq num1 (dec-fix da1lst));;得到第二段长度
      (setq $3end (+ $3 num1));$表示取得第二段字符流结束
      (setq strdat1 (tiqulst bb $3 $3end));取得第二段字符流
      (setq keylen (nth $3end bb));得到 KEY索引值
      (setq keyend (+ $3end keylen 1));得到KEY最后一个索引
      (setq keylst (tiqulst bb (+ 1 $3end) keyend));取得KEY表
      (setq lastlst (tiqulst bb keyend (length bb)));取得KEY后最后的表
      (setq zjlst (tiqulst bb (- $2 1) $3));取得中间长度那一段字符流
      ;(setq nstrlst0 (mixor strdat0 keylst));进行解码运算
      (setq t1 (gettime));设置时间点
      (princ (strcat "\n第一段读取文件切割成表耗时" (rtos (- t1 t0) 2 3) "秒"));
      (setq nstrlst1 (mixor strdat1 keylst));进行解码运算
      (setq fpl (list toupl strdat0 zjlst nstrlst1 (list keylen) (cons  0 (cdr keylst)) lastlst))      
      (setq fpl1 (append toupl strdat0 zjlst nstrlst1 (list keylen) (cons  0 (cdr keylst)) lastlst))
      (PRINC FPL)
      (Text fpl1 "d:\\new_test1.fas")
      (princ "\n   转换成功")
      )
    (progn
      (setq $2 42);第二个$
      (setq tmpl (tiqulst bb $2 (+ $2 50)));临时设置有50个字符
      (setq $3 (+ $2 1 (vl-position 36 tmpl)));取得第三个$位置
      (setq tmpl1 (cddr (tiqulst bb $2 $3)));做一个临时表，为取得第二段长度做前提
      (setq de32 (vl-position 32 tmpl1));找到位置
      (setq da1lst (tiqulst tmpl1 0 de32));得到第二段字符长度表
      (setq num1 (dec-fix da1lst));;得到第二段长度
      (setq $3end (+ $3 num1));$表示取得第二段字符流结束
      (setq strdat1 (tiqulst bb $3 $3end));取得第二段字符流
      (setq keylen (nth $3end bb));得到 KEY索引值
      (setq keyend (+ $3end keylen 1));得到KEY最后一个索引
      (setq keylst (tiqulst bb (+ 1 $3end) keyend));取得KEY表
      (setq lastlst (tiqulst bb keyend (length bb)));取得KEY后最后的表
      (setq toupl (tiqulst bb 0 $3));取得表头固定资料
      (setq t1 (gettime));设置时间点
      (princ (strcat "\n第一段读取文件切割成表耗时" (rtos (- t1 t0) 2 3) "秒"));
      (setq nstrlst1 (mixor strdat1 keylst));进行解码运算
      (setq fpl (list toupl nstrlst1 (list keylen) (cons  0 (cdr keylst)) lastlst))
      (setq fpl1 (append toupl nstrlst1 (list keylen) (cons 0 (cdr keylst)) lastlst))
      (PRINC FPL)
      (Text fpl1 "d:\\new_test2.fas")
      (princ "\n   转换成功")
      )
    )
  (princ)
)


;;表转成整数值 (49 50 51 53)->1234
(defun dec-fix(lst / ii 1str len 1str 2str qq)
  (setq len (length lst))
  ;(setq ii 0)
  (setq 2str "")
  (repeat len
    (setq 1str (chr (car lst)))    
    (setq 2str (strcat 2str 1str))
    (setq lst (cdr lst))
    ;(setq ii (1+ ii))
    )
  (setq qq (atoi 2str))
  qq
  )

;提取表         表 开始 结束索引值
(defun tiqulst (lst sta end / newpl num ann)
  (if (< end (+ (length lst) 1))
    (progn
      (setq newpl '())
      (repeat sta
	(setq lst (cdr lst ))
	)
      (setq ann (- end sta))
      (repeat ann
	(setq num (car lst))
	(setq lst (cdr lst))
	(setq newpl (cons  num newpl))
	)
      )
    )
  (reverse newpl)
  )

;;;异或运算
(defun booledata (num1 mi1 mi2)
  (boole 6 (boole 6 num1 mi2)mi1)
  )

;;;对字符流与密码解码,两个参数都是表
(defun mixor (datapl  mipl / num1  ni newpl wn mi1 mi2 jiemapl wn)
  (while (< (length mipl) (length datapl))
    (setq mipl (append mipl mipl))
    )
  (setq jiemapl '())
  (setq ti1 (gettime));设置时间点
  (setq ni (length datapl))
  (repeat ni
    (setq num1   (car datapl)
          mi1    (car mipl)
	  mi2    (cadr mipl)
	  datapl (cdr datapl)
	  mipl   (cdr mipl)
	  )
    (setq newpl (booledata num1 mi1 mi2))
    (setq jiemapl (cons  newpl  jiemapl))
    )
  (setq jiemapl (reverse jiemapl))
  (setq ti2 (gettime));设置时间点
  (princ (strcat "\n第二段进行布尔运算耗时" (rtos (- ti2 ti1) 2 3) "秒"));
  jiemapl
)


(defun gettime ()
 (* 86400 (getvar "tdusrtimer"))
)