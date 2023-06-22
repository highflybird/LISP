;|**************************************************************
长度面积和体积统计工具                                          
****************************************************************
软件作者: Highflybird                                           
软件用途: 长度面积和体积统计                                    
日期地点: 2016.03.13 深圳                                       
程序语言: AutoLISP,Visual LISP                                  
版本号:   Ver. 1.0.160313                                       
================================================================
本软件为开源软件: 以下是开源申明:                               
----------------------------------------------------------------
本页面的软件遵照 GPL协议开放源代码，您可以自由传播和修改，在遵照
下面的约束条件的前提下:                                         
                                                                
一. 只要你在本开源软件的每一副本上明显和恰当地出版版权声明，保持
此许可证的声明和没有担保的声明完整无损，并和程序一起给每个其他的
程序接受者一份许可证的副本，你就可以用任何媒体复制和发布你收到的
原始的程序的源代码。你也可以为转让副本的实际行动收取一定费用，但
必须事先得到的同意。                                            
                                                                
二. 你可以修改本开源软件的一个或几个副本或程序的任何部分，以此形
成基于程序的作品。只要你同时满足下面的所有条件，你就可以按前面第
一款的要求复制和发布这一经过修改的程序或作品。                  
1.你必须在修改的文件中附有明确的说明: 你修改了这一文件及具体的修
  改日期。                                                      
2.你必须使你发布或出版的作品（它包含程序的全部或一部分，或包含由
  程序的全部或部分衍生的作品）允许第三方作为整体按许可证条款免费
  使用。                                                        
3.如果修改的程序在运行时以交互方式读取命令，你必须使它在开始进入
  常规的交互使用方式时打印或显示声明: 包括适当的版权声明和没有担
  保的声明（或者你提供担保的声明）；用户可以按此许可证条款重新发
  布程序的说明；并告诉用户如何看到这一许可证的副本。（例外情况: 
  如果原始程序以交互方式工作，它并不打印这样的声明，你的基于程序
  的作品也就不用打印声明。                                      
                                                                
三. 只要你遵循一、二条款规定，您就可以自由使用并传播本源代码，但
必须原封不动地保留原作者信息。                                  
**************************************************************|;
(prompt "\n统计数值命令为:AM.设置命令为:ASET")
(vl-load-com)
(defun C:am (/ d    e    h    i    n    o    p    x    ll   ur
	       pt   fil  sel  Lst  foo  val  str  sum  prop Data 
	       *DOC mode PREC Unit pref suff fact )
  ;;读取设置
  (if (null (setq Data (getenv "Statistics")))
    (setq Data (SetDefault))
    (setq Data (read Data))
  )
  ;;过滤选择
  (setq fil "")
  (foreach n (S:TypeList)
    (if	(= "1" (cdr (assoc n Data)))
      (if (= "OTHER" n)
	(setq fil (strcat fil "*,"))
	(setq fil (strcat fil n ","))
      )
    )
  )
  (if (setq Sel (ssget (list (cons 0 fil))))
    (progn
      ;;文字属性,精度,位置
      (setq prop (GetTextProps))
      (setq mode (cdr (assoc "Position" Data)))
      (setq Prec (atoi (cdr (assoc "Precision" Data))))
      (setq Unit (getvar "LUNITS"))
      ;;前缀和测量单位，比例因子，计算函数
      (GetFactor&Unit Data 'Suff 'fact 'foo)
      (if (= "1" (cdr (assoc "HasPrefix" Data)))
	(setq pref (cdr (assoc "Prefix" Data)))
	(setq pref "")
      )
      ;;设置回退标志
      (setq *DOC (vla-get-activeDocument (vlax-get-acad-object)))
      (vla-StartUndoMark *DOC)
      ;;取得每个图元的信息和测量值
      ;(setq TIME (getvar "millisecs"))
      (setq i 0)
      (setq sum 0)
      (repeat (sslength Sel)
	(setq e (ssname sel i))
	(setq o (vlax-ename->vla-object e))
	(setq n (substr (vla-get-objectname o) 5))
	(setq h (vla-get-Handle o))
	(setq x (list e n h))
	(setq i (1+ i))

	(vla-GetBoundingBox o 'll 'ur)
	(setq ll (vlax-safearray->list ll))
	(setq ur (vlax-safearray->list ur))
	(setq Pt (S:Text-Position ll ur mode))

	(setq val (* fact (apply foo (list e))))
	(setq str (rtos val Unit Prec))
	(setq Lst (cons (list X str Pt) Lst))
	(setq Sum (+ val Sum))
      )
      ;(setq TIME (- (getvar "millisecs") TIME))
      ;(princ "\n共费时：")
      ;(princ time)
      ;;对数值表排序
      ;(setq Lst (vl-sort Lst 'S:Compare))
      (setq Lst (reverse lst))
      ;;插入点
      (initget 1)
      (setq foo (cdr (assoc "Output" Data)))
      (if (or (= foo "DRAWING") (= foo "TABLE"))
	(setq pt (trans (getpoint "\n插入点:") 1 0))
	(setq pt '(0 0 0))
      )
      ;;统计测量值
      (setq str (rtos Sum Unit Prec))
      (setq val (strcat (itoa (length lst)) "个"))
      (setq val (list (list nil "总计" val) str pt))
      (if (= "1" (cdr (assoc "JustTotal" Data)))
	(setq Lst (list val))
	(setq lst (append Lst (list val)))
      )
      ;;为其他输出方式增加
      (setq Str (strcat "测量值(" suff ")"))
      (setq val (list (list nil "图元类型" "句柄") str pt))
      (setq lst (cons val lst))
      ;;输出方式
      (setq foo (read (strcat "S:Output-By-" foo)))
      (apply foo (list Lst pref suff prop))			;((eval foo) Lst pref suff prop)
      (vla-EndUndoMark *DOC)
    )
    (alert "你没有选取物体或者输入正确的数据!")
  )
  (princ)
)

;;;-------------------------------------------------------------
;;; 屏幕输出和文件输出的共同段                                  
;;;-------------------------------------------------------------
(defun S:OutPut-Common (lst pref suff file)
  (if file
    (defun S:Princ (str f) (princ str f))
    (defun S:Princ (str f) (princ str))
  )
  (S:Princ "每行内容依次为: " file)
  (foreach n Lst
    (S:Princ (strcat "\n" (cadar n) ": " (caddar n) ": " (cadr n)) file)			
  )
)

;;;-------------------------------------------------------------
;;; 屏幕显示                                                    
;;;-------------------------------------------------------------
(defun S:Output-By-Window (lst pref suff prop)
  (textSCR)
  (S:OutPut-Common lst pref suff nil)
  (textSCR)
)

;;;-------------------------------------------------------------
;;; 写文件方式输出                                              
;;;-------------------------------------------------------------
(defun S:Output-By-File (lst pref suff prop / file)
  (if (setq file (getfiled "输入文件名: " "C:/temp/" "txt" 5))
    (progn
      (setq file (open file "w"))
      (S:OutPut-Common lst pref suff file)
      (close file)
    )
    (alert "输出文件失败!")
  )
)

;;;-------------------------------------------------------------
;;; 用表格方式输出                                              
;;;-------------------------------------------------------------
(defun S:Output-By-Table (lst pref suff A1 / A2 H0 H1 P P0 P1 Q S X0 X1 X2 X3 Y Y0 Z0)
  (initget 1)                                       
  (setq A1 (subst '(72 . 0) (assoc 72 A1) A1))			;左对齐
  (setq A2 (subst '(72 . 2) (assoc 72 A1) A1))			;右对齐
  (setq p0 (caddr (last lst)))					;表格插入点
  (setq x0 (car p0))						;插入点X
  (setq y0 (cadr p0))						;插入点Y
  (setq z0 (caddr p0))                                          ;插入点Z
  (setq h0 (cdr (assoc 40 A1)))					;文字高
  (setq h1 (* h0 0.2))
  (setq x1 (+ (*  8 h0) x0))					;表格第二根竖线X值
  (setq x2 (+ (* 12 h0) x0))					;表格第三根竖线X值
  (setq x3 (+ (* 24 h0) x0))					;表格第四根竖线X值
  ;;表格抬头线
  (setq P1 (list x3 y0 z0))
  (Ent:make-line P0 P1)
  ;;画表格，并填写内容
  (setq y y0)
  (foreach n Lst
    (setq y (- y h0))
    (S:Make-Text nil (cadar  n) (list (+ x0 h1) y z0) A1)	;图元类型
    (S:Make-Text nil (caddar n) (list (- x2 h1) y z0) A2)	;图元句柄
    (S:Make-Text nil (cadr   n) (list (- x3 h1) y z0) A2)	;图元测量值
    (setq y (- y h0))
    (setq p (list x0 y z0))
    (setq q (list x3 y z0))
    (ent:make-line p q)						;画表格线
  )
  (ent:make-line P0 p)						;表格第一根竖线
  (ent:make-line P1 q)						;表格第四根竖线
  (ent:make-line (list x1 y0 z0) (list x1 y z0))		;表格第二根竖线
  (ent:make-line (list x2 y0 z0) (list x2 y z0))		;表格第三根竖线
)

;;;-------------------------------------------------------------
;;; 写Excel方式输出                                             
;;;-------------------------------------------------------------
(defun S:Output-By-Excel (lst pref suff prop / exl)
  (if (setq exl (vlax-get-or-create-object "Excel.Application"))
    (S:FillCells exl lst pref suff)
    (alert "找不到Excel程序或者不能正常加载Excel，程序退出!")
  )
)
(defun S:FillCells (exl lst pref suff / books book sheets sheet cells row prec col str)
  ;;从excel程序中获取excel的book,sheet和单元格集合
  (setq books  (vlax-get-property exl 'Workbooks))
  (setq book   (vlax-invoke-method books 'add))
  (setq sheets (vlax-get-property book 'worksheets))
  (setq sheet  (vlax-get-property sheets 'item 1))
  (setq cells  (vlax-get-property sheet 'cells))
  (setq col    (vlax-get-property sheet 'range "C:C"))
  ;;测量值的精确位数
  (setq Prec   (atoi (GetKeyData "Precision")))
  (setq prec   (strcat (rtos 0 (getvar "LUNITS") Prec) "_ "))    
  (vlax-put-property cells 'numberformat "@")			;其他单元格为文本
  (vlax-put-property cells 'ColumnWidth 12)			;列宽
  (vlax-put-property col 'numberformat prec)                    ;数值精确位
  (vlax-put-property col 'ColumnWidth 16)                       ;测量值列宽
  (vlax-put-property col 'HorizontalAlignment 4)
  ;;输出每行
  (setq row 1)
  (foreach n Lst
    (vlax-put-property cells 'item row 1 (cadar  n))
    (vlax-put-property cells 'item row 2 (caddar n))
    (vlax-put-property cells 'item row 3 (cadr   n))
    (setq row (1+ row))
  )
  (vlax-put-property exl 'UserControl 1)
  (vlax-put-property exl 'Visible 1)
)

;;;-------------------------------------------------------------
;;; 在图中标注输出                                              
;;;-------------------------------------------------------------
(defun S:Output-By-Drawing (lst pref suff prop / str)
  (foreach n (cdr Lst)
    (S:Make-Text
      (car n)
      (strcat pref (cadr n) suff)
      (caddr n)
      prop
    )
  )
)

;;;-------------------------------------------------------------
;;; 获取物体的长度                                              
;;;-------------------------------------------------------------
(defun Ent:Get-Length (ent / D N O)
  (setq O (vlax-ename->vla-object ent))
  (setq D (entget ent))
  (setq N (cdr (assoc 0 D)))
  (cond
    ( (vlax-property-available-p O 'length)
      (vla-get-length O)
    )
    ( (vlax-property-available-p O 'Perimeter)
      (vla-get-Perimeter O)
    )
    ( (member N '("ARC" "CIRCLE" "LINE" "POLYLINE" "LWPOLYLINE" "SPLINE" "ELLIPSE"))
      (vlax-curve-getdistatparam ent (vlax-curve-getendparam ent))
    )
    ( (= N "MLINE") (ml-length D))
    (T 0)
  )
)

;;;-------------------------------------------------------------
;;; MLINE的长度                                                 
;;;-------------------------------------------------------------
(defun ml-length (DXF / pts)
  (setq pts (vl-remove-if-not '(lambda (x) (= (car x) 11)) dxf))
  (setq pts (mapcar 'cdr pts))
  (length-of-verties pts (zerop (logand (cdr (assoc 71 dxf)) 2)))
)

;;;-------------------------------------------------------------
;;; n个点的长度                                                 
;;;-------------------------------------------------------------
(defun length-of-verties (pts IsClose /)
  (if isClose
    (apply '+ (mapcar 'distance pts (cons (last pts) pts)))
    (apply '+ (mapcar 'distance pts (cdr pts)))
  )
)
 
;;;-------------------------------------------------------------
;;; 获取物体的面积                                              
;;;-------------------------------------------------------------
(defun Ent:Get-Area (ent / O N D)
  (setq O (vlax-ename->vla-object ent))
  (setq D (entget ent))
  (setq N (cdr (assoc 0 D)))
  (cond
    ( (and (wcmatch N "*LINE") (/= N "MLINE"))
      (vlax-curve-getArea ent)
    )
    ( (vlax-property-available-p O 'area)
      (vla-get-area O)
    )
    ( (= N "3DFACE")
      (Area-of-Verties
        (list
	  (cdr (assoc 10 D))
	  (cdr (assoc 11 D))
	  (cdr (assoc 12 D))
	  (cdr (assoc 13 D))
        )
      )
    )
    ( (= N "SOLID") 
      (Area-of-Verties
        (list
	  (cdr (assoc 10 D))
	  (cdr (assoc 11 D))
	  (cdr (assoc 13 D))
	  (cdr (assoc 12 D))
        )
      )
    )
    (T 0)
  )
)

;;;-------------------------------------------------------------
;;; n个点的面积                                                 
;;;-------------------------------------------------------------
(defun S:Det2 (p1 p2)
  (- (* (car p1) (cadr p2)) (* (car p2) (cadr p1)))
)
(defun Area-of-Verties(pts /)
  (* 0.5 (abs (apply '+ (mapcar 'S:Det2 (cons (last pts) pts) pts))))
)


;;;-------------------------------------------------------------
;;; 获取物体的体积                                              
;;;-------------------------------------------------------------
(defun Ent:Get-Volume (ent / O N D area thick)
  (setq O (vlax-ename->vla-object ent))
  (setq D (entget ent))
  (setq N (cdr (assoc 0 D)))
  (cond
    ( (vlax-property-available-p O 'Volume)
      (vla-get-Volume O)
    )
    ( (and
	(> (setq area (Ent:Get-Area ent)) 0)
	(numberp (setq thick (cdr (assoc 39 D))))
      )
      (* area thick)
    )
    (T 0)
  )
)

;;;-------------------------------------------------------------
;;; 按句柄和名称比较两个图元                                    
;;;-------------------------------------------------------------
(defun S:Compare (x1 x2 / N1 N2)
  (setq N1 (cadar x1))					 
  (setq N2 (cadar x2))					 
  (if (= N1 N2)
    (< (caddar x1) (caddar x2))
    (< N1 N2)
  )
)

;;;-------------------------------------------------------------
;;; 两点之间的中点                                              
;;;-------------------------------------------------------------
(defun GEO:MidPoint (p1 p2)
  (mapcar (function (lambda (x y) (* (+ x y) 0.5))) p1 p2)
)

;;;-------------------------------------------------------------
;;; 取得文本的插入点                                            
;;;-------------------------------------------------------------
(defun S:Text-Position (ll ur mode)
  (cond
    ( (= mode "Center")
      (GEO:MidPoint ll ur)
    )
    ( (= mode "Upon")
      (GEO:MidPoint (list (car ll) (cadr ur)) ur)
    )
    ( (= mode "Below")
      (GEO:MidPoint (list (car ur) (cadr ll)) ll)
    )
    ( (= mode "Specify")
      (initget 1)
      (trans (getpoint (GEO:MidPoint ll ur) "\n请指定点:") 1 0)
    )
  )
)

;;;-------------------------------------------------------------
;;; 标注文字                                                    
;;;-------------------------------------------------------------
(defun S:Make-Text (Info string Insert prop /)
  (entmakeX
    (append
      (list
	'(0 . "TEXT")
	(cons 1 string)
	(cons 10 Insert)
	(cons 11 Insert)
      )
      prop
    )
  )
)

;;;-------------------------------------------------------------
;;; 根据两点画直线段                                            
;;;-------------------------------------------------------------
(defun Ent:Make-line (p q)
  (entmakeX (list '(0 . "LINE") (cons 10 p) (cons 11 q)))
)
