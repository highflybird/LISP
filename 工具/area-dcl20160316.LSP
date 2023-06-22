;;;=============================================================
;;;长度面积和体积统计工具主对话框运行程序                       
;;;=============================================================
(defun C:ASet (/ dcl_id DlgRet DCLDATA)
  (setq dcl_id (load_dialog "area.dcl")) 			;装入对话框文件
  (if (null (setq DCLData (getenv "Statistics")))
    (setq DCLData (SetDefault))
    (setq DCLData (read DCLData))
  )
  
  ;;开始对话框操作
  (setq DlgRet 2)
  (while (> DlgRet 1)						;如果没有离开对话框
    (new_dialog "Area" dcl_id)					;因为是动态对话框，所以可以不检查dcl_id
    (ActionKeys)                                                ;控件动作
    (ACTION_TILE "COLOR"  "(SetColor  $key $value)")            ;设置文字颜色
    (ACTION_TILE "SCALE"  "(SetScale  $key $value)")            ;设置文字比例
    (ACTION_TILE "HEIGHT" "(SetHeight $key $value)")		;设置文字高度
    (ACTION_TILE "Reset" "(DONE_DIALOG 2)")                     ;恢复默认设置
    (ACTION_TILE "Pick" "(DONE_DIALOG 3)")			;点取图中文字
    (ACTION_TILE "help" "(help)")                               ;帮助文件
    (ACTION_TILE "Language" "(ChangeLanguage $key $value)")     ;单位中英文切换
    (AddUnitList (GETKEYDATA "Language"))			;单位列表
    (RefreshSymTable "LAYER")					;获取图层列表
    (RefreshSymTable "STYLE")					;获取文字样式列表
    
    (setq DCLData (read (getenv "Statistics")))                 ;读取保存的设置
    (start_list "COLOR")					;颜色列表
    (mapcar 'add_list (strtok (cdar DCLData) "\n"))             
    (end_list)
    (foreach n (cdr DCLData)					;其他保存的设置
      (set_tile (car n) (cdr n))                                
    )
    (setq DlgRet (start_dialog))				;显示对话框
    (cond
      ( (= DlgRet 2)
        (SetDefault)						;参数默认
      )
      ( (= DlgRet 3)
        (PickText)						;拾取图中文字
      )
    )
  )
  
  ;;离开对话框之后的动作
  (if (zerop DlgRet)
    (princ "\n你取消了设置.")					;返回一个取消的信息给用户
    (princ (getenv "Statistics"))
  )
  (princ)
)

;;;-------------------------------------------------------------
;;; 更新某个控件的值                                            
;;;-------------------------------------------------------------
(defun SetKeyData ($key $value / DCLData)
  (setq DCLData (read (getenv "Statistics")))
  (setq DCLData (subst (cons $key $value) (assoc $key DCLData) DCLData))
  (setenv "Statistics" (VL-PRIN1-TO-STRING DCLData))
)

;;;-------------------------------------------------------------
;;; 获取某个控件的值                                            
;;;-------------------------------------------------------------
(defun GetKeyData ($key / DCLData)
  (setq DCLData (read (getenv "Statistics")))
  (cdr (assoc $key DCLData))
)
;;;-------------------------------------------------------------
;;; 控件动作                                                    
;;;-------------------------------------------------------------
 
(defun ActionKeys (/ keylst)
  (setq	keylst '("Dim1"        "Dim2"        "Dim3" 	        ;统计类型
		 "Unit"	       "Unit1"	     "Unit2"		;单位
		 "Unit3"       "LAYER"	     "STYLE"		;图层，样式
		 "ALIGNMENT"   "IsDefault"   "Precision"	;对齐方式，精度
		 "Type"        "HasPrefix"   "Prefix" 		;统计度，前缀
		 "HasSuffix"   "Suffix"      "JustTotal"        ;后缀，仅总数
		 "Language"    "Position"    "Reactor"          ;语言、位置和关联
	         "Output"	   	                        ;输出方式
		)
  )
  (setq keylst (append keylst (S:TypeList)))                    ;图元类型
  (foreach key keylst
    (ACTION_TILE key "(SetKeyData $key $value)")
  )
  keylst
)

;;;-------------------------------------------------------------
;;; 更新控件表的数据                                            
;;;-------------------------------------------------------------
(defun UpdateDCLData (keyLst / DCLData)
  (setq DCLData (read (getenv "Statistics")))
  (foreach n keyLst
    (setq DCLData (subst n (assoc (car n) DCLData) DCLData))
  )
  (setenv "Statistics" (VL-PRIN1-TO-STRING DCLData))
  DCLData
)

;;;-------------------------------------------------------------
;;; 设置文字颜色                                                
;;;-------------------------------------------------------------
(defun SetColor ($key $value / ncolor strColors currentColor Index)
  (SetKeyData $key $value)
  (cond
    ( (= $value "0") (GetCurrentColor))
    ( (= $value "1") (list (cons 62 256)))
    ( (= $value "2") (list (cons 62 0)))
    ( (= $value "3")
      (setq currentColor (GetCurrentColor))
      (if (< (GetVersion) 16)
	(setq nColor (ACAD_COLORDLG (cdar currentColor)))
	(if (cdr currentColor)
	  (setq nColor (ACAD_TRUECOLORDLG (cadr currentColor)))
	  (setq nColor (ACAD_TRUECOLORDLG (car currentColor)))
	)
      )
      (if nColor
	(progn
	  (and (atom nColor) (setq nColor (list (cons 62 nColor))))
          (GetColor nColor $key)
	  nColor
	)
	(progn
	  (SetKeyData $key "0")
	  currentColor
	)
      )
    )
    ( (= $value "4")
      (setq strColors (get_attr $key "list"))
      (SetKeyData  "ColorList" strColors)
      (SetKeyData $key "0")
      (start_list $key)
      (mapcar 'add_list (strtok strColors "\n"))
      (end_list)
      (set_tile $key "0")
      (GetCurrentColor)
    )
    (t
      (setq Index (atoi $value))
      (setq ncolor (nth Index (strtok (getkeyData  "ColorList") "\n")))
      (setq ncolor (mapcar 'atoi  (strtok (substr ncolor 6) ",")))
      (if (cdr ncolor)
	(list
	  (cons 62 (RGB->Index ncolor))
	  (cons 420  (RGB->NUMBER ncolor))
	)
	(list (cons 62 (car ncolor)))
      )
    )
  )
)

;;;-------------------------------------------------------------
;;;从颜色对话框或者实体中取得颜色并更新列表                     
;;;-------------------------------------------------------------
(defun GetColor (DXF $key / color colors Index StrColor strColors TrueColor)
  (and (atom DXF) (setq DXF (list (cons 62 DXF))))
  (setq color (cdr (assoc 62 dxf)))
  (cond
    ( (or (null color) (= color 256))
      (setq StrColor "随层")
    )
    ( (zerop color) (setq StrColor "随块"))
    ( (setq TrueColor (cdr (assoc 420 dxf)))
      (setq strColor (list->String (Number->RGB TrueColor) ","))
      (setq strColor (strcat "颜色 " strColor))
    )
    (t
      (setq StrColor (strcat "颜色 " (itoa color)))
    )
  )

  (setq strColors (getKeyData "ColorList"))
  (setq colors (strtok strColors "\n"))
  (if (setq index (vl-position strColor colors))
    (progn
      (setq Index (itoa index))
      (SetKeyData "COLOR" Index)
      (and $key (set_tile $key Index))
    )
    (progn
      (setq Index (itoa (length colors)))
      (setq strColors (strcat strColors "\n" strColor))
      (SetKeyData "ColorList" strColors)
      (SetKeyData "COLOR" Index)
      (if $key
	(progn
	  (start_list $key 2)
	  (add_list strColor)
	  (end_list)
	  (set_tile $key Index)
	)
      )
    )
  )
)

;;;-------------------------------------------------------------
;;; 设置文字比例                                                
;;;-------------------------------------------------------------
(defun SetScale ($key $value)
  (if (<= (atof $value) 0.0)
    (progn
      (princ "\n输入错误的文字比例!")
      (set_tile $key (cdr (assoc $key (read (getenv "Statistics")))))
    )
    (SetKeyData $key $value)
  )
)

;;;-------------------------------------------------------------
;;; 设置文字高度                                                
;;;-------------------------------------------------------------
(defun SetHeight ($key $value)
  (if (<= (atof $value) 0.0)
    (progn
      (princ "\n输入错误的文字高度!")
      (set_tile $key (cdr (assoc $key (read (getenv "Statistics")))))
    )
    (SetKeyData $key $value)
  )
)

;;;-------------------------------------------------------------
;;; 设置默认参数                                                
;;;-------------------------------------------------------------
(defun SetDefault (/ Default)
  (setq	Default
	 '(("ColorList" . "当前颜色\n随层\n随块\n选取颜色...\n清除历史记录")	;此处是color控件的列表
	   ("LAYER" . "0")
	   ("STYLE" . "0")
	   ("HEIGHT" . "100")
	   ("SCALE" . "1")
	   ("COLOR" . "1")
	   ("ALIGNMENT" . "10")
	   
	   ("Type" . "Dim2")
	   ("Unit" . "4")
	   ("Unit1" . "4")
	   ("Unit2" . "2")
	   ("Unit3" . "1")

	   ("IsDefault" . "1")
	   ("Precision" . "2")
	   ("HasPrefix" . "0")
	   ("Prefix" . "")
	   ("HasSuffix" . "0")
	   ("Suffix" . "")
	   ("JustTotal" . "0")

	   ("Language" . "Chinese")
	   ("Position" . "Center")
	   ("Reactor" . "None")
	   ("Output" . "DRAWING")
	   
	   ("LINE" . "0")
	   ("CIRCLE" . "1")
	   ("ARC" . "0")
	   ("LWPOLYLINE" . "1")
	   ("POLYLINE" . "1")
	   ("SPLINE" . "1")
	   ("MLINE" . "0")
	   ("ELLIPSE" . "1")
	   ("HATCH" . "1")
	   ("REGION" . "1")
	   ("3DFACE" . "1")
	   ("SOLID" . "1")
	   ("3DSOLID" . "0")
	   ("MESH" . "0")
	   ("PLANESURFACE" . "0")
	   ("OTHER" . "0")
	  )
  )
  (setenv "Statistics" (VL-PRIN1-TO-STRING Default))
  Default
)

;;;-------------------------------------------------------------
;;; 拾取图中文字                                                
;;;-------------------------------------------------------------
(defun PickText (/ ent dxf name obj laylst stylst style layer height color scale layIdx styIdx)
  (if (setq ent (car (nentsel "\n选取文字类物<文字或属性，可在块内>:")))
    (if (setq name (cdr (assoc 0 (setq dxf (entget ent)))))
      (if (or (wcmatch name "*TEXT") (= name "ATTDEF") (= name "ATTRIB"))
        (progn
	  (setq obj (vlax-ename->vla-object ent))
          (setq laylst (GetSymTableList "LAYER"))
          (setq stylst (GetSymTableList "STYLE"))
	  (setq style  (cdr (assoc 7  dxf)))
	  (setq layer  (cdr (assoc 8  dxf)))
	  (setq height (cdr (assoc 40 dxf)))
	  (setq color  (cdr (assoc 62 dxf)))
	  (GetColor DXF nil)
	  (if (= name "MTEXT")
	    (setq scale (GetStyleScaleFactor style))
	    (setq scale (cdr (assoc 41 dxf)))
	  )
          (setq layIdx (itoa (1+ (vl-position layer laylst))))
          (setq styIdx (itoa (1+ (vl-position style stylst))))
	  (UpdateDCLData
	    (list
	      (cons "LAYER" layIdx)
	      (cons "STYLE" styIdx)
	      (cons "HEIGHT" (rtos height))
	      (cons "SCALE" (rtos scale))
	    )
	  )
        )
      )
    )
  )
)
     
;;;-------------------------------------------------------------
;;; 把样式或者图层添加到下拉列表                                
;;;-------------------------------------------------------------
(defun RefreshSymTable (key / lst)
  (start_list key)
  (Cond
    ((= key "STYLE") (add_list (strcat "当前样式(" (getvar 'textstyle) ")")))
    ((= key "LAYER") (add_list (strcat "当前图层(" (getvar 'clayer) ")")))
  )
  (setq lst (GetSymTableList key))
  (foreach n lst
    (add_list n)
  )
  (end_list)
  lst
)

;;;*************************************************************
;;; 颜色相关函数                                                
;;;*************************************************************

;;;-------------------------------------------------------------
;;;颜色号转为RGB.                                               
;;;-------------------------------------------------------------
(defun Number->RGB (C)
  (list (lsh C -16)
        (lsh (lsh C 16) -24)
        (lsh (lsh C 24) -24)
  )
)

;;;-------------------------------------------------------------
;;;RGB转为颜色号.                                               
;;;-------------------------------------------------------------
(defun RGB->Number (lst)
  (+ (lsh (car lst) 16) (lsh (cadr lst) 8) (caddr lst))
)

;;;-------------------------------------------------------------
;;;RGB转为索引号                                                
;;;-------------------------------------------------------------
(defun RGB->Index (colRGB / colorObj Index)
  (setq colorObj (vlax-create-object  "AutoCAD.AcCmColor.16"))
  (vla-setRGB colorobj (car colRGB) (cadr colRGB) (caddr colRGB)) 
  (setq Index (vla-get-ColorIndex colorobj))
  (vlax-release-object colorObj)
  Index
)

 
;;;-------------------------------------------------------------
;;;读取当前颜色值                                               
;;;-------------------------------------------------------------
(defun GetCurrentColor (/ Color)
  (setq color (getvar 'cecolor))
  (cond
    ( (= color "BYLAYER") (list (cons 62 256)))
    ( (= color "BYBLOCK") (list (cons 62 0)))
    ( (= (substr color 1 3) "RGB")
      (setq color (vl-string-translate ","  " " (substr color 5)))
      (setq color (read (strcat "(" color ")")))
      (list
        (cons 62 (RGB->Index color))
        (cons 420 (RGB->Number color))
      )
    )
    (t (list (cons 62 (atoi color))))
  )
)

;;;-------------------------------------------------------------
;;; 获取样式的比例系数                                          
;;;-------------------------------------------------------------
(defun GetStyleScaleFactor (styleName / ent)
  (if (setq ent (tblobjname "style" styleName))
    (cdr (assoc 41 (entget ent)))
    1.0                      
  )
)

;;;-------------------------------------------------------------
;;; 获取要标注文字属性                                          
;;;-------------------------------------------------------------
(defun GetTextProps (/ color layIdx layer height scale styIdx style pos L1 L2)
  (setq color (SetColor "COLOR" (getkeyData "COLOR")))
  (setq layIdx (atoi (GetKeyData "LAYER")))
  (if (zerop layIdx)
    (setq layer (getvar "CLAYER"))
    (setq layer (nth (1- layIdx) (REFRESHSYMTABLE "LAYER")))
  )
  (setq styIdx (atoi (getkeyData "STYLE")))
  (if (zerop styIdx)
    (setq style (getvar "TEXTSTYLE"))
    (setq style (nth (1- styIdx) (REFRESHSYMTABLE "STYLE")))
  )
  ;;确保style和layer的值不为nil
  (or style (setq style (getvar "TEXTSTYLE")))
  (or layer (setq layer (getvar "CLAYER")))
  (setq height (atof (getKeyData "HEIGHT")))
  (setq scale (atof (getKeyData "SCALE")))
  (setq pos (atoi (getkeydata "ALIGNMENT")))
  (if (< pos 6)
    (setq L1  (cons 72 pos)
	  L2  (cons 73 0)
    )
    (setq L1  (cons 72 (rem pos 3))
	  L2  (cons 73 (- 5 (/ pos 3)))
    ) 
  )
  (append
    color 
    (list
      (cons 40 height)
      (cons 41 scale)
      (cons 7  style)
      (cons 8  layer)
      L1
      L2
    )
  )
)

;;;-------------------------------------------------------------
;;; 对话框中英文单位切换                                        
;;;-------------------------------------------------------------
(defun ChangeLanguage ($key $value)
  (AddUnitList $value)
  (SetKeyData $key $value)
)

;;;-------------------------------------------------------------
;;; 对话框中英文单位列表                                        
;;;-------------------------------------------------------------
(defun AddUnitList ($key / DCLData)
  (setq DCLData (getenv "Statistics"))
  (setq DCLData (read DCLData))
  (if (= $key "English" )
    (mapcar
      'enUnitList
      '("Unit" "Unit1" "Unit2" "Unit3")
      '(EnUnit-1 EnUnit-1 EnUnit-2 EnUnit-3)
    )
    (mapcar
      'CnUnitList
      '("Unit" "Unit1" "Unit2" "Unit3")
      '(CnUnit-1 CnUnit-1 CnUnit-2 CnUnit-3)
    )
  )
  (foreach n '("Unit" "Unit1" "Unit2" "Unit3")
    (set_tile n (cdr (assoc n DCLData)))             		;恢复对话框保存的设置
  )
)

;;;-------------------------------------------------------------
;;;单位名称列表							
;;;-------------------------------------------------------------
(defun EnUnit-1 ()
  '("Inches"	  "Feet"	 "Miles"       "mm"
    "cm"	  "m"		 "km"	       "Microinches"
    "Mils"	  "Yards"	 "Angstroms"   "nm"
    "Microns"	  "dm"		 "Dekameters"  "Hectometers"
    "Gigameters"  "Astronomical" "LightYears"  "Parsecs"
    
   )
)

(defun CnUnit-1 ()
  '("英寸"     "英尺"	  "英里"     "毫米"
    "厘米"     "米"       "千米"     "微英寸"	  
    "密耳"     "码"	  "埃"	     "毫微米"
    "微米"     "分米"	  "十米"     "百米"	
    "百万公里" "天文单位" "光年"     "秒差距"
   )
)

(defun EnUnit-2 ()
  '("km^2"	   "m^2"	  "dm^2"	 "cm^2"
    "mm^2"	   "ha"		  "acre"	 "square miles"
    "square inches" "square feet" "square yards"  "square rod"
   )
)

(defun CnUnit-2 ()
  '("平方公里"	  "平方米"	"平方分米"    "平方厘米"
    "平方毫米"	  "公顷"	"市亩"	      "平方英里"
    "平方英寸"	  "平方英尺"	"平方英码"    "平方竿"
   )
)

(defun EnUnit-3 ()
  '("m^3"	 "dm^3"       "cm^3"       "mm^3"	"l"
    "dl"	 "cl"	      "ml"	   "yard^3"	"Inch^3"
    "feet^3"	 "gallon"     "ounce"	   "pint"
   )
)

(defun CnUnit-3 ()
  '("立方米"	 "立方分米"   "立方厘米"   "立方毫米"	"升"
    "分升"	 "厘升"	      "毫升"	   "立方码"	"立方英寸"
    "立方英尺"	 "加仑"	      "盎司"	   "品脱"
   )
)

(defun EnUnitList (key func)
  (start_list key)
  (add_list "None")
  (mapcar 'add_list ((eval func)))
  (end_list)
)

(defun CnUnitList (key func)
  (start_list key)
  (add_list "无单位")
  (mapcar 'add_list ((eval func)))
  (end_list)
)

;;;-------------------------------------------------------------
;;;获取换算因子							
;;;-------------------------------------------------------------
(defun GetFactor&Unit (DCLData sU fc fn / n Lan fun txt map1 map2 map3 Index Unit units)
  (setq n (substr (cdr (assoc "Type" DCLData)) 4))		;测量类型
  (setq Lan (cdr (assoc "Language" DCLData)))			;单位名称语言
  (setq fun (strcat (substr Lan 1 1) "nUnit-" n))		
  (setq txt (cons "" ((eval (read fun)))))			;获取单位名称列表

  ;;比例因子映射
  (setq	Map1 (list 1.0		0.0254	     0.3048	  1609.344
		   0.001	0.01	     1.0	  1000.0
		   2.54e-8	2.54e-5	     0.9144	  1e-10
		   1e-9		1e-6	     0.1	  10.0
		   100.0	1e9	     149597870700.0
		   9460730472580800.0	     30851442071085988.8)
  )
  (setq	Map2 (list 1.0 1e6 1.0 1e-2 1e-4 1e-6 10000.0 (/ 2000 3.0) 2589988.110336 0.00064516 0.0929030 0.8361274 25.2928526))
  (setq	Map3 (list 1.0 1.0 1e-3 1e-6 1e-9 1e-3 1e-4 1e-5 1e-6 0.7645549 0.0000164 0.0283168 0.0037854  0.0000296 0.0004732))

  ;;绘图单位
  (setq Index (atoi (cdr (assoc "Unit" DCLData))))
  (setq Unit (nth Index Map1))

  ;;测量单位，换算因子，计算函数
  (setq Index (atoi (cdr (assoc (strcat "Unit" n) DCLData))))
  (setq Units (nth Index (eval (read (strcat "Map" n)))))
  (setq n (atoi n))
  (set fn (nth (1- n) '(Ent:Get-Length Ent:Get-Area Ent:Get-Volume)))
  (set fc (/ (expt Unit n) Units))
  (if (= "1" (cdr (assoc "HasSuffix" DCLData)))
    (set sU (strcat (nth Index Txt) (cdr (assoc "Suffix" DCLData))))
    (set sU (nth Index Txt))
  )
)

;;;-------------------------------------------------------------
;;;图元类型    							
;;;-------------------------------------------------------------
(defun S:TypeList ()
  '("LINE" "CIRCLE" "ARC" "LWPOLYLINE" "POLYLINE" "SPLINE" "ELLIPSE" "MLINE"
    "HATCH" "REGION" "3DFACE" "SOLID" "3DSOLID" "PLANESURFACE" "MESH" "OTHER"
   )
)
;;;*************************************************************
;;; 其他实用函数                                                
;;;*************************************************************

;;;-------------------------------------------------------------
;;;获取CAD版本                                                  
;;;-------------------------------------------------------------
(defun GetVersion ()
  (atof (substr (getvar "ACADVER") 1 4))
)

;;;-------------------------------------------------------------
;;;符号表                                                       
;;;-------------------------------------------------------------
(defun GetSymTableList (SymName / name Lst)
  (while (setq name (cdr (assoc 2 (tblnext SymName (not name)))))
    (setq Lst (cons name Lst))
  )
  (ACAD_STRLSORT Lst)
)

;;;-------------------------------------------------------------
;;;字符分割函数，分隔符不能为双引号"本身                        
;;;-------------------------------------------------------------
(defun strtok (str delim / str1)
  (setq str1 str)
  (while (/= str (setq str1 (VL-STRING-SUBST "\"\"" delim str)))
    (setq str str1)
  )
  (vl-remove "" (read (strcat "(\"" str "\")")))
)

;;;-------------------------------------------------------------
;;;列表转字符串(strtok的逆转换)                                 
;;;-------------------------------------------------------------
(defun List->String (lst Delim / str)
  (setq str "")
  (foreach n (mapcar 'VL-PRIN1-TO-STRING lst)
    (setq str (strcat str n delim))
  )
  (substr str 1 (1- (strlen str)))
)