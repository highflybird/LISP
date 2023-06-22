;;;=============================================================
;;; CAD计算器主程序                                             
;;;=============================================================
(defun C:TT (/ dcl_id DlgRet Prec)
  (CAL:Clear)
  (setq dcl_id (load_dialog "Calculator.dcl")) 			;装入对话框文件
  (setq DlgRet 2)
  (while (> DlgRet 1)						;如果没有离开对话框
    (new_dialog "Calculator" dcl_id)				;创建新的对话框
    (ActionNumberKeys)                                          ;数字按钮动作
    (ActionFunctionKeys)					;函数按钮动作
    (action_tile "pick" "(DONE_DIALOG 3)")                      ;拾取文字动作
    (action_tile "change" "(DONE_DIALOG 4)")			;改变文字动作
    (action_tile "batch" "(DONE_DIALOG 5)")			;批量修改动作
    (setq Prec (vl-bb-ref 'Precision))
    (set_tile "Precision" Prec)
    (set_tile "TextPrec" (strcat "精度: " Prec)) 
    (set_tile "Expression" (vl-bb-ref 'TmpStr))                 
    (setq DlgRet (start_dialog))				;显示对话框.开始对话框操作
    (cond
      ( (= DlgRet 3)
        (CAL:PickText)						;拾取图中文字
      )
      ( (= DlgRet 4)
        (CAL:ChangeText)					;根据计算结果改变文字
      )
      ( (= DlgRet 5)
        (CAL:BatchWork)						;批量修改文字
      )
    )
  )
  (unload_dialog dcl_id)					;卸载对话框
  (princ)
)

;;;-------------------------------------------------------------
;;; 清空计算器内容                                              
;;;-------------------------------------------------------------
(defun CAL:Clear ()
  (vl-bb-set 'CurPos 0)                                         ;当前输入位置
  (vl-bb-set 'TmpStr "")					;临时字符，用于图形交互时
  (vl-bb-set 'HasCAL nil)					;是否计算过了
  (vl-bb-set 'Error nil)                                        ;求值时是否有错误发生
  (vl-bb-set 'Precision "8")
  (set_tile "error" "")
  (set_tile "Expression" "")
)

(defun CAL:Precision ($value)
  (set_tile "TextPrec" (strcat "精度:  " $value))
  (vl-bb-set 'Precision $value)
)
;;;-------------------------------------------------------------
;;; 按下CE动作                                                  
;;;-------------------------------------------------------------
(defun CAL:Correct ()
  (set_tile "Expression" "")
  (set_tile "error" "")
  (vl-bb-set 'TmpStr "")					;临时字符，用于图形交互时
  (vl-bb-set 'CurPos 0)
  (vl-bb-set 'HasCAL T)
  (vl-bb-set 'Error nil)
)

;;;-------------------------------------------------------------
;;; 按下退格动作                                                
;;;-------------------------------------------------------------
(defun CAL:Backspace (/ expr)
  (setq expr (get_tile "Expression"))
  (setq expr (substr expr 1 (1- (strlen expr))))
  (set_tile "Expression" expr)
  (vl-bb-set 'TmpStr expr)
  (vl-bb-set 'HasCAL nil)
  (vl-bb-set 'Error nil)
)

;;;-------------------------------------------------------------
;;; 按下等号，求结果                                            
;;;-------------------------------------------------------------
(defun CAL:GetAnswer (/ expr ret)
  (setq expr (get_tile "Expression"))
  (setq ret (vl-catch-all-apply 'CAL:Expr2Value (list expr)))
  (if (numberp ret)
    (progn
      (setq precision (atoi (get_tile "Precision")))
      (if (< (abs (- (read (rtos ret 2 0)) ret)) 1e-14)
	(setq ret (rtos ret 2 0))
        (setq ret (rtos ret 2 precision))
      )
      ;(setq ret (vl-princ-to-string ret))
      (set_tile "Expression" ret)
      (vl-bb-set 'TmpStr ret)
      (vl-bb-set 'curpos (strlen ret))
      (vl-bb-set 'Error nil)
      (vl-bb-set 'HasCAL T)
    )
    (progn 
      (if (VL-CATCH-ALL-ERROR-P ret)
	(setq ret (VL-CATCH-ALL-ERROR-MESSAGE ret))
	(setq ret "出错!请检查表达式.")
      )
      (set_tile "error" ret)
      (vl-bb-set 'Error T)
    )
  )
)

;;;-------------------------------------------------------------
;;; 编辑框动作                                                  
;;;-------------------------------------------------------------
(defun CAL:Expression ($value /)
  (set_tile "error" "")
  (vl-bb-set 'Curpos (strlen $value))                           ;可惜不能得到光标位置
  (vl-bb-set 'TmpStr $value)
  (vl-bb-set 'HasCAL nil)
  (vl-bb-set 'Error nil)
)

;;;-------------------------------------------------------------
;;; 添加字符                                                    
;;;-------------------------------------------------------------
(defun CAL:AddString (Oldstr NewStr NewPos / str Pos)
  (setq pos (vl-bb-ref 'curpos))
  (setq str (strcat (substr oldstr 1 pos) NewStr (substr oldstr (1+ pos))))
  (setq pos (+ pos NewPos))
  (set_tile "Expression" str)
  (set_tile "error" "")
  (vl-bb-set 'CurPos pos)
  (vl-bb-set 'TmpStr str)
  (vl-bb-set 'HasCAL nil)
  (vl-bb-set 'Error nil)
)

;;;-------------------------------------------------------------
;;; 数字和括号                                                  
;;;-------------------------------------------------------------
(defun NumberList ()
  '("0"	"1" "2" "3" "4" "5" "6" "7" "8" "9" "pi" "2.71828182845904523536" "0.61803398874989484820" "." "(" ")")
)
(defun ActionNumberKeys (/)
  (foreach key (NumberList)
    (action_tile key "(CAL:ClickNumber $key))")
  )
)
(defun CAL:ClickNumber ($key / oldStr)
  (if (and (vl-bb-ref 'HasCAL) (not (vl-bb-ref 'Error)))
    (progn
      (setq OldStr "")
      (vl-bb-set 'CurPos 0)
    )
    (setq OldStr (get_tile "Expression"))
  )
  (CAL:AddString OldStr $key (strlen $key))
) 

;;;-------------------------------------------------------------
;;; 运算符及其函数                                              
;;;-------------------------------------------------------------
(defun FunctionList ()
  '("+"	     "-"      "*"      "/"	"%"	 "^"	  "abs"
    "sin"    "cos"    "tan"    "sqr"	"cube"	 "sqrt"	  "cuberoot"
    "asin"   "acos"   "atan"   "ln"	"exp"    "log10"  "exp10"
    "r2d"    "d2r"    "fix"    "float"  "round" 
   )
)
(defun ActionFunctionKeys (/ expr)
  (foreach key (FunctionList)
    (action_tile key "(CAL:ClickFunction $key))")
  )
)
(defun CAL:ClickFunction ($key / OldStr newStr NewPos curpos)
  (setq OldStr (get_tile "Expression"))
  (if (CAL:IsFunction (read $key))
    (if (and (vl-bb-ref 'HasCAL) (not (vl-bb-ref 'error)))
      (setq newstr oldStr
	    newPos (1- (strlen NewStr))
	    oldStr (strcat $key "()")
	    curpos (vl-bb-set 'curpos (1+ (strlen $key)))
      )
      (setq newStr (strcat $key "()")
	    newPos (1- (strlen NewStr))
      )
    )
    (setq newStr $key
	  newPos (strlen NewStr)
    )
  )
  (CAL:AddString OldStr NewStr NewPos)
)

;;;反号
(defun CAL:RevSign (/ expr) 
  (if (vl-bb-ref 'HasCAL)
    (progn 
      (setq expr (get_tile "Expression"))
      (setq num (read expr))
      (if (< num 0)
	(setq expr (substr expr 2))
	(setq expr (strcat "-" expr))
      )
      (set_tile "Expression" Expr)
      (vl-bb-set 'CurPos (strlen expr))
      (vl-bb-set 'TmpStr Expr)
      (vl-bb-set 'HasCAL nil)
      (CAL:GetAnswer)
    )
  )
)

;;;倒数
(defun CAL:Reciprocal (/ expr)
  (if (vl-bb-ref 'HasCAL)
    (progn 
      (setq expr (get_tile "Expression"))
      (if (minusp (read expr))
	(setq expr (strcat "1/(" expr ")"))
	(setq expr (strcat "1/" expr))
      )
      (set_tile "Expression" expr)
      (vl-bb-set 'CurPos (strlen expr))
      (vl-bb-set 'TmpStr Expr)
      (vl-bb-set 'HasCAL nil)
      (CAL:GetAnswer)
    )
  )     
)

;;;-------------------------------------------------------------
;;; 拾取图中文字，提取到计算器中来                              
;;;-------------------------------------------------------------
(defun CAL:PickText (/ ent dxf name obj str OldStr CurPos)
  (if (setq ent (car (nentsel "\n选取文字类物<文字或属性，可在块内>:")))
    (if (setq name (cdr (assoc 0 (setq dxf (entget ent)))))
      (if (or (wcmatch name "*TEXT") (= name "ATTDEF") (= name "ATTRIB"))
        (progn
	  (setq obj (vlax-ename->vla-object ent))
	  (setq str (vla-get-TextString obj))
	  (if (and (vl-bb-ref 'HasCAL) (not (vl-bb-ref 'Error)))
            (setq OldStr ""
		  CurPos (vl-bb-set 'CurPos 0)
            )
            (setq OldStr (vl-bb-ref 'TmpStr))
          )
	  (CAL:AddString OldStr str (strlen str))
        )
      )
    )
  )
)

;;;-------------------------------------------------------------
;;; 单个修改文本(按照计算器的结果修改图形中文本)                
;;;-------------------------------------------------------------
(defun CAL:ChangeText (/ ent dxf name obj str)
  (if (setq ent (car (nentsel "\n选取文字类物<文字或属性，可在块内>:")))
    (if (setq name (cdr (assoc 0 (setq dxf (entget ent)))))
      (if (or (wcmatch name "*TEXT") (= name "ATTDEF") (= name "ATTRIB"))
        (progn
	  (UndoBegin)
	  (setq obj (vlax-ename->vla-object ent))
	  (setq str (vl-bb-ref 'TmpStr))
	  (vl-catch-all-apply 'vla-put-TextString (list obj str))
	  (UndoEnd)
        )
      )
    )
  )
)

;;;-------------------------------------------------------------
;;; 批量修改文本(对图形中算式文本求值并修改文本)                
;;;-------------------------------------------------------------
(defun CAL:BatchWork (/ ent sel i obj str ret)
  (if (setq sel (ssget '((0 . "*TEXT"))))
    (progn
      (UndoBegin)
      (setq i 0)
      (repeat (sslength sel)
	(setq ent (ssname sel i))
	(setq obj (vlax-ename->vla-object ent))
	(setq str (vla-get-textstring obj))
	(setq ret (vl-catch-all-apply  'CAL:Expr2Value (list str)))
	(if (numberp ret)
	  (progn
	    (setq str (rtos ret (getvar "LUNITS") (getvar "LUPREC")))
	    (vl-catch-all-apply 'vla-put-textstring (list obj str))
	  )
	)
	(setq i (1+ i))
      )
      (UndoEnd)
    )
  )
)

;;;*************************************************************
;;; 其他实用函数                                                
;;;*************************************************************
(defun UndoBegin ()
  (vla-StartUndoMark (vla-get-activedocument (vlax-get-acad-object)))
)
(defun UndoEnd ()
  (vla-EndUndoMark (vla-get-activedocument (vlax-get-acad-object)))
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