;;;=============================================================
;;; CAD计算器主程序                                             
;;;=============================================================
(defun C:JS (/ dcl_id DlgRet Prec)
  (CAL:Clear)
  (vl-bb-set 'MStore "")
  (setq dcl_id (load_dialog "Calculator.dcl")) 			;装入对话框文件
  (setq DlgRet 2)
  (while (> DlgRet 1)						;如果没有离开对话框
    (new_dialog "Calculator" dcl_id)				;创建新的对话框
    (ActionNumberKeys)                                          ;数字按钮动作
    (ActionFunctionKeys)					;函数按钮动作
    (action_tile "pick" "(DONE_DIALOG 3)")                      ;拾取文字动作
    (action_tile "change" "(DONE_DIALOG 4)")			;改变文字动作
    (action_tile "batch" "(DONE_DIALOG 5)")			;批量修改动作
    (action_tile "help" "(CAL:Help)")
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

;;;-------------------------------------------------------------
;;; 设置计算精度                                                
;;;-------------------------------------------------------------
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
  (vl-bb-set 'CurPos (strlen expr))
  (vl-bb-set 'HasCAL nil)
  (vl-bb-set 'Error nil)
)

;;;-------------------------------------------------------------
;;; 按下等号，求结果                                            
;;;-------------------------------------------------------------
(defun CAL:GetAnswer (/ str ret)
  (setq str (get_tile "Expression"))
  (setq ret (vl-catch-all-apply 'CAL:Expr2Value (list str)))
  (if (numberp ret)
    (progn
      (setq precision (atoi (get_tile "Precision")))
      (if (< (abs (- (read (rtos ret 2 0)) ret)) 1e-14)
	(setq ret (rtos ret 2 0))
        (setq ret (rtos ret 2 precision))
      )
      (set_tile "Expression" ret)
      ;;(set_tile "error" ret)
      (vl-bb-set 'TmpStr ret)
      (vl-bb-set 'curpos (strlen ret))
      (vl-bb-set 'Error nil)
      (vl-bb-set 'HasCAL T)
    )
    (progn
      (vl-bb-set 'Error T)
      (if (VL-CATCH-ALL-ERROR-P ret)
	(set_tile "error" (VL-CATCH-ALL-ERROR-MESSAGE ret))
	(set_tile "error" "错误!请检查表达式语法.")
      )
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
  '("0"		  "1"		"2"	      "3"
    "4"		  "5"		"6"	      "7"
    "8"		  "9"		"."	      "("
    ")"		  "pi"		"2.71828182845904523536"
    "0.61803398874989484820"
   )
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
    "asin"   "acos"   "atan"   "ln"	"exp"	 "log10"  "exp10"
    "r2d"    "d2r"    "fix"    "float"	"round"
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
    (if (numberp (eval (vl-catch-all-apply 'read (list oldstr))))
      (setq newstr (strcat oldStr ")")
	    oldStr (strcat $key "(")
	    curpos (vl-bb-set 'curpos (strlen oldStr))
	    newPos (strlen newstr)
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

;;;-------------------------------------------------------------
;;; 反号                                                        
;;;-------------------------------------------------------------
(defun CAL:RevSign (/ str)
  (setq str (get_tile "Expression"))
  (setq str (strcat "-(" str ")"))
  (set_tile "Expression" str)
  (vl-bb-set 'CurPos (strlen str))
  (vl-bb-set 'TmpStr str)
  (vl-bb-set 'HasCAL nil)
  (CAL:GetAnswer)
)

;;;-------------------------------------------------------------
;;; 倒数                                                        
;;;-------------------------------------------------------------
(defun CAL:Reciprocal (/ str)
  (setq str (get_tile "Expression"))
  (setq str (strcat "1/(" str ")"))
  (set_tile "Expression" str)
  (vl-bb-set 'CurPos (strlen str))
  (vl-bb-set 'TmpStr str)
  (vl-bb-set 'HasCAL nil)
  (CAL:GetAnswer)
)


;;;-------------------------------------------------------------
;;; MS键(存储计算数据)                                          
;;;-------------------------------------------------------------
(defun CAL:MS (/ str)
  (CAL:GETANSWER)
  (setq str (get_tile "Expression"))
  (if (numberp (vl-catch-all-apply 'read (list str)))
    (vl-bb-set 'MStore str)
  )
)

;;;-------------------------------------------------------------
;;; MR键(读取存储数据)                                          
;;;-------------------------------------------------------------
(defun CAL:MR (/ str MStore)
  (setq str (get_tile "Expression"))
  (if (numberp (vl-catch-all-apply 'read (list str)))
    (setq str "")
  )
  (setq MStore (vl-bb-ref 'MStore))
  (CAL:ADDSTRING str MStore (strlen MStore))
)

;;;-------------------------------------------------------------
;;; MC键(清楚存储数据)                                          
;;;-------------------------------------------------------------
(defun CAL:MC ()
  (vl-bb-set 'MStore "")
)

;;;-------------------------------------------------------------
;;; M+键(与存储数据相加减)                                      
;;;-------------------------------------------------------------
(defun CAL:M+ ($key / str x0 x1 op N)
  (setq str (get_tile "Expression"))
  (if (numberp (setq x1 (vl-catch-all-apply 'read (list str))))
    (if	(setq x0 (read (vl-bb-ref 'MStore)))
      (setq Op (read (substr $key (strlen $key)))
	    x0 (apply Op (list x0 x1))
	    N  (atoi (get_tile "Precision"))
	    x0 (vl-bb-set 'MStore (rtos x0 2 n))
      )
    )
  )
)

;;;-------------------------------------------------------------
;;; 拾取图中文字，提取到计算器中来                              
;;;-------------------------------------------------------------
(defun CAL:PickText (/ ent dxf name object NewStr OldStr CurPos)
  (if (setq ent (car (nentsel "\n选取文字类物<文字或属性，可在块内>:")))
    (if	(setq name (cdr (assoc 0 (setq dxf (entget ent)))))
      (if (or (wcmatch name "*TEXT")
	      (= name "ATTDEF")
	      (= name "ATTRIB")
	  )
	(progn
	  (setq object (vlax-ename->vla-object ent))
	  (if (= name "ATTDEF")
	    (setq newstr (vla-get-tagstring object))
	    (setq newstr (vla-get-TextString object))
	  )
	  (setq oldstr (vl-bb-ref 'TmpStr))
	  (if (numberp (vl-catch-all-apply 'read (list oldStr)))
	    (setq OldStr ""
		  CurPos (vl-bb-set 'CurPos 0)
	    )
	  )
	  (CAL:AddString OldStr newstr (strlen newstr))
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
      (if (or (wcmatch name "*TEXT")
	      (= name "ATTDEF")
	      (= name "ATTRIB")
	  )
        (progn
	  (UndoBegin)
	  (setq obj (vlax-ename->vla-object ent))
	  (setq str (vl-bb-ref 'TmpStr))
	  (if (= name "ATTDEF")
	    (vla-put-tagString obj str)
	    (vla-put-TextString obj str)
	  )
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

;;;-------------------------------------------------------------
;;; 帮助说明                                                    
;;;-------------------------------------------------------------
(defun CAL:help ()
  (alert "说明:
         1,书写计算式时按一般的计算表达式(中缀表达式)即可。
         2,运算符后面接负数时候，要用括号。
         3,函数可以自定义。一些未列出的可用函数有:
           cot,sec,csc,sinh,cosh,tanh,coth,sech,csch。
         4,因受LISP语言所限制，故此对话框不能浮动。待以后改成ARX版本。"
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

(prompt "\n加载命令是:JS")
(vl-load-com)