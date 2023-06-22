;;验证图元用Entmake产生的必要条件
(defun C:w3 (/ E EN FLAG LASTENT YAO)
  (defun Mytest	(x en)
    (entmake (vl-remove x en))
  )
  (cond
    ( (and
        (setq e (entsel "\n 图元用Entmake产生的必要条件: "))
        (setq e (car e))
        (setq en (entget e))
      )
      (foreach x en
        (setq lastent (entlast))
        (setq Flag (VL-CATCH-ALL-ERROR-P (VL-CATCH-ALL-APPLY 'Mytest (list x en))))
        (cond
	  ((and (equal (entlast) lastent) (not Flag))
	   (setq yao (cons x yao))
	  )
        )
      )
    )
  )
  (reverse yao)
)

;;Entmake 符号表的必要条件
(defun C:w4 (/ DATA FLN FLO I L1 LST NEWNAME OLDNAME YAO)
  (cond	((and (setq FLN (getfiled "輸出的LISP文件:" "" "LSP" 1))
	      (setq FLO (open FLN "w"))
	 )
	 (setq lst '("Layer"	"Ltype"	   "View"     "Style"
		     "Appid"	"Ucs"	   "Dimstyle" "Vport"
		    )
	 )
	 (setq L1 '("AcDbLayerTableRecord"
		    "AcDbLinetypeTableRecord"
		    "AcDbViewTableRecord"
		    "AcDbTextStyleTableRecord"
		    "AcDbRegAppTableRecord"
		    "AcDbUCSTableRecord"
		    "AcDbDimStyleTableRecord"
		    "AcDbViewportTableRecord"
		   )
	 )
	 (foreach n lst
	   (setq yao nil)
	   (setq data (TBLNEXT n T))
	   (setq yao (cons (car data) yao))
	   (setq data (cdr data))
	   (setq yao (cons '(100 . "AcDbSymbolTableRecord") yao))
	   (setq yao (cons (cons 100 (car L1)) yao))
	   (setq L1 (cdr L1))
	   (foreach X data (setq yao (cons x yao)))
	   (setq yao (REVERSE yao))
	   (setq i 0)
	   (setq lst nil)
	   (setq oldName (cdr (assoc 2 yao)))
	   (foreach X yao
	     (setq i (1+ i))
	     (setq NewName (strcat oldname (VL-PRINC-TO-STRING i)))
	     (setq data (subst (cons 2 NewName) (assoc 2 yao) yao))
	     (cond ((not (entmakeX (vl-remove X data)))
		    (setq lst (cons X lst))
		   )
	     )
	   )
	   (setq lst (reverse lst))
	   (write-line
	     (strcat "(EntMakeX '" (vl-prin1-to-string lst) ")")
	     FLO
	   )
	 )
	 (close FLO)
	)
  )
  (princ)
)