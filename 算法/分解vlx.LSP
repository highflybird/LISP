(defun c:v2f (/ FILE vlist nl ff n i filelist aa)
  (foreach sym (list "vrtlib-list" "vrtlib-get" "_write-nb-str")
    (syz-tran-fun sym)
  )
  (setq syz-vlx-list vrtlib-list)
  (setq syz-open open)
  (setq syz-close close)
  (setq syz-output "W") :OUTPUT
  (SETQ syz-vlx-get  vrtlib-get)
  (setq syz-write-nb-str _write-nb-str)
  (setq file (getfiled "\n选择需要分解的vlx文件" "" "vlx" 2))
  (if file
    (progn
      (setq vlist (syz-vlx-list file))
      (setq nl (length vlist))
      (setq n (- nl 1))
      (setq i 1)
      (repeat n
	(setq filelist (nth i vlist))
	(setq file_type (car filelist))
	(setq file_name (cadr filelist))
	(cond
	  ((= 1330 file_type)		;这里的1330是fas格式，
	   (setq file_ext ".fas")
	  )
	  ((= 1240 file_type)		;1240是文件头的load模块
	   (setq file_ext ".loa")
	  )
	  (T
	   (setq file_ext ".TXT")	;其他的位置，都当txt处理
	  )
	) ;_ cond
	(setq file_namef (STRCAT (vl-filename-directory file) "\\" file_name file_ext))
	(princ file_namef)
	(SETQ FF (syz-open file_namef "W")) syz-OUTPUT
	(setq aa (syz-vlx-get file file_type file_name))
	(syz-write-nb-str aa ff)
	(syz-close ff)
	(setq i (+ 1 i))
      ) ;_ repeat
      (princ (strcat "\n文件保存在:" (vl-filename-directory file))
      ) ;_ princ
    ) ;_ progn
    (princ "file not found!")
  ) ;_ if
) ;_ defun

(defun syz-tran-fun (fun / dat file fo len fun1)
  (setq len (+ (* 2 (strlen fun)) 28))	;长度
  (setq file (vl-filename-mktemp "syz.fas"))
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
  (vl-file-delete file)			;删除临时文件
  (eval (read fun))			;如果函数不存在则返回nil
)

(princ)