;;;=============================================================
;;; 程序加载后，就可以用tranf来激活一个系统隐藏的函数           
;;; 用法：(tranf "内部函数名")                                  
;;; 参数：函数名的字符串                                        
;;; 返回：T存在或者是可用的内部函数，nil不存在或则无效          
;;; 作者：highflybird                                           
;;; 例子：(tranf "get-logical-drives")                          
;;; 此程序得到网友baitang36和不死猫的大力帮助，特此致谢！       
;;; 另外借鉴了tryhi和其他网友的代码，在此一并感谢！             
;;;-------------------------------------------------------------
(defun HFB-LOAD-TRANF (/ f o l)
  (setq o (strcat (getenv "UserProfile") "\\Intern.fas"))
  (if (findfile o)
    (vl-file-delete o)
  )
  (setq f (open o "w"))
  (foreach a '(70  65  83  52  45  70  73  76  69  13  49  13  48  32
	       36  22  36  49  54  32  48  32  36  86 105 110 116 101
	      114 110 256  58 108 112 112 256 256  42  22  36)
    (write-char a f)
  )
  (close f)
  (setq l (load o))
  (setq intern (eval (car l)))
  (setq :lpp (eval (cdr l)))
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

(princ "已经激活内部函数转普通函数TRANF.\n  作者：highflybird\n")
(princ)