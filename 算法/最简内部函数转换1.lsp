;;;=============================================================
;;; 程序加载后，就可以用tranf来激活一个系统隐藏的函数           
;;; 用法：(tranf "内部函数名")                                  
;;; 参数：函数名的字符串                                        
;;; 返回：T存在或者是可用的内部函数，nil不存在或则无效          
;;; 作者：highflybird                                           
;;; 例子：(tranf "get-logical-drives")                          
;;; 此程序得到网友baitang36的大力帮助，特此致谢！               
;;; 另外借鉴了不死猫和tryhi的代码，在此一并感谢！               
;;;-------------------------------------------------------------
(defun HFB-LOAD-TRANF (/ f o a b c)
  (setq o (strcat (getenv "UserProfile") "\\Intern.fas"))
  (if (findfile o)
    (vl-file-delete o)
  )
  (setq f (open o "w"))
  (foreach a '(13   70	 65   83   52	45   70	  73   76   69	 32
	       59   13	 49   13   49	32   36	  1    36   13	 54
	       55   32	 54   32   36	20   1	  1    1    256	 91
	       65   256	 66   256  67	256  256  86   108  112	 112
	       45   115	 121  109  102	117  110  45   62   97	 108
	       256  105	 110  116  101	114  110  256  58   76	 80
	       80   256	 256  1	   67	256  256  6    256  3	 5
	       256  6	 2    256  3	4    256  6    1    256	 3
	       3    256	 6    256  256	22   36
	      )
    (write-char a f)
  )
  (close f)
  (load o)
  (vl-file-delete o)
  (foreach s '(al-add-subr-name lpp-symfun->al intern :lpp)
    (a (b s c))
    (or (eq s ':lpp) (al-add-subr-name s))
  )
  (if intern
    (princ "\n已打开内部函数转普通函数大门.\n")
    (princ "\n激活内部函数转普通函数失败!\n")
  )
)

(if (null :lpp)
  (progn
    (HFB-LOAD-TRANF)
    (defun tranf (s)
      (lpp-symfun->al (intern s :lpp))
    )
  )
)

(princ "已经激活内部函数转普通函数TRANF.\n  作者：highflybird\n")
(princ)

 