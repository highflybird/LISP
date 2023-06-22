(defun c:tttt()
  (setq i 0)
  
  (repeat 256
    (setq l (cons i l))
    (setq i (1+ i))
  )
  
  (setq b (vlax-make-safearray 17 (cons 0 (1- (length l)))))
  (vlax-safearray-fill b l)
  (setq b (vlax-make-variant l))

  (setq f "c:\\1.dat")
  (setq s (vlax-create-object "ADODB.Stream"))
  (vlax-put s 'type 1) 				;adTypeBinary
  (vlax-invoke  s 'open)		;adModeRead  =1 adModeWrite  =2 adModeReadWrite =3
  (vlax-invoke-method s 'Write b)
  (vlax-invoke-method s 'saveToFile f 2)
  (vlax-invoke-method s 'close)
  (vlax-release-object s)

)