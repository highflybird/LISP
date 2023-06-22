(defun c:tttt()
  (setq i 0)
  (setq l nil)
  (repeat 256
    (setq l (cons i l))
    (setq i (1+ i))
  )
  
  (setq b (vlax-make-safearray 17 (cons 0 (1- (length l)))))
  (vlax-safearray-fill b l)

  (setq f "c:\\1.dat")
  (setq s (vlax-create-object "ADODB.Stream"))
  (vlax-put s 'type 1) 				
  (vlax-invoke s 'open)		
  (vlax-invoke-method s 'Write b)
  (vlax-invoke s 'saveToFile f 2)
  (vlax-invoke s 'close)
  (vlax-release-object s)

  (setq f (open "c:\\2.dat" "w"))
  (foreach a l
    (cond
      ( (= 0 a)
        (write-char 256 f)
      )
      ( (= 10 a)
        (write-char 266 f)
      )
      (t
        (write-char a f)
      )
    )
  )
  (close f)
  ;(VL-FILE-DELETE  "c:\\1.dat")
  ;(VL-FILE-DELETE  "c:\\2.dat")
  (princ)
)