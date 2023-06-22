;;;使用前先加载syz-midi32.fas
;;;上海滩
(defun c:sht (/	 1H 1P 2H 2P 3H 3P 4P 5P 6P 7P C1 C1+ C2 C2+ C3 C3+
	         C4 C5 C5- C6 C6- C7 C7- I JP P0 P2 P4 Q1 R0 X Y Z
	     )
  (setq JP 125)				;(/ 60. 88)? 速度
  (setq P4 (*  1 JP))
  (setq P2 (*  2 JP))
  (setq Q1 (*  3 JP))
  (setq 1P (*  4 JP))
  (setq 1H (*  6 JP))
  (setq 2P (*  8 JP))
  (setq 2H (* 10 JP))
  (setq 3P (* 12 JP))
  (setq 3H (* 14 JP))
  (setq 4P (* 16 JP))
  (setq 5P (* 20 JP))
  (setq 6P (* 24 JP))
  (setq 7P (* 28 JP))
  
  (setq P0  0)
  (setq R0  0)      ;空一个音
  (setq C5- 4208528);-5 0x00403790=4208528
  (setq C6- 4209040);-6 0x00403990=4209040
  (setq C7- 4209552);-7 0x00403B90=4209552
  (setq C1  4209808);1  0x00403c90=4209808
  (setq C2  4210320);2  0x00403e90=4210320
  (setq C3  4210832);3  0x00404090=4210832
  (setq C4  4211088);4  0x00404190=4211088
  (setq C5  4211600);5  0x00404390=4211600
  (setq C6  4212112);6  0x00404590=4212112
  (setq C7  4212624);7  0x00404790=4212624
  (setq C1+ 4212880);+1 0x00404890=4212880
  (setq C2+ 4213392);+2 0x00404A90=4213392
  (setq C3+ 4213904);+3 0x00404C90=4213904

  (openmidi)
  
  (setq i 0)
  (SETQ	X (LIST C3  C5  C6  C6  C6  C6  C3  C5  C2  C2  C2  C2  C3  C5  C6  C6  C1+ C6  C5  C3  C6  C5  C2  C3  C2  C1  C3  C2  C2  C7- C6- C5
	        C1  R0  C3  C5  C6  C3  C5  C2  C3  C5  C6  C1+ C6  C5  C1  C3
		C2  C2	C3  C5  C2  C3  C6- C6- C1  C2	C3  C2  C7- C6- C1  C5-	C3  C5
		C6  C3  C5  C2  C3  C5  C6  C1+ C6  C5  C1  C3  C2  C2  C3  C5  C2  C3
		C6- C6- C1  C2	C3  C2  C7- C6- C5- C1  R0  C1+ C1+ C6	C1+ R0  C6  C1+ C6  C5  C5  C3 
		C6  C5  C1  C2  C1  C2  C3  R0  C3  C3  C2  C3  R0  C1+ C1+ C7  C6  C3  C3  C2  C3  C1+ C7  C6  C3
		C5  C3  C5  C6  C3  C5  C2  C3  C5  C6  C1+ C6  C5  C1  C3  C2  C2  C3
		C5  C2  C3  C6- C6- C1  C2  C3  C2  C7- C6- C5- C1  C3  C5  C6  C3  C5    
	        C2  C3  C5  C6  C1+ C6  C5  C1  C3  C2  C2  C3  C5  C2  C3  C6- C6- C1
		C2  C3  C2  C7- C6- C5- C1  R0  C1+ C1+ C6  C1+ R0  C6  C1+ C6  C5  C5  C3  C6  C5  C1  C2  C1  C2
		C3  R0  C3  C3  C2  C3  R0  C1+ C1+ C7  C6  C3  C3  C2  C3  C1+ C7  C6  C3  C5  C3  C5
		C6  C3  C5  C2  C3  C5  C6  C1+ C6  C5  C1  C3  C2  C2  C3  C5  C2  C3
		C6- C6- C1  C2  C3  C2  C7- C6- C5- C1  C2  C3  C5  C2  C3  C6- C6- C1
		C2  C3  C2  C7- C6- C5- C1
	       )
  )
  (SETQ	Y (LIST	2p  2p  1P  Q1  P4  1P  p2  p2  1p  Q1  P4  1p  p2  p2  1P  P4  P4  P4  P4  P4  P4  P4  P4  P4  P4  P4  P4  2P  P2  P2  P2  P2 
		6P  1P  P2  P2  3P  P2  P2  3P  P2  P2  P2  1P  P2  1P  P2  P2
		3P  P2	P2  3P	P2  P2	3P  P2	P2  1H	P2  P2	P2  P2  P2  3P	P2  P2
		3P  P2	P2  3P	P2  P2	P2  1P	P2  1P 	P2  P2	3P  P2  P2  3P	P2  P2
                3P  P2	P2  1H	P2  P2	P2  P2	P2  2P 	P2  P2	P2  P2  2P  P2	P2  Q1  P4  3P  P2  P2
	        1H  P2  1P  P4  P4  P2  2P  P2  P2  P2  P2  2P  P2  P2  P2  P2  3P  P2  P2  1H  P2  P2  P2  P2  P2
	        3P  P2	P2  3P 	P2  P2	3P  P2  P2  P2  1P  P2  1P  P2  P2  3P  P2  P2
		3P  P2	P2  3P 	P2  P2  1H  P2  P2  P2  P2  P2  3P  P2  P2  3P  P2  P2
		3P  P2  P2  P2  1P  P2  1P  P2  P2  3P  P2  P2  3P  P2  P2  3P  P2  P2
		1H  P2  P2  P2  P2  P2  2P  P2  P2  P2  P2  2P  P2  P2  P2  P2  3P  P2  P2  1H  P2  1P  P4  P4  P2
		2P  P2  P2  P2  P2  2P  P2  P2  P2  P2  3P  P2  P2  1H  P2  P2  P2  P2  P2  3P  P2  P2
		3P  P2  P2  3P  P2  P2  P2  1P  P2  1P  P2  P2  3P  P2  P2  3P  P2  P2
		3P  P2  P2  1H  P2  P2  P2  P2  P2  3P  P2  P2  3P  P2  P2  3P  P2  P2 
		1H  P2  P2  P2  P2  P2  4P 
	       )
  )
  (SETQ Z (MAPCAR 'CONS X Y))
  (foreach e z
    (if (not (zerop (car e)))
      (playmidi (car e))
    )
    (delay (cdr e))
  )
  (closemidi)
  
  (princ)
)

;;;两只老虎
(defun c:LZLH (/ 1H 1P 2H 2P 3H 3P 4P 5P 6P 7P C0 C1 C1+ C2 C2+ C3
	         C3+ C4 C5 C5- C6 C6- C7 C7- JP P0 P2 P4 Q1 X Y Z
	      )
  (setq JP 125)				;速度
  (setq P4 (*  1 JP))
  (setq P2 (*  2 JP))
  (setq Q1 (*  3 JP))
  (setq 1P (*  4 JP))
  (setq 1H (*  6 JP))
  (setq 2P (*  8 JP))
  (setq 2H (* 10 JP))
  (setq 3P (* 12 JP))
  (setq 3H (* 14 JP))
  (setq 4P (* 16 JP))
  (setq 5P (* 20 JP))
  (setq 6P (* 24 JP))
  (setq 7P (* 28 JP))
  
  (setq P0  0)
  (setq C0  0)      ;空一个音
  (setq C5- 4208528);-5 0x00403790=4208528
  (setq C6- 4209040);-6 0x00403990=4209040
  (setq C7- 4209552);-7 0x00403B90=4209552
  (setq C1  4209808);1  0x00403c90=4209808
  (setq C2  4210320);2  0x00403e90=4210320
  (setq C3  4210832);3  0x00404090=4210832
  (setq C4  4211088);4  0x00404190=4211088
  (setq C5  4211600);5  0x00404390=4211600
  (setq C6  4212112);6  0x00404590=4212112
  (setq C7  4212624);7  0x00404790=4212624
  (setq C1+ 4212880);+1 0x00404890=4212880
  (setq C2+ 4213392);+2 0x00404A90=4213392
  (setq C3+ 4213904);+3 0x00404C90=4213904
  
  (openmidi)
  (SETQ	X (LIST C1 C2 C3 C1 C1 C2 C3 C1 C3 C4 C5 C3 C4 C5 C5 C6 C5 C4 C3 C1 C5 C6 C5 C4 C3 C1 C1 C5- C1 C1 C5- C1))
  (SETQ	Y (LIST	1P 1P 1P 1P 1P 1P 1P 1P 1P 1P 2P 1P 1P 2P Q1 P4 Q1 P4 1P 1P Q1 P4 Q1 P4 1P 1P 1P 1P  2P 1P 1P  2P))
  (SETQ Z (MAPCAR 'CONS X Y))
  (foreach e z
    (if (not (zerop (car e)))
      (playmidi (car e))
    )
    (delay (cdr e))
  )
  (closemidi)
  
  (princ)
)