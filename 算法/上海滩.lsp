(defun c:sht (/	  1H  1P  2H  2P  3H  3P  4P  C0  C1  C1+ C2  C3  C5
	      C5- C6  C6- C7  C7- I   JP  P0  P2  P4  S	  X   Y	  Z
	      JD  5p  6P  7P  Q1
	     )
  (setq JP 30)				;(/ 60. 88)? 速度
  (setq 1P (/ 15000 JP))
  (setq 1H (/ 22500 JP))
  (setq 2P (/ 30000 JP))
  (setq 2H (/ 37500 JP))
  (setq 3P (/ 45000 JP))
  (setq 3H (/ 52500 JP))
  (setq 4P (/ 60000 JP))
  (setq 5P (/ 75000 JP))
  (setq 6P (/ 90000 JP))
  (setq 7P (/ 105000 JP))
  (setq P2 (/ 7500 JP))
  (setq P4 (/ 3750 JP))
  (setq Q1 (/ 11250 JP))
  (setq P0 10)
  (setq C0 10)
  (setq JD 261.23)			;基调 261.23
  (setq i 0.0)
  (foreach l '("-" "" "+")
    (foreach n '("1" "1#" "2" "2#" "3" "4" "4#" "5" "5#" "6" "6#" "7")
      (setq s (read (strcat "C" n l)))
      (set s (fix (* JD (expt 2 (/ i 12)))))
      (setq i (1+ i))
    )
  )
  (setq i 0)
  (SETQ	X (LIST C3  C5  C6  C6  C6  C6  C3  C5  C2  C2  C2  C2  C3  C5  C6  C6  C1+ C6  C5  C3  C6  C5  C2  C3  C2  C1  C3  C2  C2  C7- C6- C5
	        C1  C3  C5  C6  C3  C5  C2  C3  C5  C6  C1+ C6  C5  C1  C3
		C2  C2	C3  C5  C2  C3  C6- C6- C1  C2	C3  C2  C7- C6- C1  C5-	C3  C5
		C6  C3  C5  C2  C3  C5  C6  C1+ C6  C5  C1  C3  C2  C2  C3  C5  C2  C3
		C6- C6- C1  C2	C3  C2  C7- C6- C5- C1  C0  C1+ C1+ C6	C1+ C1+ C6  C1+ C6  C5  C5  C3 
		C6  C5  C1  C2  C1  C2  C3  C0  C3  C3  C2  C3  C0  C1+ C1+ C7  C6  C3  C3  C2  C3  C1+ C7  C6  C3
		C5  C3  C5  C6  C3  C5  C2  C3  C5  C6  C1+ C6  C5  C1  C3  C2  C2  C3
		C5  C2  C3  C3  C6- C6- C6- C1  C2  C3  C2  C7- C6- C5- C1  C3  C5  C6  C3  C5    
	        C2  C3  C5  C6  C1+ C6  C5  C1  C3  C2  C2  C3  C5  C2  C3  C6- C6- C1
		C2  C3  C2  C7- C6- C5- C1  C0  C1+ C1+ C6  C1+ C0  C6  C1+ C6  C5  C5  C3  C6  C5  C1  C2  C1  C2
		C3  C0  C3  C3  C2  C3  C0  C1+ C1+ C7  C6  C3  C3  C2  C3  C1+ C7  C6  C3  C5  C3  C5
		C6  C3  C5  C2  C3  C5  C6  C1+ C6  C5  C1  C3  C2  C2  C3  C3  C5  C5  C2  C3
		C3  C6- C6- C6- C1  C2  C3  C2  C7- C6- C5- C1  C2  C3  C3  C5  C5  C2  C3  C3  C6- C6- C6- C1
		C2  C3  C2  C7- C6- C5- C1 
	       )
  )
  (SETQ	Y (LIST	2p  2p  1P  Q1  P4  1P  p2  p2  1p  Q1  P4  1p  p2  p2  1P  P4  P4  P4  P4  P4  P4  P4  P4  P4  P4  P4  P4  2P  P2  P2  P2  P2 
		7p  P2  P2  3P  P2  P2  3P  P2  P2  P2  1P  P2  1P  P2  P2
		3P  P2	P2  3P	P2  P2	3P  P2	P2  1H	P2  P2	P2  P2  P2  3P	P2  P2
		3P  P2	P2  3P	P2  P2	P2  1P	P2  1P 	P2  P2	3P  P2  P2  3P	P2  P2
                3P  P2	P2  1H	P2  P2	P2  P2	P2  2P 	P2  P2	P2  P2  2P  P2	P2  Q1  P4  3P  P2  P2
	        1H  P2  1P  P4  P4  P2  2P  P2  P2  P2  P2  2P  P2  P2  P2  P2  3P  P2  P2  1H  P2  P2  P2  P2  P2
	        3P  P2	P2  3P 	P2  P2	3P  P2  P2  P2  1P  P2  1P  P2  P2  3P  P2  P2
		3P  P2	P2  P2 	P2  2P	P2  P2  1H  P2  P2  P2  P2  P2  3P  P2  P2  3P  P2  P2
		3P  P2  P2  P2  1P  P2  1P  P2  P2  3P  P2  P2  3P  P2  P2  3P  P2  P2
		1H  P2  P2  P2  P2  P2  2P  P2  P2  P2  P2  2P  P2  P2  P2  P2  3P  P2  P2  1H  P2  1P  P4  P4  P2
		2P  P2  P2  P2  P2  2P  P2  P2  P2  P2  3P  P2  P2  1H  P2  P2  P2  P2  P2  3P  P2  P2
		3P  P2  P2  3P  P2  P2  P2  1P  P2  1P  P2  P2  3P  P2  P2  P2  P2  2P  P2  P2
		P2  P2  2P  P2  P2  1H  P2  P2  P2  P2  P2  3P  P2  P2  P2  P2  2P  P2  P2  P2  P2  2P  P2  P2
		1H  P2  P2  P2  P2  P2  6P 
	       )
  )
  (SETQ Z (MAPCAR 'CONS X Y))
  (foreach e z
    (beep (car e) (cdr e))
  )
  (princ)
)

(defun c:LZLH (/ 1H  1P  2H  2P  3H  3P  4P  C0  C1  C1+ C2  C3  C5
	      C5- C6  C6- C7  C7- I   JP  P0  P2  P4  S	  X   Y	  Z
	      JD  5p  6P  7P  Q1 C4
	     )
  (setq JP 30)				;(/ 60. 88)? 速度
  (setq 1P (/ 15000 JP))
  (setq 1H (/ 22500 JP))
  (setq 2P (/ 30000 JP))
  (setq 2H (/ 37500 JP))
  (setq 3P (/ 45000 JP))
  (setq 3H (/ 52500 JP))
  (setq 4P (/ 60000 JP))
  (setq 5P (/ 75000 JP))
  (setq 6P (/ 90000 JP))
  (setq 7P (/ 105000 JP))
  (setq P2 (/ 7500 JP))
  (setq P4 (/ 3750 JP))
  (setq Q1 (/ 11250 JP))
  (setq P0 10)
  (setq C0 10)
  (setq JD 261.23)			;基调 261.23
  (setq i 0.0)
  (foreach l '("-" "" "+")
    (foreach n '("1" "1#" "2" "2#" "3" "4" "4#" "5" "5#" "6" "6#" "7")
      (setq s (read (strcat "C" n l)))
      (set s (fix (* JD (expt 2 (/ i 12)))))
      (setq i (1+ i))
    )
  )
  (setq i 0)
  (SETQ	X (LIST C1 C2 C3 C1 C1 C2 C3 C1 C3 C4 C5 C3 C4 C5 C5 C6 C5 C4 C3 C1 C5 C6 C5 C4 C3 C1 C1 C5- C1 C1 C5- C1))
  (SETQ	Y (LIST	1P 1P 1P 1P 1P 1P 1P 1P 1P 1P 2P 1P 1P 2P Q1 P4 Q1 P4 1P 1P Q1 P4 Q1 P4 1P 1P 1P 1P  2P 1P 1P  2P))
  (SETQ Z (MAPCAR 'CONS X Y))
  (foreach e z
    (beep (car e) (cdr e))
  )
  (princ)
)