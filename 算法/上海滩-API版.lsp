;;;使用前确保已经注册DynamicWrapperX
;;;沧海一声笑
(defun c:SHT (/ 1H 1P 2H 2P 3H 3P 4P 5P 6P 7P C1 C1+ C2 C2+ C3 C3+ C4 C5 C5- 
	        C6 C6- C7 C7- DWX H JD JP T0 P2 P4 PHANDLE Q1 R0 VL X Y YS 
	     )
  (setq JP 170)				;节拍88
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
  
  (setq T0 0)
  (setq R0 0)      			;休止
  (setq ys 144)                         ;钢琴
  (setq vl (* 127 65536)) 		;音量
  
  (setq JD 60)
  (setq C5- (+ vl (* (- JD  5) 256) ys));-5  
  (setq C6- (+ vl (* (- JD  3) 256) ys));-6  
  (setq C7- (+ vl (* (- JD  1) 256) ys));-7  
  (setq C1  (+ vl (* (+ JD  0) 256) ys));1   
  (setq C2  (+ vl (* (+ JD  2) 256) ys));2   
  (setq C3  (+ vl (* (+ JD  4) 256) ys));3   
  (setq C4  (+ vl (* (+ JD  5) 256) ys));4   
  (setq C5  (+ vl (* (+ JD  7) 256) ys));5   
  (setq C6  (+ vl (* (+ JD  9) 256) ys));6   
  (setq C7  (+ vl (* (+ JD 11) 256) ys));7   
  (setq C1+ (+ vl (* (+ JD 12) 256) ys));+1  
  (setq C2+ (+ vl (* (+ JD 14) 256) ys));+2  
  (setq C3+ (+ vl (* (+ JD 16) 256) ys));+3  
  
 
  ;;打开MIDI设备
  (loadlibrary "winmm")
  ;(setq pHandle (make-string 8))
  ;(setq pHandle (_addr-of pHandle))
  (register "msvcrt" "calloc" "c" "l")
  (register "msvcrt" "free" "c" "l")
  (register "winmm" "midiOutOpen" "s" "p")
  (setq pHandle (runAPI "msvcrt" "calloc" 2 4))
  
  (runAPI "winmm" "midiOutOpen" pHandle 0 0 0 0)
  (setq h (_ptr@ phandle))  

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
		1H  P2  P2  P2  P2  P2  7P 
	       )
  )

  (setq t0 (getvar "TDUSRTIMER"))       ;开始计时
  ;;播放音乐
  (foreach e (MAPCAR 'CONS X Y)
    (if (not (zerop (car e)))
      (runAPI "winmm" "midiOutShortMsg" h (car e))
    )
    (runAPI "kernel32" "Sleep" (cdr e))
  )
  (setq t0 (* (- (getvar "TDUSRTIMER") t0) 86400));结束计时
  (princ (strcat "\n用时" (rtos t0 2 3) "秒"));大概3分钟

  ;;关闭MIDI设备
  (runAPI "winmm" "midiOutClose" h)
  (runAPI "msvcrt" "free" pHandle)
  (princ)
)

(princ "\n命令是:SHT")
(princ)
