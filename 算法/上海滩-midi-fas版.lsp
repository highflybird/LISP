;;;ʹ��ǰȷ���Ѿ�ע��DynamicWrapperX
;;;�Ϻ�̲
(defun c:sht (/	1H 1P 2H 2P 3H 3P 4P 5P 6P 7P C1 C1+ C2 C2+ C3 C3+ C4 C5 C5- 
	        C6 C6- C7 C7- DWX H JD JP T0 P2 P4 PHANDLE Q1 R0 VL X Y YS) 	     
  (setq JP 170)				;����
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
  (setq R0 0)      			;��ֹ
  (setq ys 144)                         ;����
  (setq vl (* 127 65536)) 		;����
  
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
  
  (load-dll "winmm")  
  (defun Sleep (sec)
    (_run-dll "kernel32" "Sleep" sec)
  )
  (defun midiOutShortMsg (h u)
    (_run-dll "winmm" "midiOutShortMsg" h u)
  )
  (setq pHandle (_run-dll "msvcrt" "malloc" 16))
  ;;��MIDI�豸
  (setq ret (_run-dll "winmm" "midiOutOpen" pHandle 0 0 0 0))
  (setq h (_ptr@ pHandle))
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

  (setq t0 (getvar "TDUSRTIMER"))       ;��ʼ��ʱ
  ;;��������
  (foreach e (MAPCAR 'CONS X Y)
    (if (not (zerop (car e)))
      (midiOutShortMsg h (car e))
    )
    (Sleep (cdr e))
  )
  (setq t0 (* (- (getvar "TDUSRTIMER") t0) 86400));������ʱ
  (princ (strcat "\n��ʱ" (rtos t0 2 3) "��"));���3����

  ;;�ر�MIDI�豸
  (_run-dll "winmm" "midiOutClose" h)
  (_run-dll "msvcrt" "free" pHandle)

  (princ)
)

(princ "\n������:SHT")
(princ)
