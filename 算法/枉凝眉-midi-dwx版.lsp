;;;ʹ��ǰȷ���Ѿ�ע��DynamicWrapperX
;;;����ü
(defun c:WNM (/ 1H 1P 2H 2P 3H 3P 4P 5P 6P 7P C1 C1+ C2 C2+ C3 C3+ C4 C5 C5- 
	        C6 C6- C7 C7- DWX H JD JP T0 P2 P4 PHANDLE Q1 R0 VL X Y YS 
	     )
  (setq JP 250)				;����64
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
  (setq ys 150)                         ;����
  (setq vl (* 127 65536)) 		;����
  
  (setq JD 60)
  (setq 5- (+ vl (* (- JD  5) 256) ys));-5  
  (setq 6- (+ vl (* (- JD  3) 256) ys));-6  
  (setq 7- (+ vl (* (- JD  1) 256) ys));-7  
  (setq 1= (+ vl (* (+ JD  0) 256) ys));1   
  (setq 2= (+ vl (* (+ JD  2) 256) ys));2   
  (setq 3= (+ vl (* (+ JD  4) 256) ys));3   
  (setq 4= (+ vl (* (+ JD  5) 256) ys));4   
  (setq 5= (+ vl (* (+ JD  7) 256) ys));5   
  (setq 6= (+ vl (* (+ JD  9) 256) ys));6   
  (setq 7= (+ vl (* (+ JD 11) 256) ys));7   
  (setq l+ (+ vl (* (+ JD 12) 256) ys));+1  
  (setq 2+ (+ vl (* (+ JD 14) 256) ys));+2  
  (setq 3+ (+ vl (* (+ JD 16) 256) ys));+3  
  
  (setq dwx (vlax-create-object "DynamicWrapperX"))
  (vlax-invoke dwx 'register "Kernel32" "Sleep" "i=l")
  (vlax-invoke dwx 'register "winmm.dll" "midiOutOpen" "i=puuuu" "r=l")
  (vlax-invoke dwx 'register "winmm.dll" "midiOutShortMsg" "i=hu" "r=l")
  (vlax-invoke dwx 'register "winmm.dll" "midiOutClose" "i=h" "r=l")

  ;;��MIDI�豸
  (setq pHandle (vlax-invoke DWX 'MemAlloc 8))
  (vlax-invoke DWX 'midiOutOpen pHandle 0 0 0 0)
  (setq h (vlax-invoke DWX 'Numget phandle "h"))  

  (SETQ	X (LIST 6= 7= 6= 6= 3= 5= 6= 6= 3=
		2= 3= 2= 2= 6- 1= 2= 1= 2= 6-
		1= 2= 3= 3= 6- 3= 2= 3= 5=
		6= 7= 6= 7= 6= 7= 3= 5= 3= 5-
		6- 1= 6- 3= 2= 3= 6- 1=
		7- 7- 6- 7- 2= 3= 2= 7- 5= 3= 2= 7- 3= 2= 3= 5-
                1= 6- l+ 6=
		l+ 2+ l+ 2+ l+ 2+ 6= l+ 6=
	       )
  )
  (SETQ	Y (LIST p2 1p p2 p2 p2 p2 p2 1p 3p
		p2 1p p2 p2 p2 p2 p2 p2 p2 3p
		p2 1p p2 p2 1p p2 3p p2 p2
		1H p2 p2 p2 p4 p4 p2 3p p2 p2
		1H p2 p2 p2 p4 p4 p2 4p
		p2 1p p4 p4 Q1 p4 1P P2 1P p4 p4 p2 p2 p4 p4 p2
		p2 3H p2 3H
		1p 1p p2 p2 p4 p4 p2 p2 3H
	       )
  )

  (setq t0 (getvar "TDUSRTIMER"))       ;��ʼ��ʱ
  ;;��������
  (foreach e (MAPCAR 'CONS X Y)
    (if (not (zerop (car e)))
      (vlax-invoke dwx 'midiOutShortMsg h (car e))
    )
    (vlax-invoke Dwx 'Sleep (cdr e))
  )
  (setq t0 (* (- (getvar "TDUSRTIMER") t0) 86400));������ʱ
  (princ (strcat "\n��ʱ" (rtos t0 2 3) "��"));���3����

  ;;�ر�MIDI�豸
  (vlax-invoke DWX 'midiOutClose h)
  (vlax-invoke DWX 'MemFree pHandle)
  (vlax-release-object DWX)
  
  (princ)
)

(princ "\n������:WNM")
(princ)
;|
https://blog.csdn.net/qq_39096769/article/details/105187079
midi���� ����<<16+����<<8+��������
������Χ 0x0-0x7f
������Χ 0x90-0x9f ����0x99Ϊ���࣬�����Ϊ����
���׷�Χ 0x0-7f ����Ϊ����������16���Ʋ������ձ�
(ע�⣺�������ݽ�Ϊ����ͨ���Լ��ָ������Եó��ģ���һ����ȷ�������ο�)
Do:0x3c Re:0x3e Mi:0x40 Fa:0x41 So:0x43 La:0x45 Xi:0x47 Do+1:0x48
|;
