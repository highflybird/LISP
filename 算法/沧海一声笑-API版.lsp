;;;使用前确保已经注册DynamicWrapperX
;;;沧海一声笑
(defun c:CHX (/ 1H 1P 2H 2P 3H 3P 4P 5P 6P 7P C1 C1+ C2 C2+ C3 C3+ C4 C5 C5- 
	        C6 C6- C7 C7- DWX H JD JP T0 P2 P4 PHANDLE Q1 R0 VL X Y YS 
	     )
  (setq JP 234)				;节拍64
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
  (setq fMalloc (register "msvcrt" "malloc" "c" "l"))
  (setq fCalloc (register "msvcrt" "calloc" "c" "l"))
  (setq fFree (register "msvcrt" "free" "c" "l"))
  (setq fOpen (register "winmm" "midiOutOpen" "s" "l"))
  (setq fPlay (register "winmm" "midiOutShortMsg" "s" "l"))
  (setq fClose (register "winmm" "midiOutClose" "s" "l"))
  (setq fSleep (register "kernel32" "Sleep" "s" "l"))
  ;(setq pHandle (call "msvcrt" "malloc" 8))
  ;(setq pHandle (call "msvcrt" "malloc" 16))
  (if (> (strlen (VL-PRINC-TO-STRING +)) 19)
    (setq pHandle (call fCalloc 2 8)
          ret (call fopen pHandle 0 0 0 0)
          h (_ptr64@ phandle)
    )
    (setq pHandle (call fCalloc 2 4)
     	  ret (call fopen pHandle 0 0 0 0)
     	  h (_ptr@ phandle)
    )
  )	  
  (setq h1 (gethandle phandle))

  (SETQ	X (LIST C6 C5 C3 C2 C1 C3 C2 C1 C6- C5- C5- C6- C5- C6- C1 C2 C3 C5 C6 C5 C3 C2 C1 C2  
		C6 C5 C3 C2 C1 C3 C2 C1 C6- C5- C5- C6- C5- C6- C1 C2 C3 C5 C6 C5 C3 C2 C1 C2
		C6 C5 C3 C2 C1 C3 C2 C1 C6- C5- C5- C6- C5- C6- C1 C2 C3 C5 C6 C5 C3 C2 C1 C2
		C6 C5 C3 C2 C1 C3 C2 C1 C6- C5- C5- C6- C5- C6- C1 C2 C3 C5 C6 C5 C3 C2 C1  
	       )
  )
  (SETQ	Y (LIST	Q1 P4 P2 P2 2P Q1 P4 P2 P2 2P Q1 P4 Q1 P4 Q1 P4 P2 P2 Q1 P4 P4 P4 P2 2P
		Q1 P4 P2 P2 2P Q1 P4 P2 P2 2P Q1 P4 Q1 P4 Q1 P4 P2 P2 Q1 P4 P4 P4 P2 2P
		Q1 P4 P2 P2 2P Q1 P4 P2 P2 2P Q1 P4 Q1 P4 Q1 P4 P2 P2 Q1 P4 P4 P4 P2 2P
		Q1 P4 P2 P2 2P Q1 P4 P2 P2 2P Q1 P4 Q1 P4 Q1 P4 P2 P2 Q1 P4 P4 P4 2H
	       )
  )

  (setq t0 (getvar "TDUSRTIMER"))       ;开始计时
  ;;播放音乐
  (foreach e (MAPCAR 'CONS X Y)
    (if (/= 0 (car e))
      (call fPlay h (car e))
    )
    (call fSleep (cdr e))
  )
  (setq t0 (* (- (getvar "TDUSRTIMER") t0) 86400));结束计时
  (princ (strcat "\n用时" (rtos t0 2 3) "秒"));大概3分钟

  ;;关闭MIDI设备
  (call fClose h)
  (call fFree pHandle)
  (princ)
)

(princ "\n命令是:CHX")
(c:chx)
(princ)
;|
https://blog.csdn.net/qq_39096769/article/details/105187079
midi参数 音量<<16+声调<<8+乐器类型
音量范围 0x0-0x7f
乐器范围 0x90-0x9f 其中0x99为鼓类，其余皆为钢琴
音阶范围 0x0-7f 以下为中音的音调16进制参数对照表
(注意：以下数据仅为本人通过自己乐感来测试得出的，不一定正确，仅供参考)
Do:0x3c Re:0x3e Mi:0x40 Fa:0x41 So:0x43 La:0x45 Xi:0x47 Do+1:0x48
|;
