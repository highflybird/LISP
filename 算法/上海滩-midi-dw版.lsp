;;;使用前确保已经注册DynamicWrapperX
;;;上海滩
(defun c:sht (/	1H 1P 2H 2P 3H 3P 4P 5P 6P 7P C1 C1+ C2 C2+ C3 C3+ C4 C5 C5- 
	        C6 C6- C7 C7- DWX H JD JP T0 P2 P4 PHANDLE Q1 R0 VL X Y YS 
	     )
  (setq JP 170)				;节拍
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
  (setq ys 152)                         ;钢琴
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
  
  (setq dwx (vlax-create-object "DynamicWrapper"))
  (vlax-invoke dwx 'register "Kernel32" "Sleep" "i=l")
  (vlax-invoke dwx 'register "Kernel32" "LoadLibraryA" "i=s" "r=l")
  (vlax-invoke dwx 'register "winmm.dll" "midiOutOpen" "i=uuuuu" "r=l")
  (vlax-invoke dwx 'register "winmm.dll" "midiOutShortMsg" "i=hu" "r=l")
  (vlax-invoke dwx 'register "winmm.dll" "midiOutClose" "i=h" "r=l")
  (vlax-invoke dwx 'register "msvcrt" "malloc" "i=l" "f=c" "r=l")
  (vlax-invoke dwx 'register "msvcrt" "free" "i=l" "f=c" )
  (vlax-invoke dwx 'register "msvcrt" "sin" "i=d" "f=8" "r=d")

  ;;打开MIDI设备
  (vlax-invoke DWX 'loadLibraryA "winmm")
  (setq pHandle (vlax-invoke DWX 'malloc 8))
  (vlax-invoke DWX 'midiOutOpen pHandle 0 0 0 0)
  ;(setq h (vlax-invoke DWX 'Numget phandle "h"))
  (setq h (_ptr@ phandle))

  (SETQ	X (LIST C3  C5))
  (SETQ	Y (LIST	2p  2p))

  (setq t0 (getvar "TDUSRTIMER"))       ;开始计时
  ;;播放音乐
  (foreach e (MAPCAR 'CONS X Y)
    (if (not (zerop (car e)))
      (vlax-invoke dwx 'midiOutShortMsg h (car e))
    )
    (vlax-invoke Dwx 'Sleep (cdr e))
  )
  (setq t0 (* (- (getvar "TDUSRTIMER") t0) 86400));结束计时
  (princ (strcat "\n用时" (rtos t0 2 3) "秒"));大概3分钟

  ;;关闭MIDI设备
  (vlax-invoke DWX 'midiOutClose h)
  (vlax-invoke DWX 'free pHandle)
  (vlax-release-object DWX)
  
  (princ)
)

(princ "\n命令是:SHT")
(princ)
;|
https://blog.csdn.net/qq_39096769/article/details/105187079
midi参数 音量<<16+声调<<8+乐器类型
音量范围 0x0-0x7f
乐器范围 0x90-0x9f 其中0x99为鼓类，其余皆为钢琴
音阶范围 0x0-7f 以下为中音的音调16进制参数对照表
(注意：以下数据仅为本人通过自己乐感来测试得出的，不一定正确，仅供参考)
Do:0x3c Re:0x3e Mi:0x40 Fa:0x41 So:0x43 La:0x45 Xi:0x47 Do+1:0x48
enum Scale//这是群里的大仙帮我找到的音阶参数
{
	Rest = 0, C8 = 108, B7 = 107, A7s = 106, A7 = 105, G7s = 104, G7 = 103, F7s = 102, F7 = 101, E7 = 100,
	D7s = 99, D7 = 98, C7s = 97, C7 = 96, B6 = 95, A6s = 94, A6 = 93, G6s = 92, G6 = 91, F6s = 90, F6 = 89,
	E6 = 88, D6s = 87, D6 = 86, C6s = 85, C6 = 84, B5 = 83, A5s = 82, A5 = 81, G5s = 80, G5 = 79, F5s = 78,
	F5 = 77, E5 = 76, D5s = 75, D5 = 74, C5s = 73, C5 = 72, B4 = 71, A4s = 70, A4 = 69, G4s = 68, G4 = 67,
	F4s = 66, F4 = 65, E4 = 64, D4s = 63, D4 = 62, C4s = 61, C4 = 60, B3 = 59, A3s = 58, A3 = 57, G3s = 56,
	G3 = 55, F3s = 54, F3 = 53, E3 = 52, D3s = 51, D3 = 50, C3s = 49, C3 = 48, B2 = 47, A2s = 46, A2 = 45,
	G2s = 44, G2 = 43, F2s = 42, F2 = 41, E2 = 40, D2s = 39, D2 = 38, C2s = 37, C2 = 36, B1 = 35, A1s = 34,
	A1 = 33, G1s = 32, G1 = 31, F1s = 30, F1 = 29, E1 = 28, D1s = 27, D1 = 26, C1s = 25, C1 = 24, B0 = 23,
	A0s = 22, A0 = 21
};
enum Voice
{
	L1 = C3, L2 = D3, L3 = E3, L4 = F3, L5 = G3, L6 = A3, L7 = B3,
	M1 = C4, M2 = D4, M3 = E4, M4 = F4, M5 = G4, M6 = A4, M7 = B4,
	H1 = C5, H2 = D5, H3 = E5, H4 = F5, H5 = G5, H6 = A5, H7 = B5,
	LOW_SPEED = 500, MIDDLE_SPEED = 400, HIGH_SPEED = 300,
	_ = 0XFF
};
|;
