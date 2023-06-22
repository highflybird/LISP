;;;�˳����tryhi�����Ƚ�:
(defun c:test (/ ss ls)
  (if (setq ss (ssget))
    (progn 
      (uti:bench 10
        (list
          (cons 'ALG:MinBox (list ss 20))
        )
      )
      (make-poly (ALG:MinBox ss 20) 1)
    )
  )
)

  
;;;The procedure for Test
(defun C:test3 (/ PP Pts SEL T0 n fil)
  (princ "\n������test(The command is Test).")
  (princ "\n��ѡ��㡢�������������ͼ��")
  (setq fil '((0 . "POINT,*LINE,ARC,CIRCLE,ELLIPSE,INSERT")))   ;select curve or point
  ;(initget 7)
  (setq n (getint "\nȡ������(10-2000):"))
  (if (null n) (setq n 100))
  (if (setq sel (ssget fil))                                                         
    (progn
      (setq Pts (getpt sel n))                                  ;construct the set of points
      (setq Pts (ALG:GrahamScan Pts))                           ;construct the CCW Hull of this set.
      (if (<= (det (car Pts) (cadr Pts) (caddr Pts)) 0.0)       ;ensure the hull is CCW.
        (setq Pts (reverse Pts))                                ;if it isn't CCW,then reverse it
      )
      (make-poly Pts 1)
      (setq t0 (getvar "TDUSRTIMER"))                           ;The start time of this algorithm
      (setq pp (car (MinAreaRectangle Pts)))                    ;start calculating
      (princ "\nIt takes :")                                            
      (princ (* (- (getvar "TDUSRTIMER") t0) 86400))            ;The End time
      (princ "seconds")
      (if pp
        (make-poly pp 1)                                        ;draw rectangle.
      )
    )
  )
  (princ)
)

(defun ALG:MinBox (sel n / Pnts Rect)
  (setq Pnts (getpt sel n))					;construct the set of points
  (setq Pnts (ALG:GrahamScan Pnts))				;construct the CCW Hull of this set.
  (if (<= (det (car Pnts) (cadr Pnts) (caddr Pnts)) 0.0)	;ensure the hull is CCW.
    (setq Pnts (reverse Pnts))				        ;if it isn't CCW,then reverse it
  )		 
  (setq Rect (car (MinAreaRectangle Pnts))) 			;start calculating
)

;;;=============================================================
;;;Function : Find the minimum area of encasing rectangle.      
;;;Arguments : A CCW HULL                                       
;;;Return: The Four points of Rectangle and its Area            
;;;=============================================================
(defun MinAreaRectangle (ptlist      /     AA    AI    BB    D1
                         D2    EDGE  I     I1X   I1Y   I2X   I2Y
                         IL    INF   IX    IY    J1    J2    MINA
                         MINH  MINW  NORH  NORM  PI1   PI2   PTI0
                         PTI1  PTI2  PTJ1  PTK1  PTM1  PTS1  PTS2
                         PTS3  PTS4  REC1  REC2  REC3  REC4  RECT
                         VECH  VECL  VJ12  VM12
                        )
  (setq INF 1e309)                                                      
  (setq minA INF)                                               ;Initiating the Minimum area is infinite
  (setq pti0 (car ptlist))                                      ;the first point of Hull.
  (setq pts1 (append ptlist (list pti0)))                       ;add the first point at back of Hull
  (setq pts2 (cdr (append ptlist ptlist (list pti0))))          ;Construct a loop for the hull
  (setq i 0)                                                            

  ;;Find area of encasing rectangle anchored on each edge.
  (repeat (length ptlist)
    (setq pi1 (car   pts1)                                              
          pi2 (cadr  pts1)
          i1x (car   pi1)
          i1y (cadr  pi1)
          i2x (car   pi2)
          i2y (cadr  pi2)
          ix  (- i2x i1x)
          iy  (- i2y i1y)
          il  (distance (list ix iy) '(0.0 0.0))
    )

    ;;Ѱ�������
    ;;Find a vertex on on first perpendicular line of support
    (while (> (DOTPR ix iy pts2) 0.0)
      (setq pts2 (cdr pts2))
    )

    ;;Ѱ�����ϵ�
    ;;Find a vertex on second perpendicular line of suppoer
    (if (= i 0)
      (setq pts3 pts2)
    )
    (while (> (CROSSPR ix iy pts3) 0.0)
      (setq pts3 (cdr pts3))
    )

    ;;Ѱ�����ҵ�
    ;;Find a vertex on second perpendicular line of suppoer
    (if (= i 0)
      (setq pts4 pts3)
    )
    (while (< (DOTPR ix iy pts4) 0.0)
      (setq pts4 (cdr pts4))
    )

    ;;�ó���ÿ�ߵľ���
    ;;Find distances between parallel and perpendicular lines of support
    (cond
      ((equal i1x i2x 1e-4)                                     ;����������Xֵ��ͬ
       (setq d1 (- (caar pts3) i1x)                             ;��ô���εĸ߾������ϵ���ߵ�X�Ĳ�ֵ
             d2 (- (cadar pts4) (cadar pts2))                   ;���εĿ������������ҵ�Y�Ĳ�ֵ
       )
      )
      ((equal i1y i2y 1e-4)                                     ;����������Yֵ��ͬ
       (setq d1 (- (cadar pts3) i1y)                            ;��ô���εĸ߾������ϵ���ߵ�Y�Ĳ�ֵ
             d2 (- (caar pts4) (caar pts2))                     ;���εĿ������������ҵ�X�Ĳ�ֵ
       )
      )

      (T
       (setq aa (det pi1 pi2 (car pts3)))                       ;�������ߺ����ϵ㹹�ɵ�����Ķ���(det)
       (setq d1 (/ aa il))                                      ;�߾���detֵ���Ա߳�
       (setq j1 (car pts2))                                     ;���ұߵ�
       (setq j2 (list (- (car j1) iy) (+ (cadr j1) ix)))        ;ͨ�����ұߵ�Ĵ�ֱ�ߵĵ�
       (setq bb (det j1 j2 (car pts4)))                         ;���ұߵ㣬����ĵ������ߵĵ�
       (setq d2 (/ bb il))                                      ;�������det���Ա߳����ǿ�
      )
    )

    ;;������ε��������Ҫʱ������С���
    ;;Compute area of encasing rectangle anchored on current edge.
    ;;if the area is smaller than the old Minimum area,then update,and record the width,height and five points.
    (setq Ai (abs (* d1 d2)))                                   ;������ǸߺͿ�Ļ�
    (if (< Ai MinA)                                             ;������С����ǰ����С��������¼��
      (setq MinA Ai                                             ;������С���
            MinH d1                                             ;��С����ĸ�
            MinW d2                                             ;��С����Ŀ�
            pti1 pi1                                            ;��С����ıߵĵ�һ���˵�
            pti2 pi2                                            ;��С����ıߵĵڶ����˵�
            ptj1 (car pts2)                                     ;���ұߵĵ�
            ptk1 (car pts3)                                     ;������ĵ�
            ptm1 (car pts4)                                     ;����ߵĵ�
      )
    )
    (setq pts1 (cdr pts1))                                      ;�����һ����
    (setq i (1+ i))                                             ;��������һ
  )

  ;;according to the result ,draw the Minimum Area Rectangle
  (setq edge (mapcar '- pti2 pti1))                             ;��С����ı߶�Ӧ������
  (setq VecL (distance edge '(0.0 0.0)))                        ;��С����ıߵĳ���
  (setq NorH (abs (/ MinH VecL)))                               ;��ߵķ���
  
  (setq Norm (list (- (cadr edge)) (car edge)))                 ;��ߵĴ�ֱ����
  (setq vj12 (mapcar '+ ptj1 Norm))                             ;ͨ�����ҵ�Ĵ�ֱ����
  (setq vm12 (mapcar '+ ptm1 Norm))                             ;ͨ�������Ĵ�ֱ����
  (setq vecH (mapcar '* (list NorH NorH) Norm))                         

  (setq rec1 (inters pti1 pti2 ptj1 vj12 nil))                  ;���εĵ�һ��
  (setq rec4 (inters pti1 pti2 ptm1 vm12 nil))                  ;���εĵ��ĵ�
  (setq rec2 (mapcar '+ rec1 vecH))                             ;���εĵڶ���
  (setq rec3 (mapcar '+ rec4 vecH))                             ;���εĵ�����
  (setq rect (list Rec1 rec2 rec3 rec4))                        ;���εĵ��
  (cons rect MinA)                                              ;����������εĵ���������
)

;;;=============================================================
;;;��͹�ǵ�ֱ���ĳ���                                           
;;;��������ʱ���͹�� H-------ע����ʱ��!!!                     
;;;����ֵ: ֱ���������˵��ֱ�� Pair . MaxD                     
;;;=============================================================
(defun ALG:MaxCaliber (H / D M MAXD P PAIR Q U V W)
  (setq Q (cdr (append H H (list (car H)))))                    ;����һ����βѭ����͹��,����ʼ��Ϊ͹�ǵĵڶ���
  (setq MaxD 0.0)                                               ;��ʼ����С����Ϊ0
  (foreach U H                                                  ;���μ��͹�ǵı�
    (setq V (car Q))                                            ;ѭ�����ĵ�һ��
    (setq W (cadr Q))                                           ;ѭ�����ĵڶ���
    (setq M (GEO:Midpoint V W))                                 ;��������е�
    (while (> (dot M U V) 0.0)                                  ;����н�С��90�ȣ����������0��
      (setq Q (cdr Q))                                          ;ѭ�����ƽ�
      (setq V (car Q))                                          ;ȡ��һ��
      (setq W (cadr Q))                                         ;����һ��
      (setq M (GEO:Midpoint V W))                               ;��������е�
    )
    (setq D (distance U V))                                     ;������ʱ��������
    (if (> D MaxD)                                              ;�������ǰ���������
      (setq MaxD D                                              ;���滻ǰ���������
            Pair (list U V)                                     ;����¼��Ե�
      )
    )
  )
  (cons Pair MaxD)                                              ;������Ե��������
)

;;;=============================================================
;;; ����: ��Gramhamɨ�跨����͹��                               
;;; ����: ��ά�㼯                                              
;;; ���: �㼯��͹��                                            
;;;=============================================================
(defun ALG:GrahamScan (Pnts / H P pMax)
  (if (caddr Pnts)                                              ;����3��
    (progn
      (setq pMax (assoc (apply 'max (mapcar 'car Pnts)) Pnts)) 	;���ұߵĵ�
      (setq Pnts (ALG:SortByAngle Pnts pMax))                   ;����㼯
      (setq H    (list (cadr Pnts) pMax))                       ;��ʼ������
      (foreach n (cddr Pnts)                                    ;�ӵ�3�㿪ʼ
        (setq H (cons n H))                                     ;��Pi���뵽͹��
        (while (and (setq P (caddr H)) (ALG:CCW n (cadr H) P))  ;�����ת
          (setq H (cons n (cddr H)))                            ;ɾ��Pi-1��
        )
      )
      (reverse H)                                               ;����͹��
    ) 
  )
)

;;;=============================================================
;;; �ж��Ƿ���ʱ��                                              
;;;=============================================================
(defun ALG:CCW (p1 p2 p3 / x2 y2)
  (setq	x2 (car p2) y2 (cadr p2))
  (> (- (* (- x2 (car p3)) (- y2 (cadr p1)))
        (* (- x2 (car p1)) (- y2 (cadr p3)))
     )
    -1e-5
  )
)

;;;=============================================================
;;; ��ĳ��Ϊ���㣬���սǶȺ;������㼯                        
;;;=============================================================
(defun ALG:SortByAngle (lst p /)
  (vl-sort
    lst
    (function
      (lambda (e1 e2 / a1 a2)
        (if (= (setq a1 (angle p e1)) (setq a2 (angle p e2)))
          (< (distance p e1) (distance p e2))
          (< a1 a2)
        )
      )
    )
  )
)

(defun c:test2 (/ sel Pts)
  (if (setq sel (ssget))
    (progn
      (setq Pts (getpt sel 20))
      (uti:bench 100
        (list
          (cons 'ALG:FindMaxPoint (list Pts))
          (cons 'ALG:FindMaxPoint1 (list Pts))
        )
      )
    )
  )
) 
(defun ALG:FindMaxPoint (pts)
  (assoc (apply 'max (mapcar 'car pts)) pts)
)
(defun ALG:FindMaxPoint1 (pts)
  (setq pMax (car pts))
  (foreach n pts (if (> (car n) (car pMax)) (setq pMax n)))	;�����ұߵĵ�
  pMax
)


;;;=============================================================
;;; �е㺯��                                                    
;;;=============================================================
(defun GEO:Midpoint (p1 p2)
  (list
    (* (+ (car p1) (car p2)) 0.5)
    (* (+ (cadr p1) (cadr p2)) 0.5)
  )
)

;;;-------------------------------------------------------------
;;; ���= x1*x2 + y1*y2                                         
;;;-------------------------------------------------------------
(defun DOTPR (ix iy pts / pt1 pt2)
  (setq pt1 (car pts))
  (setq pt2 (cadr pts))
  (+ (* ix (- (car pt2) (car pt1)))
     (* iy (- (cadr pt2) (cadr pt1)))
  )
)

;;;-------------------------------------------------------------
;;; ���= x1*y2 - x2*y1                                         
;;;-------------------------------------------------------------
(defun CROSSPR (ix iy pts / pt1 pt2)
  (setq pt1 (car pts))
  (setq pt2 (cadr pts))
  (- (* ix (- (cadr pt2) (cadr pt1)))
     (* iy (- (car pt2) (car pt1)))
  )
)

;;;-------------------------------------------------------------
;;; �������������ʽ,������֮�����                             
;;;-------------------------------------------------------------
(defun det (p1 p2 p3 / x2 y2)
  (setq x2 (car p2)
        y2 (cadr p2)
  )
  (- (* (- x2 (car p3)) (- y2 (cadr p1)))
     (* (- x2 (car p1)) (- y2 (cadr p3)))
  )
)

;;;-------------------------------------------------------------
;;; ��������P1P2,P1P3�ĵ������                                 
;;;-------------------------------------------------------------
(defun dot (p1 p2 p3 / x1 y1)
  (setq x1 (car p1)
        y1 (cadr p1)
  )
  (+ (* (- (car p2) x1) (- (car p3) x1))
     (* (- (cadr p2) y1) (- (cadr p3) y1))
  )
)

;;;-------------------------------------------------------------
;;; ȡ�㺯��2                                                   
;;;-------------------------------------------------------------
(defun getpt (ss num / i E O N C S)
  (repeat (setq i (sslength ss))
    (setq e (ssname ss (setq i (1- i))))
    (setq O (vlax-ename->vla-object E))
    (setq N (vla-get-ObjectName O))
    (cond
      ( (= N "AcDbPolyline")
        (setq c (get-pline-vertexs E num))
        (setq s (append c s))
      )
      ( (= N "AcDbPoint")
        (setq s (cons (vlax-get O 'Coordinates) s))
      )
      ( (= N "AcDbLine")
        (setq s (cons (vlax-get O 'startpoint) s))
        (setq s (cons (vlax-get O 'Endpoint) s))
      )
      ( (= 0 (cdr (assoc 75 (entget E))))
        (setq C (get-3dpolyline-vertexs E))
        (setq s (append C s))
      )
      (t
        (setq c (get-spline-vertexs E num))
        (setq s (append c s))
      )
    )
  )
)

;;;-------------------------------------------------------------
;;; ȡ�������ζ���                                            
;;;-------------------------------------------------------------
(defun get-LWpolyline-vertexs (DXF / lst)
  (foreach n DXF
    (if (= (car n) 10)
      (setq lst (cons (cdr n) lst))
    )
  )
  (reverse lst)
)

;;;-------------------------------------------------------------
;;; ȡ����ϵ�POLYLINE(2d��3d)����                              
;;;-------------------------------------------------------------
(defun get-3dpolyline-vertexs (poly / vtx DXF lst)
  (setq vtx (entnext poly))
  (while (/= (cdr (assoc 0 (setq DXF (entget vtx)))) "SEQEND")
    (setq lst (cons (cdr (assoc 10 DXF)) lst)
	  vtx (entnext vtx)
    )
  )
  (reverse lst)
)

;;;-------------------------------------------------------------
;;; ȡ���������ߵĵ�                                            
;;;-------------------------------------------------------------
(defun get-spline-vertexs (ent n / DIST ENDPAR I LEN OBJ PT PTS SEG)
  (setq obj (vlax-ename->vla-object ent))
  (setq len (vlax-curve-getDistAtParam ent (vlax-curve-getEndParam ent)))
  (setq name (vla-get-objectname obj))
  (if (= name (vla-get-objectname obj) "AcDbSpline")
    (setq n (* n (vla-get-NumberOfControlPoints obj)))
    (if (or (= name "AcDb3dPolyline") (= name "AcDb2dPolyline"))
      (setq n (* n (/ (length (vlax-get obj 'Coordinates)) 3)))
    )
  )
  (setq seg (/ len n))
  (setq dist 0)
  (repeat n  
    (setq pt (vlax-curve-getPointAtDist ent dist))
    (setq pts (cons pt pts))
    (setq dist (+ seg dist))   
  )
  (if (not (vlax-curve-isClosed ent))
    (setq pt (vlax-curve-getEndPoint ent)
          pts (cons pt pts)
    )
  )
  (reverse pts)
)

;;;-------------------------------------------------------------
;;; ȡ�������ߵĵ�                                            
;;;-------------------------------------------------------------
(defun get-pline-vertexs (ent n / BLG DIST ENDPAR I L1 L2 L3 LI OBJ PT PTS VEXNUM)
  (setq obj (vlax-ename->vla-object ent))
  (setq endpar (vlax-curve-getEndParam ent))
  (setq vexNum (fix endPar))
  (setq pts nil)
  (setq i 0)
  (repeat vexNum
    (setq pt (vlax-curve-getPointAtParam ent i))
    (setq pts (cons pt pts))
    (setq blg (vla-getbulge obj i))
    (if (/= blg 0.0)
      (progn
        (setq l1 (vlax-curve-getDistAtParam ent i))
        (setq l2 (vlax-curve-getDistAtParam ent (1+ i)))
        (setq l3 (- l2 l1))
        (setq li (/ l3 n))
        (setq dist l1)
        (repeat (1- n)
          (setq dist (+ dist li))
          (setq pt (vlax-curve-getPointAtDist ent dist))
          (setq pts (cons pt pts))
        )
      )
    )
    (setq i (1+ i))
  )
  (if (not (vlax-curve-isClosed ent))
    (setq pt (vlax-curve-getEndPoint ent)
          pts (cons pt pts)
    )
  )
  pts
)

;;;-------------------------------------------------------------
;;; ���ƶ����                                                  
;;;-------------------------------------------------------------
(defun Make-Poly (pp closed / x)
  (entmakeX                                                      
    (vl-list*
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbPolyline")
      '(62 . 1)							;��ɫ��
      (cons 90 (length pp))                                     ;�������
      (cons 70 closed)						;1�պϣ�0���պ�
      (mapcar (function (lambda (x) (cons 10 x))) pp)           ;����߶���
    )
  )
)

;;;-------------------------------------------------------------
;;; �����ú���(benchMark function)                              
;;;-------------------------------------------------------------
(defun UTI:Bench (Times Expressions / s fName TIME0 TIME1 Value Speed)
  (defun Princ-Column (l value / s)
    (setq s (vl-princ-to-string value))
    (princ s)
    (repeat (- l (strlen s))
      (princ " ")
    )
  )
  (defun Print-Result (lst)
    (princ "\n")
    (mapcar 'princ-Column '(19 9 14 16) lst)
  )
  
  (foreach Func Expressions
    (setq fName (car Func))
    (setq TIME0 (getvar "millisecs"))
    (repeat times
      (setq Value (apply fName (cdr func)))
    )
    (setq TIME1 (getvar "millisecs"))
    (setq TIME1 (- TIME1 TIME0 0.0))
    (setq Speed (/ TIME1 times))
    (setq S (cons (list fName times TIME1 Speed Value) S))
  )
  
  (princ "\nStatement          Times    Elapse(ms)    Average(ms/time)")
  (princ "\n----------------------------------------------------------")
  (setq s (vl-sort s (function (lambda (a b) (< (caddr a) (caddr b))))))
  (mapcar 'Print-Result s) 
  (gc)
  s
)
