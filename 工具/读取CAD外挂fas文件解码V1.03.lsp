;;���³����������ռ�������ѧϰ�������� ��������������ҵʹ��V1.01 2014-8-1
;;�����ļ�����FAS�ļ���ԭ���빤�ߣ�����32λCAD��ʹ��������64λCAD���ɹ� ������CAD 2004-2006
;;2014-9-5�Ż��ٶ� ��append��Ϊcons�� ��nthΪcar cdr ���
;;�ٶ����100�� 1M��FAS�ļ� ������10��㶨

;;�����߰汾64λת�����ƴ���,CAD2011X64����ͨ�� by edata @2017-9-15

(vl-load-com)
;��ȡ�ļ�ת��10����_BY����è
;;��ȡfas�ļ����ر�
(defun dec(fname /)
  (Setq ADO (Vlax-Get-Or-Create-Object "ADODB.Stream"))
  (Vlax-Put-Property ADO 'Type 1)
  (Vlax-Invoke ADO 'Open)
  (Vlax-Invoke-Method ADO 'LoadFromFile fname)
  (Vlax-Put-Property ADO 'Position 0)
  (setq FileGet (Vlax-Invoke-Method ADO 'Read (Vlax-Get ADO 'Size)))
  (Vlax-Invoke-Method ADO 'Close)
  (setq File_list (vlax-safearray->list (vlax-variant-value FileGet)))
  (vlax-release-object ADO)
  file_list
)

;BY ����è
;;��ȡ��д��fas�ļ�
(defun Text (File_list Out)
  (setq array (vlax-make-safearray 17 (cons 0 (1- (length File_list)))))
  (setq	FileGet	(vlax-make-variant
		  (vlax-safearray-fill array File_list)
		  8209
		)
  )
  (Setq ADO (Vlax-Get-Or-Create-Object "ADODB.Stream"))
  (Vlax-Put-Property ADO 'Type 1)
  (Vlax-Invoke ADO 'Open)
  (Vlax-Put-Property ADO 'Position 0)
  (Vlax-Invoke-Method ADO 'Write FileGet)
  (Vlax-Invoke ADO 'SaveToFile Out 2)
  (Vlax-Invoke-Method ADO 'Close)
  (vlax-release-object ADO)
)

(defun c:q1(/ aa bb toupl $1 toupl ee1 $1sta de13 da0lst num $1end $2 strdat0 tmpl
	    $3 tmpl1 de32 da1lst num1 $3end strdat1 keylen keyend keylst lastlst zjlst
	    nstrlst0 nstrlst1 fpl fpl1 dfg ee tmp tlst err)
  (setq t0 (gettime));�������
  (setq f (getfiled "��ѡ���ļ�" "c:/" "" 8))
  (setq bb (dec f))  ;"d:\\a1.fas"
  ;;;;;;;;;;;;;;;;;;
  (if (> (atoi (getvar "acadver"))16)
    (progn
      (setq decint (- (car bb) 13))
      (setq bb (mapcar '(lambda(x) (- x decint)) bb))
      )
    )
  ;;;;;;;;;�����߰汾64λת�����ƴ���,CAD2011X64����ͨ�� by edata @2017-9-15
  ;;;;;;;;;;;;;;;;
  (setq $1 (vl-position 36 bb))
  (setq tmp (tiqulst bb 34 $1))
  (setq ee (vl-position 13 tmp))
  (setq tlst (tiqulst tmp 0 ee))
  (setq dfg (dec-fix tlst))
  (setq toupl (tiqulst bb 0 (+ $1 1)))
  (if (/= dfg 1)
    (progn
      (setq ee1 (tiqulst bb 34 $1))
      (setq $1sta (1+ $1));��һ���ַ�����ʼ
      (setq de13 (vl-position 13 ee1));����λ��
      (setq da0lst (tiqulst ee1 0 de13));�õ���һ���ַ����ȱ�
      (setq num (dec-fix da0lst));;�õ���һ�γ���
      (setq $1end (+ $1sta num));��һ���ַ�������
      (setq $2 (1+ $1end));�ڶ���$
      (setq strdat0 (tiqulst bb $1sta $1end));ȡ�õ�һ���ַ���
      (setq tmpl (tiqulst bb $2 (+ $2 50)));��ʱ������50���ַ�
      (setq $3 (+ $2 1 (vl-position 36 tmpl)));ȡ�õ�����$λ��
      (setq tmpl1 (cddr (tiqulst bb $2 $3)));��һ����ʱ��Ϊȡ�õڶ��γ�����ǰ��
      (setq de32 (vl-position 32 tmpl1));�ҵ�λ��
      (setq da1lst (tiqulst tmpl1 0 de32));�õ��ڶ����ַ����ȱ�
      (setq num1 (dec-fix da1lst));;�õ��ڶ��γ���
      (setq $3end (+ $3 num1));$��ʾȡ�õڶ����ַ�������
      (setq strdat1 (tiqulst bb $3 $3end));ȡ�õڶ����ַ���
      (setq keylen (nth $3end bb));�õ� KEY����ֵ
      (setq keyend (+ $3end keylen 1));�õ�KEY���һ������
      (setq keylst (tiqulst bb (+ 1 $3end) keyend));ȡ��KEY��
      (setq lastlst (tiqulst bb keyend (length bb)));ȡ��KEY�����ı�
      (setq zjlst (tiqulst bb (- $2 1) $3));ȡ���м䳤����һ���ַ���
      ;(setq nstrlst0 (mixor strdat0 keylst));���н�������
      (setq t1 (gettime));����ʱ���
      (princ (strcat "\n��һ�ζ�ȡ�ļ��и�ɱ��ʱ" (rtos (- t1 t0) 2 3) "��"));
      (setq nstrlst1 (mixor strdat1 keylst));���н�������
      (setq fpl (list toupl strdat0 zjlst nstrlst1 (list keylen) (cons  0 (cdr keylst)) lastlst))      
      (setq fpl1 (append toupl strdat0 zjlst nstrlst1 (list keylen) (cons  0 (cdr keylst)) lastlst))
      (PRINC FPL)
      (Text fpl1 "d:\\new_test1.fas")
      (princ "\n   ת���ɹ�")
      )
    (progn
      (setq $2 42);�ڶ���$
      (setq tmpl (tiqulst bb $2 (+ $2 50)));��ʱ������50���ַ�
      (setq $3 (+ $2 1 (vl-position 36 tmpl)));ȡ�õ�����$λ��
      (setq tmpl1 (cddr (tiqulst bb $2 $3)));��һ����ʱ��Ϊȡ�õڶ��γ�����ǰ��
      (setq de32 (vl-position 32 tmpl1));�ҵ�λ��
      (setq da1lst (tiqulst tmpl1 0 de32));�õ��ڶ����ַ����ȱ�
      (setq num1 (dec-fix da1lst));;�õ��ڶ��γ���
      (setq $3end (+ $3 num1));$��ʾȡ�õڶ����ַ�������
      (setq strdat1 (tiqulst bb $3 $3end));ȡ�õڶ����ַ���
      (setq keylen (nth $3end bb));�õ� KEY����ֵ
      (setq keyend (+ $3end keylen 1));�õ�KEY���һ������
      (setq keylst (tiqulst bb (+ 1 $3end) keyend));ȡ��KEY��
      (setq lastlst (tiqulst bb keyend (length bb)));ȡ��KEY�����ı�
      (setq toupl (tiqulst bb 0 $3));ȡ�ñ�ͷ�̶�����
      (setq t1 (gettime));����ʱ���
      (princ (strcat "\n��һ�ζ�ȡ�ļ��и�ɱ��ʱ" (rtos (- t1 t0) 2 3) "��"));
      (setq nstrlst1 (mixor strdat1 keylst));���н�������
      (setq fpl (list toupl nstrlst1 (list keylen) (cons  0 (cdr keylst)) lastlst))
      (setq fpl1 (append toupl nstrlst1 (list keylen) (cons 0 (cdr keylst)) lastlst))
      (PRINC FPL)
      (Text fpl1 "d:\\new_test2.fas")
      (princ "\n   ת���ɹ�")
      )
    )
  (princ)
)


;;��ת������ֵ (49 50 51 53)->1234
(defun dec-fix(lst / ii 1str len 1str 2str qq)
  (setq len (length lst))
  ;(setq ii 0)
  (setq 2str "")
  (repeat len
    (setq 1str (chr (car lst)))    
    (setq 2str (strcat 2str 1str))
    (setq lst (cdr lst))
    ;(setq ii (1+ ii))
    )
  (setq qq (atoi 2str))
  qq
  )

;��ȡ��         �� ��ʼ ��������ֵ
(defun tiqulst (lst sta end / newpl num ann)
  (if (< end (+ (length lst) 1))
    (progn
      (setq newpl '())
      (repeat sta
	(setq lst (cdr lst ))
	)
      (setq ann (- end sta))
      (repeat ann
	(setq num (car lst))
	(setq lst (cdr lst))
	(setq newpl (cons  num newpl))
	)
      )
    )
  (reverse newpl)
  )

;;;�������
(defun booledata (num1 mi1 mi2)
  (boole 6 (boole 6 num1 mi2)mi1)
  )

;;;���ַ������������,�����������Ǳ�
(defun mixor (datapl  mipl / num1  ni newpl wn mi1 mi2 jiemapl wn)
  (while (< (length mipl) (length datapl))
    (setq mipl (append mipl mipl))
    )
  (setq jiemapl '())
  (setq ti1 (gettime));����ʱ���
  (setq ni (length datapl))
  (repeat ni
    (setq num1   (car datapl)
          mi1    (car mipl)
	  mi2    (cadr mipl)
	  datapl (cdr datapl)
	  mipl   (cdr mipl)
	  )
    (setq newpl (booledata num1 mi1 mi2))
    (setq jiemapl (cons  newpl  jiemapl))
    )
  (setq jiemapl (reverse jiemapl))
  (setq ti2 (gettime));����ʱ���
  (princ (strcat "\n�ڶ��ν��в��������ʱ" (rtos (- ti2 ti1) 2 3) "��"));
  jiemapl
)


(defun gettime ()
 (* 86400 (getvar "tdusrtimer"))
)