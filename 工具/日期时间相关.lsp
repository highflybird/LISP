;;;Ref https://www.w3school.com.cn/vbscript/vbscript_ref_functions.asp
;;;��������ʱ����غ���
(defun c:getDate (/ scr date now)
  (or
    (setq SCR (vlax-create-object "Aec32BitAppServer.AecScriptControl.1"))
    (setq SCR (vlax-create-object "ScriptControl"))
  )
  (if scr
    (progn
      (vlax-put scr 'language "VBScript")
      (vlax-invoke scr 'addcode
        "Function MyDate
        Mydate = Date
        End Function
        Function MyNow
	MyNow = Now
        End Function
        Function MyFormatDateTime(nDate)
        MyFormatDateTime = FormatDateTime(nDate)
        End Function
        Function MyDateAdd(interval,number,sdate)
	MyDateAdd = FormatDateTime(DateAdd(interval,number,sdate))
        End Function
        Function MyDateValue(sdate)
	MyDateValue = DateValue(sdate)
        End Function
        Function MyDateDiff(interval,date1,date2)
        MyDateDiff = DateDiff(interval,date1,date2)
        End Function"
      )
      (setq date (vlax-invoke scr 'run "MyDate"));��ǰϵͳ����
      (setq now (vlax-invoke scr 'run "MyNow"));��ǰϵͳʱ��
      (vlax-invoke scr 'run "MyFormatDateTime" now);��ʽ������
      (vlax-invoke scr 'run "MyDateAdd" "d" -3 "2022/9/18")
      (vlax-invoke scr 'run "MyDateAdd" "d" -3 date);����ǰ����
      (vlax-invoke scr 'run "MyDateValue" "2022-Jan-02");����ת��ֵ
      (vlax-invoke scr 'run "MyDateDiff" "d" "1949/10/1" now);��������֮���ʱ������
      (vlax-release-object scr)
      (princ)
    )
  )
)