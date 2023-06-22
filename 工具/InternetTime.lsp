;;;xshrimp 2022.06.01
(defun xzj-InternetTime( / fs timeStr tmpFile true ts wscript)
  (setq tmpFile (vl-filename-mktemp nil nil ".txt"))
  (setq wscript (vlax-create-object "wscript.shell"))
  (setq fs     (vlax-create-object "scripting.filesystemobject"))
  (vlax-invoke wscript 'run (strcat "cmd.exe /c curl  https://www.beijing-time.org/t/time.asp>\"" tmpFile "\"")  0 1)  
  (setq ts (vlax-invoke fs 'opentextfile tmpFile  1 -1));true -1
  (setq timeStr (vlax-invoke ts 'readall))
  (vlax-invoke ts 'Close)  
  (vlax-invoke fs 'DeleteFile tmpFile)
  (alert timeStr)
)
(xzj-InternetTime)

(defun HFB:InternetTime (/ xml url str pos val)
  (setq xml (vlax-create-object "Msxml2.XMLHTTP"))
  (setq url "http://api.k780.com:88/?app=life.time&appkey=10003&sign=b59bc3ef6191eb9f747dd4e83c99f2a4&format=json")
  (vlax-invoke xml 'open "GET" url nil)
  (command "delay" 1000)
  (vlax-invoke xml 'SetRequestHeader "If-Modified-Since" "0")
  (vlax-invoke xml 'send)
  (if (= "OK" (vlax-get xml 'statusText))
    (setq str (vlax-get xml 'responsetext)
	      xml (vlax-release-object xml)
	      pos (+ 14 (vl-string-search "datetime_1" str))
		  val (substr str pos 19)
	)
  )
)

(defun HFB:InternetTime ( / xml url)
  (setq xml (vlax-create-object "Msxml2.XMLHTTP"))
  (setq url "https://www.baidu.com")
  (vlax-invoke xml 'open "GET" url nil)
  (vlax-invoke xml 'send)
  (command "delay" 200)
  (vlax-invoke xml 'GetResponseHeader "Date")
)
(HFB:InternetTime)
;;---------------------=={ Internet Time }==------------------;;
;;                                                            ;;
;;  Returns the date and/or UTC time as a string in the       ;;
;;  format specified. Data is sourced from a NIST server.     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright &#169; 2011 - <a href="http://www.lee-mac.com" target="_blank">www.lee-mac.com</a>       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  format - string specifying format of returned information ;;
;;           using the following identifiers to represent     ;;
;;           date & time quantities:                          ;;
;;           YY = Year, MO = Month,   DD = Day                ;;
;;           HH = Hour, MM = Minutes, SS = Seconds            ;;
;;------------------------------------------------------------;;
;;  Returns:  String containing formatted date/time data      ;;
;;------------------------------------------------------------;;

(defun LM:InternetTime ( format / result rgx server xml )
    (setq server "http://time.nist.gov:13")
    (setq result
        (vl-catch-all-apply
            (function
                (lambda ( / str )
                    (setq xml (vlax-create-object "MSXML2.XMLHTTP.3.0"))
                    (setq rgx (vlax-create-object "VBScript.RegExp"))
                    (vlax-invoke-method xml 'open "POST" server :vlax-false)
                    (vlax-invoke-method xml 'send)
                    (if (setq str (vlax-get-property xml 'responsetext))
                        (progn
                            (vlax-put-property rgx 'global     actrue)
                            (vlax-put-property rgx 'ignorecase actrue)
                            (vlax-put-property rgx 'multiline  actrue)
                            (mapcar
                                (function
                                    (lambda ( a b )
                                        (vlax-put-property rgx 'pattern a)
                                        (setq format (vlax-invoke rgx 'replace format b))
                                    )
                                )
                               '("YY" "MO" "DD" "HH" "MM" "SS")
                               '("$1" "$2" "$3" "$4" "$5" "$6")
                            )
                            (vlax-put-property rgx 'pattern
                                (strcat
                                    "(?:[^\\d]+[\\d]+[^\\d]+)"
                                    "([\\d]+)(?:[^\\d]+)([\\d]+)(?:[^\\d]+)([\\d]+)(?:[^\\d]+)"
                                    "([\\d]+)(?:[^\\d]+)([\\d]+)(?:[^\\d]+)([\\d]+)(?:.+)\\n"
                                )
                            )
                            (vlax-invoke-method rgx 'replace str format)
                        )
                    )
                )
            )
        )
    )
    (if xml  (vlax-release-object xml))
    (if rgx  (vlax-release-object rgx))
    (if (not (vl-catch-all-error-p result))
        result
    )
)
;;;(LM:InternetTime "DD/MO/YY, HH:MM:SS")
;;; (LM:InternetTime "MO.DD.YY")
;(LM:InternetTime "HH:MM:SS")