(defun syz-read-disk-boot-sector (drive / aa fi)
  (setq fi (open (strcat "\\\\.\\" drive) "r"))
  (if fi
    (progn
      (setq aa (_read-nb 512 fi))
      (close fi)
      (setq aa (vl-string->list aa))
    )
    (progn
      (princ "usage: (syz-read-disk-boot-sector \"D:\")")
      (princ "\n")
      (setq aa nil)
    )
  )
)
