(defun tranf (str1 / sym1)
  (setq	lpp-symfun->al
	 lpp-symfun->al
	:lpp :lpp
	make-symbol
	 make-symbol
	intern intern
  )
  (setq sym1 (intern (make-symbol str1) :lpp))
  (lpp-symfun->al sym1)
)