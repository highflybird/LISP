(defun c:ttt (/ i l v n)
  (setq i 0)
  (setq l nil)
  (setq n 100000)
  (repeat n
    (setq l (cons i l))
    (setq i (1+ i))
  )

  (setq v (apply 'vector l))

  ;;第一个元素
  (uti:bench 10000
    (list
      (list 'vector-elt v 0)
      (list 'car l)
      (list 'nth 0 l)
      (list 'list-elt l 0)
    )
  )

  ;;中间元素
  (uti:bench 10000
    (list
      (list 'vector-elt v (/ n 2))
      (list 'nth (/ n 2) l)
      (list 'list-elt l (/ n 2))
    )
  )

  ;;最后一个元素
  (UTI:BENCH 10000
    (list
      (list 'nth (1- N) l)
      (list 'list-elt l (1- N))
      (list 'vector-elt v (1- n))
      ;(list 'last l)
    )
  )

  (defun forvector (v / i)
    (setq i 0)
    (repeat (vector-length v)
      (vector-elt v i)
      (setq i (1+ i))
    )
  )

  (defun forList (lst / i)
    (setq i 0)
    (repeat (length lst)
      (nth i lst)
      (setq i (1+ i))
    )
  )
  
  ;;平均用时
  (uti:bench 10
    (list
      (list 'forvector v)
      (list 'forlist l)
    )
  )
    
  (princ)
)

(defun UTI:Bench (Times Expressions / s)
  (defun Benchmark (Func times / TIME0 TIME1 Speed Value fName)
    (setq fName (car Func))
    (setq TIME0 (getvar "millisecs"))
    (repeat times
      (setq Value (apply fName (cdr func)))
    )
    (setq TIME1 (getvar "millisecs"))
    (setq TIME1 (- TIME1 TIME0 0.0))
    (setq Speed (/ TIME1 times))
    (list fName times TIME1 Speed Value)
  )
  (BenchCommon Times Expressions)
)