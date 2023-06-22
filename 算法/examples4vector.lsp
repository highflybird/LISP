(defun c:qqq(/ E I L N V U X w)
  (setq N 500000)
  (defun rnd ()
    (random& N)
  )
  (setq i 0)
  (setq l nil)
  (setq v (make-vector N 0))
  (setq u (make-vector N 0))
  (repeat N
    (setq e (rnd))
    (vector-elt<- u e i)
    (vector-elt<- v e i)
    (setq l (cons e l))
    (setq i (1+ i))
  )
  (setq l (reverse l))
  (setq x (copy-list l))
  (setq y (copy-list l))
  (setq w (copy-vector v))
  ;(setq l (vl-sort l '<))
  ;(qsort v 0 (1- N))

  (UTI:BENCH 1
    (list
      (list 'vl-sort l '<)
      (list 'sort y '<)
      (list 'qsort v 0 (1- n))
      (list 'qsort1 u 0 (1- n))
      (list 'heapSort w n)
      (list 'H:QSort L)
      (list 'H:MergeSort L)
      ;(list 'qsort2 X 0 (1- n))
    )
  )
  (princ)
)

;;;=============================================================
;;; Highflybird's  堆排序算法                                   
;;;=============================================================
;;; 生成大根堆                                                  
(defun adjust (v N I / x L R)
  (while (< (setq L (+ I I 1)) N)
    (setq x L)
    (setq R (1+ L))
    
    (if	(and (< R N) (< (vector-elt v L) (vector-elt v R)))     ;比较左子树和右子树，记录最大值的Index
      (setq x R)
    )
    (if	(< (vector-elt v I) (vector-elt v x))
      (progn
	(vector-swap v I x)                                     ;交换i与MaxChildrenIndex的数据
	(setq I X)                                              ;堆被破坏，需要重新调整
      )
      (setq I N)                                                ;比较左右孩子均大则堆未破坏，不再需要调整
    )
  )
)

;;; 另外一种方式生成大根堆                                      
;;; 递归方式构建大根堆(N是A的长度,index是第一个非叶子节点的下标)
(defun adjust1 (A N index / LEFT MAXIDX RIGHT)
  (setq left (+ index index 1))                                 ;index的左子节点
  (setq right (1+ left))                                        ;index的右子节点
  (setq maxIdx index)
  (if (and (< left N) (> (vector-elt A left) (vector-elt A maxIdx)))
    (setq maxIdx left)
  )
  (if (and (< right N) (> (vector-elt A right) (vector-elt A maxIdx)))
    (setq maxIdx right)
  )
  (if (/= maxIdx index)
    (progn
      (vector-swap A maxidx index)
      (adjust1 A N maxidx)
    )
  )
)

;;; 堆排序                                                      
(defun heapSort(arr size / i)
  ;;构建大根堆（从最后一个非叶子节点向上）
  (setq i (1- (/ size 2)))
  (while (>= i 0)
    (adjust arr size i)
    (setq i (1- i))
  )
  ;;调整大根堆
  (setq i (1- size))
  (while (> i 0)
    (vector-swap arr 0 i)                                       ;与最后一个记录交换
    (adjust arr i 0)                                            ;重新调整为大根堆
    (setq i (1- i))
  )
)

;;;=============================================================
;;;Highflybird's  快速排序算法1                                 
;;;考虑如下情况：如果已经是一个排序好的表                       
;;;如果是一个随机表，则算法稍微慢于H:QSORT2                     
;;;但对于一个已排序好的标，则算法要快于H:QSORT2                 
;;;=============================================================
(defun qsort (v i j / HIGH LOW PIVOT)
  (if (>= i j)
    v
    (progn
      (setq pivot (vector-elt v i))
      (setq low i)
      (setq high j)
      (while (< i j)
	(while (and (< i j) (> (vector-elt v j) pivot))
	  (setq j (1- j))
	)
	(if (< i j)
	  (progn 
	    (vector-elt<- v (vector-elt v j) i)
	    (setq i (1+ i))
	  )
	)
	(while (and (< i j) (< (vector-elt v i) pivot))
	  (setq i (1+ i))
	)
	(if (< i j)
	  (progn 
	    (vector-elt<- v (vector-elt v i) j)
	    (setq j (1- j))
	  )
	)
      )
      (vector-elt<- v pivot j)
      (qsort v low (1- i))
      (qsort v (1+ i) high)
    )
  )
)

(defun qsort1 (v i j / HIGH LOW PIVOT)
  (if (>= i j)
    v
    (progn
      (setq pivot (vector-elt v i))
      (setq low i)
      (setq high j)
      (while (< i j)
	(while (and (< i j) (>= (vector-elt v j) pivot))
	  (setq j (1- j))
	)
	(vector-elt<- v (vector-elt v j) i)
	(while (and (< i j) (<= (vector-elt v i) pivot))
	  (setq i (1+ i))
	)
	(vector-elt<- v (vector-elt v i) j)
      )
      (vector-elt<- v pivot j)
      (qsort1 v low (1- i))
      (qsort1 v (1+ i) high)
    )
  )
)

(defun qsort2 (L i j / HIGH LOW PIVOT)
  (if (>= i j)
    L
    (progn
      (setq pivot (nth i  L))
      (setq low i)
      (setq high j)
      (while (< i j)
	(while (and (< i j) (> (list-elt L j) pivot))
	  (setq j (1- j))
	)
	(if (< i j)
	  (progn 
	    (list-elt<- L (list-elt L j) i)
	    (setq i (1+ i))
	  )
	)
	(while (and (< i j) (< (list-elt L i) pivot))
	  (setq i (1+ i))
	)
	(if (< i j)
	  (progn 
	    (list-elt<- L (list-elt L i) j)
	    (setq j (1- j))
	  )
	)
      )
      (list-elt<- L pivot j)
      (qsort2 L low (1- i))
      (qsort2 L (1+ i) high)
    )
  )
)

;;;=============================================================
;;;Highflybird's  快速排序算法2                                 
;;;考虑如下情况：如果已经是一个排序好的表                       
;;;如果是一个随机表，则算法稍微慢于H:QSORT2                     
;;;但对于一个已排序好的标，则算法要快于H:QSORT2                 
;;;=============================================================
(defun H:QSort (s / a k L R)
  (if (cddr s)
    (progn
      (setq k (* (+ (car s) (last s)) 0.5))			;考虑最坏的情况，取其平均值为关键数
      (while s
	(if (< (setq a (car S)) k)                              ;每个数与关键数比较
          (setq L (cons a L))					;小于它的放到左边
	  (Setq R (cons a R))                                   ;大于或等于它的放到右边
        )
	(setq S (cdr S))                                        
      )
      (if L
	(append (H:QSort L) (H:QSort R))
	(cons (car R) (H:QSort (CDR R)))			;左边为空,则递归右边(为何没有右边为空的情况？请思考.)
      )
    )
    (if (and (cdr s) (> (car s) (cadr s)))                	;只有两个数的情况
      (reverse s)
      s
    )
  )
)

;;;=============================================================
;;;Highflybird's  Merge Sorting algorithm                       
;;;=============================================================
(defun H:Merge (L R / a b s)
  (setq a (car L))						;the first one in left 
  (setq b (car R))						;the first one in right
  (while (and a b)
    (if	(< a b)							;if the left one is less than right one
      (setq s (cons a s)                                        ;then put it into the sorted list
	    L (cdr L)                                           ;remove it from the left 
	    a (car L)						;point to the next one in the left
      )
      (setq s (cons b s) 					;then put the right one into the sorted list
	    R (cdr R)						;remove it from the right 
	    b (car R)						;point to the next one in the right
      )
    )
  )
  (if L								;Copy the others to the sorted list
    (while L
      (setq s (cons (car L) s))                                 
      (setq L (cdr L))
    )
    (while R
      (setq s (cons (car R) s))
      (setq R (cdr R))
    )  
  )
  (reverse S)							;reverse the sorted list to keep the order
)

(defun H:MergeSort (lst / L R)
  (cond
    ( (cddr lst)
      (setq R lst)
      (repeat (/ (length lst) 2)                                ;from the middle of list
        (setq L (cons (car R) L))                               ;the left part
        (setq R (cdr R))                                        ;the right part
      )
      (H:Merge (H:MergeSort (reverse L)) (H:MergeSort R))       ;recurse the two parts
    )
    ( (and (cdr lst) (> (car lst) (cadr lst))) 			;the length of list = 2	
      (reverse lst)
    )
    ( t
      lst
    )
  )
)

;;;-------------------------------------------------------------
;;;测试用函数(benchMark function)                               
;;;-------------------------------------------------------------
(defun BenchCommon (Times Expressions / s)
  (defun Princ-Column (l value / s)
    (setq s (vl-princ-to-string value))
    (princ s)
    (repeat (- l (strlen s))
      (princ " ")
    )
  )
  (defun Print-Result (lst)
    (princ "\n")
    (mapcar 'princ-Column '(25 10 15 16) lst)
  )
  (foreach Func Expressions 
    (setq S (cons (BenchMark Func Times) S))
  )
  (princ "\nStatement                Times     Elapse(ms)     Average(ms/time)")
  (princ "\n------------------------------------------------------------------")
  (setq s (vl-sort s (function (lambda (a b) (< (caddr a) (caddr b))))))
  (mapcar 'Print-Result s) 
  (gc)
  s
)

;;;-------------------------------------------------------------
;;;方式一：采用函数名加参数的方式测试，不出错，但不方便         
;;;-------------------------------------------------------------
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