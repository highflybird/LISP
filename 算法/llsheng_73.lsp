(defun play(str / strsplit jpsplit jp pai a b lst)
  (defun strsplit(str splits / i a b)
    (while(<""str)
      (if(vl-remove'nil(mapcar'(lambda(x)(vl-string-search x str))splits))
        (setq i(car(vl-sort(vl-remove'nil(mapcar'(lambda(x)(if(setq l(vl-string-search x str))(cons l x)))splits))
                           '(lambda(s1 s2)(<(car s1)(car s2)))))
              a(cons(substr str 1(car i))a)b(cons(cdr i)b)
              str(substr str(+(car i)(strlen(cdr i))1)))
        (setq a(cons str a)b(cons "" b)str"")))
    (reverse b))
  (defun jpsplit(str)(strsplit str'("A""0""-5""-6""-7""+1""+2""+3""1""2""3""4""5""6""7"" ""___""__.""__""_.""_""---""--.""--""-.""-")))
  (setq jp'(("0"0)("-5" 4208528)("-6" 4209040)("-7" 4209552)("1" 4209808)("2" 4210320)("3" 4210832)("4" 4211088)("5" 4211600)
            ("6" 4212112)("7" 4212624)("+1" 4212880)("+2" 4213392)("+3" 4213904))
        pai'((" "500)("_"250)("__"125)("___"62.5)("-"1000)("--"1500)("---"2000)("."750)("-."1250)("--."2250)("_."375)("__."97.35))
        lst(vl-remove"A"(jpsplit str)))
  (openmidi)
  (while lst
    (if(setq a(car lst)lst(cdr lst)b(assoc a jp))
      (playmidi(cadr b))
      (if(setq b(assoc a pai))
        (delay(cadr b)))))
  (closemidi))
(play"6_6__6__6_5_6 0_.3__5_5__5__5_3_5-2_2__2__2_2__2__2_2_2 2_2_5_4__5__3-6_6__6__6_5__6__6-5_5__5__5_5__5__5_5__6__5 0_.1__2_2__2__2_2_2_2__3__2_.-6__-7_-7 -6__-5__-6-")
(play"-6 6 3 3 -6 2 1 -6.-6 1 -6 1 -6 1 2_3_3--6 3 3 -6_-6_2 1 -6.-6_1 -6_-6_5 3-0_3_5_3_5 3 6_.6__5 -6-0_-6_-6_-6_2_.2__1 -6---A-6 3 3 -6 2 1 -6.-6_1 -6 1 -6 1 2_3_3-A-6 3 3 -6_-6_2 1 -6.-6_1 -6_-6_2 1 -6---0_3_5_3_5 3 6 5_5_-6-0_3_5_3_5 3 6_.6__5 3-0_3_5_3_5 3 6 5_.5__-6-0 -6 -6_-6_6 5 3---")