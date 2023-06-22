; 获取对象包围盒、最小包围盒-----(也适于UCS)
; http://bbs.xdcad.net/forum.php?mod=viewthread&tid=670711&fromuid=132766

;;[功能] 图元当前坐标系下包围盒,4角点坐标
; 4 = 左上；3 = 右上
; 1 = 左下；2 = 右下

;;Flag : T时，返回最小包围盒角点；nil时，返回包围盒角点
;;说明 1 使用前加载须加载highflybird的程序Matrix-Lib.LSP
;;     2 作者保留本程序的一切权利，但你可以自由拷贝与复制、修改本程序用于非商业目的 
;;     3 自贡黄明儒 2013年10月8日
;;示例(setq ll(HH:Ent4pt (setq ent (car (entsel))) T)),返回UCS坐标系下坐标
(defun HH:Ent4pt (ent Flag / ENT LST MAT MAT1 MAXPT MINPT OBJ UCSFLAG X)
  (cond 
  ((= (type ent) 'ENAME)
  (setq obj (vlax-ename->vla-object ent))
  )
  ((= (type ent) 'VLA-OBJECT) (setq obj ent))
  (T (exit))
  )
  (and Flag
       (setq Mat (Mat:EntityMatrix ent))
       (setq Mat1 (cadr Mat));Mat1 4x4
       (setq Mat (car Mat));Mat 4x4
  )
  (if (= (getvar "WORLDUCS") 0)
    (setq UcsFlag T)
  )
  (cond ((and Flag UcsFlag)
  (vla-TransformBy obj (vlax-tmatrix Mat))
)
(UcsFlag (vla-TransformBy obj (vlax-tmatrix (MAT:u2w))))
(Flag (vla-TransformBy obj (vlax-tmatrix Mat)))
  )
  (vla-GetBoundingBox obj 'minPt 'maxPt)   ;得到包围框
  (setq minPt (vlax-safearray->list minPt))
  (setq maxPt (vlax-safearray->list maxPt))
  (cond ((and Flag UcsFlag)
  (vla-TransformBy obj (vlax-tmatrix Mat1))
)
(UcsFlag (vla-TransformBy obj (vlax-tmatrix (MAT:w2u))))
(Flag (vla-TransformBy obj (vlax-tmatrix Mat1)))
  )
  (setq lst (list minPt
    (list (car maxPt) (cadr minpt) (caddr minPt))
    maxPt
    (list (car minpt) (cadr maxPt) (caddr minPt))
     )
  )
  (COND (Flag nil)
(UcsFlag (setq mat1 (MAT:w2u)))
  )
  (cond ((or Flag UcsFlag)
  (setq lst (mapcar '(lambda (x) (mat:mxp mat1 x)) lst)) ;wcs坐标
  (setq lst (mapcar '(lambda (x) (trans x ent 1)) lst))
)
  )
  lst
)
;;;-----------------------------------------------------------;;
;;; 通用变换矩阵 by highflybird                               ;;
;;; 输入：from - 原坐标系，                                   ;;
;;;       to   - 目的坐标系，                                 ;;
;;;       Org  - 目的坐标系的原点相对原坐标系的位置           ;;
;;;       Ang  - 相对于原坐标系的旋转角度                     ;;
;;; 输出：两个矩阵，一个是从原坐标系变换到目的坐标系的变换矩阵;;
;;;       一个是从目的坐标系变换到原坐标系的变换矩阵          ;;
;;;-----------------------------------------------------------;;
(defun MAT:Align (from to Org Ang / Mat Rot Inv Cen)
  (setq Mat (mapcar (function (lambda (v) (trans v from to T)))
                    '((1. 0. 0.) (0. 1. 0.) (0. 0. 1.))
            )
  )
  (if (not (equal ang 0 1e-14))
    (setq Rot (list (list (cos ang) (- (sin ang)) 0.)
                    (list (sin ang) (cos ang) 0.)
                    (list 0. 0. 1.)
              )
          mat (MAT:mxm mat Rot)
    )
  )
  (setq Cen (trans Org to from))
  (setq Inv (mat:trp mat))
  (list
    (Mat:DispToMatrix mat Cen)                                  ;from->to
    (Mat:DispToMatrix Inv (mat:mxv mat (mapcar '- Cen)))        ;to->from
  )
)
;;;-----------------------------------------------------------;;
;;; Append displacement vector to a matrix      -Highflybird- ;;
;;; 把位移矢量添加到矩阵中                                    ;;
;;; 输入：mat -- 矩阵(3x3)，disp -- 位移矢量                  ;;
;;; 输出：一个4X4的变换CAD的标准变换矩阵                      ;;
;;;-----------------------------------------------------------;;
(defun Mat:DispToMatrix (mat disp)
  (append
    (mapcar 'append mat (mapcar 'list disp))
    '((0. 0. 0. 1.))
  )
)
;;;-----------------------------------------------------------;;
;;; 向量或点的矩阵变换(向量乘矩阵)                            ;;
;;; Matrix x Vector - Vladimir Nesterovsky                    ;;
;;; Args: m - nxn matrix, v - vector in R^n                   ;;
;;;-----------------------------------------------------------;;
(defun MAT:mxv (m v)
  (mapcar (function (lambda (r) (apply '+ (mapcar '* r v)))) m)
)
;;;-----------------------------------------------------------;;
;;; 矩阵相乘                                                  ;;
;;; MAT:mxm Multiply two matrices -Vladimir Nesterovsky-      ;;
;;;-----------------------------------------------------------;;
(defun MAT:mxm (m q)
  (mapcar (function (lambda (r) (MAT:mxv (MAT:trp q) r))) m)
)
;;;-----------------------------------------------------------;;
;;; 点的矩阵(4x4 matrix) 变换                                 ;;
;;; 输入：矩阵m和一个三维点p                                  ;;
;;; 输出：点变换后的位置                                      ;;
;;;-----------------------------------------------------------;;
(defun MAT:mxp (m p)
  (reverse (cdr (reverse (MAT:mxv m (append p '(1.0))))))
)
;;;-----------------------------------------------------------;;
;;; 平齐实体的变换矩阵  -by highflybird                       ;;
;;; 输入：Ent - 实体名                                        ;;
;;; 输出：平齐这个实体的变换矩阵和它的逆矩阵                  ;;
;;;-----------------------------------------------------------;;
(defun Mat:EntityMatrix (Ent / z dxf Cen obj an m1 mat Inv org)
  (setq dxf (entget ent))
  (if (setq Cen (cdr (assoc 10 dxf)))                           ;Insertpoint,center or startpoint,etc.
    (if (null (caddr Cen))
      (setq Cen (append Cen '(0.0)))
    )
    (setq Cen '(0 0 0))
  )
  (setq obj (vlax-ename->vla-object Ent))                       
  (if (and (vlax-property-available-p obj 'elevation)           ;If it has elevation value.
           (wcmatch (vla-get-objectname obj) "*Polyline")       ;It's a "AcDb2dPolyline" or "AcDbPolyline" object
      )
    (setq z   (vla-get-elevation obj)
          Cen (list (car Cen) (cadr Cen) (+ (caddr Cen) z))     ;add elevation value
    )
  )
  (if (vlax-property-available-p obj 'rotation)                 ;if it has a rotaion angle
    (setq an (vla-get-rotation obj))
    (setq an 0)
  )
  (MAT:Align 0 Ent Cen an)                                      ;return two matrices, the first is WCS->OCS,the second is OCS->WCS
)
;;;-----------------------------------------------------------;;
;;; 从一个坐标系统到另一个坐标系统的变换矩阵                  ;;
;;; 输入：from - 源坐标系；to - 目的坐标系                    ;;
;;; 输出：一个4X4的CAD变换矩阵                                ;;
;;;-----------------------------------------------------------;;
(defun MAT:Trans (from to)
  (append
    (MAT:trp 
      (list
        (trans '(1 0 0) from to t)
        (trans '(0 1 0) from to t)
        (trans '(0 0 1) from to t)
        (trans '(0 0 0) from to nil)
      )
    )
    '((0. 0. 0. 1.))
  )
)
;;;-----------------------------------------------------------;;
;;; 矩阵转置                                                  ;;
;;; MAT:trp Transpose a matrix -Doug Wilson-                  ;;
;;; 输入：矩阵                                                ;;
;;; 输出：转置后的矩阵                                        ;;
;;;-----------------------------------------------------------;;
(defun MAT:trp (m)
  (apply 'mapcar (cons 'list m))
)
;;;-----------------------------------------------------------;;
;;; ucs到wcs矩阵，也可称UCS的变换矩阵                         ;;
;;;-----------------------------------------------------------;;
(defun MAT:u2w () (MAT:Trans 1 0))

;;;-----------------------------------------------------------;;
;;; wcs到ucs矩阵，也可称UCS的逆变换矩阵                       ;;
;;;-----------------------------------------------------------;;
(defun MAT:w2u () (MAT:Trans 0 1))