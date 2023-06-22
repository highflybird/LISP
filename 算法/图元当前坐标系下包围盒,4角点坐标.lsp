; ��ȡ�����Χ�С���С��Χ��-----(Ҳ����UCS)
; http://bbs.xdcad.net/forum.php?mod=viewthread&tid=670711&fromuid=132766

;;[����] ͼԪ��ǰ����ϵ�°�Χ��,4�ǵ�����
; 4 = ���ϣ�3 = ����
; 1 = ���£�2 = ����

;;Flag : Tʱ��������С��Χ�нǵ㣻nilʱ�����ذ�Χ�нǵ�
;;˵�� 1 ʹ��ǰ���������highflybird�ĳ���Matrix-Lib.LSP
;;     2 ���߱����������һ��Ȩ��������������ɿ����븴�ơ��޸ı��������ڷ���ҵĿ�� 
;;     3 �Թ������� 2013��10��8��
;;ʾ��(setq ll(HH:Ent4pt (setq ent (car (entsel))) T)),����UCS����ϵ������
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
  (vla-GetBoundingBox obj 'minPt 'maxPt)   ;�õ���Χ��
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
  (setq lst (mapcar '(lambda (x) (mat:mxp mat1 x)) lst)) ;wcs����
  (setq lst (mapcar '(lambda (x) (trans x ent 1)) lst))
)
  )
  lst
)
;;;-----------------------------------------------------------;;
;;; ͨ�ñ任���� by highflybird                               ;;
;;; ���룺from - ԭ����ϵ��                                   ;;
;;;       to   - Ŀ������ϵ��                                 ;;
;;;       Org  - Ŀ������ϵ��ԭ�����ԭ����ϵ��λ��           ;;
;;;       Ang  - �����ԭ����ϵ����ת�Ƕ�                     ;;
;;; �������������һ���Ǵ�ԭ����ϵ�任��Ŀ������ϵ�ı任����;;
;;;       һ���Ǵ�Ŀ������ϵ�任��ԭ����ϵ�ı任����          ;;
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
;;; ��λ��ʸ����ӵ�������                                    ;;
;;; ���룺mat -- ����(3x3)��disp -- λ��ʸ��                  ;;
;;; �����һ��4X4�ı任CAD�ı�׼�任����                      ;;
;;;-----------------------------------------------------------;;
(defun Mat:DispToMatrix (mat disp)
  (append
    (mapcar 'append mat (mapcar 'list disp))
    '((0. 0. 0. 1.))
  )
)
;;;-----------------------------------------------------------;;
;;; �������ľ���任(�����˾���)                            ;;
;;; Matrix x Vector - Vladimir Nesterovsky                    ;;
;;; Args: m - nxn matrix, v - vector in R^n                   ;;
;;;-----------------------------------------------------------;;
(defun MAT:mxv (m v)
  (mapcar (function (lambda (r) (apply '+ (mapcar '* r v)))) m)
)
;;;-----------------------------------------------------------;;
;;; �������                                                  ;;
;;; MAT:mxm Multiply two matrices -Vladimir Nesterovsky-      ;;
;;;-----------------------------------------------------------;;
(defun MAT:mxm (m q)
  (mapcar (function (lambda (r) (MAT:mxv (MAT:trp q) r))) m)
)
;;;-----------------------------------------------------------;;
;;; ��ľ���(4x4 matrix) �任                                 ;;
;;; ���룺����m��һ����ά��p                                  ;;
;;; �������任���λ��                                      ;;
;;;-----------------------------------------------------------;;
(defun MAT:mxp (m p)
  (reverse (cdr (reverse (MAT:mxv m (append p '(1.0))))))
)
;;;-----------------------------------------------------------;;
;;; ƽ��ʵ��ı任����  -by highflybird                       ;;
;;; ���룺Ent - ʵ����                                        ;;
;;; �����ƽ�����ʵ��ı任��������������                  ;;
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
;;; ��һ������ϵͳ����һ������ϵͳ�ı任����                  ;;
;;; ���룺from - Դ����ϵ��to - Ŀ������ϵ                    ;;
;;; �����һ��4X4��CAD�任����                                ;;
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
;;; ����ת��                                                  ;;
;;; MAT:trp Transpose a matrix -Doug Wilson-                  ;;
;;; ���룺����                                                ;;
;;; �����ת�ú�ľ���                                        ;;
;;;-----------------------------------------------------------;;
(defun MAT:trp (m)
  (apply 'mapcar (cons 'list m))
)
;;;-----------------------------------------------------------;;
;;; ucs��wcs����Ҳ�ɳ�UCS�ı任����                         ;;
;;;-----------------------------------------------------------;;
(defun MAT:u2w () (MAT:Trans 1 0))

;;;-----------------------------------------------------------;;
;;; wcs��ucs����Ҳ�ɳ�UCS����任����                       ;;
;;;-----------------------------------------------------------;;
(defun MAT:w2u () (MAT:Trans 0 1))