(in-package :%3b-swf)

(defvar *swf-version*)
(defparameter *array-print-verbose* nil)

(defun read-tag&size (source)
  (with-swf-readers (source)
    (align 8) ;; parser dsl, align to 8bits
    (let* ((x (ui16)) ;; parser dsl, read 2byte le
           (size (ldb (byte 6 0) x)) ;; should this be dsl too?
           (tag (ldb (byte 10 6) x)))
      (if (= size 63)
          (values tag (ui32))
          (values tag size)))))


(define-swf-type rect ()
  :auto ((nbits (ub 5 :align 8)
                :derived (min-bitfield-size-twips (xmin o) (xmax o)
                                                   (ymin o) (ymax o)))
         (xmin (sb-twips nbits))
         (xmax (sb-twips nbits))
         (ymin (sb-twips nbits))
         (ymax (sb-twips nbits)))
  ;;:align-after 8
  :this-var o
  :print-unreadably ("(~s,~s) (~s,~s)" (xmin o) (ymin o) (xmax o) (ymax o)))

 ;; should these store a single u32 instead?
(define-swf-type rgb ()
  :auto ((r (ui8) :initform 255) (g (ui8) :initform 0) (b (ui8) :initform 128)))


(define-swf-type rgba ()
  :auto ((r (ui8) :initform 255)
         (g (ui8) :initform 0)
         (b (ui8) :initform 128)
         (a (ui8) :initform 255)))

;; handle using extra/missing alpha channel in colors...
  ;; possibly should warn here, or make sure the proper type gets
  ;; assigned in the first place?
(defmethod write-swf-part swf-part ((type (eql 'rgba)) (o rgb) s)
  (write-swf-part 'rgba (make-instance 'rgba
                                       'r (r o) 'g (g o) 'b (b o) 'a 255) s))
(defmethod write-swf-part swf-part ((type (eql 'rgb)) (o rgba) s)
  (write-swf-part 'rgb (make-instance 'rgb 'r (r o) 'g (g o) 'b (b o)) s))
;; fixme: implement these directly
(defmethod %swf-part-size swf-part ((type (eql 'rgba)) (o rgb) &key &allow-other-keys)
  (%swf-part-size 'rgba (make-instance 'rgba
                                      'r (r o) 'g (g o) 'b (b o) 'a 255)))
(defmethod %swf-part-size swf-part ((type (eql 'rgb)) (o rgba) &key &allow-other-keys)
  (%swf-part-size 'rgb (make-instance 'rgb 'r (r o) 'g (g o) 'b (b o))))


(define-swf-type argb ()
  :auto ((a (ui8)) (r (ui8)) (g (ui8)) (b (ui8))))

;;; matrix

(define-swf-type matrix-part-fixed ()
  :auto ((nbits (ub 5) :derived (min-bitfield-size-fixed16
                                 (value1 o) (value2 o)))
         (value1 (fb nbits))
         (value2 (fb nbits)))
  :this-var o
  :print-unreadably ("(~s,~s)" (value1 o) (value2 o)))
(define-swf-type matrix-part-translate ()
  :auto ((nbits (ub 5) :derived (min-bitfield-size-twips
                                 (value1 o) (value2 o)))
         (value1 (sb-twips nbits))
         (value2 (sb-twips nbits)))
  :this-var o
  :print-unreadably ("(~s,~s)" (value1 o) (value2 o)))

(define-swf-type matrix ()
  :auto
  ((has-scale (bit-flag :align 8) :derived (not (null (scale o))))
   (scale (swf-type 'matrix-part-fixed) :optional has-scale)
   (has-rotate (bit-flag) :derived (not (null (rotate-skew o))))
   (rotate-skew (swf-type 'matrix-part-fixed) :optional has-rotate)
   (translate (swf-type 'matrix-part-translate)
              :initform (make-instance 'matrix-part-translate
                                       'value1 0 'value2 0)))
  :this-var o)

;;; color transform

(defvar *cxform-bits*)
(define-swf-type cxform-part-rgb ()
  :auto
  ((r (fb8 *cxform-bits*))
   (g (fb8 *cxform-bits*))
   (b (fb8 *cxform-bits*))))

(define-swf-type cxform-part-rgba ()
  :auto
  ((r (fb8 *cxform-bits*))
   (g (fb8 *cxform-bits*))
   (b (fb8 *cxform-bits*))
   (a (fb8 *cxform-bits*))))

(define-swf-type cxform ()
  :auto
  ((has-add (bit-flag :align 8) :derived (not (null (add o))))
   (has-mult (bit-flag) :derived (not (null (mult o))))
   (nbits (ub 4) :derived (max
                           (if (has-add o)
                               (min-bitfield-size-fixed8
                                        (r (add o)) (g (add o)) (b (add o)))
                               0)
                            (if (has-mult o)
                                (min-bitfield-size-fixed8
                                 (r (mult o)) (g (mult o)) (b (mult o)))
                                0)))
   (*cxform-bits* nbits :local t)
   (add (swf-type 'cxform-part-rgb) :initform nil :optional has-add)
   (mult (swf-type 'cxform-part-rgb) :initform nil :optional has-mult))
  :this-var o)

(define-swf-type cxform-with-alpha ()
  :auto
  ((has-add (bit-flag :align 8) :derived (not (null (add o))))
   (has-mult (bit-flag) :derived (not (null (mult o))))
   (nbits (ub 4)
          :derived  (max
                     (if (has-add o)
                         (min-bitfield-size-fixed8
                          (r (add o)) (g (add o)) (b (add o)) (a (add o)))
                         0)
                     (if (has-mult o)
                         (min-bitfield-size-fixed8
                          (r (mult o)) (g (mult o)) (b (mult o)) (a (mult o)))
                         0))
)
   (*cxform-bits* nbits :local t)
   (add (swf-type 'cxform-part-rgba) :initform nil :optional has-add)
   (mult (swf-type 'cxform-part-rgba) :initform nil :optional has-mult))
  :this-var o)




;;;~~~ todo : lists, counted lists, end-of-tag delimited lists
;; not sure if lists should be slot option, or a special reader like
;; swf-type?
;; - probably special reader, since those go into a simpler macro if
;;   nothing else
;;   - should it only work for swf-type? or pass a lamba/body to read
;;     an individual entry?
;;     = going with general case (pass reader body) for now...
;; also need to add methods on CONS or VECTOR or whatever to handle sizing
;;    and writing lists


;;;;; todo : raw bytes?

;;;;; todo : bitmap types (convert to/from multiple-of-4 padding)




;;; action-records moved to swf-action-records.lisp

(defvar *shape-tag-version*)
(define-swf-type fill-style-array ()
  :this-var o
  :value-var value
  :auto
  ((fill-style-count () :derived (length (fill-styles o)))
   (fill-styles (counted-list (swf-type 'fill-style) fill-style-count)))
;  :align-after 8
  :reader ((fill-style-count
            (let ((short (ui8)))
              (if (and (<= 2 *shape-tag-version* 3) (= short #xff))
                  (ui16)
                  short))))
  ;; fixme: should probably error if version == 1 and count is too large
  :sizer ((fill-style-count
           (progn
             (ui8)
             (when (and (<= 2 *shape-tag-version* 3) (>= value #xff))
               (ui16)))))
  :writer ((fill-style-count
            (if (and (<= 2 *shape-tag-version* 3) (>= value #xff))
                (progn
                  (let ((value #xff))
                    (ui8))
                  (ui16))
                (ui8))))
  :print-unreadably ("~{~s~#[~:; ~:_~]~}" (if *array-print-verbose* (fill-styles o) (list 'length (length (fill-styles o)))) ))

(defmethod fill-style-count ((a null))
  0)

;; fixme: we should probably handle this in some way that allows validation
;; on size/write, either splitting the containing classes into versions
;; for the various shape tags, or making a special color class that
;; checks container version...
(defun maybe-rgba ()
  (assert (<= 1 *shape-tag-version* 4))
  ;; not sure how ver 4 should be handled, fill-style doesn't say...
  (if (>= *shape-tag-version* 3) 'rgba 'rgb))

(define-swf-type fill-style ()
  :this-var o
  :auto ((fill-style-type (ui8) :derived (subclass-id o 'fill-style)))
  :subclass (subclass-from-id 'fill-style fill-style-type))

(define-swf-type fill-style-solid (fill-style)
  :id #x00
  :auto ((color (swf-type (maybe-rgba)))))

(define-swf-type fill-linear-gradient (fill-style)
  :id #x10
  :auto ((gradient-matrix (swf-type 'matrix))
         (gradient (swf-type 'gradient))))

(define-swf-type fill-radial-gradient (fill-style)
  :id #x12
  :auto ((gradient-matrix (swf-type 'matrix))
         (gradient (swf-type 'gradient))))

(define-swf-type fill-focal-gradient (fill-style)
  :swf-min-version 8
  :id #x13
  :auto ((gradient-matrix (swf-type 'matrix))
         (gradient (swf-type 'focal-gradient))))

(define-swf-type fill-repeating-bitmap-fill (fill-style)
  :id #x40
  :auto ((bitmap-id (swf-type 'character-id))
         (bitmap-matrix (swf-type 'matrix))))

(define-swf-type fill-clipped-bitmap-fill (fill-style)
  :id #x41
  :auto ((bitmap-id (swf-type 'character-id))
         (bitmap-matrix (swf-type 'matrix))))

(define-swf-type fill-non-smoothed-repeating-bitmap-fill (fill-style)
  :id #x42
  :auto ((bitmap-id (swf-type 'character-id))
         (bitmap-matrix (swf-type 'matrix))))

(define-swf-type fill-non-smoothed-clipped-bitmap-fill (fill-style)
  :id #x43
  :auto ((bitmap-id (swf-type 'character-id))
         (bitmap-matrix (swf-type 'matrix))))



(define-swf-type line-style-array ()
  :this-var o
  :value-var value
  :auto
  ((line-style-count () :derived (length (line-styles o)))
   (line-styles (counted-list (swf-type (if (= *shape-tag-version* 4)
                                            'line-style-2
                                            'line-style))
                              line-style-count)))
;;  :align-after 8
  :reader ((line-style-count
            (let ((short (ui8)))
              (if (= short #xff)
                  (ui16)
                  short))))
  :sizer ((line-style-count
           (progn
             (ui8)
             (when (>= value #xff)
               (ui16)))))
  :writer ((line-style-count
            (if (>= value #xff)
                (progn
                  (let ((value #xff))
                    (ui8))
                  (ui16))
                (ui8))))
  :print-unreadably ("~{~s~#[~:; ~:_~]~}" (if *array-print-verbose* (line-styles o) (list 'length (length (line-styles o)))) ))

(defmethod line-style-count ((a null))
  0)

(define-swf-type line-style ()
  :auto
  ((width (twips-u16))
   (color (swf-type (maybe-rgba)))))

(define-swf-type line-style-2 ()
  :this-var o
  :auto
  ((width (twips-u16))
   (start-cap-style (ub 2))
   (join-style (ub 2))
   ;; fixme: make sure we don't have color and fill set at once
   (has-fill (bit-flag) :derived (not (null (fill-type o))))
   (no-h-scale (bit-flag) :initform nil)
   (no-v-scale (bit-flag) :initform nil)
   (pixel-hinting-flag (bit-flag))
   (reserved (ub 5) :initform 0)
   (no-close (bit-flag))
   (end-cap-style (ub 2))
   ;; fixme: decide good default for miter-limit-factor?
   (miter-limit-factor (fixed8) :initform 10.0 :optional (= 2 join-style))
   (color (swf-type 'rgba) :optional (not has-fill))
   (fill-type (swf-type 'fill-style) :optional has-fill)))


(define-swf-type gradient ()
  :this-var o
  :auto
  ((spread-mode (ub 2 :align 8)) ;; :align 8
   (interpolation-mode (ub 2))
   (num-gradients (ub 4) :derived (length (gradient-records o)))
   (gradient-records (counted-list (swf-type 'grad-record) num-gradients))))

;; fixme: this should be a subclass of gradient
(define-swf-type focal-gradient ()
  :this-var o
  :auto
  ((spread-mode (ub 2 :align 8))
   (interpolation-mode (ub 2))
   (num-gradients (ub 4) :derived (length (gradient-records o)))
   (gradient-records (counted-list (swf-type 'grad-record) num-gradients))
   (focal-point (fixed8))))


(define-swf-type grad-record ()
  :auto
  ((gradient-ratio (ui8))
   (color (swf-type (maybe-rgba)))))




;; fixme: multiple struct versions instead of special?
(defvar *define-button-ver*)
;; fixme: need to pull out the first byte, since 0 is used as end marker
(define-swf-type button-record ()
  :auto
  ((reserved (ub 2 :align 8))
   (has-blend-mode (bit-flag))
   (has-filter-list (bit-flag))
   (state-hit-test (bit-flag))
   (state-down (bit-flag))
   (state-over (bit-flag))
   (state-up (bit-flag))
   (character-id (swf-type 'character-id))
   (place-depth (ui16))
   (place-matrix (swf-type 'matrix))
   (color-transform (swf-type 'cxform-with-alpha) :optional (= 2 *define-button-ver*))
   (filter-list (swf-type 'filter-list) :optional (and (= 2 *define-button-ver*)
                                                       has-filter-list))
   (blend-mode (ui8) :optional (and (= 2 *define-button-ver*)
                                    has-blend-mode))))

(define-swf-type button-cond-action ()
  :auto
  ((cond-action-size (ui16))
   (cond-idle-to-over-down (bit-flag))
   (cond-out-down-to-idle (bit-flag))
   (cond-out-down-to-over-down (bit-flag))
   (cond-over-down-to-out-down (bit-flag))
   (cond-over-down-to-over-up (bit-flag))
   (cond-over-up-to-over-down (bit-flag))
   (cond-over-up-to-idle (bit-flag))
   (cond-idle-to-over-up (bit-flag))
   (cond-key-press (ub 7))
   (cond-over-down-to-idle (bit-flag))
   ;; fixme: use the read to 0 mode, and leave explicit 0 slot
   (actions (list-until-type (swf-type 'action-record) 'action-record-end))
   #+nil (action-end-flag (ui8) :derived 0)
   #+nil :reader
   #+nil ((action-end-flag 0))
   ))

;;;; todo video