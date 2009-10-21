(in-package :%3b-swf)

;;; low level .swf types

(defvar *swf-sizer-bitpos*)

(defun size-encodedu32 (u32)
  ;; todo: verify this is the correct algo (copied from abc code)
  ;; fixme: calculate directly from integer-length instead of using loop
  (let ((s (loop
              for i = u32 then i2
              for i2 = (ash i -7)
              for b = (ldb (byte 7 0) i)
              for done = (or (= i2 0) (= i2 -1))
              when (or (not (eql (logbitp 6 i2) (logbitp 6 i))) (not done))
              do (setf b (logior #x80 b))
              sum 8
              when (and done (logbitp 7 b))
              sum 8
              until done)))
    (incf *swf-sizer-bitpos* s)
    s))

;;; macros for calculating size of an object
(defmacro with-swf-sizers ((value-arg) &body body)
  ;; sizer accessors just update the bit pos, which is intended to be
  ;; bound by a method specialized on t
  `(macrolet ((align (bits)
                `(unless (zerop *swf-sizer-bitpos*)
                   (setf *swf-sizer-bitpos*
                         (* ,bits (1+ (truncate (1- *swf-sizer-bitpos*) ,bits)))))))
     (symbol-macrolet ((%primitive-type-macro-mode% :size)
                       (%value-arg% ,value-arg))
       ,@body)))


;;; attempt at optimizing it a bit...
;;; doesn't really work though, since fixnum is small enough on some lisps
;;; that we could overflow it here, and bignums probably aren't much faster
;;; than generic ops
;;; (declaring (integer 0) does quiet the compiler a bit on sbcl, so might
;;;  be worth doing anyway if we need speed)
;;;  --- less worthwhile on readers/writers currently thouh, since we
;;;      dispatch to a generic to actually read the bytes...
;; (defmacro with-swf-sizers (() &body body)
;;  ;; sizer accessors just update the bit pos, which is intended to be
;;  ;; bound by a method specialized on t
;;  `(locally (declare (fixnum *swf-sizer-bitpos*))
;;     (labels ((align (bits)
;;               (declare (fixnum bits))
;;               (* bits (1+ (truncate (1- *swf-sizer-bitpos*) bits))))
;;             (ub (bits)
;;               (declare (fixnum bits))
;;               (incf *swf-sizer-bitpos* bits))
;;             (sb (bits)
;;               (declare (fixnum bits))
;;               (incf *swf-sizer-bitpos* bits))
;;             ,@(loop for (name bytes) in '((ui8 1)
;;                                           (ui16 2)
;;                                           (ui32 4)
;;                                           (ui64 8)
;;                                           (si8 1)
;;                                           (si16 2)
;;                                           (si32 4)
;;                                           (fixed8 2)
;;                                           (fixed 4)
;;                                           (float16 2)
;;                                           (float32 4)
;;                                           (float64 8))
;;                     collect `(,name ()
;;                                     (align 8)
;;                                     (incf *swf-sizer-bitpos* ,(* 8 bytes))))
;;             (encodedu32 ()
;;               (error "encoded32 sizer not done yet..."))
;;             (bit-flag ()
;;               (incf *swf-sizer-bitpos*)))
;;      (declare (inline align ub sb ui8 ui16 ui32 ui64 si8 si16 si32
;;                       fixed8 fixed float16 float32 float64
;;                       bit-flag)
;;               (ignorable #'align #'ub #'sb
;;                          #'ui8 #'ui16 #'ui32 #'ui64 #'si8 #'si16 #'si32
;;                          #'fixed8 #'fixed #'float16 #'float32 #'float64
;;                          #'bit-flag #'encodedu32))
;;      ;; would it be evil to define symbol macros for all the readers here?
;;      ,@body)))



;; we allow nil in these so we don't need to be as careful about optional
;; values
(defun min-bitfield-size-signed (&rest values)
  "calculate min bitfield size to store all of a set of signed values"
  ;; values are sign extended, so add 1 to allow space for sign bit
  (1+ (reduce 'max values :key (lambda (x)
                                 (if x (integer-length (floor x)) 0)))))

(defun min-bitfield-size-twips (&rest values)
  "calculate min bitfield size to store all of a set of signed values"
  ;; values are sign extended, so add 1 to allow space for sign bit
  (1+ (reduce 'max values :key (lambda (x)
                                 (if x (integer-length (floor (* 20 x))) 0)))))


(defun min-bitfield-size-fixed16 (&rest values)
  "calculate min bitfield size to store all of a set of float values
 when stored as fixed16"
  ;; values are sign extended, so add 1 to allow space for sign bit
  (1+ (reduce 'max values
              ;; fixme: is floor correct here?
              :key (lambda (x)
                     (if x (integer-length (floor (* x (expt 2 16))))
                         0)))))
(defun min-bitfield-size-fixed8 (&rest values)
  "calculate min bitfield size to store all of a set of float values
 when stored as fixed8"
  ;; values are sign extended, so add 1 to allow space for sign bit
  (1+ (reduce 'max values
              ;; fixme: is floor correct here?
              :key (lambda (x)
                     (if x (integer-length (floor (* x (expt 2 8))))
                         0)))))


(defun min-bitfield-size-unsigned (&rest values)
  "calculate min bitfield size to store all of a set of unsigned values"
  (reduce 'max values :key (lambda (x)
                             (if x (integer-length (floor x)) 0))))





