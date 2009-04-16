(in-package :3b-swf)

;;; low level .swf types

;;; macros for calculating size of an object
(defvar *swf-sizer-bitpos*)
(defmacro with-swf-sizers ((value-arg) &body body)
  ;; sizer accessors just update the bit pos, which is intended to be
  ;; bound by a method specialized on t
  `(macrolet ((align (bits)
                `(unless (zerop *swf-sizer-bitpos*)
                   (setf *swf-sizer-bitpos*
                         (* ,bits (1+ (truncate (1- *swf-sizer-bitpos*) ,bits)))))))
     (macrolet ((ub (bits &key align)
                  `(progn
                     ,@(when align `((align ,align)))
                     (incf *swf-sizer-bitpos* ,bits)))
                (sb (bits)
                  `(incf *swf-sizer-bitpos* ,bits))
                (sb-twips (bits)
                  `(incf *swf-sizer-bitpos* ,bits))
                (fb (bits)
                  `(incf *swf-sizer-bitpos* ,bits))
                ,@(loop for (name bytes) in '((ui8 1)
                                              (ui16 2)
                                              (ui32 4)
                                              (ui64 8)
                                              (si8 1)
                                              (si16 2)
                                              (si32 4)
                                              (fixed8 2)
                                              (fixed 4)
                                              (float16 2)
                                              (float32 4)
                                              (float64 8)
                                              (twips-u16 2)
                                              (twips-s16 2))
                        collect `(,name ()
                                        `(progn
                                           (when ,',value-arg
                                               (align 8)
                                               (incf *swf-sizer-bitpos* ,',(* 8 bytes))))))
                (encodedu32 ()
                  `(error "encoded32 sizer not done yet..."))
                (bit-flag (&key align)
                  `(progn
                     ,@(when align `((align ,align)))
                     (incf *swf-sizer-bitpos*)))
                (swf-type (type)
                  (declare (ignore type))
                  `(%swf-part-size ,',value-arg))

                (sized-list (type size)
                  (declare (ignore size))
                  ;; fixme: verify we don't go over size...
                  (alexandria:with-gensyms (i)
                    `(loop for ,i in ,',value-arg
                           do (let ((,',value-arg ,i))
                                (declare (ignorable ,',value-arg))
                                ,type))))
                (counted-list (type count)
                  (alexandria:with-gensyms (i)
                    `(progn
                       (assert (= (length ,',value-arg) ,count))
                       (loop for ,i in ,',value-arg
                             do (let ((,',value-arg ,i))
                                  (declare (ignorable ,',value-arg))
                                  ,type)))))
                (list-until (type test)
                  (declare (ignore test))
                  ;; fixme: probably should verify that test hold for last element an no others
                  (alexandria:with-gensyms (i)
                    `(loop for ,i in ,',value-arg
                           do (let ((,',value-arg ,i))
                                ,type))))
                (list-until-type (type end-type)
                  (declare (ignore end-type))
                  ;; fixme: probably should verify types
                  (alexandria:with-gensyms (i)
                    `(loop for ,i in ,',value-arg
                           do (let ((,',value-arg ,i))
                                ,type))))

              (enumerated-list (&rest types)
                `(progn
                   ,@(loop for i in types
                           collect `(let ((,',value-arg (pop ,',value-arg)))
                                      ,i))))

                (string-sz-utf8 (&optional max-length)
                  ;; fixme: probably need to ignore max-length here
                  ;; since max length might be set to something that
                  ;; tries to call this to determine actual length...
                  (declare (ignore max-length))
                  `(progn
                     (align 8)
                     ;; add 1 to account for 0 term
                     (let ((len (1+ (babel:string-size-in-octets
                                     ,',value-arg :encoding :utf-8))))
                       #+(or) (when ,max-length
                                (setf len (min len ,max-length)))
                       (incf *swf-sizer-bitpos*
                             (* 8 len)))))

                ;; fixme: this is ugly :/
                (zlib-data ()
                  `(progn
                     (align 8)
                     (incd *swf-sizer-bitpos*
                           (* 8 (length (salza2:compress-data ,',value-arg 'salza2:zlib-compressor))))))
                (rest-of-tag ()
                  `(incf *swf-sizer-bitpos*
                         (* 8 (length ,',value-arg))))
                (bytes-left-in-tag ()
                  `(progn (format t "called bytes-left-in-tag during size?")
                          1))
              (next-octet-zero-p ()
                `(progn (format t "called next-octet-zero-p during size?")
                        t))
              (next-bits-zero-p (bits)
                (declare (ignore bits))
                `(progn (format t "called next-octet-zero-p during size?")
                        t))
                )

       ;; would it be evil to define symbol macros for all the readers here?
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


(defun min-bitfield-size-unsigned (&rest values)
  "calculate min bitfield size to store all of a set of unsigned values"
  (reduce 'max values :key (lambda (x)
                             (if x (integer-length (floor x)) 0))))





