(in-package :%3b-swf)

;;; readers for low level .swf types

;;; fixme: rearrange the read/write protocol a bit to minimize generic dispatch?
;;; (ex. read multiple bytes at a time for u32 instead of 4 calls to read-octet)
;;
;;; alterntely, minimize the protocal at the possible expense of speed?
;;; (ex. call read-octets from read-bits)


;; if set, car=current octet, cdr=# bits left (< 8)
(defparameter *partial-octet-read* nil)
(defparameter *reader-end-of-block* nil)

(defmethod read-octet ((stream stream))
    (setf *partial-octet-read* nil)
    (read-byte stream))
(defmethod read-octet-vector (count (stream stream))
  (let ((a (make-array count :element-type '(unsigned-byte 8))))
    (read-sequence a stream)
    a))
(defmethod read-octets-into-sequence (count sequence (stream stream))
  (error "not done"))

(defun check-end (stream &optional (bytes 0))
  ;; should check this more places, here just for a sanity check...
  (assert (if *reader-end-of-block*
            (<= (+ bytes (file-position stream)) (car *reader-end-of-block*))
            t) ()
          "went past end of block? pos=~s end=~s leftover=~s"
          (file-position stream) *reader-end-of-block* *partial-octet-read*))
(defmethod read-align-source (granularity-bits (stream stream))
  (unless (= granularity-bits 8)
    (error "can't align stream to ~s bits yet" granularity-bits))
  (check-end stream)
  ;; for debugging... complain if we discard non-zero bits
  (when (and *partial-octet-read*
             (not (zerop (mod (cdr *partial-octet-read*) 8)))
             (not (zerop (ldb (byte (cdr *partial-octet-read*) 0)
                              (car *partial-octet-read*)))))
    (format t "--- discarding bits ~b (~x . ~s)~%"
            (ldb (byte (cdr *partial-octet-read*) 0)
                 (car *partial-octet-read*))
            (car *partial-octet-read*)
            (cdr *partial-octet-read*)))
  (unless (and *partial-octet-read*
               (zerop (mod (cdr *partial-octet-read*) 8)))
    (setf *partial-octet-read* nil)))

(defmethod read-bits (bits (stream stream))
  (let ((value 0)
        (orig-count bits))
    (if *partial-octet-read*
        (if (>= (cdr *partial-octet-read*) bits)
            (progn ;; we have enough bits read already, use them
              (decf (cdr *partial-octet-read*) bits)
              (return-from read-bits
                (ldb (byte bits (cdr *partial-octet-read*))
                     (car *partial-octet-read*))))
            ;; use already read bits first
            (progn
              (decf bits (cdr *partial-octet-read*))
              (setf (ldb (byte (cdr *partial-octet-read*) bits) value)
                    (car *partial-octet-read*)))))
    (setf *partial-octet-read* nil)
    (loop for octet = (read-byte stream)
          if (> bits 8)
          do (decf bits 8)
          (setf (ldb (byte 8 bits) value) octet)
          else
          do
          (setf (ldb (byte bits 0) value) (ldb (byte bits (- 8 bits)) octet))
          (setf *partial-octet-read* (cons octet (- 8 bits)))
          (setf bits 0)
          until (zerop bits))
    value))



(declaim (inline u->s u8->s8 u16->s16 u32->s32 u16->fixed8 u32->fixed))
(defun u->s (bits value) ;; convert unsigned bits to signed
  (if (zerop bits)
      value
      (if (logbitp (1- bits) value)
          (dpb value (byte (1- bits) 0) -1)
          value)))
(defun u8->s8 (value) (u->s 8 value))
(defun u16->s16 (value) (u->s 16 value))
(defun u32->s32 (value) (u->s 32 value))
(defun u16->fixed8 (value) (/ (u->s 16 value) (expt 2.0 8))) ;;s8.8 -> float
(defun u32->fixed (value) (/ (u->s 32 value) (expt 2.0 16))) ;;s16.16 -> float
(defun u16->twips (value) (/ value 20.0)) ;;twips -> float
(defun s16->twips (value) (/ (u->s 16 value) 20.0)) ;;twips -> float

(declaim (inline read-ub read-sb))
(defun read-ub (count source) ;; read unaligned bits
  (read-bits count source))
(defun read-sb (count source) ;; read unaligned bits into signed int
  (u->s count (read-bits count source)))

(ieee-floats:make-float-converters encode-float16 decode-float16 5 10 t)
;; we redefine these instead of using defaults so we can support nan/inf
(ieee-floats:make-float-converters encode-float32 decode-float32 8 23 t)
(ieee-floats:make-float-converters encode-float64 decode-float64 11 52 t)

(defmacro make-byte-readers (endian &body specs)
  `(progn
     ,@(loop for spec in specs
          append
          (destructuring-bind (name bytes
                                    &key (align 1) ;; alignment in bytes
                                    convert)
              spec
            `((declaim (inline ,name))
              (defun ,name (source)
                ,@(when align `((read-align-source ,(* align 8) source)))
                (,(if convert convert 'values)
                  ;; fixme: using read-octet here is wrong when align #=8
                  ;; (and the align above is redundant when it is 8)
                  ,@(if (eq endian :little)
                        `((loop with value = 0
                                for low-bit upto ,(* 8 (1- bytes)) by 8
                                do (setf (ldb (byte 8 low-bit) value)
                                         (read-octet source))
                                finally (return value)))
                        `((loop with value = 0
                                for low-bit from ,(* 8 (1- bytes)) downto 0 by 8
                                do (setf (ldb (byte 8 low-bit) value)
                                         (read-octet source))
                                finally (return value)))))))))))

(make-byte-readers :little
  (read-ui8 1)
  (read-ui16 2)
  (read-ui32 4)
  (read-ui64 8)
  (read-si8 1 :convert u8->s8)
  (read-si16 2 :convert u16->s16)
  (read-si32 4 :convert u32->s32)
  (read-fixed8 2 :convert u16->fixed8)
  (read-fixed 4 :convert u32->fixed)
  (read-float16 2 :convert decode-float16)
  (read-float32 4 :convert decode-float32)
  (read-float64 8 :convert decode-float64)
  (read-twips-u16 2 :convert u16->twips)
  (read-twips-s16 2 :convert s16->twips))

(defun read-encodedu32 (source)
  (loop with value = 0
        for low-bit below 32 by 7
        for octet = (read-ui8 source)
        do (setf (ldb (byte 7 low-bit) value) octet)
        while (logbitp 7 octet)
        finally (return value)))

(declaim (inline read-bit-flag))
(defun read-bit-flag (source)
  (not (zerop (read-ub 1 source))))

(defmacro with-swf-readers ((source)
                               &body body)
  ;; todo: allow setting byte order for bitfields
  `(macrolet ((align (bits)
                `(read-align-source ,bits ,',source))
              (ub (bits &key align)
                `(progn
                   (check-end ,',source)
                   ,@(when align `((align ,align)))
                   (read-ub ,bits ,',source)))
              (sb (bits)
                `(read-sb ,bits ,',source))
              (sb-twips (bits)
                `(/ (read-sb ,bits ,',source) 20.0))
              (fb (bits)
                `(/ (read-sb ,bits ,',source) ,(expt 2.0 16)))
              (fb8 (bits)
                `(/ (read-sb ,bits ,',source) ,(expt 2.0 8)))
              ,@(loop for a in '(ui8 ui16 ui32 ui64 si8 si16 si32
                                 fixed8 fixed float16 float32 float64
                                 twips-u16 twips-s16)
                      for b in '(read-ui8 read-ui16 read-ui32 read-ui64
                                 read-si8 read-si16 read-si32
                                 read-fixed8 read-fixed
                                 read-float16 read-float32 read-float64
                                 read-twips-u16 read-twips-s16)
                      collect `(,a () `(,',b ,',source)))
              (encodedu32 ()
                `(read-encodedu32 ,',source))
              (bit-flag (&key align)
                `(progn
                   ,@(when align `((align ,align)))
                   (read-bit-flag ,',source)))
              (swf-type (type)
                `(read-swf-part ,type ,',source))

              (sized-list (reader size)
                (alexandria:with-gensyms (end)
                 `(loop with ,end = (+ ,size (file-position ,',source))
                        while (< (file-position ,',source) ,end)
                        collect ,reader)))
              (list-until (reader test)
                (alexandria:with-gensyms (value)
                  ;; todo: this should probably be limited to current tag
                  ;; to better handle buggy code/bad data
                 `(loop with ,value = nil
                        do (setf ,value ,reader)
                        collect ,value
                        until (funcall ,test ,value))))
              (list-until-type (reader end-type)
                (alexandria:with-gensyms (value)
                  ;; todo: this should probably be limited to current tag
                  ;; to better handle buggy code/bad data
                 `(loop with ,value = nil
                        do (setf ,value ,reader)
                        collect ,value
                        ;; should this quote 'end-type?
                        until (typep ,value ,end-type))))
              (counted-list (reader count &key align-elements)
                `(when ,count
                   (loop repeat ,count
                     collect ,reader
                     ,@(when align-elements
                             '(do (align 8))))))

              (enumerated-list (&rest types)
                `(list
                   ,@types))

              (string-sz-utf8 (&optional max-length)
                (alexandria:once-only (max-length)
                  `(progn
                     (align 8)
                     (loop with octets = (make-array 128
                                                     :element-type '(unsigned-byte 8)
                                                     :adjustable t
                                                     :fill-pointer 0)
                           for octet = (read-byte ,',source)
                           until (or (zerop octet)
                                     (and ,max-length
                                          (>= (fill-pointer octets) ,max-length)))
                           do (vector-push-extend octet octets
                                                  (* 2 (array-dimension octets 0)))
                           finally (return (babel:octets-to-string
                                            octets :encoding :utf-8
                                            :errorp nil))))))
              (zlib-data ()
                `(progn
                   (align 8)
                   ;;; fixme: don't use this, chipz reads too much...
                   (chipz:decompress 'nil 'chipz:zlib ,',source)))
              (with-reader-block (size &body body)
                ;;fixme: finish this, idea is that we track the end of block
                ;; position, and all the readers can check it if available
                ;; and complain if they try to read past the end
                ;;  (or use it to read to the end, which is immediate reason
                ;;   for adding it)
                `(let ((*reader-end-of-block*
                        (cons (+ (file-position ,',source) ,size)
                              *reader-end-of-block*)))
                  ,@body))
              (rest-of-tag ()
                `(let ((seq (make-array (- (car *reader-end-of-block*)
                                           (file-position ,',source))
                                        :element-type '(unsigned-byte 8))))
                   (read-sequence seq ,',source)
                   seq))
              (next-octet-zero-p ()
                `(progn
                   (read-align-source 8 ,',source)
                   (let ((a (ui8)))
                     (setf *partial-octet-read* (cons a 8))
                     (zerop a))))
              (next-bits-zero-p (bits)
                `(let ((a (ub ,bits)))
                   (if *partial-octet-read*
                       (progn
                         (setf (car *partial-octet-read*)
                               (dpb a
                                    (byte ,bits (cdr *partial-octet-read*))
                                    (car *partial-octet-read*)))
                         (incf (cdr *partial-octet-read*) ,bits))
                       (setf *partial-octet-read* (cons a ,bits)))
                   (zerop a)))

              (bytes-left-in-tag ()
                `(- (car *reader-end-of-block*) (file-position ,',source)))

)

     ;; would it be evil to define symbol macros for all the readers here?
     ,@body))
