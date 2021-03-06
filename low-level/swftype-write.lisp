(in-package :%3b-swf)

;;; writers for low level .swf types

;; if set, car=bits to write, cdr=# of bits to write
(defparameter *partial-octet-write* nil)


(defun finish-partial-write (stream)
  (when (and *partial-octet-write* (not (zerop (cdr *partial-octet-write*))))
    #+nil(format t "flushed partial byte ~s = ~x (~s)~%"
            (ash (car *partial-octet-write*)
                 (- 8 (cdr *partial-octet-write*)))
            (ash (car *partial-octet-write*)
                 (- 8 (cdr *partial-octet-write*)))
            *partial-octet-write*)
    (write-byte (ash (car *partial-octet-write*)
                          (- 8 (cdr *partial-octet-write*)))
                     stream))
  (setf *partial-octet-write* nil))

(defmethod write-octet (datum (stream stream))
  (finish-partial-write stream)
  (write-byte datum stream))

(defmethod write-octet-vector (data (stream stream))
  (finish-partial-write stream)
  (write-sequence data stream))

(defmethod write-align-source (granularity-bits (stream stream))
  (unless (= granularity-bits 8)
    (error "can't align stream to ~s bits yet" granularity-bits))
  (finish-partial-write stream))

;; not sure if value arg should go at beginning or end of arglist
;; (end is easiest to add during macroexpansion, but first
;;  looks more like write-byte... going with first for now)
(defmethod write-bits (data count (stream stream))
  (setf data (ldb (byte count 0) data))
  (when *partial-octet-write*
    ;; we have partial data, combine with current write
    (setf (ldb (byte (cdr *partial-octet-write*) count)
               data)
          (car *partial-octet-write*))
    (incf count (cdr *partial-octet-write*)))

  ;; fixme: probably should reuse existing cons if possible
  (setf *partial-octet-write* nil)
  (loop for bits-left = count then (- bits-left 8)
        for low-bit = (- bits-left 8)
        for octet = (when (>= low-bit 0) (ldb (byte 8 low-bit) data))
        while (>= bits-left 8)
        do (write-octet octet stream)
        finally (when (> bits-left 0)
                  (setf *partial-octet-write*
                        (cons (ldb (byte bits-left 0) data)
                              bits-left))))
  (values))


;; we don't need to convert signed to unsigned before writing, since
;; ldb takes care of it...
(declaim (inline fixed8->u16 fixed->u32))
(defun fixed8->u16 (value) (floor (* value (expt 2 8)))) ;; float -> s8.8
(defun fixed->u32 (value) (floor (* value (expt 2 16)))) ;; float -> s16.16
(defun fixed16->u32 (value) (floor (* value (expt 2 16)))) ;; float -> s16.16
(defun twips->u16 (value) (floor (* value 20))) ;; float -> twips

(declaim (inline write-ub write-sb))
(defun write-ub (data count source) ;; write unaligned bits
  (write-bits data count source))
(defun write-sb (data count source) ;; write signed int to unaligned bits
  (write-bits data count source))

(defmacro make-byte-writers (endian &body specs)
  `(progn
     ,@(loop for spec in specs
             append
             (destructuring-bind (name bytes
                                       &key (align 1) ;; alignment in bytes
                                       convert)
                 spec
               `((declaim (inline ,name))
                 (defun ,name (data source)
                   ,@(when align `((write-align-source ,(* align 8) source)))
                   ,@(when convert `((setf data (,convert data))))
                   ;; fixme: using write-octet here is wrong when align #=8
                   ;; (and the align above is redundant when it is 8)
                   ,@(if (eq endian :little)

                         `((loop for low-bit upto ,(* 8 (1- bytes)) by 8
                                do (write-octet (ldb (byte 8 low-bit) data)
                                                source)))

                         `((loop for low-bit from ,(* 8 (1- bytes))
                                 downto 0 by 8
                                 do (write-octet (ldb (byte 8 low-bit) data)
                                                source)
)))))))))

(make-byte-writers :little
  (write-ui8 1)
  (write-ui16 2)
  (write-ui32 4)
  (write-ui64 8)
  (write-si8 1)
  (write-si16 2)
  (write-si32 4)
  (write-fixed8 2 :convert fixed8->u16)
  (write-fixed 4 :convert fixed->u32)
  (write-float16 2 :convert encode-float16)
  (write-float32 4 :convert encode-float32)
  (write-float64 8 :convert encode-float64)
  (write-twips-u16 2 :convert twips->u16)
  (write-twips-s16 2 :convert twips->u16))

(defun write-encodedu32 (u32 source)
  ;; todo: verify this is the correct algo (copied from abc code)
  (loop
     for i = u32 then i2
     for i2 = (ash i -7)
     for b = (ldb (byte 7 0) i)
     for done = (or (= i2 0) (= i2 -1))
     when (or (not (eql (logbitp 6 i2) (logbitp 6 i))) (not done))
     do (setf b (logior #x80 b))
     do (write-byte b source)
     when (and done (logbitp 7 b))
     do (write-byte (if (minusp i2) #x7f 0) source)
     until done))

(declaim (inline write-bit-flag))
(defun write-bit-flag (flag source)
  (write-ub (if flag 1 0) 1 source))

(defmacro with-swf-writers ((source value-arg)
                               &body body)
  ;; todo: allow setting byte order for bitfields
  `(macrolet ((align (bits)
                `(write-align-source ,bits ,',source)))
     (symbol-macrolet ((%primitive-type-macro-mode% :write)
                       (%source% ,source)
                       (%value-arg% ,value-arg))
       ,@body)))
