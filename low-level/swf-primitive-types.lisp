(in-package :%3b-swf)

;;; magic symbols:
;;; %primitive-type-macro-mode%
;;;   one of :read, :size, :write to control expansion of primitive type
;;;   macros
;;; %source%
;;;   should expand to source stream/etc for reader
;;;   expand to dest stream/etc for writer
;;; %value-arg%
;;;   should expand to var containing value to be written
;;;    (possibly should expand to actual value? need to check usage ...)

(defmacro define-primitive-type (name (&rest args) &key read size write decl)
  (let ((env (gensym))
        (mode (gensym)))
    `(defmacro ,name (,@args &environment ,env)
       ,@(if decl (list decl))
       (let ((,mode (macroexpand-1 '%primitive-type-macro-mode% ,env)))
         ;;(format t "mode = ~s~%" ,mode)
         (ecase ,mode
           (:read ,read)
           (:size ,size)
           (:write ,write))))))


(define-primitive-type ub (bits &key align)
  :read `(progn
           (check-end %source%)
           ,@(when align `((align ,align)))
           (read-ub ,bits %source%))
  :size `(progn
           ,@(when align `((align ,align)))
           (incf *swf-sizer-bitpos* ,bits))
  :write `(progn
            ,@(when align `((align ,align)))
            (write-ub (floor %value-arg%) ,bits %source%)))


(define-primitive-type sb (bits)
  :read `(read-sb ,bits %source%)
  :size `(incf *swf-sizer-bitpos* ,bits)
  :write `(write-sb (floor %value-arg%) ,bits %source%))


(define-primitive-type sb-twips (bits)
  :read`(/ (read-sb ,bits %source%) 20.0)
  :size `(incf *swf-sizer-bitpos* ,bits)
  :write `(write-sb (twips->u16 %value-arg%) ,bits %source%))


(define-primitive-type fb (bits)
  :read `(/ (read-sb ,bits %source%) ,(expt 2.0 16))
  :size `(incf *swf-sizer-bitpos* ,bits)
  :write `(write-sb (floor (* ,(expt 2 16) %value-arg%)) ,bits %source%))

(define-primitive-type fb8 (bits)
  :read `(/ (read-sb ,bits %source%) ,(expt 2.0 8))
  :size `(incf *swf-sizer-bitpos* ,bits)
  :write `(write-sb (floor (* ,(expt 2 8) %value-arg%)) ,bits %source%))



(macrolet
    ((foo ()
       `(progn
          ,@(loop  for (name bytes floor) in '((ui8 1 t)
                                               (ui16 2 t)
                                               (ui32 4 t)
                                               (ui64 8 t)
                                               (si8 1 t)
                                               (si16 2 t)
                                               (si32 4 t)
                                               (fixed8 2 nil)
                                               (fixed 4 nil)
                                               (float16 2 nil)
                                               (float32 4 nil)
                                               (float64 8 nil)
                                               (twips-u16 2 nil)
                                               (twips-s16 2 nil))
               for read in '(read-ui8 read-ui16 read-ui32 read-ui64
                             read-si8 read-si16 read-si32
                             read-fixed8 read-fixed
                             read-float16 read-float32 read-float64
                             read-twips-u16 read-twips-s16)
               for write in '(write-ui8 write-ui16 write-ui32 write-ui64
                              write-si8 write-si16 write-si32
                              write-fixed8 write-fixed
                              write-float16 write-float32 write-float64
                              write-twips-u16 write-twips-s16)
               collect `(define-primitive-type ,name ()
                          :Read `(,',read %source%)
                          :size `(progn
                                   (when %value-arg%
                                     (align 8)
                                     (incf *swf-sizer-bitpos* ,',(* 8 bytes))))
                          :write ,(if floor
                                      `'(progn
                                         (when %value-arg%
                                           (,write (floor %value-arg%) %source%)))
                                      `'(progn
                                         (when %value-arg%
                                           (,write %value-arg% %source%)))))

               ))))
  (foo))

(define-primitive-type encodedu32 ()
  :read `(read-encodedu32 %source%)
  :size `(size-encodedu32 %value-arg%)
  :write `(write-encodedu32 %value-arg% %source%))


(define-primitive-type bit-flag (&key align)
  :read `(progn
           ,@(when align `((align ,align)))
           (read-bit-flag %source%))
  :size `(progn
           ,@(when align `((align ,align)))
           (incf *swf-sizer-bitpos*))
  :write `(progn
            ,@(when align `((align ,align)))
            (write-bit-flag %value-arg% %source%)))

(define-primitive-type swf-type (type)
  :read `(read-swf-part ,type %source%)
  :size `(%swf-part-size ,type %value-arg%)
  :write `(write-swf-part ,type %value-arg% %source%))

(define-primitive-type sized-list (type size)
  :read (alexandria:with-gensyms (end)
          `(loop with ,end = (+ ,size (file-position %source%))
              while (< (file-position %source%) ,end)
              collect ,type))
  :decl (declare (ignorable size))
  :size
  ;; fixme: verify we don't go over size...
  (alexandria:with-gensyms (i)
    `(loop for ,i in %value-arg%
        do (let ((%value-arg% ,i))
             (declare (ignorable %value-arg%))
             ,type)))
  :write ;; fixme: verify we don't go over size...
  (alexandria:with-gensyms (i)
    `(loop for ,i in %value-arg%
        do (let ((%value-arg% ,i))
             (declare (ignorable %value-arg%))
             ,type))))


(define-primitive-type list-while (type test)
  :read (alexandria:with-gensyms (value)
          ;; todo: this should probably be limited to current tag
          ;; to better handle buggy code/bad data
          `(loop with ,value = nil
              while (funcall ,test ,value)
              do (setf ,value ,type)
              collect ,value))
  :decl (declare (ignorable test))
  :size ;; fixme: probably should verify that test hold for last element an no others
  (alexandria:with-gensyms (i)
    `(loop for ,i in %value-arg%
        do (let ((%value-arg% ,i))
             ,type)))
  :write ;; fixme: probably should verify that test hold for last element an no others
  (alexandria:with-gensyms (i)
    `(loop for ,i in %value-arg%
        do (let ((%value-arg% ,i))
             ,type))))

(define-primitive-type list-until (type test)
  :read (alexandria:with-gensyms (value)
          ;; todo: this should probably be limited to current tag
          ;; to better handle buggy code/bad data
          `(loop with ,value = nil
              do (setf ,value ,type)
              collect ,value
              until (funcall ,test ,value)))
  :decl (declare (ignorable test))
  :size ;; fixme: probably should verify that test hold for last element an no others
  (alexandria:with-gensyms (i)
    `(loop for ,i in %value-arg%
        do (let ((%value-arg% ,i))
             ,type)))
  :write ;; fixme: probably should verify that test hold for last element an no others
  (alexandria:with-gensyms (i)
    `(loop for ,i in %value-arg%
        do (let ((%value-arg% ,i))
             ,type))))

(define-primitive-type list-until-type (type end-type)
  :read (alexandria:with-gensyms (value)
          ;; todo: this should probably be limited to current tag
          ;; to better handle buggy code/bad data
          `(loop with ,value = nil
              do (setf ,value ,type)
              collect ,value
              ;; should this quote 'end-type?
              until (typep ,value ,end-type)))
  :decl (declare (ignorable end-type))
  :size ;; fixme: probably should verify types
  (alexandria:with-gensyms (i)
    `(loop for ,i in %value-arg%
        do (let ((%value-arg% ,i))
             ,type)))
  :write ;; fixme: probably should verify types
  (alexandria:with-gensyms (i)
    `(loop for ,i in %value-arg%
        do (let ((%value-arg% ,i))
             ,type))))


(define-primitive-type counted-list (type count &key align-elements)
  :read `(when ,count
           (loop repeat ,count
              collect ,type
              ,@(when align-elements
                      '(do (align 8)))))
  :size (alexandria:with-gensyms (i)
          `(progn
             (assert (= (length %value-arg%) ,count))
             (loop for ,i in %value-arg%
                do (let ((%value-arg% ,i))
                     (declare (ignorable %value-arg%))
                     ,type
                     ,@(when align-elements '((align 8)))))))
  :write (alexandria:with-gensyms (i)
           `(progn
              (assert (= (length %value-arg%) ,count))
              (loop for ,i in %value-arg%
                 do (let ((%value-arg% ,i))
                      ,@(when align-elements '((align 8)))
                      ,type)))))


(define-primitive-type enumerated-list (&rest types)
  :read `(list ,@types)
  :size `(progn
           ,@(loop for i in types
                collect `(let ((%value-arg% (pop %value-arg%)))
                           ,i)))
  :write ;; override LIST for write and size to go by component
  `(progn
     ,@(loop for i in types
          collect `(let ((%value-arg% (pop %value-arg%)))
                     ,i))))


(define-primitive-type string-sz-utf8 (&optional max-length)
  :read (alexandria:once-only (max-length)
          `(progn
             (align 8)
             (loop with octets = (make-array 128
                                             :element-type '(unsigned-byte 8)
                                             :adjustable t
                                             :fill-pointer 0)
                for octet = (read-byte %source%)
                until (or (zerop octet)
                          (and ,max-length
                               (>= (fill-pointer octets) ,max-length)))
                do (vector-push-extend octet octets
                                       (* 2 (array-dimension octets 0)))
                finally (return (babel:octets-to-string
                                 octets :encoding :utf-8
                                 :errorp nil)))))
  :decl (declare (ignorable max-length))
  :size
  ;; fixme: probably need to ignore max-length here
  ;; since max length might be set to something that
  ;; tries to call this to determine actual length...
  `(progn
     (align 8)
     ;; add 1 to account for 0 term
     (let ((len (1+ (babel:string-size-in-octets
                     %value-arg% :encoding :utf-8))))
       #+(or) (when ,max-length
                (setf len (min len ,max-length)))
       (incf *swf-sizer-bitpos*
             (* 8 len))))
  :write (alexandria:once-only (max-length)
           `(progn
              (align 8)
              (let ((octets (babel:string-to-octets
                             %value-arg% :encoding :utf-8)))
                (when ,max-length
                  (assert (<= (1+ (length octets)) ,max-length)))
                (write-sequence octets
                                %source%))
              (write-byte 0 %source%))))


(define-primitive-type zlib-data ()
  :read `(progn
           (align 8)
           ;; fixme: don't use this, chipz reads too much...
           (chipz:decompress 'nil 'chipz:zlib %source%))
  :size ;; fixme: this is ugly :/
  `(progn
     (align 8)
     (incf *swf-sizer-bitpos*
           (* 8 (length (salza2:compress-data %value-arg%
                                              'salza2:zlib-compressor)))))

  :write `(progn
            (align 8)
            (write-sequence
             (salza2:compress-data %value-arg% 'salza2:zlib-compressor)
             %source%)))


(define-primitive-type rest-of-tag ()
  :read `(let ((seq (make-array (- (car *reader-end-of-block*)
                                   (file-position %source%))
                                :element-type '(unsigned-byte 8))))
           (read-sequence seq %source%)
           seq)
  :size `(incf *swf-sizer-bitpos*
               (* 8 (length %value-arg%)))
  :write `(write-sequence %value-arg% %source%))



(define-primitive-type next-octet-zero-p ()
  :read `(progn
           (read-align-source 8 %source%)
           (let ((a (ui8)))
             (setf *partial-octet-read* (cons a 8))
             (zerop a)))
  :size `(progn (format t "called next-octet-zero-p during size?")
                t)
  :write `(progn (format t "called next-octet-zero-p during write?")
                 t))

(define-primitive-type next-bits-zero-p (bits)
  :read `(let ((a (ub ,bits)))
           (if *partial-octet-read*
               (progn
                 (setf (car *partial-octet-read*)
                       (dpb a
                            (byte ,bits (cdr *partial-octet-read*))
                            (car *partial-octet-read*)))
                 (incf (cdr *partial-octet-read*) ,bits))
               (setf *partial-octet-read* (cons a ,bits)))
           (zerop a))
  :decl (declare (ignorable bits))
  :size `(progn (format t "called next-octet-zero-p during size?")
                t)
  :write `(progn (format t "called next-octet-zero-p during write?")
                  t))

(define-primitive-type bytes-left-in-tag ()
  :read `(- (car *reader-end-of-block*) (file-position %source%))
  :size `(progn
           (format t "called bytes-left-in-tag during size?")
           1)
  :write `(progn
            (format t "called bytes-left-in-tag during write? pos = ~s~%"
                    (file-position %source%))
            1))

