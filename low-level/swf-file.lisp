(in-package :%3b-swf)
(defparameter *trace-tags* '())

;; testing interface

(defun read-swf (stream)
  (with-character-id-maps
    (with-swf-readers (stream)
     (let* ((fc (ui8))
            (w (ui8))
            (s (ui8))
            (*swf-version* (ui8))
            (size (ui32))
            (*blob-tags* *blob-tags*))
       (declare (ignorable size))
       ;; check signature ;; fixme: handle error properly
       (assert (and (= s 83) (= w 87) (member fc '(67 70)))) ;;S W F/C
       (ecase fc
         (67 (flex:with-input-from-sequence
                 (s (chipz:decompress nil 'chipz:zlib stream))
               (read-swf-file-body s)))
         (70 (read-swf-file-body stream)))))))

;; swf-tag is odd, so defining it by hand for now...
(defclass swf-tag ()
  ((tag :initform nil :initarg :tag)))

(defmethod tag ((s swf-tag))
  (or (slot-value s 'tag)
      (subclass-id s 'swf-tag)))

(defmethod subclass-from-id ((type (eql 'swf-tag)) (id number))
  ;; default to reading tag as a blob if no more specific reader is available
  'swf-blob-tag)

(defparameter *read-blobs* nil)
(defmethod read-swf-part ((type (eql 'swf-tag)) source &rest initargs)
  (with-swf-readers (source)
    (multiple-value-bind (tag size) (read-tag&size source)
      (with-reader-block size
        (unless (or (= tag 777)  ;; swftools junk
                    (= tag 255)  ;; unknown junk?
                    (= tag 253)  ;; unknown junk?
                    (getf *tag-id-plist* tag))
          (error "bad tag ~s~%" tag))
        (when (member tag '(255 253))
          ;; don't try to decode everything in encrypted files...
          (setf *blob-tags* (nconc (list 12 34 255 253) *blob-tags*)))
        (when *trace-tags* (format t "--read tag ~s~%" (getf *tag-id-plist* tag)))
        (if (member tag *trace-tags*)
            (trace read-swf-part)
            (when *trace-tags* (untrace read-swf-part)))
        (prog1
            (if (or *read-blobs* (member tag *blob-tags*))
                (apply 'read-swf-part (if (zerop tag) 'swf-end-tag
                                          'swf-blob-tag)
                       source :tag tag initargs)
                (apply 'read-swf-part (subclass-from-id 'swf-tag tag) source :tag tag initargs))
          (if (member tag *trace-tags*) (untrace read-swf-part))
          (when *trace-tags* (format t "<<done tag ~s left=~s~%" (getf *tag-id-plist* tag) (bytes-left-in-tag)))
          ;; adjust for buggy files (and reader bugs) by always skipping to
          ;; the end of a tag after a read
          (when (not (zerop (bytes-left-in-tag)))
            (Assert (>= (bytes-left-in-tag) 0))
            (format t "got extra bytes left in tag ~s (~s):~%~s~%"
                    (getf *tag-id-plist* tag) tag
                    (rest-of-tag))
            #+nil(break)))))))


#+nil
(defmethod read-swf-part ((type (eql 'swf-tag)) source &rest initargs)
  (with-swf-readers (source)
    (multiple-value-bind (tag size) (read-tag&size source)
      (with-reader-block size
        (apply 'read-swf-part (if (zerop tag) 'swf-end-tag
                                  'swf-blob-tag) source :tag tag initargs)))))



(defparameter *blobs* 0)
(defparameter *show-blobs* nil)
(define-swf-type swf-blob-tag (swf-tag)
  :this-var o
  :auto
  ((blob (rest-of-tag)
         :extra (progn
                  (incf *blobs*)
                  (when *show-blobs*
                    (format t "read blob tag :~%;; ~s (~s) = ~s bytes~%"
                            (getf *tag-id-plist* (super :tag)) (super :tag)
                            (length blob))))))
  :print-unreadably ("tag:~d blob:~d bytes" (or (getf *tag-id-plist* (tag o))
                                                (tag o)) (length (blob o))))

(define-swf-type swf-end-tag (swf-tag)
  :id 0)


(defparameter *trace-count* nil)
(defun read-swf-file-body (source)
  (let ((count 0))
    (with-swf-readers (source)
      (list :swf :version *swf-version*
            :frame-size (read-swf-part 'rect source)
            :frame-rate (fixed8)
            :frame-count (ui16)
            :tags (list-until (swf-type 'swf-tag)
                              (lambda (x)
                                (incf count)
                                (when (and (numberp *trace-count*)
                                           (> count *trace-count*))
                                  (trace read-swf-part))
                                (when *trace-count* (format t "~s~%" count))
                                (typep x 'swf-end-tag)))))))








(defmethod write-swf-part swf-part (type (tag swf-tag) source)
  (declare (ignore type))
  (with-swf-writers (source v)
    (let ((tag-no (subclass-id tag 'swf-tag))
          (start (file-position source)))
      ;; not sure if calculating size on fly is better, or just writing
      ;; to a buffer... trying size first to test the code
      (let ((size (swf-part-size type tag :body-only t)))
        (align 8)
        (unless (zerop (rem size 8))
          (format t "tag not multiple of 8 bits? = ~s bits" size))
        (setf size (ash size -3))
        (if (and (not (member tag-no '(2 26))) (<= 0 size 62))
            ;; fixme: write shorter way to pass args to writes when
            ;; calling them directly
            (progn
              (write-ui16 (dpb tag-no (byte 10 6) size) source)
              (incf size 2))
            (progn
              (write-ui16 (dpb tag-no (byte 10 6) 63) source)
              (write-ui32 size source)
              (incf size 6)))
        (if (member tag *trace-tags*)
            (trace write-swf-part)
            (when *trace-tags* (untrace read-swf-part)))
        (prog1
            (call-next-method type tag source)
          (align 8)
          (unless (= (- (file-position source) start) size)
              #+nil(format t "tag size = ~s, calculated = ~s  -- (~s ~s ~s ~s)~%"
                   (- (file-position source) start) size
                   start (file-position source) size size)
            (error "tag size = ~s, calculated = ~s  -- (~s ~s ~s ~s)"
                   (- (file-position source) start) size
                   start (file-position source) size size))
          (if (member tag *trace-tags*) (untrace read-swf-part)))))))


(defmethod %swf-part-size swf-part (type (tag swf-tag) &key body-only)
  (declare (ignore type))
  (with-swf-sizers (v)
    (let (#+nil(tag-no (subclass-id tag 'swf-tag))
          (v t))
      ;; fixme: need some better way to count alignment into size...
      (let* ((start *swf-sizer-bitpos*)
             (size 0))
        (align 8)
        (call-next-method type tag :body-only body-only)
        (align 8)
        (setf size (- *swf-sizer-bitpos* start))
        (unless body-only
          (if (and (not (member (tag tag) '(2 26))) (<= 0 (/ size 8) 62))
              ;; fixme: write shorter way to pass args to writes when
              ;; calling them directly
              (ui16)
              (progn
                (ui16)
                (ui32))))
        (- *swf-sizer-bitpos* start)))))