(in-package :3b-swf)
(defparameter *trace-tags* '())

(defun read-swf (stream)
  (with-swf-readers (stream)
    (let* ((fc (ui8))
           (w (ui8))
           (s (ui8))
           (*swf-version* (ui8))
           (size (ui32)))
      (declare (ignorable size))
      (format t "siz=~s~%" size)
      ;; check signature ;; fixme: handle error properly
      (assert (and (= s 83) (= w 87) (member fc '(67 70)))) ;;S W F/C
      (ecase fc
        (67 (flex:with-input-from-sequence
                (s (chipz:decompress nil 'chipz:zlib stream))
              (read-swf-file-body s)))
        (70 (read-swf-file-body stream))))))

;; swf-tag is odd, so defining it by hand for now...
(defclass swf-tag ()
  ((tag :initform nil :initarg :tag :reader tag)))

(defmethod subclass-from-id ((type (eql 'swf-tag)) (id number))
  ;; default to reading tag as a blob if no more specific reader is available
  'swf-blob-tag)

(defmethod read-swf-part ((type (eql 'swf-tag)) source &rest initargs)
  (with-swf-readers (source)
    (multiple-value-bind (tag size) (read-tag&size source)
      (with-reader-block size
        (unless (or (= tag 777) ;; swftools junk
                    (getf *tag-id-plist* tag))
            (error "bad tag ~s~%" tag))
        (when *trace-tags* (format t "--read tag ~s~%" (getf *tag-id-plist* tag)))
        (if (member tag *trace-tags*)
            (trace read-swf-part)
            (when *trace-tags* (untrace read-swf-part)))
        (prog1
            (apply 'read-swf-part (subclass-from-id 'swf-tag tag) source :tag tag initargs)
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
(define-swf-type swf-blob-tag (swf-tag)
  :this-var o
  :auto
  ((blob (rest-of-tag)
         :extra (progn
                  (incf *blobs*)
                  (format t "read blob tag :~%;; ~s (~s)~%" (getf *tag-id-plist* (super :tag)) (super :tag)))))
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

