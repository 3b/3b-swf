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



#+nil

(let ((a (let ((*partial-octet-read* nil))
            (with-open-file (s "/tmp/kongregate-shootorial.swf" :element-type '(unsigned-byte 8))
              (time (prog1 (read-swf s)
                      (assert (not *partial-octet-read*))))))))
  (let ((*array-print-verbose* nil))
    (format t "~{~s~%~}~%" a))
  #+nil(loop for i in a
        for x from 0
        do (let ((*array-print-verbose* (> x 180)))
             (format t "~s~%" i)))
  (format t "~%~s tags (should be 196)~%" (length (getf (cdr a) :tags))))

#+nil
(let ((*partial-octet-read* nil))
  (with-open-file (s "/tmp/foo.swf" :element-type '(unsigned-byte 8))
    (read-swf s)))

;;+ SOUND-STREAM-HEAD-2-TAG
;;+ CSM-TEXT-SETTINGS-TAG
;;+ DEFINE-TEXT-TAG
;;+ DEFINE-SHAPE-3-TAG
;;+ REMOVE-OBJECT-2-TAG
;;+ DEFINE-SHAPE-4-TAG
;;+ DEFINE-BITS-JPEG-2-TAG
;;+ DEFINE-FONT-NAME-TAG
;;+ DEFINE-FONT-3-TAG
;;DEFINE-FONT-ALIGN-ZONES-TAG

;;+ DEFINE-SCENE-AND-FRAME-LABEL-DATA-TAG
;;+ DEFINE-BITS-JPEG-3-TAG
;;+ DEFINE-SHAPE-2-TAG
;;+ DEFINE-EDIT-TEXT-TAG

;;+ DO-ACTION-TAG
;;+ DEFINE-MORPH-SHAPE-TAG
;;+ DEFINE-FONT-2-TAG

;;+ START-SOUND-TAG (15)
;;+ DEFINE-BUTTON-SOUND-TAG (17)
;;+ SOUND-STREAM-HEAD-TAG (18)
;;+ SOUND-STREAM-BLOCK-TAG (19)
;;+ PROTECT-TAG (24)
;;+ DEFINE-TEXT-2-TAG (33)
;;+ DEFINE-BUTTON-2-TAG (34)
;;+ DEFINE-BITS-LOSSLESS-2-TAG (36)
; FRAME-LABEL-TAG (43) -3
;;+ EXPORT-ASSETS-TAG (56)




;(trace read-swf-part)
;(untrace read-swf-part)
;(defparameter *trace-tags* '(10))
(defparameter *trace-tags* '())
(defparameter *trace-count* 20)
(defparameter *trace-count* nil)