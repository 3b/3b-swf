(in-package :%3b-swf)

(defvar *shape-fill-bits*)
(defvar *shape-line-bits*)
(defgeneric shape-min-fill-bits (shape-record))
(defgeneric shape-min-line-bits (shape-record))

(define-swf-type state-move-to ()
  :this-var o
  :auto
  ((bits (ub 5) :derived (min-bitfield-size-twips (delta-x o) (delta-y o)))
   (delta-x (sb-twips bits) :initform 0)
   (delta-y (sb-twips bits) :initform 0)))

(define-swf-type shape-record ()
  :this-var o
  :auto
  ((type-flag (bit-flag) :derived (typep o 'edge-shape-record ))) ;; aligned?
  :subclass (if type-flag
                'edge-shape-record
                'style-change-or-end-shape-record))

(define-swf-type style-change-or-end-shape-record (shape-record)
  :this-var o
  :auto
  ((state-new-styles (bit-flag) :derived (not (null (or (fill-styles o)
                                                        (line-styles o)))))
   (state-line-style (bit-flag) :derived (not (null (line-style o))))
   (state-fill-style-1 (bit-flag) :derived (not (null (fill-style-1 o))))
   (state-fill-style-0 (bit-flag) :derived (not (null (fill-style-0 o))))
   (state-move-to (bit-flag) :derived (not (null (move-to o)))))
  :subclass (if (or state-move-to
                    state-fill-style-1 state-fill-style-0
                    state-line-style state-new-styles)
                'style-change-shape-record
                'shape-end-record))

(define-swf-type style-change-shape-record (style-change-or-end-shape-record)
  :this-var o
  :auto
  ((move-to (swf-type 'state-move-to) :optional (super state-move-to))
   (fill-style-0 (ub *shape-fill-bits*) :optional (super state-fill-style-0))
   (fill-style-1 (ub *shape-fill-bits*) :optional (super state-fill-style-1))

   (line-style (ub *shape-line-bits*) :optional (super state-line-style))
   ;; fixme: need to make this smarter so it creates the other style if
   ;; only 1 is specified at creation time...
   (fill-styles (swf-type 'fill-style-array) :initform nil
                :optional (super state-new-styles))
   (line-styles (swf-type 'line-style-array) :initform nil
                :optional (super state-new-styles))
   (new-fill-bits (ub 4) :optional (super state-new-styles)
                  :extra (setf *shape-fill-bits* (or new-fill-bits
                                                     *shape-fill-bits*))
                  :derived (when (fill-styles o)
                             (integer-length (length (fill-styles
                                                      (fill-styles o))))))
   (new-line-bits (ub 4) :optional (super state-new-styles)
                  :extra (setf *shape-line-bits* (or new-line-bits
                                                     *shape-line-bits*))
                  :derived (when (line-styles o)
                             (integer-length (length (line-styles
                                                      (line-styles o))))))))

(define-swf-type shape-end-record (style-change-or-end-shape-record)
    :align-after 8
    ;;:auto ((junk (align 8) :local t))
)
(defmethod state-new-styles ((o shape-end-record)) nil)
(defmethod state-line-style ((o shape-end-record)) nil)
(defmethod state-fill-style-0 ((o shape-end-record)) nil)
(defmethod state-fill-style-1 ((o shape-end-record)) nil)
(defmethod state-move-to ((o shape-end-record)) nil)



(define-swf-type edge-shape-record (shape-record)
  :this-var o
  :auto
  ;; fixme: convert to flag?
  ((straight-edge (ub 1) :derived (subclass-id o 'edge-shape-record)))
  :subclass (subclass-from-id 'edge-shape-record straight-edge))

;; fixme: run initform when optional part not present?
(define-swf-type straight-edge-shape-record (edge-shape-record)
  :id 1
  :this-var o
  :auto
  ((num-bits (ub 4) :derived (max 0
                                  (- (min-bitfield-size-twips
                                      (delta-x o)
                                      (delta-y o)) 2)))
   ;; fixme: clean up logic on these...
   (general-line (bit-flag) :derived (not (or (not (delta-x o)) (zerop (delta-x o))
                                              (not (delta-y o)) (zerop (delta-y o)))))
   (vertical-line (bit-flag) :optional (not general-line)
                  :derived (or (not (delta-x o))
                               (and (delta-y o) (not (zerop (delta-y o)))
                                    (or (not (delta-x o)) (zerop (delta-x o))))))
   ;; fixme: set these to 0 instead of nil when missing, and adjust the
   ;; flag derives accordingly
   (delta-x (sb-twips (+ 2 num-bits)) :initform 0 :optional (or general-line (not vertical-line)))
   (delta-y (sb-twips (+ 2 num-bits)) :initform 0 :optional (or general-line vertical-line))))

(define-swf-type curved-edge-shape-record (edge-shape-record)
  :id 0
  :this-var o
  :auto
  ((num-bits (ub 4) :derived (max 0 (- (min-bitfield-size-twips
                                        (control-delta-x o)
                                        (control-delta-y o)
                                        (anchor-delta-x o)
                                        (anchor-delta-y o))
                                 2)))
   (control-delta-x (sb-twips (+ 2 num-bits)))
   (control-delta-y (sb-twips (+ 2 num-bits)))
   (anchor-delta-x (sb-twips (+ 2 num-bits)))
   (anchor-delta-y (sb-twips (+ 2 num-bits)))))

(defparameter *allow-short-shapes* nil
  "workaround for .swf files with empty shapes 1 byte apart in font tags")

(define-swf-type shape ()
  :this-var o
  :auto
  ((num-fill-bits (ub 4)) ;; fixme: can we derive these?
   ;; :derived (reduce 'max (shape-records o) :key 'shape-min-fill-bits)
   (*shape-fill-bits* num-fill-bits :local t)
   (num-line-bits (ub 4)) ;; fixme: can we derive these?
   ;;:derived (reduce 'max (shape-records o) :key 'shape-min-line-bits)
   (*shape-line-bits* num-line-bits :local t)
   #+nil(foo 0 :local t)
   (shape-records (list-until-type (swf-type 'shape-record) 'shape-end-record)
                  ;; hack around some files with 1-byte shapes in font tags
                  :optional (not (and *allow-short-shapes*
                                      (zerop num-fill-bits)
                                      (zerop num-line-bits)))))
  :print-unreadably ("~s" (if *array-print-verbose* (shape-records o) (list 'length (length (shape-records o)))) )
)

(define-swf-type shape-with-style ()
  :this-var o
  :auto
  ((fill-styles (swf-type 'fill-style-array)
                :initform (make-instance 'fill-style-array 'fill-styles nil))
   (line-styles (swf-type 'line-style-array)
                :initform (make-instance 'line-style-array 'line-styles nil))
   (num-fill-bits (ub 4)
                  :derived (integer-length (fill-style-count (fill-styles o))))
   (*shape-fill-bits* num-fill-bits :local t)
   (num-line-bits (ub 4)
                  :derived (integer-length (line-style-count (line-styles o))))
   (*shape-line-bits* num-line-bits :local t)
   #+nil(shape-records (list-until-type (swf-type 'shape-record)
                                   'shape-end-record
                                   #+nil(lambda (x) (declare (ignore x))
                                      (next-bits-zero-p 8))))
   (shape-records (list-until (swf-type 'shape-record)
                           (lambda (x) (declare (ignore x))
                                   (next-bits-zero-p 6))))
   (shape-record-end-marker (prog1 (ub 6)) :derived 0))
  :align-after 8
  :print-unreadably ("fillstyles:~s linestyles:~s bits=~s/~s ~s"
                     (fill-styles o) (line-styles o)
                     (num-fill-bits o) (num-line-bits o)
                     (if *array-print-verbose*
                         (shape-records o)
                         (list 'length (length (shape-records o)))) ))
