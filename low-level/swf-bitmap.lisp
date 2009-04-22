(in-package :%3b-swf)

;;; these are probably broken


(defmacro make-bitmap-reader (bytes-per-pel element-size
                              &optional colormap colormap-rgba)
  `(let* ((w (super width))
          (h (super height))
          (pad (mod (- 4 (mod (* w ,bytes-per-pel) 4)) 4))
          (zdata (rest-of-tag))
          (zi -1)
          (data (make-array (list w h)
                            :element-type '(unsigned-byte ,element-size))))
     (setf zdata (chipz:decompress nil 'chipz:zlib zdata))
     ,@ (when colormap
          `((setf color-table
                  (loop for i upto bitmap-color-table-size
                        collect (make-instance
                                 ,(if colormap-rgba ''rgba ''rgb)
                                 'r (aref zdata (incf zi))
                                 'g (aref zdata (incf zi))
                                 'b (aref zdata (incf zi))
                                 ,@ (when colormap-rgba
                                      '('a (aref zdata (incf zi)))))))))
        (loop for j below h
              do (loop for i below w
                       for pel = (loop for p below ,bytes-per-pel
                                       sum (ash (aref zdata (incf zi)) (* p 8)))
                       do (setf (aref data i j) pel))
              (loop for i below pad do (incf zi)))
        data))
(defmacro make-bitmap-sizer (bytes-per-pel v)
  `(let* ((w (array-dimension ,v 0))
          (h (array-dimension ,v 1))
          (pad (mod (- 4 (mod (* w ,bytes-per-pel) 4)) 4))
          (octet-count 0))
     (align 8)
     (error "bitmap sizer not done yet")
     (salza2:with-compressor (c 'salza2:zlib-compressor
                                :callback (lambda (buffer end)
                                            (declare (ignore buffer))
                                            (incf octet-count end)))
       (loop for j below h
             do (loop for i below w
                      for pel = (aref ,v i j)
                      do (loop for p upto ,bytes-per-pel
                               do (salza2:compress-octet
                                   (ldb (byte 8 (* p 8)) pel) c)))
             (loop for i below pad
                   do (salza2:compress-octet 0 c))))
     (ub (* octet-count 8))))
(defmacro make-bitmap-writer (bytes-per-pel v)
  `(let* ((w (array-dimension ,v 0))
          (h (array-dimension ,v 1))
          (pad (mod (- 4 (mod (* w ,bytes-per-pel) 4)) 4)))
     (align 8)
     (error "bitmap writer not done yet")
     (salza2:with-compressor (c 'salza2:zlib-compressor
                                :callback (lambda (buffer end)
                                            (loop for i below end
                                                  for j across buffer
                                                  do (let ((v j))
                                                       (ui8)))))
       (loop for j below h
             do (loop for i below w
                      for pel = (aref ,v i j)
                      do (loop for p upto ,bytes-per-pel
                               do (salza2:compress-octet
                                   (ldb (byte 8 (* p 8)) pel) c)))
             (loop for i below pad do (salza2:compress-octet 0 c))))))
;;; modified version of bitmapdata/colormapdata/etc to contain size/format
;;;   instead of leaving it in tag


(define-swf-type bitmap-tag-data-rgb-colormapped (bitmap-tag-data-rgb)
  :id 3
  :this-var o
  :value-var v
  :auto ((bitmap-color-table-size (ui8) :derived (1- (length (color-table o))))
         (color-table ()) ;; read by bitmap-data reader
         (bitmap-data ()))
  :reader (
           (bitmap-data
            (make-bitmap-reader 1 8 t nil)))
  ;;; fixme: probably should cache the compressed data (or at least the size)
  :sizer ((bitmap-data
           (make-bitmap-sizer 1 v))) ;; fixme: not done yet...
  :writer ((bitmap-data
            (make-bitmap-writer 1 v)))
  :print-unreadably ("~s,~s" (width o) (height o)))

(define-swf-type bitmap-tag-data-rgb-pix15 (bitmap-tag-data-rgb)
  :id 4
  :this-var o
  :value-var v
  :auto ((bitmap-data ()))
  :reader ((bitmap-data
            (progn
              (format t "bitmap-tag-data-rgb-pix15 - untested...~%")
             (make-bitmap-reader 2 16))))
  ;;; fixme: probably should cache the compressed data (or at least the size)
  :sizer ((bitmap-data
           (make-bitmap-sizer 2 v)))
  :writer ((bitmap-data
            (make-bitmap-writer 2 v))))

(define-swf-type bitmap-tag-data-rgb-pix24 (bitmap-tag-data-rgb)
  :id 5
  :this-var o
  :value-var v
  :auto ((bitmap-data ()))
  :reader ((bitmap-data
            (progn
              (format t "bitmap-tag-data-rgb-pix24 - untested...~%")
              (make-bitmap-reader 4 32))))
  ;;; fixme: probably should cache the compressed data (or at least the size)
  :sizer ((bitmap-data
           (make-bitmap-sizer 4 v)))
  :writer ((bitmap-data
            (make-bitmap-writer 4 v))))

;;; rgba

(define-swf-type bitmap-tag-data-rgba-colormapped (bitmap-tag-data-rgba)
  :id 3
  :this-var o
  :value-var v
  :auto ((bitmap-color-table-size (ui8) :derived (1- (length (color-table o))))
         (color-table ()) ;; read by bitmap-data reader
         (bitmap-data ()))
  :reader ((bitmap-data
            (make-bitmap-reader 1 8 t t)))
  ;;; fixme: probably should cache the compressed data (or at least the size)
  :sizer ((bitmap-data
           (make-bitmap-sizer 1 v)))
  :writer ((bitmap-data
            (make-bitmap-writer 1 v)))
  :print-unreadably ("~s,~s" (width o) (height o)))


(define-swf-type bitmap-tag-data-rgba-argb (bitmap-tag-data-rgba)
  :id 5
  :this-var o
  :value-var v
  :auto ((bitmap-data ()))
  :reader ((bitmap-data
            (make-bitmap-reader 4 32)))
  ;;; fixme: probably should cache the compressed data (or at least the size)
  :sizer ((bitmap-data
           (make-bitmap-sizer 4 v)))
  :writer ((bitmap-data
            (make-bitmap-writer 4 v))))


;;; outer classes

(define-swf-type bitmap-tag-data-rgb ()
  :this-var o
  :auto
  ((bitmap-format (ui8) :derived (subclass-id o 'bitmap-tag-data-rgb))
   (width (ui16) :derived (array-dimension (bitmap-data o) 0))
   (height (ui16) :derived (array-dimension (bitmap-data o) 1)))
  :subclass (subclass-from-id 'bitmap-tag-data-rgb bitmap-format))

(define-swf-type bitmap-tag-data-rgba ()
  :this-var o
  :auto
  ((bitmap-format (ui8) :derived (subclass-id o 'bitmap-tag-data-rgba))
   (width (ui16) :derived (array-dimension (bitmap-data o) 0))
   (height (ui16) :derived (array-dimension (bitmap-data o) 1)))
  :subclass (subclass-from-id 'bitmap-tag-data-rgba bitmap-format))

(defmethod color-table ((a bitmap-tag-data-rgba))
  nil)
(defmethod color-table ((a bitmap-tag-data-rgb))
  nil)