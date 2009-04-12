
(in-package #:vecto)

(defclass swf-image ()
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height)
   (color-type :initform :truecolor :initarg :color-type :reader color-type)
   (shapes :initform nil :accessor shapes)
   (line-styles :initform nil :accessor line-styles)
   (fill-styles :initform nil :accessor fill-styles)
   ))

;;(defconstant +png-channels+ 4)
;;(defconstant +png-color-type+ :truecolor-alpha)

(defun state-image-swf (state width height)
  "Set the backing image of the graphics state to a shape of the
specified dimensions."
  (setf (image state)
        (make-instance 'swf-image
                       :width width
                       :height height
                       :color-type :truecolor-alpha)
        (width state) width
        (height state) height
        (clipping-path state) (make-clipping-path width height))
  (apply-matrix state (translation-matrix 0 (- height))))


(defparameter *count* 0)
(defparameter *curve-tolerance* 1.0)
(defun subdivide-curve (c curve-fun line-fun
                        &key
                        (q-combine-range *curve-tolerance*)
                        (max-depth 3))
  (declare (ignorable line-fun))
  (labels
      ;; todo: check for degenerate cases?
      ;; control points between endpoints on a line, ...
      ((dist (x1 y1 x2 y2)
         (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))
       (subdivide (c depth)
         (let* ((x0 (car (aref c 0)))
                (y0 (cdr (aref c 0)))
                (x1 (car (aref c 1)))
                (y1 (cdr (aref c 1)))
                (x2 (car (aref c 2)))
                (y2 (cdr (aref c 2)))
                (x3 (car (aref c 3)))
                (y3 (cdr (aref c 3)))
                (qx1 (+ x0 (* 1.5 (- x1 x0))))
                (qy1 (+ y0 (* 1.5 (- y1 y0))))
                (qx2 (+ x3 (* 1.5 (- x2 x3))))
                (qy2 (+ y3 (* 1.5 (- y2 y3)))))
           (cond
             ((< (dist qx1 qy1 qx2 qy2) q-combine-range)
              (funcall curve-fun (/ (+ qx1 qx2) 2) (/ (+ qy1 qy2) 2) x3 y3)
              #+nil(funcall line-fun (/ (+ qx1 qx2) 2) (/ (+ qy1 qy2) 2))
              #+nil(funcall line-fun  x3 y3))
             ((>= depth max-depth) ;; possibly should pick better points?
              (incf *count*)
              (funcall curve-fun (/ (+ qx1 qx2) 2) (/ (+ qy1 qy2) 2) x3 y3))
             (t (multiple-value-bind (l r)
                    (paths::split-bezier c)
                  (subdivide l (1+ depth))
                  (subdivide r (1+ depth))))))))
    (subdivide c 0)))


(defun split-curve (k1 k2 interpolants curve-fun line-fun)
  (let ((c1 (aref interpolants 0)))
    (ecase (length interpolants)
      ;; quadratic, return it directly
      (1 (funcall curve-fun (car c1) (cdr c1) (car k2) (cdr k2)))
      ;; subdivide cubic splines a few times
      (2
       (subdivide-curve (make-array 4 :initial-contents (list k1 c1 (aref interpolants 1) k2))
                        curve-fun line-fun)))))

(defparameter *join-types*
  '(:round 0 :bevel 1 :none 1 :miter 2))
(defparameter *end-types*
  '(:round 0 :square 2 :none 2 :butt 1))
(defun vecto-rgba (color)
  (list '3b-swf::r (floor (* 255 (red color)))
        '3b-swf::g (floor (* 255 (green color)))
        '3b-swf::b (floor (* 255 (blue color)))
        '3b-swf::a (floor (* 255 (alpha color)))))
(defun add-shape (id)
  (format t "shape-id = ~s~%" id)
  (format t "shapes = ~s~%" (reverse (shapes (image *graphics-state*))))
  (format t "linestyles = ~s~%" (reverse (line-styles (image *graphics-state*))))
  (format t "fill-styles = ~s~%" (reverse (fill-styles (image *graphics-state*))))
  (let* ((image (image *graphics-state*))
         (shapes (reverse (shapes image)))
         (shape-records nil)
         (segment nil)
         (seg-line-styles nil)
         (seg-fill-styles nil)
         (line-id-map nil)
         (fill-id-map nil)
         (lid 0)
         (fid 0))
         ;; swf uses relative moves, while vecto is absolute, so track position

    (multiple-value-bind (x y)
        (funcall (transform-function *graphics-state*) 0 0)
      (labels ((make-fill (fill )
                 (destructuring-bind (&key id color gradient-fill) fill
                   (declare (ignore id))
                   (cond
                     (color
                      (make-instance
                       '3b-swf::fill-style-solid
                       ;; todo: support more options
                       '3b-swf::color (apply 'make-instance
                                             '3b-swf::rgba color)))
                     (gradient-fill
                      (destructuring-bind ((x1 y1 x2 y2) &rest colors)
                          gradient-fill
                        (format t "grad======~s ~s - ~s ~s~%" x1 y1 x2 y2)
                        (let* ((mx1 (/ (- x2 x1) 1638.4))
                               (my1 (/ (- y2 y1) 1638.4))
                               (mx2 (- my1))
                               (my2 mx1))
                          (make-instance
                           '3b-swf::fill-linear-gradient
                           ;; todo: support more options
                           '3b-swf::gradient-matrix
                           (make-instance
                            '3b-swf::matrix
                            '3b-swf::rotate-skew
                            (make-instance '3b-swf::matrix-part-fixed
                                           '3b-swf::value1 mx2
                                           '3b-swf::value2 my1)
                            '3b-swf::scale
                            (make-instance '3b-swf::matrix-part-fixed
                                           '3b-swf::value1 mx1
                                           '3b-swf::value2 my2)
                            '3b-swf::translate
                            (make-instance '3b-swf::matrix-part-translate
                                           '3b-swf::value1 (abs (/ (- x2 x1) 2.0))
                                           '3b-swf::value2 (abs (/ (- y2 y1) 2.0))))
                           ;; (- x2 x1) (- (- y2 y1))
                           ;; (- y2 y1) (- y2 y1)

                           '3b-swf::gradient
                           (make-instance
                            '3b-swf::gradient
                            '3b-swf::spread-mode 0
                            '3b-swf::interpolation-mode 0
                            '3b-swf::gradient-records
                            (loop for (i r g b a) in colors
                                  collect (make-instance
                                           '3b-swf::grad-record
                                           '3b-swf::gradient-ratio i
                                           '3b-swf::color
                                           (make-instance '3b-swf::rgba
                                                          '3b-swf::r r
                                                          '3b-swf::g g
                                                          '3b-swf::b b
                                                          '3b-swf::a a)))))))))))
               (finish-segment ()
                 (push (make-instance
                        '3b-swf::style-change-shape-record
                        ;; set defaults
                        '3b-swf::line-style 0
                        '3b-swf::fill-style-0 0
                        '3b-swf::fill-style-1 0
                        ;;
                        ;;'3b-swf::move-to (make-instance '3b-swf::state-move-to
                        ;;                                '3b-swf::delta-x 0
                        ;;                                '3b-swf::delta-y 0)
                        ;; need both style arrays if either is set
                        '3b-swf::fill-styles
                        (make-instance
                         '3b-swf::fill-style-array
                         '3b-swf::fill-styles
                         (loop for i in (reverse seg-fill-styles)
                               collect (make-fill i)))
                        '3b-swf::line-styles
                        (make-instance
                         '3b-swf::line-style-array
                         '3b-swf::line-styles
                         (loop for i in (reverse seg-line-styles)
                               collect (make-instance
                                        '3b-swf::line-style-2
                                        ;; todo: support more options
                                        '3b-swf::width (* 1 (getf i :width))
                                        '3b-swf::join-style (getf *join-types*
                                                                  (getf i :join))
                                        '3b-swf::start-cap-style (getf
                                                                  *end-types*
                                                                  (getf i :cap))
                                        '3b-swf::end-cap-style (getf
                                                                *end-types*
                                                                (getf i :cap))
                                        '3b-swf::no-close (eq (getf i :open)
                                                              :open-polyline)
                                        '3b-swf::color (apply 'make-instance
                                                              '3b-swf::rgba
                                                              (getf i :color))))))
                       shape-records)
                 (format t "finish-segment~%sr=~s~%sls=~s~%s=~s~%"
                         shape-records seg-line-styles segment)
                 (setf shape-records (append segment shape-records))
                 (setf segment nil)
                 ;; reset index and clear style list

                 (setf lid 0)
                 (setf line-id-map nil)
                 (setf seg-line-styles nil)
                 (setf fid 0)
                 (setf fill-id-map nil)
                 (setf seg-fill-styles nil))
               (add-lid (id)
                 (when (>= lid 31)
                   ;; list is full, write a state-change record
                   ;; and write out the records using that those styles
                   (finish-segment))
                 ;; add new style to list
                 (let ((ls (find id (line-styles image)
                                 :key (lambda (x) (getf x :id)))))
                   (push ls seg-line-styles)
                   (assert ls)
                   (format t "added lid ~s ~s~%~s~% ~s~%"
                           id (line-styles image)
                           ls
                           seg-line-styles))
                 (format t "map=~s~%" line-id-map)
                 ;; and save it on the id->index map and return index
                 (setf (getf line-id-map id) (incf lid)))
               (add-fid (id)
                 (when (>= fid 31)
                   (finish-segment))
                 (let ((fs (find id (fill-styles image)
                                 :key (lambda (x) (getf x :id)))))
                   (push fs seg-fill-styles)
                   (assert fs)
                   (format t "added fid ~s ~s~%~s~% ~s~%"
                           id (fill-styles image)
                           fs
                           seg-fill-styles))
                 (format t "map=~s~%" fill-id-map)
                 (setf (getf fill-id-map id) (incf fid)))
               (line-style (id)
                 (let ((index (if (eql id 0)
                                  0
                                  (or (getf line-id-map id)
                                      (add-lid id)))))
                   ;; todo: check for redundant state changes, combine
                   ;; adjacent changes
                   ;; (redundant as in setting to current state, or changing a state
                   ;;  twice in a row)
                   (push
                    (make-instance
                     '3b-swf::style-change-shape-record
                     '3b-swf::line-style index)
                    segment)))
               (fill-style (id)
                 (let ((index (if (eql id 0)
                                  0
                                  (or (getf fill-id-map id)
                                      (add-fid id)))))
                   (push
                    (make-instance
                     '3b-swf::style-change-shape-record
                     '3b-swf::fill-style-0 index)
                    segment)))
               (add-move (nx ny)
                 (push (make-instance
                        '3b-swf::style-change-shape-record
                        '3b-swf::move-to (make-instance '3b-swf::state-move-to
                                                        '3b-swf::delta-x nx
                                                        '3b-swf::delta-y ny))
                       segment)
                 (setf x (floor nx 0.05)
                       y (floor ny 0.05)))
               (dx (nx)
                 (let* ((nxt (truncate  nx 0.05))
                        (dx (- nxt x)))
                   (setf x nxt)
                   (/ dx 20.0)))
               (dy (ny)
                 (let* ((nyt (truncate ny 0.05))
                        (dy (- nyt y)))
                   (setf y nyt)
                   (/ dy 20.0)))
               (add-line (nx ny)
                 (let ((dx (dx nx))
                       (dy (dy ny)))
                   (unless (and (zerop dx) (zerop dy))
                     (push (make-instance
                            '3b-swf::straight-edge-shape-record
                            '3b-swf::delta-x (if (zerop dx) nil dx)
                            '3b-swf::delta-y (if (zerop dy) nil dy))
                           segment))))
               (add-curve (cx cy ax ay)
                 (push (make-instance
                        '3b-swf::curved-edge-shape-record
                        '3b-swf::control-delta-x (dx cx)
                        '3b-swf::control-delta-y (dy cy)
                        '3b-swf::anchor-delta-x (dx ax)
                        '3b-swf::anchor-delta-y (dy ay)
)
                       segment)))
        (loop for i in shapes
              do (ecase (car i)
                   (:line-style (line-style (second i)))
                   (:fill-style (fill-style (second i)))
                   (:move-to (add-move (car (second i)) (cdr (second i))))
                   (:line-to (add-line (car (second i)) (cdr (second i))))
                   (:curve-to (split-curve (cons (/ x 20.0) (/ y 20.0))
                                           (second i)
                                           (third i)
                                           #'add-curve #'add-line))))
        (finish-segment)
        (format t "shapes=~%~s~%" (reverse shape-records))


        (make-instance '3b-swf::define-shape-4-tag
                       '3b-swf::shape-id id
                       '3b-swf::shape-bounds (3b-swf::make-rect
                                              0 0 (width image) (height image))
                       '3b-swf::edge-bounds (3b-swf::make-rect
                                             0 0 (width image) (height image))
                       '3b-swf::uses-scaling-strokes t ;;??
                       '3b-swf::uses-fill-winding-rule nil
                       '3b-swf::shapes (make-instance
                                        '3b-swf::shape-with-style
                                        '3b-swf::fill-styles
                                        (make-instance
                                         '3b-swf::fill-style-array
                                         '3b-swf::fill-styles nil)
                                        '3b-swf::line-styles
                                        (make-instance
                                         '3b-swf::line-style-array
                                         '3b-swf::line-styles
                                         (list (make-instance
                                                '3b-swf::line-style-2
                                                '3b-swf::width 8
                                                '3b-swf::color (3b-swf::rgba :r 22 :g 222 :b 0 :a 255)
                                                '3b-swf::join-style 0
                                                '3b-swf::start-cap-style 0
                                                '3b-swf::end-cap-style 0
                                                '3b-swf::no-close t)))
                                        '3b-swf::shape-records (reverse shape-records)))))))


(defun swf-draw-paths (state &key (stroke t) fill paths)
  (let* ((image (image state))
         (paths (mapcar (lambda (path)
                          (transform-path (paths:path-clone path)
                                          (transform-function state)))
                        (dash-paths (or paths
                                        (paths state))
                                    (dash-vector state) (dash-phase state)))))
    (format t "=========================================~%")
    (format t "width=~s height=~s~%paths=~s~%"
            (width state) (height state)
            paths)

    (loop for path in paths
          for open = (paths::path-type path)
          for stroke-id = (if stroke
                              (let* ((line-style
                                      (list :id (gensym)
                                            :width (line-width state)
                                            :join (join-style state)
                                            :cap (cap-style state)
                                            :color (vecto-rgba
                                                    (stroke-color state))
                                            :open open))
                                     (id (getf (find (cddr line-style)
                                                     (line-styles image)
                                                     :key 'cddr :test 'equal)
                                               :id)))
                                (unless id
                                  (push line-style (line-styles image))
                                  (setf id (getf line-style :id)))
                                id)
                              0)
          for fill-id = (if fill
                            (let* ((fill-style
                                    (if (fill-source state)
                                        (list* :id (gensym)
                                               (fill-source state))
                                        (list :id (gensym)
                                              :color (vecto-rgba
                                                      (fill-color state)))))
                                   (id (getf (find (cddr fill-style)
                                                   (fill-styles image)
                                                   :key 'cddr :test 'equal)
                                             :id)))
                              (unless id
                                  (push fill-style (fill-styles image))
                                  (setf id (getf fill-style :id)))
                                id)
                            0)
          do (push (list :fill-style fill-id)
                   (shapes image))
          do (push (list :line-style stroke-id)
                   (shapes image))

          do (loop with start = nil
                   for prev = nil then k
                   for k across (paths::path-knots path)
                   for interp across (paths::path-interpolations path)
                   for op =  (if (eq :straight-line interp)
                                 (list :line-to k)
                                 (with-slots (paths::control-points) interp
                                   (list :curve-to k paths::control-points)))
                   unless prev
                   do (push (list :move-to k) (shapes image))
                   and do (when (eq (paths::path-type path) :closed-polyline)
                            (setf start op))
                   when prev
                   do (push op (shapes image))
                   finally (when start (push start (shapes image)))
                   ))
))
(defun swf-stroke ()
  (swf-draw-paths *graphics-state* :stroke t)
  (clear-paths *graphics-state*)
)

(defun swf-draw-string (x y string)
  (swf-draw-paths *graphics-state* :stroke t :fill t
                  :paths (%string-paths *graphics-state* x y string)))

(defun swf-draw-centered-string (x y string)
  (let* ((font (font *graphics-state*))
         (bbox (string-bounding-box string (size font) (loader font)))
         (width/2 (/ (- (xmax bbox) (xmin bbox)) 2.0)))
    (swf-draw-string (- x width/2) y string)))

(defun swf-clear-canvas ()
  (with-graphics-state
    (setf (transform-matrix *graphics-state*) (identity-matrix))
    (rectangle 0 0 (width *graphics-state*) (height *graphics-state*))
    (swf-draw-paths *graphics-state* :fill t :stroke nil)
    )
)
#+nil
(defun stroke-to-paths ()
  (let ((paths (state-stroke-paths *graphics-state*)))
    (clear-paths *graphics-state*)
    (setf (paths *graphics-state*) paths)
    (%close-subpath *graphics-state*)))

(defun swf-fill-path ()
  (swf-draw-paths *graphics-state* :stroke nil :fill t)
  (after-painting *graphics-state*)
  (clear-paths *graphics-state*))

#+nil
(defun even-odd-fill ()
  (draw-even-odd-filled-paths *graphics-state*)
  (after-painting *graphics-state*)
  (clear-paths *graphics-state*))


(defun swf-fill-and-stroke ()
  (let ((state *graphics-state*))
    (close-paths (paths state))
    (swf-draw-paths *graphics-state* :stroke t :fill t)
    (clear-paths *graphics-state*)))

#+nil
(defun even-odd-fill-and-stroke ()
  (draw-even-odd-filled-paths *graphics-state*)
  (draw-stroked-paths *graphics-state*)
  (after-painting *graphics-state*)
  (clear-paths *graphics-state*))



(defun swf-set-gradient-fill (x0 y0
                              r0 g0 b0 a0
                              x1 y1
                              r1 g1 b1 a1
                              &key
                              (extend-start t)
                              (extend-end t)
                              (domain-function 'linear-domain))
  (setf r0 (float-octet r0)
        g0 (float-octet g0)
        b0 (float-octet b0)
        a0 (float-octet a0)
        r1 (float-octet r1)
        g1 (float-octet g1)
        b1 (float-octet b1)
        a1 (float-octet a1))
  (setf (fill-source *graphics-state*)
        `(:gradient-fill ((,x0 ,y0 ,x1 ,y1)
                          ,@(unless extend-start `((0 0 0 0)))
                          (1 ,r0 ,g0 ,b0 ,a0)
                          ,@(if (eq domain-function 'bilinear-domain)
                                `((128 ,r1 ,g1 ,b1 ,a1) (254 ,r0 ,g0 ,b0 ,a0))
                                `((254 ,r1 ,g1 ,b1 ,a1)))
                          ,@(unless extend-end `((255 0 0 0)))))))









(defmacro replace-functions ((&rest names) &body body)
  `(flet (,@(loop for (i j) in names
                  collect `(,i (&rest args)
                               (apply #',j args))))
     ,@body))
(defmacro with-swf-canvas ((&key width height) &body body)
  `(replace-functions
      ((save-png add-shape)
       (draw-string swf-draw-string)
       (draw-centered-string swf-draw-centered-string)
       (stroke swf-stroke)
       (fill-and-stroke swf-fill-and-stroke)
       (fill-path swf-fill-path)
       (set-gradient-fill swf-set-gradient-fill)
       (clear-canvas swf-clear-canvas))
    (let ((*graphics-state* (make-instance 'graphics-state)))
       (state-image-swf *graphics-state* ,width ,height)
       ;; hack to replace the default vecto functions that care about
       ;; the image type with swf versions...
       (unwind-protect
            (progn
              ,@body)
         (clear-state *graphics-state*)))))
