
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


(defmacro with-swf-canvas ((&key width height) &body body)
  `(let ((*graphics-state* (make-instance 'graphics-state)))
     (state-image-swf *graphics-state* ,width ,height)
     (unwind-protect
          (progn
            ,@body)
       (clear-state *graphics-state*))))

(dist (a b)
         (sqrt (+ (expt (- (car a) (car b)) 2)
                  (expt (- (cdr a) (cdr b)) 2))))

(defparameter *count* 0)
(defparameter *curve-tolerance* 1.0)
(defun subdivide-curve (c curve-fun line-fun
                        &key
                        (q-combine-range *curve-tolerance*)
                        (max-depth 3))
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
  '(:round 0 :bevel 1 :miter 2))
(defparameter *end-types*
  '(:round 0 :none 1 :butt 2))
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
         #+nil(seg-fill-styles nil)
         (line-id-map nil)
         #+nil(fill-id-map nil)
         (lid 0)
         #+nil(fid 0))
         ;; swf uses relative moves, while vecto is absolute, so track position

    (multiple-value-bind (x y)
        (funcall (transform-function *graphics-state*) 0 0)
     (labels ((finish-segment ()
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
                       (make-instance '3b-swf::fill-style-array
                                      '3b-swf::fill-styles nil)
                       '3b-swf::line-styles
                       (make-instance
                        '3b-swf::line-style-array
                        '3b-swf::line-styles
                        (loop for i in (reverse seg-line-styles)
                              collect (make-instance
                                       '3b-swf::line-style-2
                                       ;; todo: support more options
                                       '3b-swf::width (* 0.5 (getf i :width))
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

                )
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
              (line-style (id)
                (let ((index (or (getf line-id-map id)
                                 (add-lid id))))
                  ;; todo: check for redundant state changes, combine
                  ;; adjacent changes
                  ;; (redundant as in setting to current state, or changing a state
                  ;;  twice in a row)
                  (push
                   (make-instance
                    '3b-swf::style-change-shape-record
                    '3b-swf::line-style index)
                   segment)))
              (add-move (nx ny)
                (push (make-instance
                       '3b-swf::style-change-shape-record
                       '3b-swf::move-to (make-instance '3b-swf::state-move-to
                                                       '3b-swf::delta-x nx
                                                       '3b-swf::delta-y ny))
                      segment)
                (setf x nx y ny))
              (add-line (nx ny)
                (let ((dx (- nx x))
                      (dy (- ny y)))
                  (push (make-instance
                         '3b-swf::straight-edge-shape-record
                         '3b-swf::delta-x (if (zerop dx) nil dx)
                         '3b-swf::delta-y (if (zerop dy) nil dy))
                        segment))
                (setf x nx y ny))
              (add-curve (cx cy ax ay)
                (push (make-instance
                       '3b-swf::curved-edge-shape-record
                       '3b-swf::control-delta-x (- cx x)
                       '3b-swf::control-delta-y (- cy y)
                       '3b-swf::anchor-delta-x (- ax cx)
                       '3b-swf::anchor-delta-y (- ay cy))
                      segment)
                (setf x ax y ay))

              )
       (loop for i in shapes
             do (ecase (car i)
                  (:line-style (line-style (second i)))
                  (:move-to (add-move (car (second i)) (cdr (second i))))
                  (:line-to (add-line (car (second i)) (cdr (second i))))
                  (:curve-to (split-curve (cons x y)
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
                                       '3b-swf::shape-records (reverse shape-records))
                      )))))
(defun swf-stroke ()
  (let* ((state *graphics-state*)
         (image (image state))
         (paths (mapcar (lambda (path)
                          (transform-path (paths:path-clone path)
                                          (transform-function state)))
                        (dash-paths (paths state)
                                    (dash-vector state) (dash-phase state)))))
    (format t "=========================================~%")
    (format t "width=~s height=~s~%paths=~s~%"
            (width state) (height state)
            paths
            )

    (loop for path in paths
          for open = (paths::path-type path)
          for id = (let* ((line-style
                           (list :id (gensym) :width (line-width state)
                                 :join (join-style state) :cap (cap-style state)
                                 :color (vecto-rgba (stroke-color state)) :open open))
                          (id (getf (find (cddr line-style)
                                         (line-styles image)
                                         :key 'cddr :test 'equal) :id)))
                     (unless id
                       (push line-style (line-styles image))
                       (setf id (getf line-style :id)))
                     (push (list :line-style id)
                           (shapes image))
                     id)
          do (loop for prev = nil then k
                   for k across (paths::path-knots path)
                   for interp across (paths::path-interpolations path)
                   unless prev
                   do (push (list :move-to k) (shapes image))
                   when prev
                   do (if (eq :straight-line interp)
                          (push (list :line-to k) (shapes image))
                          ;; fixme: curves..
                          (with-slots (paths::control-points) interp
                            ;;(break paths::control-points)
                            (push (list :curve-to k
                                        paths::control-points)
                                  (shapes image))))
                   ))
                                        ;(draw-stroked-paths *graphics-state*)
    (clear-paths *graphics-state*)))

#+nil
(defun stroke-to-paths ()
  (let ((paths (state-stroke-paths *graphics-state*)))
    (clear-paths *graphics-state*)
    (setf (paths *graphics-state*) paths)
    (%close-subpath *graphics-state*)))

#+nil
(defun fill-path ()
  (draw-filled-paths *graphics-state*)
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
    (swf-stroke)
  ;;(draw-filled-paths *graphics-state*)
  ;;(draw-stroked-paths *graphics-state*)
    (clear-paths *graphics-state*)))

#+nil
(defun even-odd-fill-and-stroke ()
  (draw-even-odd-filled-paths *graphics-state*)
  (draw-stroked-paths *graphics-state*)
  (after-painting *graphics-state*)
  (clear-paths *graphics-state*))







(defun test (character-id)
  (with-swf-canvas (:width 100 :height 100)
    (set-line-width 5.0)
    ;; red stroke
    (set-rgb-stroke 1 0 0)
    (move-to 10 10)
    (line-to 90 90)
    (swf-stroke)
    ;; green stroke
    (set-rgb-stroke 0 1 0)
    (move-to 10 90)
    (line-to 90 10)
    (swf-stroke)
    ;; blue+alpha transform stroke
    (set-rgba-stroke 0 0 1 0.5)
    (flet ((elbow (radians)
	     (with-graphics-state
	       (translate 50 50)
	       (rotate radians)
	       (scale 0.25 0.25)
	       (move-to 0 0)
	       (curve-to 0 100
			 0 100
			 100 100)
	       (set-line-width 5.0)
               ;;(break *graphics-state*)
	       (swf-stroke))))
      (let* ((rotations 25)
	     (step (/ (* pi 2) rotations)))
	(dotimes (i rotations)
	  (elbow (* i step)))))
    (add-shape character-id)))

(test 2)

(defun rectangle-test (id)
  (with-swf-canvas (:width 100 :height 100)
    (rectangle 10 10 50 50)
    (swf-stroke)
    (rectangle 10 10 50 50)
    (swf-stroke)
    (add-shape id)))

(rectangle-test 4)

(defun dash-test (character-id)
  (with-swf-canvas (:width 200 :height 200)
    (rectangle 10 10 125 125)
    (set-rgba-fill 0.3 0.5 0.9 0.5)
    (set-line-width 4)
    (set-dash-pattern #(10 10) 5)
    (swf-fill-and-stroke)
    (add-shape character-id)))

(dash-test 3)

(defun curve-test (id)
  (with-swf-canvas (:width 100 :height 100)
    ;;(rotate-degrees 15)
    (set-rgb-stroke 1 0 0)
    (translate 0 00)
    (set-line-width 3)
    (flet ((a ()
             (move-to 0 0)
             (curve-to 0 100
                       100 0
                       100 100)
             (swf-stroke)))
      (a)
      (set-rgb-stroke 0 1 0)
      (set-line-width 2)
      (set-dash-pattern #(2 2) 1)
      (a)
      (set-dash-pattern nil nil)
      (set-rgb-stroke 0 0 1)
      (set-line-width 1)
      (a))
    (add-shape id)))

(defun curve-test2 (id)
  (with-swf-canvas (:width 100 :height 100)
    ;;(rotate-degrees 15)
    (set-rgb-stroke 1 0 0)
    (translate 0 00)
    (set-line-width 1)
    (flet ((a ()
             (move-to 0 0)
             (curve-to 0 100
                       100 0
                       100 100)
             (swf-stroke)))
      (a))
    (add-shape id)))


(defun center-test (string id)
  (with-swf-canvas (:width 200 :height 100)
    (let ((font (get-font #p"c:/windows/fonts/times.ttf")))
      (set-font font 36)
      (draw-centered-string 100 25 string)
      (set-rgba-fill 1 0 0 0.5)
      (set-rgb-stroke 0 0 0)
      (centered-circle-path 100 25 5)
      (swf-stroke)
      (add-shape id))))

(center-test "foo" 123)
