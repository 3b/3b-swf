
(in-package #:vecto)

(defclass swf-image ()
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height)
   (color-type :initform :truecolor :initarg :color-type :reader color-type)
   (current-shape :initform nil :accessor current-shape)
   (shapes :initform nil :accessor shapes)
   (line-styles :initform nil :accessor line-styles)
   (fill-styles :initform nil :accessor fill-styles)
   (display-list :initform nil :accessor display-list)))

(defclass graphics-state-swf (graphics-state)
  ())

(defclass list-clipping-path (clipping-path)
  ((paths :accessor paths :initarg :paths :initform nil))
  (:default-initargs :data :unused :width :no-width :height :no-height :scratch :no-scratch))

(defmethod print-object ((clipping-path list-clipping-path) stream)
  (print-unreadable-object (clipping-path stream :type t :identity t)
    (format stream "~s" (paths clipping-path))))

(defmethod state-image ((state graphics-state-swf) width height)
  "Set the backing image of the graphics state to a shape of the
specified dimensions."
  (setf (image state)
        (make-instance 'swf-image
                       :width width
                       :height height
                       :color-type :truecolor-alpha)
        (width state) width
        (height state) height
        (clipping-path state) (make-instance 'list-clipping-path))
  (apply-matrix state (translation-matrix 0 (- height))))

(defmethod writable-clipping-data ((object list-clipping-path))
  (make-instance 'list-clipping-path
                 :paths (copy-seq (paths object))))

(defmethod copy ((state graphics-state-swf))
  (make-instance 'graphics-state-swf
                 :paths (paths state)
                 :path (path state)
                 :height (height state)
                 :width (width state)
                 :image (image state)
                 :stroke-color (copy (stroke-color state))
                 :line-width (line-width state)
                 :dash-vector (copy-seq (dash-vector state))
                 :dash-phase (dash-phase state)
                 :fill-color (copy (fill-color state))
                 :fill-source (fill-source state)
                 :join-style (join-style state)
                 :cap-style (cap-style state)
                 :transform-matrix (copy-seq (transform-matrix state))
                 :clipping-path (writable-clipping-data (clipping-path state))
                 :after-paint-fun (after-paint-fun state)
                 :font-loaders (font-loaders state)
                 :font (font state)
                 :character-spacing (character-spacing state)))

(defparameter *join-types*
  '(:round 0 :bevel 1 :none 1 :miter 2))
(defparameter *end-types*
  '(:round 0 :square 2 :none 2 :butt 1))






;;===================================
;;add clip path list to image, implement copy for swf-image, and path lists
;;  (just need a list of character IDs, but might want a class to specialize on)
;;
;;when adding a clip shape, push id onto current clip path list
;;when adding a normal shape to display list, include a list of clip paths
;;
;;when compiling display list to swf tags, add place-objects for clip paths
;;as they become active for normal shapes, counting runs with that clip active
;;to determine the clip-depth value
;;
;;test cases:
;;  create image, copy 2-3x, add clip path to each, draw something in each
;;    copy and original, so swf needs to activate each clip when drawing to
;;    corresponding image, and deactivate for next one
;;  create image with clip, copy, add more clip, draw etc..
;;    so base clip stays active while switching clips from copies
;;  etc...
;;
;;
;;






(defun vecto-rgba (color)
  (list :r (floor (* 255 (red color)))
        :g (floor (* 255 (green color)))
        :b (floor (* 255 (blue color)))
        :a (floor (* 255 (alpha color)))))
(defun vecto-rgba* (r g b a)
  (list :r (floor (* 255 r))
        :g (floor (* 255 g))
        :b (floor (* 255 b))
        :a (floor (* 255 a))))

(defun add-shape (id &key clip)
  #+nil(format t "add-shape: ~s clip=~s~%" id clip)
  #+nil(format t "state = ~s~%" *graphics-state*)
  #+nil(format t "clips = ~s~%" (clipping-path *graphics-state*))
  #+nil(format t "shapes=~s~%" (shapes (image *graphics-state*)))
  #+nil(format t "current shape=~s~%" (current-shape (image *graphics-state*)))
  #+nil(format t "dlist=~s~%" (display-list (image *graphics-state*)))

  (let* ((fill-styles (reverse (fill-styles (image *graphics-state*))))
         (line-styles (reverse (line-styles (image *graphics-state*))))
         (image (image *graphics-state*))
         (shape (reverse (current-shape image)))
         (name (or id (gensym "SHAPE-"))))
    (setf (current-shape image) nil)
    (multiple-value-bind (x y)
        (values 0 0)
        #+nil(funcall (transform-function *graphics-state*) 0 0)
      (push (3b-swf::shape name fill-styles line-styles
                           (cons (list :move-to (cons x y)) shape)
                   0 0 (width image) (height image)
                   :invert clip)
            (shapes (image *graphics-state*)))
      (if clip
          (prog1
            (push (list name :transform (transform-matrix *graphics-state*))
                  (paths (clipping-path *graphics-state*)))
            #+nil(push (list :clipped name (paths (clipping-path *graphics-state*)))
                  (display-list (image *graphics-state*))))
          (if (paths (clipping-path *graphics-state*))
              (push (list :clipped name :paths (paths (clipping-path *graphics-state*))
                          :transform (transform-matrix *graphics-state*))
                    (display-list (image *graphics-state*)))
              (push (list :place name :transform (transform-matrix *graphics-state*))
                    (display-list (image *graphics-state*))))))))


(defun swf-draw-paths (state &key (stroke t) fill paths)
  #+nil(format t "draw-paths = fill ~s, stroke ~s w=~s~%" fill stroke
          (line-width state))
  ;;(format t "xform = ~s ~%" (transform-matrix state))
  (let* ((image (image state))
         #+nil(paths (mapcar (lambda (path)
                          (transform-path (paths:path-clone path)
                                          (transform-function state)))
                        (dash-paths (or paths
                                        (paths state))
                                    (dash-vector state) (dash-phase state))))
         (paths (dash-paths (or paths
                          (paths state))
                      (dash-vector state) (dash-phase state))))
    #+nil(format t "=========================================~%")
    #+nil(format t "width=~s height=~s~%paths=~s~%"
            (width state) (height state)
            paths)

    (loop with has-fill = nil
          for path in paths
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
                                (when id
                                  (format t "reusing linestyle ~s~%"
                                          (find (cddr line-style)
                                                (line-styles image)
                                                :key 'cddr :test 'equal)))
                                (unless id
                                  (push line-style (line-styles image))
                                  (setf id (getf line-style :id))
                                  (format t "new linestyle ~s~%"
                                          (find (cddr line-style)
                                                (line-styles image)
                                                :key 'cddr :test 'equal))
                                  )
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
                              (setf has-fill t)
                              (unless id
                                  (push fill-style (fill-styles image))
                                  (setf id (getf fill-style :id)))
                                id)
                            0)
          do (push (list :fill-style fill-id)
                   (current-shape image))
          do (push (list :line-style stroke-id)
                   (current-shape image))

          do (loop with start = nil
                   for prev = nil then k
                   for k across (paths::path-knots path)
                   for interp across (paths::path-interpolations path)
                   for op =  (if (eq :straight-line interp)
                                 (list :line-to k)
                                 (with-slots (paths::control-points) interp
                                   (if (= 1 (length paths::control-points))
                                       (list :quadratic-to k (elt paths::control-points 0))
                                       (list :cubic-to k paths::control-points))))
                   unless prev
                   do (push (list :move-to k) (current-shape image))
                   and do (when (or has-fill (eq (paths::path-type path) :closed-polyline))
                            (setf start op))
                   when prev
                   do (push op (current-shape image))
                   finally (when start (push start (current-shape image)))
                   ))))

(defmethod draw-stroked-paths ((state graphics-state-swf))
  (swf-draw-paths state :stroke t))
(defmethod draw-filled-paths ((state graphics-state-swf))
  (swf-draw-paths state :stroke nil :fill t))
(defmethod draw-filled-stroked-paths ((state graphics-state-swf))
  (swf-draw-paths state :stroke t :fill t))

(defmethod %draw-string ((state graphics-state-swf) x y string)
  (swf-draw-paths state :stroke t :fill t
                  :paths (%string-paths state x y string)))

(defmethod %draw-centered-string ((state graphics-state-swf) x y string)
  (let* ((font (font state))
         (bbox (string-bounding-box string (size font) (loader font)
                                    :character-spacing (character-spacing state)))
         (width/2 (/ (- (xmax bbox) (xmin bbox)) 2.0)))
    (%draw-string (- x width/2) y string)))

(defmethod fill-image ((image-data swf-image) red green blue alpha)
  ;; not sure what the best way to handle this is...
  ;; -could draw a rectangle covering the canvas, but that probably does
  ;;  the wrong thing if alpha isn't opaque
  ;; -probably need to just clear the display list and set bg color?
  (break "fill-image")
  (push (list :clear-canvas :color (vecto-rgba* red green blue alpha))
        (display-list image-data)))


(defmethod %clip-path ((state graphics-state-swf))
  (when (current-shape (image state))
    (add-shape (gensym "pre-clip-")))
  (let ((old (fill-color state)))
    ;;(set-rgba-fill 1 0 1 1)
    (swf-draw-paths state :stroke nil :fill t)
    (add-shape (gensym "CLIP-") :clip t)
    #+nil(setf (fill-color state) old)))

(defmethod %even-odd-clip-path ((state graphics-state-swf))
  (when (current-shape (image state))
         (add-shape (gensym "are-clip-")))
  (let ((old (fill-color state)))
    ;;(set-rgba-fill 0 1 0 0)
    (swf-draw-paths state :stroke nil :fill t)
    (add-shape (gensym "CLIP-")  :clip t)
    #+nil(setf (fill-color state) old)))
;; todo: fill types (even-odd/non-zero/?)

(defmethod draw-clipping-path ((state graphics-state-swf) alpha-fun)
  (let ((data (writable-clipping-data (clipping-path state)))
        (scratch (scratch (clipping-path state)))
        (width (width state))
        (height (height state)))
    (declare (type octet-vector data scratch))
    (fill scratch 0)
    (draw-paths :paths (paths state)
                :width (width state)
                :height (height state)
                :transform-function (transform-function state)
                :draw-function (draw-clipping-path-function scratch
                                                            width
                                                            height
                                                            alpha-fun))
    (intersect-clipping-paths data scratch)))


(defmethod %set-gradient-fill (state
                               x0 y0
                               r0 g0 b0 a0
                               x1 y1
                               r1 g1 b1 a1
                               &key
                               radial
                               (extend-start t)
                               (extend-end t)
                               (domain-function 'linear-domain)
                               extended)
  (declare (ignorable extended))
  (setf r0 (float-octet r0)
        g0 (float-octet g0)
        b0 (float-octet b0)
        a0 (float-octet a0)
        r1 (float-octet r1)
        g1 (float-octet g1)
        b1 (float-octet b1)
        a1 (float-octet a1))
  (setf (fill-source state)
        `(:gradient-fill ((,x0 ,y0 ,x1 ,y1 :type ,(if radial :radial :linear))
                          ,@(unless extend-start `((0 0 0 0)))
                          (,(if extend-start 0 1) ,r0 ,g0 ,b0 ,a0)
                          ,@(if (eq domain-function 'bilinear-domain)
                                `((128 ,r1 ,g1 ,b1 ,a1)
                                  (,(if extend-end 255 254) ,r0 ,g0 ,b0 ,a0))
                                `((,(if extend-end 255 254) ,r1 ,g1 ,b1 ,a1)))
                          ,@(unless extend-end `((255 0 0 0)))))))

(defun make-matrix (vecto-transform)
  (vecto::matrix-bind (a b c d e f) vecto-transform
    (let ((m (3b-swf::matrix :sx a :ry c :tx e
                             :rx b :sy d :ty f)))
      ;;(format t "matrix ~s -> ~s~%" vecto-transform m)
      m))
)


(defun swf-sprite (id)
;;  (add-shape)
  (format t "swf-sprite: ~s~%" id)
  #+nil(format t "shapes=~s~%" (shapes (image *graphics-state*)))
  #+nil(format t "current shape=~s~%" (current-shape (image *graphics-state*)))
  #+nil(format t "dlist=~s~%" (display-list (image *graphics-state*)))
  (append
   (shapes (image *graphics-state*))
   (let ((tags nil))
     (loop with active-clips = nil
           for ((op tag . args) . rest) on (reverse (display-list (image *graphics-state*)))
           when (eq op :clipped)
           do;; (format t "clip ~s~%" (list op tag args))
           ;;(format t "  rest ~s~%" rest)
           (let* ((clips (getf args :paths))
                     (new-clips (set-difference clips active-clips
                                                :test 'equal)))
                (when new-clips
                  (loop for ((clip . clip-args) . more) on new-clips
                        for depth = (loop for (i nil . args) in rest
                                          while (eq i :clipped)
                                          while (member clip (getf args :paths))
                                          count 1)
                        ;;do (format t "clipping path ~s layers ~s + ~s~%" clip  depth (length more))
                        do (push (3b-swf::place-object
                                  clip 0
                                  :clip-layers (+ 1 (+ depth (length more)))
                                  :matrix (make-matrix (getf clip-args :transform))
)
                                 tags)
                        do (setf active-clips clips))))
           do (push (3b-swf::place-object
                     tag 0 :matrix (make-matrix (getf args :transform)))
                    tags))
     (list (3b-swf::sprite id (reverse (list* (3b-swf::end-tag)
                                              (3b-swf::show-frame)
                                              tags)))))))




(defmacro replace-functions ((&rest names) &body body)
  `(flet (,@(loop for (i j) in names
                  collect `(,i (&rest args)
                               (apply #',j args))))
     ,@body))
(defmacro with-swf-canvas ((&key width height) &body body)
  `(let ((*graphics-state* (make-instance 'graphics-state-swf)))
     (state-image *graphics-state* ,width ,height)
     ;; hack to replace the default vecto functions that care about
     ;; the image type with swf versions...
     (unwind-protect
          (progn
            ,@body)
       (clear-state *graphics-state*))))
