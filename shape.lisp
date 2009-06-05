(in-package :3b-swf)

;;; intermediate format for shape data, for converting from vecto/svg,
;;; and possibly saving to svg

;;; need to be able to :
;;;   define a shape (not sure if name should be required or optional?)
;;;   define a clip mask, and specify which shapes it applies to
;;;   define a sprite containing a set of shapes/clip masks
;;; not sure if it would be better to format it similar to swf, with just a
;;; list of lists, each corresponding to a tag like:
;; ((shape foo ...)
;;  (shape bar ...)
;;  (shape baz ...)
;;  (shape hoge ...)
;;  (shape piyo ...)
;;  (sprite foo (clip bar (baz hoge)) baz hoge piyo))
;;;
;;; or try for something more structural like:
;; (sprite
;;  (shape foo ...)
;;  (clip
;;   (shape bar ...)
;;   (shape baz ...)
;;   (shape hoge ...))
;;  (shape piyo ...))

;; sprites can be nested, so putting shapes inside the sprite is probably a
;; bit odd, so for now just staying with the swf structure, and
;; just writing wrappers that translate fairly directly to the
;; low level structs...


;; todo:
;;  shape/shape records
;;  line/fill styles

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
       (lerp (a b s tt)
         (+ (* s a) (* tt b)))
       (split (x0 y0 x1 y1 x2 y2 x3 y3 &optional (tt 0.5))
         ;; based on examples from comp.graphics.algorithms faq 4.02
         (let* ((s (- 1.0 tt))
                ;; f00t
                (ax1 (lerp x0 x1 s tt))
                (ay1 (lerp y0 y1 s tt))
                (f01tx (lerp x1 x2 s tt))
                (f01ty (lerp y1 y2 s tt))
                (bx2 (lerp x2 x3 s tt))
                (by2 (lerp y2 y3 s tt))
                (ax2 (lerp ax1 f01tx s tt))
                (ay2 (lerp ay1 f01ty s tt))
                (bx1 (lerp f01tx bx2 s tt))
                (by1 (lerp f01ty by2 s tt))
                (cx (lerp ax2 bx1 s tt))
                (cy (lerp ay2 by1 s tt)))
           (values x0 y0 ax1 ay1 ax2 ay2 cx cy
                   bx1 by1 bx2 by2 x3 y3)))
       (subdivide (x0 y0 x1 y1 x2 y2 x3 y3 depth)
         (let* ((qx1 (+ x0 (* 1.5 (- x1 x0))))
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
             (t (multiple-value-call #'subdivide2
                  (split x0 y0 x1 y1 x2 y2 x3 y3)
                  (1+ depth))))))
       (subdivide2 (ax0 ay0 ax1 ay1 ax2 ay2
                        cx cy
                        bx1 by1 bx2 by2 bx3 by3
                        depth)
         (subdivide ax0 ay0 ax1 ay1 ax2 ay2 cx cy depth)
         (subdivide cx cy bx1 by1 bx2 by2 bx3 by3 depth)))
    (subdivide (car (aref c 0))
               (cdr (aref c 0))
               (car (aref c 1))
               (cdr (aref c 1))
               (car (aref c 2))
               (cdr (aref c 2))
               (car (aref c 3))
               (cdr (aref c 3))
                0)))


(defun split-curve (k1 k2 interpolants curve-fun line-fun)
  (let ((c1 (aref interpolants 0)))
    (ecase (length interpolants)
      ;; quadratic, return it directly
      (1 (funcall curve-fun (car c1) (cdr c1) (car k2) (cdr k2)))
      ;; subdivide cubic splines a few times
      (2
       (subdivide-curve (make-array 4 :initial-contents (list k1 c1 (aref interpolants 1) k2))
                        curve-fun line-fun)))))



(defun vecto-linear-gradient (x1 y1 x2 y2 colors)
  (let* ((mx1 (/ (- x2 x1) 1638.4))
         (my1 (/ (- y2 y1) 1638.4))
         (mx2 (- my1))
         (my2 mx1))
    (make-instance '%3b-swf::fill-linear-gradient
                   '%3b-swf::gradient-matrix
                   (3b-swf::matrix :rx mx2 :ry my1
                                   :sx mx1 :sy my2
                                   :tx (abs (/ (- x2 x1) 2.0))
                                   :ty (abs (/ (- y2 y1) 2.0)))
                   '%3b-swf::gradient
                   (make-instance
                    '%3b-swf::gradient
                    '%3b-swf::spread-mode 0
                    '%3b-swf::interpolation-mode 0
                    '%3b-swf::gradient-records
                    (loop for (i r g b a) in colors
                          collect (make-instance
                                   '%3b-swf::grad-record
                                   '%3b-swf::gradient-ratio i
                                   '%3b-swf::color
                                   (3b-swf::rgba :r r :g g :b b :a a)))))))

(defun vecto-radial-gradient (cx cy rx ry colors)
  (let* ((sx (/ (* 2 rx) 1638.4))
         ;(/ (* (/ 2048 20)) (/ 32768 20.0)) (/ 1.0 16)
         (sy (/ (* 2 ry) 1638.4)))
    ;;(format t "radial gradient... ~s,~s - ~s, ~s~% ~s~%" cx cy rx ry colors)
    (make-instance '%3b-swf::fill-radial-gradient
                   '%3b-swf::gradient-matrix
                   (3b-swf::matrix :sx sx :sy sy
                                   :tx cx
                                   :ty cy)
                   '%3b-swf::gradient
                   (make-instance
                    '%3b-swf::gradient
                    '%3b-swf::spread-mode 0
                    '%3b-swf::interpolation-mode 0
                    '%3b-swf::gradient-records
                    (loop for (i r g b a) in colors
                          collect (make-instance
                                   '%3b-swf::grad-record
                                   '%3b-swf::gradient-ratio i
                                   '%3b-swf::color
                                   (3b-swf::rgba :r r :g g :b b :a a)))))))

(defparameter *join-types*
  '(:round 0 :bevel 1 :none 1 :miter 2))
(defparameter *end-types*
  '(:round 0 :square 2 :none 2 :butt 1))

;;; fixme: combine this with shape* below, and unify the fill data formats
;;; fixme: figure out what format this actually uses, and update docstrings
;; (or update the code)
(defun shape (id fill-styles line-styles shape-data x1 y1 x2 y2 &key invert)
  "id = character-id of shape
fill-styles = alist? of fill-style names to fill-style specs
line-styles = alist? of line-style names to line-style specs
shape-data = list of shape commands:
 (:line-style name) -- change line style
 (:fill-style name) -- change-fill style 1
 (:fill-style-2 name) -- change-fill style 2
 (:move-to x y) -- move without drawing to absolute x,y
 (:line-to x y) -- draw straight line to absolute x,y
 (:quadratic-to x y cx cy) -- quadratic spline to x,y, control point cx,cy
 (:cubic-to x y cx1 cy1 cx2 cy2 -- cubic spline (converted to quadratic internally, probably not very well)
  --- fixme: ^^^ these are probably wrong...
"
  (let* ((shape-records nil)
         (segment nil)
         (seg-line-styles nil)
         (seg-fill-styles nil)
         (line-id-map nil)
         (fill-id-map nil)
         (lid 0)
         (fid 0)
         (x 0)
         (y 0)
         (init-line-styles nil)
         (init-fill-styles nil))
    ;; swf uses relative moves, while vecto is absolute, so track position

    (labels ((make-fill (fill )
               (destructuring-bind (&key id color gradient-fill) fill
                 (declare (ignore id))
                 (cond
                   (color
                    ;;(setf (getf color :a) 200)
                    (make-instance
                     '%3b-swf::fill-style-solid
                     ;; todo: support more options
                     '%3b-swf::color (apply '3b-swf::rgba color)))
                   (gradient-fill
                    (destructuring-bind ((x1 y1 x2 y2 &key type) &rest colors)
                        gradient-fill
                      (format t "grad======~s ~s - ~s ~s~%" x1 y1 x2 y2)
                      (ecase type
                        ((:linear nil)
                         (vecto-linear-gradient x1 y1 x2 y2 colors))
                        (:radial
                         (vecto-radial-gradient x1 y1 x2 y2 colors))))))))

             (finish-segment ()
               (if (or init-fill-styles init-line-styles)
                   (push (make-instance
                          '%3b-swf::style-change-shape-record
                          ;; set defaults
                          '%3b-swf::line-style 0
                          '%3b-swf::fill-style-0 0
                          '%3b-swf::fill-style-1 0
                          ;;
                          ;;'3b-swf::move-to (make-instance '3b-swf::state-move-to
                          ;;                                '3b-swf::delta-x 0
                          ;;                                '3b-swf::delta-y 0)
                          ;; need both style arrays if either is set
                          '%3b-swf::fill-styles
                          (make-instance
                           '%3b-swf::fill-style-array
                           '%3b-swf::fill-styles
                           (loop for i in (reverse seg-fill-styles)
                                 collect (make-fill i)))
                          '%3b-swf::line-styles
                          (make-instance
                           '%3b-swf::line-style-array
                           '%3b-swf::line-styles
                           (loop for i in (reverse seg-line-styles)
                                 collect
                                 (make-instance
                                       '%3b-swf::line-style-2
                                       ;; todo: support more options
                                       '%3b-swf::width (* 1 (getf i :width))
                                       '%3b-swf::join-style (getf *join-types*
                                                                  (getf i :join))
                                       '%3b-swf::start-cap-style (getf
                                                                  *end-types*
                                                                  (getf i :cap))
                                       '%3b-swf::end-cap-style (getf
                                                                *end-types*
                                                                (getf i :cap))
                                       '%3b-swf::no-close (eq (getf i :open)
                                                              :open-polyline)
                                       '%3b-swf::color (apply '3b-swf::rgba
                                                              (getf i :color)))
                                 #+nil(make-instance
                                  '%3b-swf::line-style
                                  ;; todo: support more options
                                  '%3b-swf::width (* 1 (getf i :width))
                                  '%3b-swf::color (apply '3b-swf::rgba
                                                         (getf i :color))))))
                         shape-records)

                   (progn
                     (setf init-fill-styles
                           (make-instance
                            '%3b-swf::fill-style-array
                            '%3b-swf::fill-styles
                            (loop for i in (reverse seg-fill-styles)
                                  collect (make-fill i))))
                     (setf init-line-styles
                           (make-instance
                            '%3b-swf::line-style-array
                            '%3b-swf::line-styles
                            (loop for i in (reverse seg-line-styles)
                                  collect
                                  (make-instance
                                        '%3b-swf::line-style-2
                                        ;; todo: support more options
                                        '%3b-swf::width (* 1 (getf i :width))
                                        '%3b-swf::join-style (getf *join-types*
                                                                   (getf i :join))
                                        '%3b-swf::start-cap-style (getf
                                                                   *end-types*
                                                                   (getf i :cap))
                                        '%3b-swf::end-cap-style (getf
                                                                 *end-types*
                                                                 (getf i :cap))
                                        '%3b-swf::no-close (eq (getf i :open)
                                                               :open-polyline)
                                        '%3b-swf::color (apply '3b-swf::rgba
                                                               (getf i :color)))

                                  #+nil(make-instance
                                   '%3b-swf::line-style
                                   ;; todo: support more options
                                   '%3b-swf::width (* 1 (getf i :width))
                                   '%3b-swf::color (apply '3b-swf::rgba
                                                          (getf i :color))))))))

               #+nil(format t "finish-segment~%sr=~s~%sls=~s~%s=~s~%"
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
               (let ((ls (find id line-styles
                               :key (lambda (x) (getf x :id)))))
                 (push ls seg-line-styles)
                 (assert ls)
                 #+nil(format t "added lid ~s ~s~%~s~% ~s~%"
                         id line-styles
                         ls
                         seg-line-styles))
               #+nil(format t "map=~s~%" line-id-map)
               ;; and save it on the id->index map and return index
               (setf (getf line-id-map id) (incf lid)))
             (add-fid (id)
               (when (>= fid 31)
                 (finish-segment))
               (let ((fs (find id fill-styles
                               :key (lambda (x) (getf x :id)))))
                 (push fs seg-fill-styles)
                 (assert fs)
                 #+nil(format t "added fid ~s ~s~%~s~% ~s~%"
                         id fill-styles
                         fs
                         seg-fill-styles))
               #+nil(format t "map=~s~%" fill-id-map)
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
                   '%3b-swf::style-change-shape-record
                   '%3b-swf::line-style index)
                  segment)))
             (fill-style (id)
               (let ((index (if (eql id 0)
                                0
                                (or (getf fill-id-map id)
                                    (add-fid id)))))
                 (push
                  (if (not invert)
                       (make-instance '%3b-swf::style-change-shape-record
                                      '%3b-swf::fill-style-1 index)
                       (make-instance '%3b-swf::style-change-shape-record
                                      '%3b-swf::fill-style-0 index))
                  segment)))
             (add-move (nx ny)
               (push (make-instance
                      '%3b-swf::style-change-shape-record
                      '%3b-swf::move-to (make-instance '%3b-swf::state-move-to
                                                       '%3b-swf::delta-x nx
                                                       '%3b-swf::delta-y ny))
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
                          '%3b-swf::straight-edge-shape-record
                          '%3b-swf::delta-x (if (zerop dx) nil dx)
                          '%3b-swf::delta-y (if (zerop dy) nil dy))
                         segment))))
             (add-curve (cx cy ax ay)
               (push (make-instance
                      '%3b-swf::curved-edge-shape-record
                      '%3b-swf::control-delta-x (dx cx)
                      '%3b-swf::control-delta-y (dy cy)
                      '%3b-swf::anchor-delta-x (dx ax)
                      '%3b-swf::anchor-delta-y (dy ay)
                      )
                     segment)))
      (loop for i in shape-data
            do (ecase (car i)
                 (:line-style (line-style (second i)))
                 (:fill-style (fill-style (second i)))
                 (:move-to (add-move (car (second i)) (cdr (second i))))
                 (:line-to (add-line (car (second i)) (cdr (second i))))
                 (:quadratic-to (add-curve (car (second i)) (cdr (second i))
                                           (car (third i)) (cdr (third i))))
                 (:cubic-to (split-curve (cons (/ x 20.0) (/ y 20.0))
                                         (second i)
                                         (third i)
                                         #'add-curve #'add-line))))
      (finish-segment)
      #+nil(format t "shapes=~%~s~%" (reverse shape-records))

      (make-instance '%3b-swf::define-shape-4-tag
                     '%3b-swf::character-id id
                     '%3b-swf::bounds (3b-swf::rect x1 y1 x2 y2)
                     '%3b-swf::edge-bounds (3b-swf::rect x1 y1 x2 y2)
                     '%3b-swf::uses-scaling-strokes t ;;??
                     '%3b-swf::uses-non-scaling-strokes nil ;;??
                     '%3b-swf::uses-fill-winding-rule nil
                     '%3b-swf::shapes (make-instance
                                       '%3b-swf::shape-with-style
                                       '%3b-swf::fill-styles
                                       init-fill-styles
                                       '%3b-swf::line-styles
                                       init-line-styles
                                       '%3b-swf::shape-records (reverse shape-records)))

      #+nil(make-instance '%3b-swf::define-shape-3-tag
                     '%3b-swf::character-id id
                     '%3b-swf::bounds (3b-swf::rect x1 y1 x2 y2)
                     '%3b-swf::shapes (make-instance
                                       '%3b-swf::shape-with-style
                                       '%3b-swf::fill-styles
                                       init-fill-styles
                                       '%3b-swf::line-styles
                                       init-line-styles
                                       '%3b-swf::shape-records (reverse shape-records)))))
  )

(defun shape* (id shape-data)
  "alternate version of SHAPE, with all style data directly in the op stream,
so caller doesn't need to worry about reusing previous styles
id = character-id of shape
shape-data = list of shape commands:
 (:line-style (line style data)) -- change line style
 (:fill-style (fill style data)) -- change-fill style 0
 (:fill-style-1 (fill style data)) -- change-fill style 1
 (:move-to x y) -- move without drawing to absolute x,y
 (:line-to x y) -- draw straight line to absolute x,y
 (:quadratic-to x y cx cy) -- quadratic spline to x,y, control point cx,cy
 (:cubic-to x y cx1 cy1 cx2 cy2 -- cubic spline (converted to quadratic internally, probably not very well)
 (:ellipse x y rx ry) -- axis aligned ellipse centered at x,y with radii rx,ry
 (:circle x y r) -- circle at x,y, radius r
 (:rect x y w h &optional rx ry) -- rectangle from x,y to (x+w),(y+h)
   optionally with rounded corners with radii rx,ry
 (:arc x1 y1 rx ry x-rotation large-flag sweep-flag x2 y2 ) -- svg style arc
  from x1,y1 to x2,y2, params interpreted as in svg
  (http://www.w3.org/TR/SVG/paths.html#PathDataEllipticalArcCommands)

 line style data:
   nil = no stroke
   (width join cap (fill style data)) -- stroke

 fill style data:
   nil = no fill
    (:solid r g b a) -- solid color
    (:linear-gradient x0 y0 x1 y1 (stops) transform spread) -- linear gradient
      from x0,y0 to x1,y1
    (:radial-gradient cx cy r (stops) transform spread) -- radial gradient
      centered at cx,cy, with radius r
    (:focal-gradient cx cy fx fy r (stops) spread) -- focal gradient
      centered at cx,cy, with radius r, and focal point at fx,fy
    gradient stops:
      list of (offset (r g b a)) where offset is 0.0 to 1.0
"
  (let* ((shape-records nil)
         (segment nil)
         (seg-line-styles nil)
         (seg-fill-styles nil)
         (line-style-map (make-hash-table :size 32 :test 'equal))
         (fill-style-map (make-hash-table :size 32 :test 'equal))
         (lid 0)
         (fid 0)
         (x 0)
         (y 0)
         (x0 0)
         (y0 0)
         (x1 0)
         (y1 0)
         (init-line-styles nil)
         (init-fill-styles nil))
    (labels (
             (finish-segment ()
               (if (or init-fill-styles init-line-styles)
                   (push (make-instance
                          '%3b-swf::style-change-shape-record
                          ;; set defaults
                          '%3b-swf::line-style 0
                          '%3b-swf::fill-style-0 0
                          '%3b-swf::fill-style-1 0
                          ;;
                          ;;'3b-swf::move-to (make-instance '3b-swf::state-move-to
                          ;;                                '3b-swf::delta-x 0
                          ;;                                '3b-swf::delta-y 0)
                          ;; need both style arrays if either is set
                          '%3b-swf::fill-styles
                          (make-instance
                           '%3b-swf::fill-style-array
                           '%3b-swf::fill-styles
                           (loop for i in (reverse seg-fill-styles)
                              collect (fill-style i)))
                          '%3b-swf::line-styles
                          (make-instance
                           '%3b-swf::line-style-array
                           '%3b-swf::line-styles
                           (loop for i in (reverse seg-line-styles)
                              collect (apply #'line-style i))))
                         shape-records)

                   (progn
                     (setf init-fill-styles
                           (make-instance
                            '%3b-swf::fill-style-array
                            '%3b-swf::fill-styles
                            (loop for i in (reverse seg-fill-styles)
                               collect (fill-style i))))
                     (setf init-line-styles
                           (make-instance
                            '%3b-swf::line-style-array
                            '%3b-swf::line-styles
                            (loop for i in (reverse seg-line-styles)
                               collect (apply #'line-style i))))))
               (setf shape-records (append segment shape-records))
               (setf segment nil)
               ;; reset index and clear style list

               (setf lid 0)
               (clrhash line-style-map)
               (setf seg-line-styles nil)
               (setf fid 0)
               (clrhash fill-style-map)
               (setf seg-fill-styles nil))

             (add-line-style (style)
               (when (>= lid 31)
                 ;; list is full, write a state-change record
                 ;; and write out the records using that those styles
                 (finish-segment))
               ;; add new style to list
               (push style seg-line-styles)
               ;; and save it on the id->index map and return index
               (setf (gethash style line-style-map) (incf lid)))
             (set-line-style (style)
               (let ((index (if (or (null style) (eql style 0))
                                0
                                (or (gethash style line-style-map)
                                    (add-line-style style)))))
                 ;; todo: check for redundant state changes, combine
                 ;; adjacent changes
                 ;; (redundant as in setting to current state, or
                 ;;  changing a state twice in a row)
                 (push
                  (make-instance
                   '%3b-swf::style-change-shape-record
                   '%3b-swf::line-style index)
                  segment)))

             (add-fill-style (style)
               (when (>= fid 31)
                 (finish-segment))
               (push style seg-fill-styles)
               (setf (gethash style fill-style-map) (incf fid)))
             (set-fill-style (style)
               (let ((index (if (or (null style) (eql style 0))
                                0
                                (or (gethash style fill-style-map)
                                    (add-fill-style style)))))
                 (push
                  (make-instance '%3b-swf::style-change-shape-record
                                 '%3b-swf::fill-style-0 index)
                  segment)))
             (set-fill-style-1 (style)
               (let ((index (if (or (null style) (eql style 0))
                                0
                                (or (gethash style fill-style-map)
                                    (add-fill-style style)))))
                 (push
                  (make-instance '%3b-swf::style-change-shape-record
                                 '%3b-swf::fill-style-1 index)
                  segment)))


             (add-move (nx ny)
               (push (make-instance
                      '%3b-swf::style-change-shape-record
                      '%3b-swf::move-to (make-instance '%3b-swf::state-move-to
                                                       '%3b-swf::delta-x nx
                                                       '%3b-swf::delta-y ny))
                     segment)
               (setf x (floor nx 0.05)
                     y (floor ny 0.05)))
             (dx (nx)
               (let* ((nxt (truncate  nx 0.05))
                      (dx (- nxt x)))
                 (setf x nxt
                       x0 (min x x0)
                       x1 (max x x1))
                 (/ dx 20.0)))
             (x ()
               (/ x 20.0))
             (y ()
               (/ y 20.0))
             (dy (ny)
               (let* ((nyt (truncate ny 0.05))
                      (dy (- nyt y)))
                 (setf y nyt
                       y0 (min y y0)
                       y1 (max y y1))
                 (/ dy 20.0)))
             (add-line (nx ny)
               (let ((dx (dx nx))
                     (dy (dy ny)))
                 (unless (and (zerop dx) (zerop dy))
                   (push (make-instance
                          '%3b-swf::straight-edge-shape-record
                          '%3b-swf::delta-x (if (zerop dx) nil dx)
                          '%3b-swf::delta-y (if (zerop dy) nil dy))
                         segment))))
             (add-curve (cx cy ax ay)
               (push (make-instance
                      '%3b-swf::curved-edge-shape-record
                      '%3b-swf::control-delta-x (dx cx)
                      '%3b-swf::control-delta-y (dy cy)
                      '%3b-swf::anchor-delta-x (dx ax)
                      '%3b-swf::anchor-delta-y (dy ay)
                      )
                     segment))
             (add-ellipse (cx cy rx ry)
               (let ((q (float (tan (/ pi 8)) 1.0))
                     (h (float (/ (sqrt 2) 2.0) 1.0)))
                 (add-move cx (- cy ry))
                 (add-curve (+ cx (* rx q)) (- cy (* ry 1))
                            (+ cx (* rx h)) (- cy (* ry h)))
                 (add-curve (+ cx (* rx 1)) (- cy (* ry q))
                            (+ cx (* rx 1)) (- cy (* ry 0)))
                 (add-curve (+ cx (* rx 1)) (+ cy (* ry q))
                            (+ cx (* rx h)) (+ cy (* ry h)))
                 (add-curve (+ cx (* rx q)) (+ cy (* ry 1))
                            (+ cx (* rx 0)) (+ cy (* ry 1)))
                 (add-curve (- cx (* rx q)) (+ cy (* ry 1))
                            (- cx (* rx h)) (+ cy (* ry h)))
                 (add-curve (- cx (* rx 1)) (+ cy (* ry q))
                            (- cx (* rx 1)) (+ cy (* ry 0)))
                 (add-curve (- cx (* rx 1)) (- cy (* ry q))
                            (- cx (* rx h)) (- cy (* ry h)))
                 (add-curve (- cx (* rx q)) (- cy (* ry 1))
                            (- cx (* rx 0)) (- cy (* ry 1)))))
             (add-circle (cx cy r)
               (add-ellipse cx cy r r))
             (rounded-rect (x y wx wy rx ry)
               (let  ((q (float (tan (/ pi 8)) 1.0))
                      (h (float (/ (sqrt 2) 2.0) 1.0))
                      (wx-rx (- wx rx))
                      (wy-ry (- wy ry)))
                 (add-move (+ x rx) y)
                 (add-line (+ x wx-rx) y)
                 (add-curve (+ x wx-rx (* q rx)) y
                            (+ x wx-rx (* h rx)) (+ y (- ry (* h ry))))
                 (add-curve (+ x wx) (+ y (- ry (* q ry)))
                            (+ x wx) (+ y ry))
                 (add-line (+ x wx) (+ y wy-ry))
                 (add-curve (+ x wx) (+ y wy-ry (* q ry))
                            (+ x wx-rx (* h rx)) (+ y wy-ry (* h ry)))
                 (add-curve (+ x wx-rx (* q rx)) (+ y wy)
                            (+ x wx-rx) (+ y wy))
                 (add-line (+ x rx) (+ y wy))
                 (add-curve (+ x (- rx (* q rx))) (+ y wy)
                            (+ x (- rx (* h rx))) (+ y wy-ry (* h ry)))
                 (add-curve x (+ y wy-ry (* q ry))
                            x (+ y wy-ry))
                 (add-line x (+ y ry))
                 (add-curve x (+ y (- ry (* q ry)))
                            (+ x (- rx (* h rx))) (+ y (- ry (* h ry))))
                 (add-curve (+ x (- rx (* q rx))) y
                            (+ x rx) y)))
             (add-rect (x y wx wy &optional rx ry)
               (if (or rx ry)
                   (rounded-rect x y wx wy rx ry)
                   (progn
                     (add-move x y)
                     (add-line (+ x wx) y)
                     (add-line (+ x wx) (+ y wy))
                     (add-line x (+ y wy))
                     (add-line x y))))

             (deg (x) ;; just for debugging
               (* 180 (/ x pi)))

             (add-ellipse* (cx cy rx ry phi theta1 theta2 x1 y1 x2 y2)
               "subdivide an ellipse segment until it spans a small
 enough angle (currently pi/4), then draw the segment using a
 quadratic spline"
               ;; todo: figure out best split size...
               (let* ((sphi (sin phi))
                     (-sphi (- sphi))
                     (cphi (cos phi)))
                 (labels ((arc (t1 t2 x1 y1 x2 y2)
                            (cond
                              ((> (- t2 t1) (/ pi 4))
                               (let* ((c (/ (+ t1 t2) 2))
                                      (rxctheta (* rx (cos c)))
                                      (rystheta (* ry (sin c)))
                                      (x (+ cx (* cphi rxctheta)
                                            (* -sphi rystheta)))
                                      (y (+ cy (* sphi rxctheta)
                                            (* cphi rystheta))))
                                 (arc t1 c x1 y1 x y)
                                 (arc c t2 x y x2 y2)))
                              (t
                               (let* ((q (float (tan (/ (- t2 t1) 2)) 1.0))
                                      (qrxsint1 (- (* q rx (sin t1))))
                                      (qrycost1 (* q ry (cos t1)))
                                      (control-x
                                       (+ x1
                                          (+ (* cphi qrxsint1)
                                             (* -sphi qrycost1))))
                                      (control-y
                                       (+ y1
                                          (+ (* sphi qrxsint1)
                                             (* cphi qrycost1)))))
                                 (add-curve control-x control-y x2 y2))))))
                   (arc theta1 theta2 x1 y1 x2 y2))))

             ;; fixme: arc code is a horrible mess, rewrite it
             (add-arc (x1 y1 rx ry rotation large-flag sweep-flag x2 y2)
               ;;http://www.w3.org/TR/SVG/implnote.html#ArcConversionEndpointToCenter
               (setf rx (abs rx)
                     ry (abs ry))
               (cond
                 ((or (zerop rx) (zerop ry))
                  (add-line x2 y2))
                 (t
                  (let* ((cosr (cos rotation))
                         (sinr (sin rotation))
                         (x1~ (+ (* cosr (/ (- x1 x2) 2.0))
                                 (*  sinr (/ (- y1 y2) 2.0))))
                         (y1~ (+ (* (- sinr) (/ (- x1 x2) 2.0))
                                 (* cosr (/ (- y1 y2) 2.0))))
                         (tmp (+ (/ (expt x1~ 2) (expt rx 2))
                                 (/ (expt y1~ 2) (expt ry 2)))))
                    (when (> tmp 1.0)
                      (setf rx (* rx (sqrt tmp))
                            ry (* ry (sqrt tmp))))
                    (let* ((sign (if (and large-flag sweep-flag) -1 1))
                           (sq (/ (- (* (expt rx 2) (expt ry 2))
                                     (* (expt rx 2) (expt y1~ 2))
                                     (* (expt ry 2) (expt x1~ 2)))
                                  (+ (* (expt rx 2) (expt y1~ 2))
                                     (* (expt ry 2) (expt x1~ 2) ))))
                           (sqrt (sqrt (abs sq)))
                           (cx~ (* sign sqrt (/ (* rx y1~) ry)))
                           (cy~ (* sign sqrt (/ (- (* ry x1~)) rx)))

                           (cx (+ (* cosr cx~) (* sinr cy~) (/ (+ x1 x2) 2.0)))
                           (cy (+ (* (- sinr) cx~) (* cosr cy~) (/ (+ y1 y2) 2.0)))
                           (theta1 (atan (/ (- y1~ cy~) ry) (/ (- x1~ cx~) rx)))
                           (theta2 (mod (atan (/ (- 0 y1~ cy~) ry) (/ (- 0 x1~ cx~) rx)) (* 2 pi))))
                      ;; fixme: simplify (and verify) this logic
                      (if (and (not large-flag) (> (abs (- theta2 theta1)) pi))
                          (add-ellipse* cx cy rx ry rotation
                                        (- theta2 pi pi) theta1
                                        x1 y1 x2 y2)
                          (if (and large-flag (< (- theta2 theta1) pi))
                              (add-ellipse* cx cy rx ry rotation
                                            (- theta1 pi pi) theta2
                                            x1 y1 x2 y2)
                              (add-ellipse* cx cy rx ry rotation theta1 theta2
                                            x1 y1 x2 y2)))))))))

      (loop for i in shape-data
         do (ecase (car i)
              (:line-style (set-line-style (rest i)))
              (:fill-style (set-fill-style (second i)))
              (:fill-style-0 (set-fill-style (second i)))
              (:fill-style-1 (set-fill-style-1 (second i)))
              (:move-to (apply #'add-move (rest i)))
              (:line-to (apply #'add-line (rest i)))
              (:quadratic-to (add-curve (fourth i) (fifth i)
                                        (second i) (third i)))
              (:cubic-to (split-curve (cons (/ x 20.0) (/ y 20.0))
                                      (cons (second i) (third i))
                                      (vector (cons (fourth i) (fifth i))
                                              (cons (sixth i) (seventh i)))
                                      #'add-curve #'add-line))
              (:ellipse (apply #'add-ellipse (rest i)))
              (:circle (apply #'add-circle (rest i)))
              (:rect (apply #'add-rect (rest i)))
              (:arc (apply #'add-arc (rest i)))))

      (finish-segment)

      (make-instance '%3b-swf::define-shape-4-tag
                     '%3b-swf::character-id id
                     '%3b-swf::bounds (3b-swf::rect
                                       (/ x0 20) (/ y0 20)
                                       (/ x1 20) (/ y1 20))
                     '%3b-swf::edge-bounds (3b-swf::rect
                                            (/ x0 20) (/ y0 20)
                                            (/ x1 20) (/ y1 20))
                     '%3b-swf::uses-scaling-strokes t       ;;??
                     '%3b-swf::uses-non-scaling-strokes nil ;;??
                     '%3b-swf::uses-fill-winding-rule nil
                     '%3b-swf::shapes (make-instance
                                       '%3b-swf::shape-with-style
                                       '%3b-swf::fill-styles
                                       init-fill-styles
                                       '%3b-swf::line-styles
                                       init-line-styles
                                       '%3b-swf::shape-records (reverse shape-records))))))