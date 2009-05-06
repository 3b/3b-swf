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
    (format t "radial gradient... ~s,~s - ~s, ~s~% ~s~%" cx cy rx ry colors)
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