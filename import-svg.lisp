;;;; import svg to 3b-swf data

;;;; based on http://paste.lisp.org/display/78719
;;;; MIT-style license

#+nil
(progn
  (require 'cxml)
  (require 'split-sequence)
  (load "svg-util"))

(in-package :svg-swf)

(defvar *document*)
(defparameter *trace* nil)
(defvar *element-count*)
(defparameter *clipping* nil)

(setf cxml:*catalog* (cxml:make-catalog))  ; Dude, WTF.

;; (setf *document* (cxml:parse-file "../../tmp/hrum_bottle.svg" (cxml-dom:make-dom-builder)))

;; (setf *document* (cxml:parse-file "/tmp/q.svg" (cxml-dom:make-dom-builder)))

;; (format t "~s~%" (render-document :qq))

#+nil
(flet ((resolver (pubid sysid)
	 (declare (ignore pubid sysid))
	 (flexi-streams:make-in-memory-input-stream nil)))
 (setf *document* (cxml:parse-file "src/svg/mario.svg" (cxml-dom:make-dom-builder)  :entity-resolver #'resolver)))


;; Attributes aren't currently inherited from their parents as
;; expected.  It might not be a good idea to store them in specials
;; anyway, if elements in a DEFS inherit attributes lexically. Oops?
;; (defvar *fill* "black")

;; fixme: rename these to make it more obvious which is keyed by id or style
(defvar *line-styles*)
(defvar *fill-styles*)
(defvar *line-style-ids*)
(defvar *fill-style-ids*)
(defvar *filters*)
;; map of svg IDs to symbols naming swf shapes or sprites
(defvar *clip-shapes*)
(defvar *shapes*)
(defvar *sprites*)

(defvar *current-line-style*)
(defvar *current-fill-style*)
(defvar *current-shape*)
(defvar *current-sprite*)

(defparameter *current-transform* (identity-matrix))



(defun render-document (name &key (document *document*))
  "Render an SVG document"
  (let* ((*element-count* 0)
         (svg (dom:item (dom:get-elements-by-tag-name document "svg") 0))
         (width (attribute svg "width" 1000 #'parse-float))
         (height (attribute svg "height" 1000  #'parse-float))
         (*shapes* nil)
         (*line-styles* (make-hash-table :test 'equal))
         (*fill-styles* (make-hash-table :test 'equal))
         (*line-style-ids* (make-hash-table))
         (*fill-style-ids* (make-hash-table))
         (*filters* (make-hash-table :test 'equal))
         (*clip-shapes* (make-hash-table :test 'equal))
         (*sprites* nil)
         (*defs* (make-hash-table :test 'equal))
         (*current-shape* nil)
         (*current-line-style* nil)
         (*current-fill-style* nil)
         (*current-sprite* nil))
    (with-sprite (name)
      (svg-render (dom:document-element document)))
    (when *trace*
      (format *trace-output* "~&Rendered ~A non-clipping graphic elements.~%" *element-count*))
    ;;(format t "shapes=~%~s~%" (mapcar (lambda (x) (list (car x) (reverse (cdr x)))) (reverse *shapes*)))
    ;;(format t "sprites=~%~s~%" (mapcar (lambda (x) (list (car x) (reverse (cdr x)))) (reverse *sprites*)))
    (values (append
              (mapcar (lambda (x) (3b-swf::shape* (car x) (reverse (cdr x))))
                                    (nreverse *shapes*))
              (mapcar (lambda (x) (3b-swf::sprite (car x) (reverse (cdr x))))
                                     (nreverse *sprites*)))
            width height)))

(defun add-shape ()
  (push *current-shape* *shapes*)
  (push (car *current-shape*) (cdr *current-sprite*))
  (setf *current-shape* nil))


(defun apply-node-transform (node &optional (mat *current-transform*))
  (let ((transforms (attribute node "transform" nil #'parse-transforms-string)))
    (when transforms
      (dolist (transform transforms mat)
        (ecase (first transform)
          (:matrix
           (setf mat (matrix* mat (apply #'svg-matrix (rest transform)))))
          (:translate
           (destructuring-bind (x y) (rest transform)
             (setf mat (translate x (or y 0.0) mat))))
          (:scale
           (destructuring-bind (x y) (rest transform)
             (setf mat (scale x (or y 1.0) mat))))
          (:rotate
           (destructuring-bind (r ax ay) (rest transform)
             (setf mat (translate (or ax 0.0) (or ay 0.0) mat))
             (setf mat (rotate (* 2 pi (/ r 360.0)) mat))
             (setf mat (translate (if ax (- ax) 0.0) (if ay (- ay) 0.0) mat))))
          (:skew-x (setf mat (skew (second transform) 0.0 mat)))
          (:skew-y (setf mat (skew 0.0 (second transform) mat))))))))

(defun composite-node-p (node)
  "return true if NODE can have children"
  ;; fixme: should we distinguish switch and clippath?
  (and (dom:element-p node)
       (member (dom:tag-name node) '("svg" "switch" "g" "defs" "clippath")
           :test 'string-equal)))

(defmacro with-shape ((id) &body body)
  `(let ((*current-shape* (list ,id)))
     ,@body
     ;; fixme: should we keep empty shapes?
     (when (cdr *current-shape*)
       (push *current-shape* *shapes*))))

(defun new-shape (id)
  (when (and *current-shape*
             (cdr *current-shape*))
    (push *current-shape* *shapes*))
  (setf *current-shape* (list id)))

(defmacro with-sprite ((id) &body body)
  `(let ((*current-sprite* (list ,id)))
     ,@body
     (push *current-sprite* *sprites*)))

(defun add-to-sprite-filtered (id transform filter opacity)
  ;; just handling a single blur/glow filter on image for now...
  (format t "filter = ~s~%" filter)
  (let ((swf-filter
         (ecase (caar filter)
           (:blur
            (make-instance '%swf:blur-filter
                           '%swf:blur-x (first (getf (cdar filter) :dev ))
                           '%swf:blur-y (second (getf (cdar filter) :dev))
                           ;; fixme: should passes be configurable?
                           '%swf:passes 3))
           (:glow
            (make-instance '%swf:glow-filter
                           '%swf:blur-x (first (getf (cdar filter) :dev))
                           '%swf:blur-y (second (getf (cdar filter) :dev))
                           ;; fixme: how should these be set?
                           '%swf:inner-glow nil
                           '%swf:knockout nil
                           '%swf:glow-color (rgba-float 0 0 0 1)
                           ;; fixme: should passes be configurable?
                           '%swf:passes 3))
           )))
    (format t "got filter, using ~s~%" swf-filter)
    (push (3b-swf::place-object
           id
           0
           :filter-list (make-instance '%swf:filter-list
                                       '%swf:filters (list swf-filter))
           :matrix transform
           :color-transform (when opacity
                              (color-transform-alpha-float
                               :multiply (list 1 1 1 opacity ))))
          (cdr *current-sprite*))))
(defun add-to-sprite (id transform filter opacity)
  (when filter (format t "adding to sprite, got filter ~s~%" filter))
  (if filter
      ;; current code generates filters that render too slowly, so
      ;; skipping for now...
      (format t "dropping filter effect ~s~%" filter)
      #+nil(add-to-sprite-filtered id transform filter opacity))
  (push (if (or transform opacity)
            (3b-swf::place-object
             id
             0
             :matrix transform
             :color-transform (when opacity
                                (color-transform-alpha-float
                                 :multiply (list 1 1 1 opacity ))))
            id)
        (cdr *current-sprite*)))

(defun svg-render-children (node)
  "Render the children of the current node"
  ;; when rendering nodes with children, we need to either create a
  ;; sprite, or create a shape, depending on whether any of the
  ;; children have children of their own
  ;;
  ;; we assume there is a valid sprite in progress, so if we don't
  ;;   have complex children, we can just create a new shape, and add it
  ;;   to the current sprite.
  ;; if we do have complex children, we always start a new sprite for now,
  ;;   possibly could optimize it away if none of the complex children
  ;;   have an opacity < 1 or a filter?
  ;; (except while drawing clip paths, in which case we never create a
  ;;  since they have no alpha)
  (let* ((opacity (attribute node "opacity" 1.0 #'parse-float))
         (filter (attribute node "filter"))
         (name (attribute node "id" "node"))
         (*current-transform* (apply-node-transform node))
         (transform (attribute node "transform" nil #'parse-transforms-string))
         (node-transform (apply-node-transform node (identity-matrix)))
         (need-sprite nil)
         (id (gensym (format nil "~a-" name)))
         (shape-id (gensym (format nil "~a-shape-" name))))
    #+nil(and (not *clipping*)
                          (or filter
                              (/= opacity 1.0)))

    ;;(format t "n-r-c ~s xform=~s/~s~%" name transform node-transform)
    (unless *clipping*
      (dom:do-node-list (node (dom:child-nodes node))
        (when (composite-node-p node)
          (setf need-sprite t))))
    (when node-transform
      (setf need-sprite t))
    (flet ((body ()
             ;; oops, turns out we need a new shape for each node
             ;; (except maybe ones without any line style), since
             ;; lines are drawn after fills...
             ;;(with-shape (shape-id)
             ;;  (dom:do-node-list (node (dom:child-nodes node))
             ;;    ;; if we have subsprites, we need to break up the shape
             ;;    ;; so the sprite children can beinterleaved with the
             ;;    ;; children in the shape
             ;;    (when (and (not *clipping*)
             ;;               (composite-node-p node)
             ;;               (cdr *current-shape*))
             ;;      (push (car *current-shape*) (cdr *current-sprite*))
             ;;      (setf shape-id (gensym (format nil "~a-shape-" name)))
             ;;      (new-shape shape-id))
             ;;    (svg-render node))
             ;;  (when (cdr *current-shape*)
             ;;    (push (car *current-shape*) (cdr *current-sprite*))))
             (with-shape (shape-id)
               (dom:do-node-list (node (dom:child-nodes node))
                 (svg-render node)))))
      ;; when we are applying modifications to the entire subtree
      ;; (opacity, filter, etc), we need to add a sprite object
      ;; to which the effect can be applied
      (if need-sprite
          (with-sprite (id)
            (body))
          ;; not creating a sprite, add the contents to current sprite
          (body)))
    ;; add new shape/sprite to current sprite
    (when need-sprite
      (cond
       (*clipping*
        (push (if node-transform
                  (3b-swf::place-object id 0 :matrix node-transform)
                  id)
              (cdr *current-sprite*)))
       ((or filter node-transform (/= opacity 1.0))
        (when filter
          ;; todo: add filters
          (format t "dropping filter effect ~s on object ~s:~s~%"
                  filter (dom:tag-name node) (attribute node "id")))
        (push (3b-swf::place-object
               id
               0
               ;; :filter-list ...
               :matrix (or  node-transform (identity-matrix))
               :blend-mode 2
               :color-transform (color-transform-alpha-float
                                 :multiply (list 1 1 1 opacity ))

)
              (cdr *current-sprite*)))
       (t
        (push id (cdr *current-sprite*)))))))

(defun svg-render (node)
  "Render a node within the SVG document tree"
  (cond
    ((dom:element-p node)
     (when *trace*
       (indent)
       (format *trace-output* "~A:~A~A~%"
               (dom:get-attribute node "id")
               (if *clipping* "(CLIPPING) " "")
               node))
     (let ((name (dom:tag-name node))
           (id (attribute node "id"))
           (*indent* (+ *indent* 3)))
       (labels ((tag (tag-name) (string-equal name tag-name)))
         (cond
           ((tag "svg") (svg-render-children node))
           ((tag "switch") (svg-render-children node))
           ((tag "defs") (definition-subtree node))
           ((tag "g") (svg-render-children node))
           ((tag "defs") (svg-render-children node))
           ;; fixme: implement "use" as place-object
           ((tag "use") (error "USE tag not implemented yet"))
           ;;((tag "use") (use-node (dom:get-attribute node "xlink:href")))
           ((tag "rect") (invoke-renderer node #'emit-rect))
           ((tag "line") (invoke-renderer node #'emit-line))
           ((tag "circle") (invoke-renderer node #'emit-circle))
           ((tag "ellipse") (invoke-renderer node #'emit-ellipse))
           ((tag "polygon") (invoke-renderer node #'emit-polygon))
           ((tag "clippath")
            (let ((*clipping* t)
                  (*current-sprite* (list (gensym (format nil "~a-" name)))))
              (record-node node)
              (svg-render-children node)
              (setf (gethash id *clip-shapes*) (car *current-sprite*))))
           ((tag "path") (invoke-renderer node #'emit-path))))))))

(defun use-node (uri)
  (unless (zerop (length uri))
    (cond
      ((char= #\# (char uri 0))
       (let ((node (gethash (subseq uri 1) *defs*)))
         (cond
           ((null node) (warn "Referenced node ~A isn't defined" uri))
           (t (svg-render node)))))
      (t (warn "Skipping ~A" uri)))))

(defun parse-linejoin (string)
  (setf string (trim string))
  (cond
    ((string-equal string "miter") 2)
    ((string-equal string "round") 0)
    ((string-equal string "bevel") 1)
    ((string-equal string "inherit") :inherit)
    (t :inherit)))

(defun parse-linecap (string)
  (setf string (trim string))
  (cond
    ((string-equal string "butt") 1)
    ((string-equal string "round") 0)
    ((string-equal string "square") 2)
    ((string-equal string "inherit") :inherit)
    (t :inherit)))

(defun parse-spread (string)
  (setf string (trim string))
  (cond
    ((string-equal string "pad") 0)
    ((string-equal string "reflect") 1)
    ((string-equal string "repeat") 2)
    (t 0)))

(defun gradient-stops (element)
  (let ((stops (dom:get-elements-by-tag-name element "stop"))
        (href (attribute element "xlink:href")))
    ;;(format t "href=~s, stops=~s~%" href stops)
    (if (and (zerop (dom:length stops)) href)
        (gradient-stops (gethash (subseq href 1) *defs*))
        stops)))

(defun translate-stops (stops)
  (let ((out nil))
    (dom:do-node-list (i stops)
      (let ((offset (attribute i "offset" 0 #'parse-float))
            (color (attribute i "stop-color"))
            (opacity (attribute i "stop-opacity" 0 #'parse-float)))
        (push (list offset (append (color-to-float (parse-color color))
                                   (list (* 1 opacity))))
              out)))
    (nreverse out)))

(defun svg-matrix (s1 r1 r2 s2 t1 t2)
  (matrix :sx s1 :sy s2 :rx r1 :ry r2 :tx t1 :ty t2))

(defun gradient-transform (element)
  (let ((transform (identity-matrix)))
    (dolist (i (attribute element "gradientTransform" nil
                          #'parse-transforms-string)
             transform)
      (setf transform
            (ecase (first i)
              (:matrix (matrix* (apply #'svg-matrix (rest i)) transform))
              (:translate (translate (second i) (third i) transform))
              (:scale (scale (second i) (third i) transform))
              (:rotate
               (destructuring-bind (r &optional (ax 0.0) (ay 0.0)) (rest i)
                 (matrix*
                  (matrix* (translate ax ay)
                           (matrix* (rotate r)
                                    (translate (- ax) (- ay))))
                  transform))))))))


(defun gradient (element &key radial opacity)
  (let* ((stops (translate-stops (gradient-stops element)))
         (x0 (attribute element (if radial "cx" "x1") 0 #'parse-float))
         (y0 (attribute element (if radial "cy" "y1") 0 #'parse-float))
         (r (when radial (attribute element "r" 1 #'parse-float)))
         (x1 (attribute element "x2" 1 #'parse-float))
         (y1 (attribute element "y2" 1 #'parse-float))
         (fx (attribute element "fx" 0 #'parse-float))
         (fy (attribute element "fy" 0 #'parse-float))
         (spread (attribute element "spreadMethod" 0 #'parse-spread t))
         (transform (gradient-transform element)))
    ;; possibly this should be in the place-object instead? we could
    ;; have separate fill and stroke opacity though, so might as well
    ;; adjust here anyway
#+nil    (when opacity
      (loop for i in stops
         do (setf (fourth (second i)) (* opacity (fourth (second i))))))
    (if radial
        (list :focal-gradient x0 y0 fx fy r stops transform spread)
        ;;(list :radial-gradient x0 y0 r stops transform spread)
        ;;(list :linear-gradient x0 y0 (+ x0 r) y0 stops transform spread)
        (list :linear-gradient x0 y0 x1 y1 stops transform spread))
    #+nil(list :linear-gradient x0 y0 (+ x0 10) (+ y0 10)
          `((0 (1 0 1 1)) (1 (0 1 0 1))) (identity-matrix) 0)

))


(defun find-clip-path (string)
  (setf string (trim string))
  (cond
    ((string-equal string "inherit") :inherit)
    ((eql 4 (mismatch string "url("))
     (let ((url (subseq string 4 (position #\) string))))
       (when (and (> (length url) 1)
                  (char= #\# (aref url 0)))
         ;;(break "~A => ~A" url (gethash (subseq url 1) *defs*))
         (gethash (subseq url 1) *defs*))))
    (t nil)))


(defun parse-transform (in)
  (let ((mode (attribute-token in)))
    (peek-char t in)
    (unless (char= #\( (read-char in)) (error 'parse-error))
    (peek-char t in)
    (labels ((optionally (n)
               (cond
                 ((char= #\, (peek-char t in))
                  (read-char in)
                  (peek-char t in)
                  (loop repeat n
                        for first = t then nil
                        unless first do (token in #'comma-whitespace-p)
                        collect (parse-float in)))
                 (t nil))))
      (prog1
          (cond
            ((string-equal mode "matrix")
             (list* :matrix
                    (loop for digit from 0 below 6
                          collect (parse-float in)
                          do (token in (if (= digit 5) #'whitespace-p #'comma-whitespace-p)))))
            ((string-equal mode "translate")
             (list* :translate (parse-float in) (optionally 1)))
            ((string-equal mode "scale")
             (list* :scale (parse-float in) (optionally 1)))
            ((string-equal mode "rotate") ; Rotations are in degrees
             (list* :rotate (parse-float in) (optionally 2)))
            ((string-equal mode "skewx") (list* :skew-x (parse-float in)))
            ((string-equal mode "skewy") (list* :xkew-y (parse-float in)))
            (t (error "Unknown transform")))
        (peek-char t in)
        (unless (char= #\) (read-char in)) (error 'parse-error))))))

(defun parse-transforms-string (string)
  (with-input-from-string (in string)
    (loop while (peek-char t in nil)
          collect (parse-transform in)
          do (token in #'comma-whitespace-p))))

(defun parse-fill (node opacity)
  (let* ((fill (parse-paint (attribute node "fill" "black" 'identity t)))
         (fill-opacity (attribute node "fill-opacity" 1.0 #'parse-float t)))
    (case fill
      (:default (list :solid 0 0 0 (* opacity fill-opacity) ))
      (:none)
      (otherwise
       (if (dom:element-p fill)
           (cond
             ((string-equal (dom:tag-name fill) "linearGradient")
              (gradient fill :opacity (* opacity fill-opacity)))
             ((string-equal (dom:tag-name fill) "radialGradient")
              (gradient fill :radial t :opacity (* opacity fill-opacity))))
           `(:solid ,@(color-to-float fill) ,(* opacity fill-opacity)))))))

(defun parse-stroke (node opacity)
  (let* ((stroke (parse-paint (attribute node "stroke" "none" 'identity t)))
         (stroke-opacity (attribute node "stroke-opacity" 1.0 #'parse-float t))
         (stroke-width (attribute node "stroke-width" 1 #'parse-float t)) ; FIXME: units?
         (stroke-linejoin (attribute node "stroke-linejoin" 2 #'parse-linejoin t))
         (stroke-linecap (attribute node "stroke-linecap" 1 #'parse-linecap t))
         (stroke-miterlimit (attribute node "stroke-miterlimit" 4 #'parse-float t)))
    ;;(setf opacity 1.0 stroke-opacity 1.0)
    ;;(format t "stroke ~s = ~s~%" stroke-width stroke)
    (case stroke
     (:default  (break "fixme"))
     (:none nil)
     (otherwise
      (list stroke-width stroke-linejoin stroke-linecap
            (if (dom:element-p stroke)
                (cond
                  ((string-equal (dom:tag-name stroke) "linearGradient")
                   (gradient stroke))
                  ((string-equal (dom:tag-name stroke) "radialGradient")
                   (gradient stroke :radial t)))
                `(:solid ,@(color-to-float stroke)
                         ,(* opacity stroke-opacity)))
            :miter-limit stroke-miterlimit)))))

(defun setup-styles (node)
  (let* ((opacity (attribute node "opacity" 1.0 #'parse-float nil))
         (fill (parse-fill node 1.0))
         (stroke (parse-stroke node 1.0))
         (*indent* (min 40 (+ *indent* 3))))
    (incf *element-count*)
    (apply #'add-op :line-style stroke)
    (add-op :fill-style fill)))

(defun invoke-renderer (node render-fn)
  "Render an SVG path element"
  (let* ((node-transform (apply-node-transform node (identity-matrix)))
         (name (attribute node "id" "node"))
         (shape-id (gensym (format nil "~a-shape-" name)))
         (filter (parse-filter (attribute node "filter" nil))))
    ;; when we have a node transform, we start a new shape so we can
    ;; transform it, since transforming the points by hand would be a
    ;; hassle (and probably wouldn't work well with animation if that
    ;; ever gets added)
   ;;; -- actually, always need a new shape due to stroke vs fill order stuff...
    (with-shape (shape-id)
       (setup-styles node)
       (funcall render-fn node)
       (when (cdr *current-shape*)
         (add-to-sprite (car *current-shape*) node-transform filter 1.0)))
    #+nil
    (cond
      (node-transform
       (new-shape shape-id)
       (setup-styles node)
       (funcall render-fn node)
       (push (3b-swf::place-object
              shape-id
              0
              :matrix node-transform)
             (cdr *current-sprite*))
       ;; fixme: set *current-shape* up so we don't need to put a dummy shape here?
       (new-shape (gensym "shape-")))
      (t
       (setup-styles node)
       (funcall render-fn node)))))

(defun record-node (node)
  "If the node defines an ID, record it in *defs*"
  (let ((id (dom:get-attribute node "id")))
    (unless (zerop (length id))
      (when *trace*
        (indent)
        (format *trace-output* "~&Defined ~A as ~A~%" id node))
      (setf (gethash id *defs*) node))))

(defun definition-subtree (node)
  "Walk a <defs> node, recording IDs"
  (dom:do-node-list (node (dom:child-nodes node))
    (when (dom:element-p node)
      (record-node node)
      (definition-subtree node))))

(defun add-op (&rest op)
  (unless *current-shape*
    (setf *current-shape* (list (gensym "SHAPE-"))))
  (push op (cdr *current-shape*)))

(defun quadratic-to (control-x control-y anchor-x anchor-y)
  (add-op :quadratic-to anchor-x anchor-y control-x control-y))

(defun move-to (x y)
  (add-op :move-to x y))
(defun line-to (x y)
  (add-op :line-to x y))
(defun cubic-to (cx1 cy1 cx2 cy2 ax ay)
  (add-op :cubic-to ax ay cx1 cy1 cx2 cy2))
(defun %rectangle (x y wx wy)
  (add-op :rect x y wx wy))
(defun %rounded-rectangle (x y wx wy rx ry)
  (add-op :rect x y wx wy rx ry))
(defun %emit-ellipse (cx cy rx ry)
  (add-op :ellipse cx cy rx ry))

(defun arc-to (&rest r)
  (apply 'add-op :arc r))


(defun emit-rect (node)
  (let ((width  (attribute node "width"  nil #'parse-float))
        (height (attribute node "height" nil #'parse-float))
        (x (attribute node "x" 0.0 #'parse-float))
        (y (attribute node "y" 0.0 #'parse-float))
        (rx (attribute node "rx" nil #'parse-float))
        (ry (attribute node "ry" nil #'parse-float)))
    (when (and width (> width 0) height (> height 0))
      ;; If a properly specified value  is provided for rx but not for
      ;; ry, then the user agent processes the 'rect' element with the
      ;; effective  value  for  ry  as  equal to  rx.  If  a  properly
      ;; specified value is  provided for ry but not  for rx, then the
      ;; user agent  processes the  'rect' element with  the effective
      ;; value for rx as equal to ry.
      (when (and rx (not ry)) (setf ry rx))
      (when (and ry (not rx)) (setf rx ry))
      (cond
        ((and rx ry (>= rx 0) (>= ry 0))
         ;; If rx is greater than  half of the width of the rectangle,
         ;; then the user agent  processes the 'rect' element with the
         ;; effective  value  for rx  as  half  of  the width  of  the
         ;; rectangle. If ry is greater than half of the height of the
         ;; rectangle,  then  the  user  agent  processes  the  'rect'
         ;; element with  the effective  value for ry  as half  of the
         ;; height of the rectangle.
         (setf rx (min rx (/ width 2))
               ry (min ry (/ height 2)))
         (%rounded-rectangle x y width height rx ry))
        (t
         ;; If neither rx nor ry  has a properly specified value, then
         ;; the  user agent  processes  the 'rect'  element  as if  no
         ;; rounding had been specified, resulting in square corners.
         (%rectangle x y width height))))))


(defun emit-circle (node)
  (let ((cx (attribute node "cx" 0.0 #'parse-float))
        (cy (attribute node "cy" 0.0 #'parse-float))
        (r  (attribute node "r"  0.0 #'parse-float)))
    (when (> r 0.0)
      (%emit-ellipse cx cy r r))))

(defun emit-ellipse (node)
  (let ((cx (attribute node "cx" 0.0 #'parse-float))
        (cy (attribute node "cy" 0.0 #'parse-float))
        (rx (attribute node "rx" 0.0 #'parse-float))
        (ry (attribute node "ry" 0.0 #'parse-float)))
    (when (and (> rx 0.0) (> ry 0.0))
      (%emit-ellipse cx cy rx ry))))

(defun emit-line (node)
  (move-to (attribute node "x1" 0.0 #'parse-float)
           (attribute node "y1" 0.0 #'parse-float))
  (line-to (attribute node "x2" 0.0 #'parse-float)
           (attribute node "y2" 0.0 #'parse-float)))

(defun emit-polyline (node &optional close-p)
  (loop for point in (attribute node "points" nil #'parse-poly-points)
     for op = #'move-to then #'line-to
     for first-point = point then first-point
     do (funcall op (coord-x point) (coord-y point))
     finally (when close-p (line-to (coord-x first-point) (coord-y first-point)))))

(defun emit-polygon (node)
  (emit-polyline node t))

(defun emit-path (node)
  ;; fixme: open/closed paths don't get translated quite correctly
  ;; to make an open path in swf, we need to use a different stroke style
  (let* ((data (parse-path-expression (dom:get-attribute node "d")))
         (coord (coord 0 0))
         (last-control-point nil)
         (path-start nil))
    (labels
        ((x (p) (coord-x p))
         (y (p) (coord-y p))
         (moveto (p)
           (setf coord p
                 last-control-point nil
                 path-start p)
           (move-to (x coord) (y coord)))
         (lineto (p)
           (line-to (x p) (y p))
           (setf coord p
                 last-control-point nil))
         (cubic (newpoint control-1 control-2)
           (cubic-to
                   (x control-1) (y control-1)
                   (x control-2) (y control-2)
                   (x newpoint) (y newpoint))
           (setf coord newpoint
                 last-control-point control-2))
         ;; Should smooth cubic curves use the last control point from
         ;; a quadratic curve?  Probably not..
         (smoothto (to control-2)
           (cond
             ((not last-control-point)
              (cubic to to control-2))
             (t (cubic to
                       (coord-reflect last-control-point coord)
                       control-2))))
         (quadratic (to control &optional offset)
           (let ((newpoint to))
             (when offset (setf newpoint (coord+ newpoint offset)))
             (quadratic-to (x control) (y control) (x newpoint) (y newpoint))
             (setf coord newpoint
                   last-control-point control)))
         (arc (arc offset)
           ;; TODO: All about arcs.
           (arc-to (x coord) (y coord)
                   (x (elliptical-arc-radii arc)) (y (elliptical-arc-radii arc))
                   (* pi (/ (elliptical-arc-x-rot arc) 180.0))
                   (elliptical-arc-large-flag arc)
                   (elliptical-arc-sweep-flag arc)
                   (x (coord+ offset (elliptical-arc-to arc)))
                   (y (coord+ offset (elliptical-arc-to arc)))
)
           (setf coord (coord+ offset (elliptical-arc-to arc))
                 last-control-point nil)))
      (for-each-path-segment data
       (lambda (type arg)
         (ecase type
           (:moveto (moveto arg))
           (:rel-moveto (moveto (coord+ coord arg)))
           (:close-path (assert path-start) (lineto path-start))
           (:lineto (lineto arg))
           (:rel-lineto (lineto (coord+ coord arg)))
           (:horizontal (lineto (coord arg (coord-y coord))))
           (:rel-horizontal (lineto (coord (+ (coord-x coord) arg) (coord-y coord))))
           (:vertical (lineto (coord (coord-x coord) arg)))
           (:rel-vertical (lineto (coord (coord-x coord) (+ (coord-y coord) arg))))
           (:curveto
            (cubic (curve-to arg)
                   (curve-control-1 arg)
                   (smooth-control-2 arg)))
           (:rel-curveto
            (cubic (coord+ coord (curve-to arg))
                   (coord+ coord (curve-control-1 arg))
                   (coord+ coord (smooth-control-2 arg))))
           (:smoothto
            (smoothto (smooth-to arg) (smooth-control-2 arg)))
           (:rel-smoothto
            (smoothto (coord+ coord (smooth-to arg))
                      (coord+ coord (smooth-control-2 arg))))
           (:qcurveto (quadratic (qcurve-to arg) (qcurve-control arg)))
           (:rel-qcurveto (quadratic (qcurve-to arg)
                                     (coord+ coord (qcurve-control arg))
                                     coord))
           (:qsmooth (quadratic arg (if last-control-point
                                        (coord-reflect last-control-point coord)
                                        coord)))
           (:arc (arc arg (coord 0.0 0.0)))
           (:rel-arc (arc arg coord))))))))

