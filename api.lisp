(in-package :3b-swf)

(defun rect (x1 y1 x2 y2)
  (make-instance '%swf:rect '%swf:xmin x1 '%swf:ymin y1 '%swf:xmax x2 '%swf:ymax y2))

(defun file-attributes (&key (direct-blit t) (use-gpu t) (as3 t)
                             has-metadata (use-network t))
  (make-instance '%swf:file-attributes-tag
                 '%swf:use-direct-blit direct-blit
                 '%swf:use-gpu use-gpu
                 '%swf:has-metadata has-metadata
                 '%swf:actionscript-3 as3
                 '%swf:use-network use-network))

(defun script-limits (timeout max-recursion)
  (make-instance '%swf::script-limits-tag
                 '%swf:max-recursion-depth max-recursion
                 '%swf:script-timeout-seconds timeout))

(defun export-assets (exports)
  (make-instance '%swf:export-assets-tag
                 '%swf:assets exports))

(defun symbol-class-tag (exports)
  (make-instance '%swf:symbol-class-tag
                 '%swf:symbol-class-symbols exports))

(defun rgb (&rest args)
  (cond
    ((integerp (car args))
     (make-instance '%swf:rgb
                    '%swf:r (ldb (byte 8 16) (car args))
                    '%swf:g (ldb (byte 8 08) (car args))
                    '%swf:b (ldb (byte 8 00) (car args))))
    ((keywordp (car args))
     (destructuring-bind (&key r g b) args
       (make-instance '%swf:rgb '%swf:r r '%swf:g g '%swf:b b)))
    (t (error "unknown args in rgb ~s" args))))

(defun rgba (&rest args)
  (cond
    ((integerp (car args))
     (make-instance '%swf:rgba
                    '%swf:a (ldb (byte 8 24) (car args))
                    '%swf:r (ldb (byte 8 16) (car args))
                    '%swf:g (ldb (byte 8 08) (car args))
                    '%swf:b (ldb (byte 8 00) (car args))))
    ((keywordp (car args))
     (destructuring-bind (&key r g b a) args
       (make-instance '%swf:rgba '%swf:r r '%swf:g g '%swf:b b '%swf:a a)))
    (t (error "unknown args in rgba ~s" args))))

(defun rgba-float (r g b a)
  (make-instance '%swf:rgba
                 '%swf:r (max 0 (min 255 (floor (* r 255))))
                 '%swf:g (max 0 (min 255 (floor (* g 255))))
                 '%swf:b (max 0 (min 255 (floor (* b 255))))
                 '%swf:a (max 0 (min 255(floor (* a 255))))))

(defun color-transform-alpha (&key add multiply)
  "add,multiply are either (r g b a) or #xaarrggbb"
  (multiple-value-bind (ar ag ab aa)
        (if (listp add)
            (values-list add)
            (values (ldb (byte 8 24) add)
                    (ldb (byte 8 16) add)
                    (ldb (byte 8 08) add)
                    (ldb (byte 8 00) add)))
    (multiple-value-bind (mr mg mb ma)
        (if (listp multiply)
            (values-list multiply)
            (values (ldb (byte 8 24) multiply)
                    (ldb (byte 8 16) multiply)
                    (ldb (byte 8 08) multiply)
                    (ldb (byte 8 00) multiply)))
      (make-instance '%swf:cxform-with-alpha
                     '%swf:add (when add (make-instance '%swf::cxform-part-rgba
                                                   '%swf::r ar
                                                   '%swf::g ag
                                                   '%swf::b ab
                                                   '%swf::a aa))
                     '%swf:mult (when multiply
                                  (make-instance '%swf::cxform-part-rgba
                                                 '%swf::r mr
                                                 '%swf::g mg
                                                 '%swf::b mb
                                                 '%swf::a ma))))))
(defun color-transform-alpha-float (&key add multiply)
  "add,multiply are (r.r g.g b.b a.a)"
  (destructuring-bind (&optional ar ag ab aa) add
    (destructuring-bind (&optional mr mg mb ma) multiply
      (make-instance '%swf:cxform-with-alpha
                     '%swf:add (when add
                                 (make-instance '%swf::cxform-part-rgba
                                                '%swf::r ar
                                                '%swf::g ag
                                                '%swf::b ab
                                                '%swf::a aa))
                     '%swf:mult (when multiply
                                  (make-instance '%swf::cxform-part-rgba
                                                 '%swf::r mr
                                                 '%swf::g mg
                                                 '%swf::b mb
                                                 '%swf::a ma))))))

(defun matrix* (a b)
  (flet ((tx (m) (%swf::value1 (%swf::translate m)))
         (ty (m) (%swf::value2 (%swf::translate m)))
         (sx (m) (if (%swf::scale m)
                     (%swf::value1 (%swf::scale m))
                     1.0))
         (sy (m) (if (%swf::scale m)
                     (%swf::value2 (%swf::scale m))
                     1.0))
         (rx (m) (if (%swf::rotate-skew m)
                     (%swf::value1 (%swf::rotate-skew m))
                     0.0))
         (ry (m) (if (%swf::rotate-skew m)
                     (%swf::value2 (%swf::rotate-skew m))
                     0.0)))
    (let* ((a1 (sx a)) (c1 (ry a)) (e1 (tx a))
           (b1 (rx a)) (d1 (sy a)) (f1 (ty a))
           (a2 (sx b)) (c2 (ry b)) (e2 (tx b))
           (b2 (rx b)) (d2 (sy b)) (f2 (ty b))

           (s1 (+ (* a1 a2) (* c1 b2) (* e1 0)))
           (r1 (+ (* b1 a2) (* d1 b2) (* f1 0)))
           (r2 (+ (* a1 c2) (* c1 d2) (* e1 0)))
           (s2 (+ (* b1 c2) (* d1 d2) (* f1 0)))
           (t1 (+ (* a1 e2) (* c1 f2) (* e1 1)))
           (t2 (+ (* b1 e2) (* d1 f2) (* f1 1)))

           (s (when (not (= s1 s2 1.0))
                (make-instance '%swf:matrix-part-fixed
                               '%swf:value1 s1 '%swf:value2 s2)))
           (r (when (not (= r1 r2 0.0))
                (make-instance '%swf:matrix-part-fixed
                               '%swf:value1 r1 '%swf:value2 r2)))
           (tt (make-instance '%swf:matrix-part-translate
                              '%swf:value1 t1 '%swf:value2 t2)))
      (when (or s r (not (= t1 t2 0.0)))
        (make-instance '%swf:matrix
                       '%swf:rotate-skew r
                       '%swf:scale s
                       '%swf:translate tt)))))

(defun matrix (&key tx ty sx sy rx ry)
  (make-instance
   '%swf:matrix
   '%swf:translate (when (or tx ty)
                     (make-instance '%swf:matrix-part-translate
                                    '%swf:value1 tx '%swf:value2 ty))
   '%swf:rotate-skew (when (or rx ry)
                       (make-instance '%swf:matrix-part-fixed
                                      '%swf:value1 rx '%swf:value2 ry))
   '%swf:scale (when (or sx sy)
                 (make-instance '%swf:matrix-part-fixed
                                '%swf:value1 sx '%swf:value2 sy))))

(defun rotate (angle-deg &optional matrix)
  (let* ((s (sin (/ (* angle-deg pi) 180)))
         (c (cos (/ (* angle-deg pi) 180)))
         (new
          (make-instance
           '%swf:matrix
           '%swf:rotate-skew (make-instance '%swf:matrix-part-fixed
                                            '%swf:value1 s '%swf:value2 (- s))
           '%swf:scale (make-instance '%swf:matrix-part-fixed
                                      '%swf:value1 c '%swf:value2 c))))
    (if matrix
        (matrix* matrix new)
        new)))

(defun scale (sx sy &optional matrix)
  (let ((new (make-instance
              '%swf:matrix
              '%swf:scale (make-instance '%swf:matrix-part-fixed
                                         '%swf:value1 sx '%swf:value2 sy))))
    (if matrix
        (matrix* matrix new)
        new)))

(defun translate (tx ty &optional matrix)
  (let ((new (make-instance
              '%swf:matrix
              '%swf:translate (make-instance '%swf:matrix-part-translate
                                             '%swf:value1 tx '%swf:value2 ty))))
    (if matrix
        (matrix* matrix new)
        new)))

(defun skew (sx sy &optional matrix)
  (declare (ignore sx sy matrix))
  (error "SKEW not implemented yet"))

(defun transform-point (matrix x y)
  (declare (ignore matrix x y))
  (error "xform point not done yet")
)
;; not sure this is useful, or if it should treat identity matrix
;; specially so it can be optimized out in the file if possible?
(defun identity-matrix (&optional matrix)
  (if matrix
      matrix
      (translate 0.0 0.0)))

(defun background-color (&rest color)
  (make-instance '%swf:set-background-color-tag
                 '%swf:background-color (apply 'rgb color)))

(defun place-object (id depth &key class-name matrix move-p color-transform
                     ratio name clip-layers filter-list blend-mode
                     cache-as-bitmap actions)
  "create a place-object-* tag (probably -3, but might eventually get
smart enough to use -2 or original if possible), with character id ID,
at depth DEPTH (0 to use current top depth)
class-name : see swf docs
name: name a sprite object for action-set-target* etc
move-p: modify or replace the sprite at specified depth
ratio: specify morph ratio for a define-morph-shape, 0-65535 (todo: use 0.0-1.0)
clip-layers: if >0, use this layer as clip mask for next N higher layers
filter-list: filter-list instance specifying filters to apply to character
actions: clip-actions instance to specify event handlers for a sprite
matrix, color-transform, blend-mode, cache-as-bitmap: specify params"
  (make-instance '%swf:place-object-3-tag
                 '%swf:move-flag move-p
                 '%swf:depth depth
                 '%swf:class-name class-name
                 '%swf:place-character-id id
                 '%swf:matrix matrix
                 '%swf:color-transform color-transform
                 '%swf:po3-ratio ratio
                 '%swf:name name
                 '%swf:clip-depth clip-layers
                 '%swf:surface-filter-list filter-list
                 '%swf:blend-mode blend-mode
                 ;; fixme: do the values in bitmap-cache mean anything beyond zero/non-zero?
                 '%swf:bitmap-cache (if cache-as-bitmap 255 nil)
                 '%swf:clip-actions actions)
  #+nil(make-instance '%swf:place-object-2-tag
                 '%swf:move-flag move-p
                 '%swf:depth depth
                 '%swf:place-character-id id
                 '%swf:matrix matrix
                 '%swf:color-transform color-transform
                 '%swf:po2-ratio ratio
                 '%swf:name name
                 '%swf:clip-depth clip-layers
                 '%swf:clip-actions actions))
;;todo: more single purpose wrappers for place-object? (move-object, etc)
(defun place-object-at (id depth x y &key (sx 1 has-sx) (sy 1 has-sy) move-p)
  (if (or has-sx has-sy)
      (place-object id depth :matrix (matrix  :tx x :ty y :sx sx :sy sy ) :move-p move-p)
      (place-object id depth :matrix (translate x y) :move-p move-p)))

(defun end-tag ()
  (make-instance '%swf:swf-end-tag))
(defun show-frame ()
  (make-instance '%swf:swf-show-frame-tag))
(defun frame-label (name &key anchor-p)
  (make-instance '%swf:frame-label-tag
                 '%swf:name name
                 '%swf:named-anchor-flag anchor-p))
(defun ensure-tag (tag)
  (cond
    ((typep tag '%swf:swf-tag)
     tag)
    ((eq tag :show-frame)
     (make-instance '%swf:swf-show-frame-tag))
    ((eq tag :end)
     (end-tag))
    ((and tag (or (symbolp tag) (integerp tag)))
     (place-object-at tag 0 0 0))
    (t (error "unknown tag ~s in ensure-tag" tag))))

;; possibly should have a version that takes a list instead of &rest?
;; might also be nice to accept an actual tag instead of a name,
;;  so we could collect a bunch of shape or whatever then just pass that
;;  list to sprite without collecting the IDs separately or extracting them
;;  in the caller
(defun sprite* (name &rest tags)
  (sprite name tags))
(defun sprite (name tags)
  "create a sprite with character id NAME, using tags in TAGS, which can
be either actual swf-tag instances, character IDs of objects to place at
increasing depths, or :show-frame, and end tag will be added automatically
if the last element of TAGS isn't :end or an swf-end-tag instance
 (see ensure-tag for other things that will automatically be converted to
 tags besides :end and :show-frame)"
  #+nil(format t "sprite ~s: tags=~s~%"  name tags)
  (let* ((frame-count 0)
         (need-end t)
         (depth 0)
         (tags (loop for maybe-tag in tags
                     for tag = (ensure-tag maybe-tag)
                     do (cond
                          ((typep tag '%swf:swf-show-frame-tag)
                           (incf frame-count))
                          ((typep tag '%swf:swf-end-tag)
                           (setf need-end nil))
                          ((and (or (typep tag '%swf:place-object-tag)
                                    (typep tag '%swf:place-object-2-tag)
                                    (typep tag '%swf:place-object-3-tag))
                                (zerop (%swf:depth tag)))
                           (setf (%swf:depth tag) (incf depth))
                           (when (and (%swf:clip-depth tag)
                                      (not (zerop (%swf:clip-depth tag))))
                             (incf (%swf:clip-depth tag)
                                   (%swf:depth tag)))))
                     collect tag)))
    (make-instance '%swf:define-sprite-tag
                   '%swf:character-id name
                   '%swf:frame-count frame-count
                   '%swf:control-tags (if need-end
                                          (append tags (list (end-tag)))
                                     tags))))

#+nil
(sprite 'foo 'bar :show-frame (place-object 'baz 1 :move t) :show-frame :end)


;; todo:
;;  symbol-class
;;  abc tag


(defun solid-fill (r g b a)
  (make-instance '%swf::fill-style-solid
                 '%swf::color (rgba-float r g b a)))

(defun linear-gradient-fill (x1 y1 x2 y2 colors transform spread)
  (let* ((mx1 (/ (- x2 x1) 1638.4))
         (my1 (/ (- y2 y1) 1638.4))
         (mx2 my1)
         (my2 (- mx1)))
    (make-instance '%swf::fill-linear-gradient
                   '%swf::gradient-matrix
                   (matrix* transform
                            (matrix :rx mx2 :ry my1
                                    :sx mx1 :sy my2
                                    :tx (/ (+ x2 x1) 2.0)
                                    :ty (/ (+ y2 y1) 2.0)))
                   '%swf::gradient
                   (make-instance
                    '%swf::gradient
                    '%swf::spread-mode spread
                    '%swf::interpolation-mode 0 ;; 0=normal,1=linear
                    '%swf::gradient-records
                    (loop for (i (r g b a)) in colors
                       collect (make-instance
                                '%swf::grad-record
                                '%swf::gradient-ratio (floor (* i 255))
                                '%swf::color
                                (rgba-float r g b a)))))))

(defun focal-gradient-fill (x y fx fy r colors transform spread)
  ;; fixme: test focal point stuff more...
  (let* ((fr (/ (sqrt (+ (expt (- fx x) 2) (expt (- fy y) 2))) r))
         (fa (atan (- fy y) (- fx x)))
         (sqrt2/2 (/ (sqrt 2.0) 2))
         (x2 (+ x (* r sqrt2/2)))
         (y2 (+ y (* r sqrt2/2)))
         (mx1 (/ (- x2 x) 1638.4))
         (my1 (/ (- y2 y) 1638.4))
         (mx2 (- my1))
         (my2 mx1)
         (rotate (rotate (/ (* fa 180) pi))))
    (make-instance '%swf::fill-focal-gradient
                   '%swf::gradient-matrix
                   (matrix*
                    (matrix*
                     transform
                     (matrix :rx 0 :ry 0
                             :sx (/ r 819.2) :sy (/ r 819.2)
                             :tx x
                             :ty y))
                    rotate)
                   '%swf::gradient
                   (make-instance
                    '%swf::focal-gradient
                    '%swf::focal-point (max -1.0 (min fr 1.0))
                    '%swf::spread-mode spread
                    '%swf::interpolation-mode 0 ;; 0=normal,1=linear
                    '%swf::gradient-records
                    (loop for (i (r g b a)) in colors
                       collect (make-instance
                                '%swf::grad-record
                                '%swf::gradient-ratio (floor (* 255 i))
                                '%swf::color
                                (rgba-float r g b a)))))))


(defun radial-gradient-fill (x y r colors transform spread)
  (focal-gradient-fill x y x y r colors transform spread))


(defun fill-style (fill)
  "fill =
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
  (case (car fill)
    (:solid (apply #'solid-fill (cdr fill)))
    (:linear-gradient
     (apply #'linear-gradient-fill (cdr fill)))
    (:focal-gradient
     (apply #'focal-gradient-fill (cdr fill)))
    (:radial-gradient
     (apply #'radial-gradient-fill (cdr fill)))
    ;; default to fill style 0
    (t
     (error "unknown fill ~s" fill))))

(defun line-style (width join cap fill &key open miter-limit)
  ;;(format t "line style = ~s, ~s, ~s, ~s~%" width join cap fill)
  (if (eq :solid (car fill))
      (make-instance
       '%3b-swf::line-style-2
       '%3b-swf::width width
       '%3b-swf::join-style join
       '%3b-swf::start-cap-style cap
       '%3b-swf::end-cap-style cap
       '%3b-swf::no-close open
       '%3b-swf::miter-limit-factor miter-limit
       '%3b-swf::color (apply #'rgba-float (cdr fill)))
      (make-instance
       '%3b-swf::line-style-2
       '%3b-swf::width (* width 1.0)
       '%3b-swf::join-style join
       '%3b-swf::start-cap-style cap
       '%3b-swf::end-cap-style cap
       '%3b-swf::no-close open
       '%3b-swf::miter-limit-factor miter-limit
       '%3b-swf::fill-type (fill-style fill))
      ))

;; fixme: make a 'map-exported-tags' function or something to combine these?
(defun list-exported-tags (tag-list)
  (loop named outer
     for tag in tag-list
     append (typecase tag
              (%swf:symbol-class-tag (%swf:symbol-class-symbols tag))
              (%swf:export-assets-tag (%swf:assets tag))
          #+nil (%swf:define-scene-and-frame-label-data-tag ...))))

(defun find-exported-tag (name tag-list)
  (loop named outer
     for tag in tag-list
     do (typecase tag
          (%swf:symbol-class-tag
           (loop for (id export-name) in (%swf:symbol-class-symbols tag)
              when (string= name export-name)
              do (return-from outer id)))
          (%swf:export-assets-tag
           (loop for (id export-name) in (%swf:assets tag)
              when (string= name export-name)
              do (return-from outer id)))
          (%swf:do-abc-tag
           (when (string= (%swf:name tag) name)
             (return-from outer tag)))
          #+nil (%swf:define-scene-and-frame-label-data-tag ...))))

(defmethod tag-dependencies (tag tag-list)
  (declare (ignore tag-list))
  ;; todo: remove this once all the tags with dependencies are handled..
  (assert tag)
  (unless (or (typep tag '%swf:sound-stream-head-2-tag)
              (typep tag '%swf:swf-show-frame-tag)
              (typep tag '%swf:remove-object-2-tag)
              (typep tag '%swf:swf-end-tag)
              (typep tag '%swf:define-font-3-tag)
              (typep tag '%swf:define-sound-tag)
              )
    (format t "unhandled tag ~s in tag-dependencies~%" tag))
  nil)

(defmethod tag-dependencies ((tag %swf:place-object-2-tag) tag-list)
  (when (%swf:has-character tag)
    (let ((dep (%swf:place-character-id tag)))
      (list* dep (tag-dependencies (find-tag-by-id dep tag-list) tag-list)))))

(defmethod tag-dependencies ((tag %swf:place-object-3-tag) tag-list)
  (when (%swf:has-character tag)
    (let ((dep (%swf:place-character-id tag)))
      (list* dep (tag-dependencies (find-tag-by-id dep tag-list) tag-list)))))


(defmethod tag-dependencies ((tag %swf:define-sprite-tag) tag-list)
  (loop for i in (%swf:control-tags tag)
     append (tag-dependencies i tag-list)))

(defmethod tag-dependencies ((tag %swf:define-text-tag) tag-list)
  (loop for i in (%swf:text-records tag)
     for id = (%swf:font-id i)
     for tag = (find-tag-by-id id tag-list)
     unless tag do (format t "id=~s not found? tag=~s~%" id tag)
     when tag
     append (tag-dependencies tag
                              tag-list)))

(defmethod tag-dependencies ((tag %swf:define-edit-text-tag) tag-list)
  (let ((f (%swf:font-id tag)))
    (when f (cons f (tag-dependencies (find-tag-by-id f tag-list) tag-list)))))

;; need to check for bitmap fills in define-shape :/
(defmethod tag-dependencies ((tag %swf:define-shape-tag) tag-list)
  (tag-dependencies (%swf:shapes tag) tag-list))
(defmethod tag-dependencies ((tag %swf:define-shape-2-tag) tag-list)
  (tag-dependencies (%swf:shapes tag) tag-list))
(defmethod tag-dependencies ((tag %swf:define-shape-3-tag) tag-list)
  (tag-dependencies (%swf:shapes tag) tag-list))
(defmethod tag-dependencies ((tag %swf:define-shape-4-tag) tag-list)
  (tag-dependencies (%swf:shapes tag) tag-list))

(defmethod tag-dependencies ((tag %swf:shape-with-style) tag-list)
  (nconc (loop for i in (%swf:fill-styles (%swf:fill-styles tag))
              append (tag-dependencies i tag-list))
         (loop for i in (%swf:line-styles (%swf:line-styles tag))
            append (tag-dependencies i tag-list))
         (loop for i in (%swf:shape-records tag)
            append (tag-dependencies i tag-list))))

(defmethod tag-dependencies ((tag %swf:fill-style) tag-list)
  nil)

(defmethod tag-dependencies ((tag %swf:fill-repeating-bitmap-fill) tag-list)
  (list (%swf:bitmap-id tag)))
(defmethod tag-dependencies ((tag %swf:fill-clipped-bitmap-fill) tag-list)
  (list (%swf:bitmap-id tag)))
(defmethod tag-dependencies ((tag %swf:fill-non-smoothed-repeating-bitmap-fill)
                             tag-list)
  (list (%swf:bitmap-id tag)))
(defmethod tag-dependencies ((tag %swf:fill-non-smoothed-clipped-bitmap-fill)
                             tag-list)
  (list (%swf:bitmap-id tag)))

(defmethod tag-dependencies ((tag %swf:line-style) tag-list)
  nil)
(defmethod tag-dependencies ((tag %swf:line-style-2) tag-list)
  (if (%swf:has-fill tag)
      (tag-dependencies (%swf:fill-type tag) tag-list)))

(defmethod tag-dependencies ((tag %swf:shape-record) tag-list)
  nil)
(defmethod tag-dependencies ((tag %swf:style-change-shape-record) tag-list)
  (nconc (when (%swf:fill-styles tag)
           (loop for i in (%swf:fill-styles (%swf:fill-styles tag))
              append (tag-dependencies i tag-list)))
         (when (%swf:line-styles tag)
           (loop for i in (%swf:line-styles (%swf:line-styles tag))
              append (tag-dependencies i tag-list)))))



(defun find-tag-by-id (id tag-list)
  (loop for i in (if (listp id) id (list id))
     ;;do (format t "looking for id ~s~%" i)
     do (loop for tag in tag-list
           ;;do (format t "  checking ~s / ~s~%" (%swf:original-character-id tag)  (%swf:new-character-id tag))
           when (typep id '%swf:swf-tag)
           do (return-from find-tag-by-id id)
           when (or (eql i (%swf:original-character-id tag))
                    (eql i (%swf:new-character-id tag)))
           do (return-from find-tag-by-id tag))))

(defun extract-tag (id tag-list &key rename)
  "extract a tag and any tags it depends on, by exported name or by id
TAG-LIST is a list of tags as returned by READ-SWF"
  (let* ((id (if (stringp id) (find-exported-tag id tag-list) id))
         (tag (find-tag-by-id id tag-list))
         (tag-deps (mapcar (lambda (x) (if (listp x) (car x) x))
                           (tag-dependencies tag tag-list))))
    (format t "id = ~s tag id = ~s, rename = ~s~%deps = ~s~%" 
            id (%swf:character-id tag) rename tag-deps)
    (when rename
      (setf (%swf:new-character-id tag) rename))
    (loop for i in tag-list
       for o-id = (%swf:original-character-id i)
       for n-id = (%swf:new-character-id i)
       ;;do (format t " id=~s => ~s~%"  id (and id (member id tag-deps)))
       when (or (eql i tag)
                (and o-id (member o-id tag-deps))
                (and n-id (member n-id tag-deps)))
       collect i)))

(defun extract-tags (id-list tag-list)
  "like EXTRACT-TAG, but avoids duplicating tags (for exaple when
 dependencies are reused)
ID-LIST is a list of either tag IDs, or lists of (tag-id rename-id)
where the tag matching TAG-ID will be renamed to RENAME-ID"
  (let ((deps (loop
                 for %id in id-list
                 for id = (let ((i (if (consp %id) (first %id) %id)))
                            (if (stringp i) (find-exported-tag i tag-list) i))
                 for rename = (if (consp %id) (second %id))
                 for tag = (find-tag-by-id id tag-list)
                 unless tag
                 do (format t "no tag? %id=~s id=~s ren=~s~%" %id id rename )
                 when (and rename (consp (%swf:character-id tag)))
                 do (setf (%swf:new-character-id tag) rename)
                 else do (format t "not renaming ~s to ~s~%" tag rename)
                 append (mapcar (lambda (x)
                                  (if (listp x) (car x) x))
                                (tag-dependencies tag tag-list))
                 collect (or rename id))))
    (loop for i in tag-list
       for o-id = (%swf:original-character-id i)
       for n-id = (%swf:new-character-id i)
       when (or (and o-id (member o-id deps))
                (and n-id (member n-id deps))
                (member i deps))
       collect i)))


(defun write-swf (stream body-tags
                  &key (flash-version 9)
                  (x-twips 400.0) (y-twips 300.0)
                  (frame-rate 30.0)
                  (frame-count  (loop for i in body-tags
                                      when (typep i '%3b-swf::swf-show-frame-tag)
                                      count 1))
                  (attributes-tag nil)
                  (script-limits-stack 1000)
                  (script-limits-timeout 60)
                  compress)
  (format t "framewriting swf,  count = ~s~%" frame-count)
  (%3b-swf::with-character-id-maps
   (let ((%3b-swf::*partial-octet-write* nil)
         (bounds (rect 0 0 x-twips y-twips))
         (header-tags
          (list
           ;; flags: reserved=000, HasMetadata=1,AS3=1,res=00, UseNetwork=1
           #+nil (or attributes-tag
                     (file-attributes
                      :has-metadata (find %3b-swf::+metadata-tag+
                                          body-tags :key '%3b-swf::tag)))
           #+nil(script-limits script-limits-timeout
                               script-limits-stack)))
         (end-tag (end-tag)))
     (flet
         ((body (s)
            (%3b-swf::with-swf-writers (s vvv)
              (%3b-swf::write-swf-part '%3b-swf::rect bounds s)
              (%3b-swf::write-fixed8 frame-rate s)
              (%3b-swf::write-ui16 frame-count s )
              (loop for tag in header-tags
                    do (%3b-swf::write-swf-part nil tag s))
              (loop for tag in body-tags
                    do (%3b-swf::write-swf-part nil tag s))
              (%3b-swf::write-swf-part '%3b-swf::swf-end-tag end-tag s)

              )))
       (%3b-swf::with-swf-writers (stream vv)
         ;; magic #, version
         (if compress
             (%3b-swf::write-ui8 #x43 stream)
             (%3b-swf::write-ui8 #x46 stream)) ;F/C
         (%3b-swf::write-ui8 #x57 stream)      ;W
         (%3b-swf::write-ui8 #x53 stream)      ;S
         (%3b-swf::write-ui8 flash-version stream)
         #+nil(let ((size (+ 2 (reduce '+ body-tags :key 'swf-part-size)) ))
                (format t "size = ~s~%" size)
                (write-ui32 size stream))
         ;; 12 bytes for header
         (let* ((%3b-swf::*swf-sizer-bitpos* (* 12 8)))
           (%3b-swf::%swf-part-size '%3b-swf::rect bounds)
           (loop for tag in header-tags
                 for i from 0
                 do (%3b-swf::%swf-part-size nil tag :body-only nil))
           (loop for tag in body-tags
                 for i from 0
                 do (%3b-swf::%swf-part-size nil tag :body-only nil))

           (%3b-swf::%swf-part-size nil end-tag)
           (format t "size = ~s~%" %3b-swf::*swf-sizer-bitpos*)
           (%3b-swf::write-ui32 (/ %3b-swf::*swf-sizer-bitpos* 8) stream))
         (if compress
             (write-sequence (salza2:compress-data
                              (coerce (flex:with-output-to-sequence (s)
                                        (body s))
                                      '(simple-array (unsigned-byte 8) (*)))
                              'salza2:zlib-compressor)
                             stream)
             (body stream)))))))