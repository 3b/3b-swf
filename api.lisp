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
    (t (error "unknown args in rgb ~s" args))))


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
  (if matrix
      (error "can't rotate matrices yet...")
      (let ((s (sin (/ (* angle-deg pi) 180)))
            (c (cos (/ (* angle-deg pi) 180))))
        (make-instance
         '%swf:matrix
         '%swf:rotate-skew (make-instance '%swf:matrix-part-fixed
                                          '%swf:value1 s '%swf:value2 (- s))
         '%swf:scale (make-instance '%swf:matrix-part-fixed
                                    '%swf:value1 c '%swf:value2 c)))))

(defun scale (sx sy &optional matrix)
  (if matrix
      (error "can't scale matrices yet...")
        (make-instance
         '%swf:matrix
         '%swf:scale (make-instance '%swf:matrix-part-fixed
                                    '%swf:value1 sx '%swf:value2 sy))))

(defun translate (tx ty &optional matrix)
  (if matrix
      (progn (incf (%swf:value1 (%swf:translate matrix)) tx)
             (incf (%swf:value2 (%swf:translate matrix)) ty))
      (make-instance
       '%swf:matrix
       '%swf:translate (make-instance '%swf:matrix-part-translate
                                      '%swf:value1 tx '%swf:value2 ty))))

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
                 '%swf:character-id id
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
                 '%swf:character-id id
                 '%swf:matrix matrix
                 '%swf:color-transform color-transform
                 '%swf:po2-ratio ratio
                 '%swf:name name
                 '%swf:clip-depth clip-layers
                 '%swf:clip-actions actions))
;;todo: more single purpose wrappers for place-object? (move-object, etc)
(defun place-object-at (id depth x y &key (sx 1 has-sx) (sy 1 has-sy) move-p)
  (if (or has-sx sy)
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