(in-package :3b-swf)


(define-swf-type filter-list ()
  :this-var o
  :auto
  ((fcount (ui8) :derived (length (filters o)))
   (filters (counted-list (swf-type 'filter) fcount)))
  :print-unreadably ("~s" (filters o)))

(define-swf-type filter ()
  :this-var o
  :auto
  ;; possibly make a shortcut for tag fields too?
  ;;  (id (ui8) :tag) ?
  ;;  maybe generate the :subclass at the same time?
  ;; (probably not common enough to worry about though)
  ((id (ui8) :derived (subclass-id o 'filter)))
  ;; todo: add a :subclass-by-id shortcut for :subclass (subclass-from-id ..) ?
  :subclass (subclass-from-id 'filter id))

(define-swf-type drop-shadow-filter (filter)
  :id 0
  :auto
  ((color (swf-type 'rgba))
   (blur-x (fixed))
   (blur-y (fixed))
   (angle (fixed))
   (distance (fixed))
   (strength (fixed8))
   (inner-shadow (bit-flag))
   (knockout (bit-flag))
   (composite-source=1 (bit-flag))
   (passes (ub 5))))

(define-swf-type blur-filter (filter)
  :id 1
  :auto
  ((blur-x (fixed))
   (blur-y (fixed))
   (passes (ub 5))
   (reserved (ub 3))))

(define-swf-type glow-filter (filter)
  :id 2
  :auto
  ((glow-color (swf-type 'rgba))
   (blur-x (fixed))
   (blur-y (fixed))
   (strength (fixed8))
   (inner-glow (bit-flag))
   (knockout (bit-flag))
   (composite-source=1 (bit-flag))
   (passes (ub 5))))

(define-swf-type bevel-filter (filter)
  :id 3
  :auto
  ((shadow-color (swf-type 'rgba))
   (highlight-color (swf-type 'rgba))
   (blur-x (fixed))
   (blur-y (fixed))
   (angle (fixed))
   (distance (fixed))
   (strength (fixed8))
   (inner-shadow (bit-flag))
   (knockout (bit-flag))
   (composite-source=1 (bit-flag))
   (on-top (bit-flag))
   (passes (ub 4))))

(define-swf-type gradient-glow-filter (filter)
  :id 4
  :auto
  ((num-colors (ui8))
   (gradient-colors (counted-list (swf-type 'rgba) num-colors))
   (gradient-ratio (counted-list (ui8) num-colors))
   (blur-x (fixed))
   (blur-y (fixed))
   (angle (fixed))
   (distance (fixed))
   (strength (fixed8))
   (inner-shadow (bit-flag)) ;; inner-glow?
   (knockout (bit-flag))
   (composite-source=1 (bit-flag))
   (on-top (bit-flag))
   (passes (ub 4))
   ))

(define-swf-type convolution-filter (filter)
  :id 5
  :auto
  ;; fixme: this should probably use a 2d array instead of a list and explicit w/h slots
  ((matrix-x (ui8))
   (matrix-y (ui8))
   (divisor (float32))
   (bias (float32))
   (matrix (counted-list (float32) (* matrix-x matrix-y)))
   (default-color (swf-type 'rgba))
   (reserved (ub 6))
   (clamp (bit-flag))
   (preserve-alpha (bit-flag))))
(defmethod %filter-id ((f convolution-filter)) 5)


(define-swf-type color-matrix-filter (filter)
  :id 6
  :auto
  ((matrix (counted-list (float32) 20))))

(define-swf-type gradient-bevel-filter (filter)
  :id 7
  :this-var o
  :auto
  ((num-colors (ui8)
               :derived (progn
                          (assert (= (length (gradient-colors o))
                                     (length (gradient-ratio o))))
                          (length (gradient-ratio o))))
   (gradient-colors (counted-list (swf-type 'rgba) num-colors))
   (gradient-ratio (counted-list (ui8) num-colors))
   (blur-x (fixed))
   (blur-y (fixed))
   (angle (fixed))
   (distance (fixed))
   (strength (fixed8))
   (inner-shadow (bit-flag)) ;; inner-bevel?
   (knockout (bit-flag))
   (composite-source=1 (bit-flag))
   (on-top (bit-flag))
   (passes (ub 4))))