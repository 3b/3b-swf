(in-package :3b-swf)

;;; fixme: factor out the common parts between the foo-*-style-array types

(define-swf-type morph-fill-style-array ()
  :this-var o
  :value-var value
  :auto
  ((fill-style-count () :derived (length (fill-styles o)))
   (fill-styles (counted-list (swf-type 'morph-fill-style)
                              fill-style-count)))
  :reader ((fill-style-count
            (let ((short (ui8)))
              (if (= short #xff)
                  (ui16)
                  short))))
  :sizer ((fill-style-count
           (progn
             (ui8)
             (when (>= value #xff)
               (ui16)))))
  :writer ((fill-style-count
            (if (>= value #xff)
                (progn
                  (let ((value #xff))
                    (ui8))
                  (ui16))
                (ui8))))
  :print-unreadably ("~{~s~#[~:; ~:_~]~}" (if *array-print-verbose* (fill-styles o) (list 'length (length (fill-styles o)))) ))

;; fixme: can these be combined with normal fill-style?
(define-swf-type morph-fill-style ()
  :this-var o
  :auto ((fill-style-type (ui8) :derived (subclass-id o 'morph-fill-style)))
  :subclass (subclass-from-id 'morph-fill-style fill-style-type))

(define-swf-type morph-fill-style-solid (morph-fill-style)
  :id #x00
  :auto ((start-color (swf-type 'rgba))
         (end-color (swf-type 'rgba))))

(define-swf-type morph-fill-linear-gradient (morph-fill-style)
  :id #x10
  :auto ((start-gradient-matrix (swf-type 'matrix))
         (end-gradient-matrix (swf-type 'matrix))
         (gradient (swf-type 'morph-gradient))))

(define-swf-type morph-fill-radial-gradient (morph-fill-style)
  :id #x12
  :auto ((start-gradient-matrix (swf-type 'matrix))
         (end-gradient-matrix (swf-type 'matrix))
         (gradient (swf-type 'morph-gradient))))

(define-swf-type morph-fill-repeating-bitmap-fill (morph-fill-style)
  :id #x40
  :auto ((bitmap-id (ui16))
         (start-bitmap-matrix (swf-type 'matrix))
         (end-bitmap-matrix (swf-type 'matrix))))

(define-swf-type morph-fill-clipped-bitmap-fill (morph-fill-style)
  :id #x41
  :auto ((bitmap-id (ui16))
         (start-bitmap-matrix (swf-type 'matrix))
         (end-bitmap-matrix (swf-type 'matrix))))

(define-swf-type morph-fill-non-smoothed-repeating-bitmap-fill (morph-fill-style)
  :id #x42
  :auto ((bitmap-id (ui16))
         (start-bitmap-matrix (swf-type 'matrix))
         (end-bitmap-matrix (swf-type 'matrix))))

(define-swf-type morph-fill-non-smoothed-clipped-bitmap-fill (morph-fill-style)
  :id #x43
  :auto ((bitmap-id (ui16))
         (start-bitmap-matrix (swf-type 'matrix))
         (end-bitmap-matrix (swf-type 'matrix))))

(define-swf-type morph-gradient ()
  :this-var o
  :auto
  ((num-gradients (ui8) :derived (length (gradient-records o)))
   (gradient-records (counted-list (swf-type 'morph-grad-record) num-gradients))))

(define-swf-type morph-grad-record ()
  :auto
  ((start-ratio (ui8))
   (start-color (swf-type 'rgba))
   (end-ratio (ui8))
   (end-color (swf-type 'rgba))))

;; fixme: use separate types instead of a flag?
(defvar *morph-shape-ver*)
(define-swf-type morph-line-style-array ()
  :this-var o
  :value-var value
  :auto
  ((line-style-count () :derived (length (line-styles o)))
   (line-styles (counted-list (swf-type (ecase *morph-shape-ver*
                                          (1 'morph-line-style)
                                          (2 'morph-line-style-2)))
                              line-style-count)))
  :reader ((line-style-count
            (let ((short (ui8)))
              (if (= short #xff)
                  (ui16)
                  short))))
  :sizer ((line-style-count
           (progn
             (ui8)
             (when (>= value #xff)
               (ui16)))))
  :writer ((line-style-count
            (if (>= value #xff)
                (progn
                  (let ((value #xff))
                    (ui8))
                  (ui16))
                (ui8)))))

(define-swf-type morph-line-style ()
  :auto
  ((start-width (twips-u16))
   (end-width (twips-u16))
   (start-color (swf-type 'rgba))
   (end-color (swf-type 'rgba))))

(define-swf-type morph-line-style-2 ()
  :this-var o
  :auto
  ((start-width (twips-u16))
   (end-width (twips-u16))
   (start-cap-style (ub 2))
   (join-style (ub 2))
   (has-fill (bit-flag) :derived (not (null (fill-type o))))
   (no-h-scale (bit-flag))
   (no-v-scale (bit-flag))
   (pixel-hinting (bit-flag))
   (reserved (ub 5))
   (no-close (bit-flag))
   (end-cap-style (ub 2))
   (miter-limit-factor (ub 2) :optional (= join-style 2))
   (start-color (swf-type 'rgba) :optional (not has-fill))
   (end-color (swf-type 'rgba) :optional (not has-fill))
   (fill-type (swf-type 'morph-fill-style) :optional has-fill)))
