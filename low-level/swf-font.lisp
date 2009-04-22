(in-package :%3b-swf)

(define-swf-type zone-record ()
  :this-var o
  :auto
  ((num-zone-data (ui8) :derived (length (zone-data o)))
   (zone-data (counted-list (swf-type 'zone-data) num-zone-data))
   (reserved (ub 6))
   (zone-mask-y (bit-flag))
   (zone-mask-x (bit-flag))))

(define-swf-type zone-data ()
  :auto ((alignment-coordinate (float16))
         (range (float16))))

;; use when not font-flags-wide-code
(define-swf-type kerning-record-narrow ()
  :auto
  ((font-kerning-code-1 (ui8))
   (font-kerning-code-2 (ui8))
   (font-kerning-adjustment (si16))))
;; use when font-flags-wide-code
(define-swf-type kerning-record-wide ()
  :auto
  ((font-kerning-code-1 (ui16))
   (font-kerning-code-2 (ui16))
   (font-kerning-adjustment (si16))))

(defvar *define-text-ver*)
(define-swf-type text-record ()
  :this-var o
  :auto
  ((text-record-type (bit-flag :align 8) :derived (if (subclass-id o 'text-record) 1 0))
   ;; text-record-type is supposedly always 1, but doesn't appear to be?
   (reserved (ub 3))
   (has-font (bit-flag) :derived (font-id o))
   (has-color (bit-flag) :derived (text-color o))
   (has-y-offset (bit-flag) :derived (y-offset o))
   (has-x-offset (bit-flag) :derived (x-offset o)))
  :subclass (if (or text-record-type has-font
                    has-color has-x-offset has-y-offset)
                'text-record-full
                'text-record-end))

(define-swf-type text-record-end (text-record)
  :id 0
  )

(define-swf-type text-record-full (text-record)
  :id 1
  :this-var o
  :auto
  ((font-id (ui16) :optional (super has-font))
   (text-color (swf-type (if (= 1 *define-text-ver*) 'rgb 'rgba))
               :optional (super has-color))
   (x-offset (si16) :optional (super has-x-offset))
   (y-offset (si16) :optional (super has-y-offset))
   (text-height (twips-u16) :optional (super has-font))
   (glyph-count (ui8) :derived (length (glyph-entries o)))
   (glyph-entries (counted-list (swf-type 'glyph-entry) glyph-count))))

(defvar *glyph-bits*)
(defvar *advance-bits*)
(define-swf-type glyph-entry ()
  :auto
  ((glyph-index (ub *glyph-bits*))
   (glyph-advance (sb *advance-bits*))))