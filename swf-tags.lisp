(in-package :3b-swf)

(define-swf-type swf-show-frame-tag (swf-tag)
  :id 1)

(defvar *shape-tag-version*)
(define-swf-type define-shape-tag (swf-tag)
  :id 2
  :auto
  ((*shape-tag-version* 1 :local t)
   (shape-id (ui16))
   (bounds (swf-type 'rect))
   (shapes (swf-type 'shape-with-style))))

(define-swf-type place-object-tag (swf-tag)
  :id 4
  :auto
  ((character-id (ui16))
   (depth (ui16))
   (matrix (swf-type 'matrix))
   (color-xform (swf-type 'cxform) :optional (not (zerop (bytes-left-in-tag))))))

(define-swf-type remove-object-tag (swf-tag)
  :id 5
  :auto
  ((character-id (ui16))
   (depth (ui16))))

;;#+untested
(define-swf-type define-bits-tag (swf-tag)
  :id 6
  :auto
  ((character-id (ui16))
   (image-data (rest-of-tag))))

#+broken
(define-swf-type define-button (swf-tag)
  :id 7
  :auto
  ((button-id (ui16))
   (characters (error "can't read terminated button-record list yet")
               ;; can't just test flags after reading, since it reads
               ;; too many fields
               (list-until (swf-type 'button-record)
                           (lambda (x) (typep))))
   (actions (list-until-type (swf-type 'action-record) 'action-record-end))))

#+untested
(define-swf-type jpeg-tables (swf-tag)
  :id 8
  :auto
  ((jpeg-data (rest-of-tag))))

(define-swf-type set-background-color-tag (swf-tag)
  :id 9
  :auto ((background-color (swf-type 'rgb))))

(define-swf-type define-font-tag (swf-tag)
  :id 10
  :this-var o
  :auto
  ((font-id (ui16))
   ;; this is sort of ugly, # of entries is derived from first entry,
   ;; so splitting into pieces...
   (first-offset (ui16) :derived (* 2 (length (glyph-shape-table o))))
   (offset-table-rest (counted-list (ui16) (1- (/ first-offset 2)))
                      :derived (loop for (i . more) on (glyph-shape-table o)
                                     when more
                                     collect (+ (first-offset o)
                                                (swf-part-size i))))
   (*shape-tag-version* 1 :local t
                        :extra (format t "d-f-t id=~s offset=~s~%~s~%table=~s~%"
                                       font-id first-offset offset-table-rest
                                       (cons first-offset offset-table-rest)))
   ;;fixme: is this right?
   (glyph-shape-table (counted-list (swf-type 'shape) (/ first-offset 2)))))

(define-swf-type define-text-tag (swf-tag)
  :id 11
  :auto
  ((*define-text-ver* 1 :local t)
   (character-id (ui16))
   (bounds (swf-type 'rect))
   (matrix (swf-type 'matrix))
   (glyph-bits (ui8))
   (*glyph-bits* glyph-bits :local t)
   (advance-bits (ui8))
   (*advance-bits* advance-bits :local t)
   (text-records (list-until-type (swf-type 'text-record) 'text-record-end)))
  )

(define-swf-type do-action (swf-tag)
  :id 12
  :auto ((actions (list-until-type (swf-type 'action-record) 'action-record-end))))

#+untested
(define-swf-type define-font-info-tag (swf-tag)
  :id 13
  :this-var o
  :auto
  ((font-id (ui16))
   (font-name-len (ui8))
   ;;todo: parse this to/from characters?
   (font-name (counted-list (ui8) font-name-len))
   (reserved (ub 2))
   (small-text (bit-flag))
   (shift-jis (bit-flag))
   (ansi (bit-flag))
   (italic (bit-flag))
   (bold (bit-flag))
   (wide-codes (bit-flag))
   (code-table (sized-list (if wide-codes (ui16) (ui8)) (bytes-left-in-tag)))))

(define-swf-type define-sound-tag (swf-tag)
  :id 14
  :this-var o
  :auto
  ((sound-id (ui16))
   (sound-format (ub 4))
   (sound-rate (ub 2)) ;0=5.5khz,1=11k,2=22,3=44
   (16bit (bit-flag))  ;; else 8 bit
   (stereo (bit-flag))
   (sound-sample-count (ui32))
   (sound-data (rest-of-tag)))
  :print-unreadably ("id:~s format:~s rate=~s 16b=~s stereo=~s count=~s"
                     (sound-id o) (sound-format o) (sound-rate o)
                     (16bit o) (stereo o) (sound-sample-count o)))

(define-swf-type start-sound-tag (swf-tag)
  :id 15
  :auto ((sound-id (ui16))
         (sound-info (swf-type 'sound-info))))

(define-swf-type define-button-sound (swf-tag)
  :id 17
  :auto
  ((button-id (ui16))
   (button-sound-char-0 (ui16))
   (button-sound-info-0 (swf-type 'sound-info)
                        :optional (not (zerop button-sound-char-0)))
   (button-sound-char-1 (ui16))
   (button-sound-info-1 (swf-type 'sound-info)
                        :optional (not (zerop button-sound-char-1)))
   (button-sound-char-2 (ui16))
   (button-sound-info-2 (swf-type 'sound-info)
                        :optional (not (zerop button-sound-char-2)))
   (button-sound-char-3 (ui16))
   (button-sound-info-3 (swf-type 'sound-info)
                        :optional (not (zerop button-sound-char-3)))))

(define-swf-type sound-stream-head (swf-tag)
  :id 18
  :auto
  ((reserved (ub 4))
   (playback-sound-rate (ub 2))
   (playback-16-bit (bit-flag)) ;; playback-sound-size
   (playback-stereo (bit-flag)) ;; playback-sound-type
   (stream-sound-compression (ub 4))
   (stream-sound-rate (ub 2))
   (stream-16-bit (bit-flag)) ;; stream-sound-size
   (stream-stereo (bit-flag)) ;; stream-sound-type
   (stream-sound-sample-count (ui16))
   (latency-seek (si16) :optional (= stream-sound-compression 2))))

(define-swf-type sound-stream-block (swf-tag)
  :id 19
  :auto ((sound-data (rest-of-tag))))

(define-swf-type define-bits-lossless-tag (swf-tag)
  :id 20
  :auto
  ((character-id (ui16))
   (bitmap-data (swf-type 'bitmap-tag-data-rgb))   ))

(define-swf-type define-bits-jpeg-2-tag (swf-tag)
  :id 21
  :auto
  ((character-id (ui16))
   (image-data (rest-of-tag)))
  :this-var o
  :print-unreadably ("id=~s bytes=~s" (character-id o) (length (image-data o))))

(define-swf-type define-shape-2-tag (swf-tag)
  :id 22
  :auto
  ((*shape-tag-version* 2 :local t)
   (shape-id (ui16))
   (bounds (swf-type 'rect))
   (shapes (swf-type 'shape-with-style))))

#+untested
(define-swf-type define-button-cxform-tag (swf-tag)
  :id 23
  :auto ((button-id (ui16))
         (button-color-transform (swf-type 'cxform))))

(define-swf-type protect-tag (swf-tag)
  :id 24
  :auto ((password (rest-of-tag))))

(define-swf-type place-object-2-tag (swf-tag)
  :id 26
  :this-var o
  :auto
  ((has-clip-actions (bit-flag) :derive (not (null (clip-actions o))))
   (has-clip-depth (bit-flag) :derive (not (null (clip-depth o))))
   (has-name (bit-flag) :derive (not (null (name o))))
   (has-ratio (bit-flag) :derive (not (null (po2-ratio o))))
   (has-color-transform (bit-flag) :derive (not (null (color-transform o))))
   (has-matrix (bit-flag) :derive (not (null (matrix o))))
   (has-character (bit-flag) :derive (not (null (character-id o))))
   (move-flag (bit-flag))
   (depth (ui16))
   (character-id (ui16) :optional  has-character)
   (matrix (swf-type 'matrix) :optional has-matrix)
   (color-transform (swf-type 'cxform-with-alpha) :optional has-color-transform)
   (po2-ratio (ui16) :optional has-ratio)
   (name (string-sz-utf8) :optional has-name)
   (clip-depth (ui16) :optional has-clip-depth)
   (clip-actions (swf-type 'clip-actions) :optional has-clip-actions)))

(define-swf-type remove-object-2-tag (swf-tag)
  :id 28
  :auto
  ((depth (ui16))))


(define-swf-type define-shape-3-tag (swf-tag)
  :id 32
  :auto
  ((*shape-tag-version* 3 :local t)
   (shape-id (ui16))
   (shape-bounds (swf-type 'rect))
   (shapes (swf-type 'shape-with-style))))

(define-swf-type define-text-2-tag (swf-tag)
  :id 33
  :auto
  ((*define-text-ver* 2 :local t)
   (character-id (ui16))
   (bounds (swf-type 'rect))
   (matrix (swf-type 'matrix))
   (glyph-bits (ui8))
   (*glyph-bits* glyph-bits :local t)
   (advance-bits (ui8))
   (*advance-bits* advance-bits :local t)
   (text-records (list-until (swf-type 'text-record) (lambda (x) x (next-octet-zero-p))))))


(define-swf-type define-button-2-tag (swf-tag)
  :id 34
  :auto
  ((*define-button-ver* 2 :local t)
   (button-id (ui16))
   (reserved-flags (ub 7))
   (track-as-menu (bit-flag))
   (action-offset (ui16)
                  #+nil(format t "action-offset=~s~%size=~s~%"
                          action-offset
                           (if (zerop action-offset)
                               (- (bytes-left-in-tag) 1)
                               (- action-offset 3))))
   (characters (sized-list (swf-type 'button-record)
                           (if (zerop action-offset)
                               (- (bytes-left-in-tag) 1)
                               (- action-offset 3))))
   (end-of-characters (ui8)) ;;0
   (actions (sized-list (swf-type 'button-cond-action) (bytes-left-in-tag)))))

(define-swf-type define-bits-jpeg-3-tag (swf-tag)
  :id 35
  :this-var o
  :auto
  ((character-id (ui16))
   (alpha-data-offset (ui32) :derived (length (image-data o)))
   (image-data (counted-list (ui8) alpha-data-offset))
   (bitmap-alpha-data (rest-of-tag)))
  :print-unreadably ("id=~s bytes=~s alpha-bytes=~s" (character-id o) (length (image-data o)) (length (bitmap-alpha-data o))))


(define-swf-type define-bits-lossless-2-tag (swf-tag)
  :id 36
  :auto
  ((character-id (ui16))
   (bitmap-data (swf-type 'bitmap-tag-data-rgba))))

(define-swf-type define-edit-text-tag (swf-tag)
  :id 37
  :this-var o
  :auto
  ((character-id (ui16))
   (bounds (swf-type 'rect))
   (has-text (bit-flag :align 8) :derived (not (null (initial-text o))))
   (word-wrap (bit-flag))
   (multiline (bit-flag))
   (password (bit-flag))
   (read-only (bit-flag))
   (has-text-color (bit-flag) :derived (not (null (text-color o))))
   (has-max-length (bit-flag) :derived (not (null (max-length o))))
   (has-font (bit-flag) :derived (not (null (font-id o))))
   (has-font-class (bit-flag) :derived (not (null (font-class o))))
   (auto-size (bit-flag))
   (has-layout (bit-flag) :derived (not (null (or (align o) (left-margin o)
                                                  (right-margin o) (indent o)
                                                  (leading o)))))
   (no-select (bit-flag))
   (border (bit-flag))
   (was-static (bit-flag))
   (html (bit-flag))
   (use-outline (bit-flag))
   (font-id (ui16) :optional has-font)
   (font-class (string-sz-utf8) :optional has-font-class)
   (font-height (ui16) :optional (or has-font has-font-class))
   (text-color (swf-type 'rgba) :optional has-text-color )
   (max-length (ui16) :optional has-max-length )
   ;; fixme: make sure these write out even if nil (or make sure that
   ;; setting 1 sets all of them
   (align (ui8) :optional has-layout)
   (left-margin (ui16) :optional has-layout)
   (right-margin (ui16) :optional has-layout)
   (indent (ui16) :optional has-layout)
   (leading (si16) :optional has-layout)

   (variable-name (string-sz-utf8))
   (initial-text (string-sz-utf8) :optional has-text)))

(define-swf-type define-sprite-tag (swf-tag)
  :id 39
  :auto
  ((sprite-id (ui16))
   (frame-count (ui16))
   (control-tags (list-until (swf-type 'swf-tag)
                             (lambda (x)
                               (typep x 'swf-end-tag))))))

(define-swf-type frame-label-tag (swf-tag)
  :id 43
  :auto
  ((name (string-sz-utf8))
   ;; todo: convert to/from boolean (0/1)
   (named-anchor-flag (ui8) :optional (not (zerop (bytes-left-in-tag))))))


(define-swf-type sound-stream-head-2-tag (swf-tag)
  :id 45
  :auto
  ((reserved (ub 4 :align 8))
   (playback-sound-rate (ub 2))
   (playback-16-bit (bit-flag)) ;; playback-sound-size
   (playback-stereo (bit-flag)) ;; playback-sound-type
   (stream-sound-compression (ub 4))
   (stream-sound-rate (ub 2))
   (stream-16-bit (bit-flag))
   (stream-stereo (bit-flag))
   (stream-sound-sample-count (ui16))
   (latency-seek (si16) :optional (and (= stream-sound-compression 2)
                                       ;; work around buggy files (or buggy spec?)...
                                       (not (zerop (bytes-left-in-tag)))))))

(define-swf-type define-morph-shape (swf-tag)
  :id 46
  :auto
  ((*morph-shape-ver* 1 :local t )
   (*shape-tag-version* 1 :local t) ;; fixme: is this right?
   (character-id (ui16))
   (start-bounds (swf-type 'rect))
   (end-bounds (swf-type 'rect))
   ;; fixme: derive this (offset to end-edges field), and use it...
   (offset (ui32)
           ;; not sure if this is file bug or parser bug yet...
           :extra (when (zerop offset)
                    (format t "got empty morph-shape tag? id=~s~%"
                            character-id)))
   (morph-fill-styles (swf-type 'morph-fill-style-array))
   (morph-line-styles (swf-type 'morph-line-style-array))
   (start-edges (swf-type 'shape) :optional (not (zerop offset)))
   (end-edges (swf-type 'shape))
))

;; fixme: determine if the scaling in font-2 vs font-3 needs to affect anything
;;   in nested data?
(define-swf-type define-font-2-tag (swf-tag)
  :id 48
  :this-var o
  :auto
  ((font-id (ui16))
   (has-layout (bit-flag)
               :derived (not (null (or (font-ascent o) (font-descent o)
                                       (font-leading o) (font-advance-table o)
                                       (font-bounds-table o) (kerning-count o)
                                       (kerning-table o)))))
   (shift-jis (bit-flag))
   (small-text (bit-flag))
   (ansi (bit-flag))
   (wide-offsets (bit-flag)) ;; fixme: derive this if possible?
   (wide-codes (bit-flag))
   (italic (bit-flag))
   (bold (bit-flag))
   (language-code (ui8));; lang-code 1=latin,2=jp,3=kr,4=simplified ch,5=trad ch
   (font-name-len (ui8))
   (font-name (counted-list (ui8) font-name-len))
   (num-glyphs (ui16)
               :extra (progn
                        #+nil(format t "id:~s l~s j~s s~s a~s wo ~s wc ~s i~s b~s lc~s namelen~s~%name ~s~%glyphs ~s~%"
                               font-id
                               has-layout shift-jis small-text ansi wide-offsets wide-codes italic bold
                               language-code font-name-len font-name num-glyphs)))
   (offset-table (counted-list (if wide-offsets (ui32) (ui16)) num-glyphs))
   (code-table-offset (if wide-offsets (ui32) (ui16))
                      :extra (progn
                               #+nil(format t "offset-table=~s~%code-table-offset=~s from ~s~%"
                                             offset-table code-table-offset
                                                 (bytes-left-in-tag))))
   (*shape-tag-version* 1 :local t) ;;fixme: what is correct value?
   #+nil(foo 0 :local t)
   ;; fixme: figure out how to use the offset table here instead of relying on
   ;; the *allow-short-shapes* hack
   (*allow-short-shapes* t :local t)
   (glyph-shape-table (counted-list (progn
                                      #+nil(format t "reading shape ~s, ~s bytes left~%"
                                              (incf foo) (bytes-left-in-tag))
                                      (swf-type 'shape)) num-glyphs)
                      :extra (format t "after shapes, ~s bytes left~%"
                                     (bytes-left-in-tag)))
   (code-table (counted-list (if wide-codes (ui16) (ui8)) num-glyphs))
   (font-ascent (si16) :optional has-layout)
   (font-descent (si16) :optional has-layout)
   (font-leading (si16) :optional has-layout)
   (font-advance-table (counted-list (si16) num-glyphs) :optional has-layout)
   (font-bounds-table (counted-list (swf-type 'rect) num-glyphs) :optional has-layout)
   (kerning-count (ui16) :optional has-layout)
   (kerning-table (counted-list (swf-type  (if wide-codes
                                            'kerning-record-wide
                                            'kerning-record-narrow))
                                kerning-count) :optional has-layout)))

(define-swf-type export-assets-tag (swf-tag)
  :id 56
  :auto
  ((asset-count (ui16))
   (assets (counted-list (list (ui16) (string-sz-utf8)) asset-count)
           :extra (format t "exported assets~%~s~%" assets))))

#+untested
(define-swf-type import-assets-tag (swf-tag)
  :id 57
  :auto
  ((url (string-sz-utf8))
   (asset-count (ui16))
   (assets (counted-list (list (ui16) (string-sz-utf8)) asset-count))))

#+untested
(define-swf-type enable-debugger (swf-tag)
  :id 58
  :auto ((password (string-sz-utf8))))

(define-swf-type do-init-action-tag (swf-tag)
  :id 59
  :auto ((shape-id (ui16))
         (actions (sized-list (swf-type 'action-record)
                              (1- (bytes-left-in-tag))))
         (action-end-flag (ui8) :derived 0)))

(define-swf-type define-video-stream-tag (swf-tag)
  :id 60
  :auto
  ((character-id (ui16))
   (num-frames (ui16))
   (width (ui16))
   (height (ui16))
   (reserved (ub 4))
   (flags-deblocking (ub 3))
   (flags-smoothing (bit-flag))
   (codec-id (ui8))))

(define-swf-type video-frame-tag (swf-tag)
  :id 61
  :auto
  ((stream-id (ui16))
   (frame-num (ui16))
   ;; not bothering to unpack video frames for now...
   (video-data (rest-of-tag))))

(define-swf-type define-font-info-2-tag (swf-tag)
  :id 62
  :this-var o
  :auto
  ((font-id (ui16))
   (font-name-len (ui8))
   ;;todo: parse this to/from characters?
   (font-name (counted-list (ui8) font-name-len))
   (reserved (ub 2))
   (small-text (bit-flag))
   (shift-jis (bit-flag))
   (ansi (bit-flag))
   (italic (bit-flag))
   (bold (bit-flag))
   (wide-codes (bit-flag))
   (language-code (ui8))
   (code-table (sized-list (if wide-codes (ui16) (ui8)) (bytes-left-in-tag)))))

#+untested
(define-swf-type enable-debugger-2 (swf-tag)
  :id 64
  :auto ((reserved (ui16))
         (password (string-sz-utf8))))

(define-swf-type script-limits-tag (swf-tag)
  :id 65
  :auto
  ((max-recurson-depth (ui16))
   (script-timeout-seconds (ui16))))

#+untested
(define-swf-type set-tab-index (swf-tag)
  :id 66
  :auto ((depth (ui16))
         (tab-index (ui16))))
(define-swf-type file-attributes-tag (swf-tag)
  :id 69
  :auto
  ((reserved1=0 (ub 1)) ;; 0
   (use-direct-blit (bit-flag))
   (use-gpu (bit-flag))
   (has-metadata (bit-flag))
   (actionscript-3 (bit-flag))
   (reserved2=0 (ub 2))
   (use-network (bit-flag))
   (reserved3=0 (ub 24))))

(define-swf-type place-object-3-tag (swf-tag)
  :id 70
  :this-var o
  :auto
  ((has-clip-actions (bit-flag) :derived (not (null (clip-actions o))))
   (has-clip-depth (bit-flag) :derived (not (null (clip-depth o))))
   (has-name (bit-flag) :derived (not (null (name o))))
   (has-ratio (bit-flag) :derived (not (null (po3-ratio o))))
   (has-color-transform (bit-flag) :derived (not (null (color-transform o))))
   (has-matrix (bit-flag) :derived (not (null (matrix o))))
   (has-character (bit-flag) :derived (not (null (character-id o))))
   (move-flag (bit-flag))
   (reserved3=0 (ub 3))
   (has-image (bit-flag)) ;; fixme: derive this?
   (has-class-name (bit-flag) :derived (not (null (class-name o))))
   (has-cache-as-bitmap (bit-flag) :derived (not (null (bitmap-cache o))))
   (has-blend-mode (bit-flag) :derived (not (null (blend-mode o))))
   (has-filter-list (bit-flag) :derived (not (null (surface-filter-list o))))
   (depth (ui16))
   ;; is the logic for this right?
   (class-name (string-sz-utf8) :optional (or has-class-name
                                              (and has-image
                                                   has-character)))
   (character-id (ui16) :optional has-character)
   (matrix (swf-type 'matrix) :optional has-matrix)
   (color-transform (swf-type 'cxform-with-alpha) :optional has-color-transform)
   (po3-ratio (ui16) :optional has-ratio)
   (name (string-sz-utf8) :optional has-name)
   (clip-depth (ui16) :optional has-clip-depth)
   ;;(todo...filters raw-bytes-to-end-of-tag)
   (surface-filter-list (swf-type 'filter-list) :optional has-filter-list)
   (blend-mode (ui8) :optional has-blend-mode)
   (bitmap-cache (ui8) :optional has-cache-as-bitmap)
   (clip-actions (swf-type 'clip-actions) :optional has-clip-actions))
)

#+untested
(define-swf-type import-assets-2-tag (swf-tag)
  :id 71
  :auto
  ((url (string-sz-utf8))
   (reserved (ui8) :derived 1)
   (reserved2 (ui8) :derived 0)
   (asset-count (ui16))
   (assets (counted-list (list (ui16) (string-sz-utf8)) asset-count))))

(define-swf-type define-font-align-zones-tag (swf-tag)
  :id 73
  :auto
  ((font-id (ui16))
   (cms-table-hint (ub 2))
   (reserved (ub 6))
   ;; not sure if we can just read to end of tag, or if we need to look
   ;; up glyph count from font
   ;;(zone-table (counted-list ...))
   (zone-table (sized-list (swf-type 'zone-record) (bytes-left-in-tag)))))

(define-swf-type csm-text-settings-tag (swf-tag)
  :id 74
  :auto
  ((text-id (ui16))
   (use-flash-type (ub 2))
   (grid-fit (ub 3))
   (reserved (ub 3))
   (thickness (float32))
   (sharpness (float32))
   (reserved2 (ui8))))

(define-swf-type define-font-3-tag (swf-tag)
  :id 75
  :this-var o
  :auto
  ((font-id (ui16))
   (has-layout (bit-flag)
               :derived (not (null (or (font-ascent o) (font-descent o)
                                       (font-leading o) (font-advance-table o)
                                       (font-bounds-table o) (kerning-count o)
                                       (kerning-table o)))))
   (shift-jis (bit-flag))
   (small-text (bit-flag))
   (ansi (bit-flag))
   (wide-offsets (bit-flag)) ;; fixme: derive this if possible?
   (wide-codes (bit-flag))
   (italic (bit-flag))
   (bold (bit-flag))
   (language-code (ui8));; lang-code 1=latin,2=jp,3=kr,4=simplified ch,5=trad ch
   (font-name-len (ui8))
   (font-name (counted-list (ui8) font-name-len))
   (num-glyphs (ui16))
   (offset-table (counted-list (if wide-offsets (ui32) (ui16)) num-glyphs))
   (code-table-offset (if wide-offsets (ui32) (ui16)))
   (*shape-tag-version* 1 :local t) ;;fixme: what is correct value?
   (glyph-shape-table (counted-list (swf-type 'shape) num-glyphs))
   (code-table (counted-list (ui16) num-glyphs))
   (font-ascent (si16) :optional has-layout)
   (font-descent (si16) :optional has-layout)
   (font-leading (si16) :optional has-layout)
   (font-advance-table (counted-list (si16) num-glyphs) :optional has-layout)
   (font-bounds-table (counted-list (swf-type 'rect) num-glyphs) :optional has-layout)
   (kerning-count (ui16) :optional has-layout)
   (kerning-table (counted-list (swf-type (if wide-codes
                                            'kerning-record-wide
                                            'kerning-record-narrow))
                                kerning-count) :optional has-layout)))

(define-swf-type symbol-class-tag (swf-tag)
  :id 76
  :this-var o
  :auto
  ((num-symbols (ui16) :derived (length (symbol-class-symbols o)))
   ;; { (tag u16) (name string) } *num-symbols
   (symbol-class-symbols (counted-list (list (ui16) (string-sz-utf8))
                                        num-symbols)))
  :print-unreadably ("~s" (symbol-class-symbols o)))

(define-swf-type metadata-tag (swf-tag)
  :id 77
  :auto ((metadata (string-sz-utf8)
                   :extra (format t "metadata=~s~%" metadata))))

(define-swf-type do-abc-tag (swf-tag)
  :id 82
  :this-var o
  :auto
  ((flags (ui32))
   (name (string-sz-utf8))
   (data (rest-of-tag)))
  :print-unreadably ("flags:~x name:~s ~d bytes" (flags o) (name o) (length (data o))))

(define-swf-type define-shape-4-tag (swf-tag)
  :id 83
  :auto
  ((*shape-tag-version* 4 :local t)
   (shape-id (ui16))
   (shape-bounds (swf-type 'rect))
   (edge-bounds (swf-type 'rect))
   (reserved (ub 5 ))
   (uses-fill-winding-rule (bit-flag))
   (uses-non-scaling-strokes (bit-flag))
   (uses-scaling-strokes (bit-flag))
   (shapes (swf-type 'shape-with-style))))

(define-swf-type define-morph-shape-2-tag (swf-tag)
  :id 84
  :auto
  ((*morph-shape-ver* 2 :local t )
   (*shape-tag-version* 1 :local t) ;; fixme: is this right?
   (character-id (ui16))
   (start-bounds (swf-type 'rect))
   (end-bounds (swf-type 'rect))
   (start-edge-bounds (swf-type 'rect))
   (end-edge-bounds (swf-type 'rect))
   (reserved (ub 6))
   (uses-non-scaling-strokes (bit-flag))
   (uses-scaling-strokes (bit-flag))
   ;; fixme: derive this (offset to end-edges field), and use it...
   (offset (ui32)
           ;; not sure if this is file bug or parser bug yet...
           :extra (when (zerop offset)
                    (format t "got empty morph-shape tag? id=~s~%"
                            character-id)))
   (morph-fill-styles (swf-type 'morph-fill-style-array))
   (morph-line-styles (swf-type 'morph-line-style-array))
   (start-edges (swf-type 'shape) :optional (not (zerop offset)))
   (end-edges (swf-type 'shape))
))

(define-swf-type define-scene-and-frame-label-data-tag (swf-tag)
  :id 86
  :auto
  ((scene-count (encodedu32))
   (scenes (counted-list (list (encodedu32) (string-sz-utf8)) scene-count))
   (frame-label-count (encodedu32))
   (frames (counted-list (list (encodedu32) (string-sz-utf8))
                         frame-label-count))))

;;    87 define-binary-data-tag

(define-swf-type define-font-name-tag (swf-tag)
  :id 88
  :auto
  ((font-id (ui16))
   (font-name (string-sz-utf8))
   (font-copyright (string-sz-utf8))))

;;    89 start-sound-2-tag
;;    90 define-bits-jpeg-4-tag
;;    91 define-font-4-tag))


(define-swf-type swftools-777 (swf-tag)
  :id 777
  :auto ((octets (rest-of-tag)
                 :extra (format t "swftools junk :~s~%" octets))))



