(defpackage #:3b-swf
  (:use #:cl)
  (:export
   #:ymax
   #:rect
   #:color-transform-alpha
   #:rotate
   #:scale
   #:translate
   #:scale
   #:matrix
   #:matrix*
   #:identity-matrix
   #:transform-point
   #:rgba
   #:rgb
   #:skew
   #:color-transform-alpha-float
   #:rgba-float
   #:symbol-class-tag
   #:export-assets
   #:write-swf
   #:background-color
   #:show-frame
   #:frame-label
   #:file-attributes
   #:script-limits
   #:place-character-id))