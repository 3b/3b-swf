(in-package :%3b-swf)

;;; not done....

(define-swf-type sound-info ()
  :this-var o
  :auto
  ((reserved (ub 2) :initform 0)
   (sync-stop (bit-flag))
   (sync-no-multiple (bit-flag))
   (has-envelope (bit-flag) :derived (not (null (envelope-records o))))
   (has-loops (bit-flag) :derived (not (null (loop-count o))))
   (has-out-point (bit-flag) :derived (not (null (out-point o))))
   (has-in-point (bit-flag) :derived (not (null (in-point o))))
   (in-point (ui32) :optional has-in-point)
   (out-point (ui32) :optional has-out-point)
   (loop-count (ui16) :optional has-loops)
   (env-points (ui8) :optional has-envelope)
   (envelope-records (counted-list (swf-type 'sound-envelope) env-points)
                     :optional has-envelope)))

(define-swf-type sound-envelope ()
  :auto
  ((pos44 (ui32))
   (left-level (ui16))
   (right-level (ui16))))

(define-swf-type mp3-stream-sound-data ()
  :auto
  ((sample-count (ui16))
   (mp3-sound-data (swf-type 'mp3-sound-data))))

(define-swf-type mp3-sound-data ()
  :auto
  ((seek-samples (ui16))
   (mp3-frames (rest-of-tag))))

;; adpcm-sound-data
;;adpcm-mono-packet
;;adpcm-steroe-packet
;;mp3-sound-data
;;mp3-frame
