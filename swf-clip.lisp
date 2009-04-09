(in-package :3b-swf)

;;; PlaceObject* parts

(define-swf-type clip-event-flags ()
  :auto
  ((key-up (bit-flag))
   (key-down (bit-flag))
   (mouse-up (bit-flag))
   (mouse-down (bit-flag))
   (mouse-move (bit-flag))
   (unload (bit-flag))
   (enter-frame (bit-flag))
   (event-load (bit-flag)))
  :subclass (if (>= *swf-version* 6)
              'clip-event-flags-6
              'clip-event-flags-short
              ))

(define-swf-type clip-event-flags-short (clip-event-flags)
  :auto
  ((reserved (ub 8))))
(defmethod key-press ((o clip-event-flags-short))
  (declare (ignore o))
  nil)
(define-swf-type clip-event-flags-6 (clip-event-flags)
  :auto
  ((drag-over (bit-flag))
   (roll-out (bit-flag))
   (roll-over (bit-flag))
   (release-outside (bit-flag))
   (release (bit-flag))
   (press (bit-flag))
   (initialize (bit-flag))
   (data (bit-flag))
   (reserved (ub 5))
   (construct (bit-flag))
   (key-press (bit-flag))
   (drag-out (bit-flag))
   (reserved2 (ub 8))))


(define-swf-type clip-action-end-record (clip-action-record-flags)
  :auto ())

(define-swf-type clip-action-record (clip-action-record-flags)
  :auto
  ((action-record-size (ui32) :derived (swf-part-size (actions o))
                       :extra
                       (progn
                         #+nil(format t "clip-action-recordsize= ~s/~s :: ~b / ~b~%"
                                 action-record-size (bytes-left-in-tag)
                                 action-record-size (bytes-left-in-tag))
                         ;; work around buggy swf files?...
                         (when (> action-record-size (bytes-left-in-tag))
                           (setf action-record-size
                                (+ 2 (ldb (byte 16 16) action-record-size)))
                           #+nil(format t "-> = ~s/~s :: ~b / ~b~%"
                                   action-record-size (bytes-left-in-tag)
                                   action-record-size (bytes-left-in-tag))
                           ;; rest of tag doesn't work
                           #+nil(setf action-record-size (- (bytes-left-in-tag)
                                                       (if (>= *swf-version* 6)
                                                           4 2)))
                           ;; doesn't seem to be wrong byte order...
                           #+nil(rotatef (ldb (byte 8 24) action-record-size)
                                         (ldb (byte 8 0) action-record-size))
                           #+nil(rotatef (ldb (byte 8 16) action-record-size)
                                         (ldb (byte 8 8) action-record-size)))
                         #+nil(format t "clip-action-record ~s bytes~%"
                                 (if (key-press (super flags))
                                     (1- action-record-size)
                                     action-record-size))))

   (key-code (ui8) :optional (key-press (super flags)))
   ;;(%action-start (tag-file-position-hack))
   ;;(:bind *tag-end* (+ %action-start action-record-size))
   (actions (sized-list (swf-type 'action-record)
                        (if (key-press (super flags))
                            (1- action-record-size)
                            action-record-size))))
  :this-var o)

(define-swf-type clip-action-record-flags ()
  :auto
  ;; wonder if it would be better to recombine the event-flags and
  ;; event-flags-6 classes and subclass this from those instead of
  ;; making it a field?
  ((flags (swf-type 'clip-event-flags)))
  :subclass
  (if (or (key-up flags) (key-down flags) (mouse-up flags) (mouse-down flags)
          (mouse-move flags) (unload flags) (enter-frame flags)
          (event-load flags)
          (and (typep flags 'clip-event-flags-6)
               (or (drag-over flags) (roll-out flags) (roll-over flags)
                   (release-outside flags) (release flags) (press flags)
                   (initialize flags) (data flags)
                   (not (zerop (reserved flags)))
                   (construct flags) (key-press flags) (drag-out flags)
                   (not (zerop (reserved2 flags))))))
      'clip-action-record
      'clip-action-end-record))

(define-swf-type clip-actions ()
  :auto
  ((reserved (ui16))
   (all-event-flags (swf-type 'clip-event-flags))
   (clip-action-records (list-until (swf-type 'clip-action-record-flags)
                                    (lambda (x)
                                      (typep x 'clip-action-end-record))))))


