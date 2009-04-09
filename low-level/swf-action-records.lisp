(in-package :3b-swf)

;;; actionrecords

(define-swf-type action-record ()
  :this-var o
  :auto
  ((action-code (ui8) :derived (subclass-id o 'action-record)))
  :subclass (if (< action-code #x80)
                (subclass-from-id 'action-record action-code)
                'action-record-long))

(define-swf-type action-record-long (action-record)
  :this-var o
  :auto
  ((action-length (ui16) :derived (- (swf-part-size o) 2)))
  :subclass (subclass-from-id 'action-record-long (super action-code)))

(define-swf-type action-record-end (action-record)
  :id #x00)

(define-swf-type action-next-frame (action-record)
  :id #x04)
(define-swf-type action-previous-frame (action-record)
  :id #x05)
(define-swf-type action-play (action-record)
  :id #x06)
(define-swf-type action-stop (action-record)
  :id #x07)
(define-swf-type action-toggle-quality (action-record)
  :id #x08)
(define-swf-type action-stop-sounds (action-record)
  :id #x09)
(define-swf-type action-add (action-record)
  :id #x0a)
(define-swf-type action-subtract (action-record)
  :id #x0b)
(define-swf-type action-multiply (action-record)
  :id #x0c)
(define-swf-type action-divide (action-record)
  :id #x0d)
(define-swf-type action-equals (action-record)
  :id #x0e)
(define-swf-type action-less (action-record)
  :id #x0f)
(define-swf-type action-and (action-record)
  :id #x10)
(define-swf-type action-or (action-record)
  :id #x11)
(define-swf-type action-not (action-record)
  :id #x12)
(define-swf-type action-string-equals (action-record)
  :id #x13)
(define-swf-type action-string-length (action-record)
  :id #x14)
(define-swf-type action-string-extract (action-record)
  :id #x15)



(define-swf-type action-pop (action-record)
  :id #x17)
(define-swf-type action-to-integer (action-record)
  :id #x18)

(define-swf-type action-get-variable (action-record)
  :id #x1c)
(define-swf-type action-set-variable (action-record)
  :id #x1d)


(define-swf-type action-set-target-2 (action-record)
  :id #x20)
(define-swf-type action-string-add (action-record)
  :id #x21)
(define-swf-type action-get-property (action-record)
  :id #x22)
(define-swf-type action-set-property (action-record)
  :id #x23)
(define-swf-type action-clone-sprite (action-record)
  :id #x24)
(define-swf-type action-remove-sprite (action-record)
  :id #x25)
(define-swf-type action-trace (action-record)
  :id #x26)
(define-swf-type action-sdrag? (action-record)
  :id #x27)
(define-swf-type action-end-drag (action-record)
  :id #x28)
(define-swf-type action-string-less (action-record)
  :id #x29)
(define-swf-type action-throw (action-record)
  :id #x2a)
(define-swf-type action-cast-op (action-record)
  :id #x2b)
(define-swf-type action-implements-op (action-record)
  :id #x2c)

(define-swf-type action-random-number (action-record)
  :id #x30)
(define-swf-type action-mb-string-length (action-record)
  :id #x31)
(define-swf-type action-char-to-ascii (action-record)
  :id #x32)
(define-swf-type action-ascii-to-char (action-record)
  :id #x33)
(define-swf-type action-get-time (action-record)
  :id #x34)
(define-swf-type action-mb-string-extract (action-record)
  :id #x35)
(define-swf-type action-mb-char-to-ascii (action-record)
  :id #x36)
(define-swf-type action-mb-ascii-to-char (action-record)
  :id #x37)

(define-swf-type action-delete (action-record)
  :id #x3a)
(define-swf-type action-delete-2 (action-record)
  :id #x3b)
(define-swf-type action-define-local (action-record)
  :id #x3c)
(define-swf-type action-call-function (action-record)
  :id #x3d)
(define-swf-type action-return (action-record)
  :id #x3e)
(define-swf-type action-modulo (action-record)
  :id #x3f)
(define-swf-type action-new-object (action-record)
  :id #x40)
(define-swf-type action-define-local-2 (action-record)
  :id #x41)
(define-swf-type action-init-array (action-record)
  :id #x42)
(define-swf-type action-init-object (action-record)
  :id #x43)
(define-swf-type action-type-of (action-record)
  :id #x44)
(define-swf-type action-target-path (action-record)
  :id #x45)
(define-swf-type action-enumerate (action-record)
  :id #x46)
(define-swf-type action-add2 (action-record)
  :id #x47)
(define-swf-type action-less2 (action-record)
  :id #x48)
(define-swf-type action-equals2 (action-record)
  :id #x49)
(define-swf-type action-to-number (action-record)
  :id #x4a)
(define-swf-type action-to-string (action-record)
  :id #x4b)
(define-swf-type action-push-duplicate (action-record)
  :id #x4c)
(define-swf-type action-stack-swap (action-record)
  :id #x4d)
(define-swf-type action-get-member (action-record)
  :id #x4e)
(define-swf-type action-set-member (action-record)
  :id #x4f)
(define-swf-type action-bit-increment (action-record)
  :id #x50)
(define-swf-type action-bit-decrement (action-record)
  :id #x51)
(define-swf-type action-call-method (action-record)
  :id #x52)
(define-swf-type action-new-method (action-record)
  :id #x53)
(define-swf-type action-instance-of (action-record)
  :id #x54)
(define-swf-type action-enumerate2 (action-record)
  :id #x55)

(define-swf-type action-bit-and (action-record)
  :id #x60)
(define-swf-type action-bit-or (action-record)
  :id #x61)
(define-swf-type action-bit-xor (action-record)
  :id #x62)
(define-swf-type action-bit-lshift (action-record)
  :id #x63)
(define-swf-type action-bit-rshift (action-record)
  :id #x64)
(define-swf-type action-bit-urshift (action-record)
  :id #x65)
(define-swf-type action-strict-equals (action-record)
  :id #x66)
(define-swf-type action-greater (action-record)
  :id #x67)
(define-swf-type action-string-greater (action-record)
  :id #x68)
(define-swf-type action-extends (action-record)
  :id #x69)




(define-swf-type action-goto-frame (action-record-long)
  :id #x81
  :auto
  ((frame (ui16))))

(define-swf-type action-get-url (action-record-long)
  :id #x83
  :auto
  ((url-string (string-sz-utf8))
   (target-string (string-sz-utf8))))

(define-swf-type action-store-register (action-record-long)
  :id #x87
  :auto ((register (ui8))))


(define-swf-type action-constant-pool (action-record-long)
  :id #x88
  :auto
  ((constant-count (ui16))
   (constant-pool (counted-list (string-sz-utf8) constant-count))))

(define-swf-type action-wait-for-frame (action-record-long)
  :id #x8a
  :auto
  ((frame (ui16))
   (skip-count (ui8))))

(define-swf-type action-set-target (action-record-long)
  :id #x8b
  :auto
  ((target-name (string-sz-utf8))))

(define-swf-type action-goto-label (action-record-long)
  :id #x8c
  :auto
  ((label (string-sz-utf8))))

(define-swf-type action-wait-for-frame2 (action-record-long)
  :id #x8d
  :auto
  ((skip-count (ui8))))
(define-swf-type action-define-function-2 (action-record-long)
  :id #x8e
  :auto
  ((function-name (string-sz-utf8)
                  :extra (unless (string= function-name "")
                           (format t "define-function2 = ~s~%" function-name)))
   (num-params (ui16))
   (register-count (ui8))
   (preload-parent (bit-flag))
   (preload-root (bit-flag))
   (suppress-super (bit-flag))
   (preload-super (bit-flag))
   (suppress-arguments (bit-flag))
   (preload-arguments (bit-flag))
   (suppress-this (bit-flag))
   (preload-this (bit-flag))
   (reserved (ub 7))
   (preload-global (bit-flag))
   (parameters (counted-list (swf-type 'register-param) num-params))
   (code-size (ui16))
   (code (sized-list (swf-type 'action-record) code-size))))

(define-swf-type register-param ()
  :auto
  ((register (ui8))
   (param-name (string-sz-utf8))))

(define-swf-type action-try (action-record-long)
  :id #x8f
  :auto
  ((reserved (ub 5))
   (catch-in-register (bit-flag))
   (finally-block (bit-flag))
   (catch-block (bit-flag))
   (try-size (ui16))
   (catch-size (ui16))
   (finally-size (ui16))
   (catch-name (string-sz-utf8) :optional (not catch-in-register))
   (catch-register (ui8) :optional catch-in-register)
   (try-body (sized-list (swf-type 'action-record) try-size))
   (catch-body (sized-list (swf-type 'action-record) catch-size))
   (finally-body (sized-list (swf-type 'action-record) finally-size))
   ))

(define-swf-type action-with (action-record-long)
  :id #x94
  :auto
  ((size (ui16))
   (body (sized-list (swf-type 'action-record) size))
   ))

(define-swf-type action-push (action-record-long)
  :id #x96
  :this-var o
  :auto ((push-values (sized-list (swf-type 'action-push-value)
                                  (super action-length))))
)

(define-swf-type action-push-value ()
  :this-var o
  :auto
  ((push-type (ui8) :derived (subclass-id o 'action-push-value)
         #+nil #+nil     :extra (format t "push ~s ~s~%"
                             (subclass-from-id 'action-push-value push-type)
                             push-type)))
  :subclass (subclass-from-id 'action-push-value push-type))

(define-swf-type action-push-string (action-push-value)
  :id 0 :auto ((push-data (string-sz-utf8))))

(define-swf-type action-push-float (action-push-value)
  :id 1 :auto ((push-data (float32))))
(define-swf-type action-push-null (action-push-value)
  :id 2 :auto ())
(define-swf-type action-push-undefined (action-push-value)
  :id 3 :auto ())
(define-swf-type action-push-register (action-push-value)
  :id 4 :auto ((push-data (ui8))))
(define-swf-type action-push-boolean (action-push-value)
  :id 5 :auto ((push-data (ui8)))) ;; fixme: add ui8-boolean class
(define-swf-type action-push-double (action-push-value)
  :id 6 :auto ((push-data (float64))))
(define-swf-type action-push-int (action-push-value)
  :id 7 :auto ((push-data (ui32))))
(define-swf-type action-push-constant8 (action-push-value)
  :id 8 :auto ((push-data (ui8))))
(define-swf-type action-push-constant16 (action-push-value)
  :id 9 :auto ((push-data (ui16))))

(define-swf-type action-jump (action-record-long)
  :id #x99
  :auto
  ((offset (si16))))

(define-swf-type action-get-url-2 (action-record-long)
  :id #x9a
  :auto
  ((send-vars-method (ub 2))
   (reserved (ub 4))
   (load-target-flag (bit-flag))
   (load-variables-flag (bit-flag))))

(define-swf-type action-define-function (action-record-long)
  :id #x9b
  :auto
  ((function-name (string-sz-utf8))
   (num-params (ui16))
   (param-names (counted-list (string-sz-utf8) num-params))
   (code-size (ui16))
   (code (sized-list (swf-type 'action-record) code-size))))

(define-swf-type action-if (action-record-long)
  :id #x9d
  :auto
  ((offset (si16))))
(define-swf-type action-call (action-record-long)
  :id #x9e)

(define-swf-type action-goto-frame-2 (action-record-long)
  :id #x9f
  :auto
  ((reserved (ub 6))
   (scene-bias-flag (bit-flag))
   (play-flag (bit-flag))
   (scene-bias (ui16) :optional scene-bias-flag)))














