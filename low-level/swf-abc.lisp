(in-package :%3b-swf)


;; variable length encoded integers
;; fixme: combine these or make a definer macro for this sort of thing
;;(define-swf-type abc-u30 ())
(defmethod read-swf-part ((type (eql 'abc-u30)) source &key)
  (read-encodedu32 source))
(defmethod %swf-part-size swf-part ((type (eql 'abc-u30)) this &key &allow-other-keys)
  (assert (<= 0 this (expt 2 30)))
  (size-encodedu32 this))
(defmethod write-swf-part swf-part ((type (eql 'abc-u30)) this source)
  (assert (<= 0 this (expt 2 30)))
  (write-encodedu32 this source))

;; constant pool uses counte arrays with implicit element 0, so
;; we have a special type for those counts
(defmethod read-swf-part ((type (eql 'abc-u30+1)) source &key)
  (1- (read-encodedu32 source)))
(defmethod %swf-part-size swf-part ((type (eql 'abc-u30+1)) this &key &allow-other-keys)
  (assert (<= 0 this (expt 2 30)))
  (with-swf-sizers (v)
    (align 8))
  (size-encodedu32 (1+ this)))
(defmethod write-swf-part swf-part ((type (eql 'abc-u30+1)) this source)
  (assert (<= 0 this (expt 2 30)))
  (write-encodedu32 (1+ this) source))


;;(define-swf-type abc-u32 ())
(defmethod read-swf-part ((type (eql 'abc-u32)) source &key)
  (read-encodedu32 source))
(defmethod %swf-part-size swf-part ((type (eql 'abc-u32)) this &key &allow-other-keys)
  (assert (<= 0 this (expt 2 32)))
  (size-encodedu32 this))
(defmethod write-swf-part swf-part ((type (eql 'abc-u32)) this source)
  (assert (<= 0 this (expt 2 32)))
  (write-encodedu32 this source))

;;(define-swf-type abc-s32 ())
(defmethod read-swf-part ((type (eql 'abc-s32)) source &key)
  (let ((u (read-encodedu32 source)))
    (if (>= u (expt 2 31))
        (- u (expt 2 32))
        u)))
(defmethod %swf-part-size swf-part ((type (eql 'abc-s32)) this &key &allow-other-keys)
  (assert (<= (abs this) (expt 2 32)))
  (when (< this 0) (incf this (expt 2 32)))
  (size-encodedu32 this))
(defmethod write-swf-part swf-part ((type (eql 'abc-s32)) this source)
  (assert (<= (abs this) (expt 2 32)))
  (when (< this 0) (incf this (expt 2 32)))
  (write-encodedu32 this source))


;; counted utf8 string
;; possibly want to define directly?
;;(define-swf-type abc-string ())
(defmethod read-swf-part ((type (eql 'abc-string)) source &key)
  (let* ((l (read-encodedu32 source))
         (octets (read-octet-vector l source))
         (string (babel:octets-to-string octets :encoding :utf-8 :errorp t)))
    string))
(defmethod %swf-part-size swf-part ((type (eql 'abc-string)) this &key &allow-other-keys)
  (let ((len (babel:string-size-in-octets this :encoding :utf-8)))
    (incf *swf-sizer-bitpos*
          (* 8 len))
    (+ (size-encodedu32 len) len)))
(defmethod write-swf-part swf-part ((type (eql 'abc-string)) this source)
  (let ((octets (babel:string-to-octets this :encoding :utf-8)))
    (write-encodedu32 (length octets) source)
    (write-octet-vector octets source)))

;;; we should probably set up all the slots that index into the
;;; constant pools so they dereference it on read, and convert back to
;;; indices on write, but then we would have to disassemble and
;;; reassemble as well, which is probbaly a bit excessive...

;; for now just aliasing the abc-interned-* types to abc-u30, to at
;; least keep some info about usage
(macrolet ((alias (from to)
             `(progn
                (defmethod read-swf-part ((type (eql ',from)) source &rest rest)
                  (apply #'read-swf-part ',to source rest))

                (defmethod %swf-part-size swf-part ((type (eql ',from)) this
                                                   &rest rest &key &allow-other-keys)
                  (apply #'%swf-part-size ',to this rest))
                (defmethod write-swf-part swf-part ((type (eql ',from)) this
                                                    source)
                  (funcall #'write-swf-part ',to this source)))))
  (alias abc-interned-string abc-u30)
  (alias abc-interned-namespace abc-u30)
  (alias abc-interned-namespace-set abc-u30)
  (alias abc-interned-multiname abc-u30)
  (alias abc-interned-method-name abc-u30)
  (alias abc-interned-class-name abc-u30)
  (alias abc-interned-function-name abc-u30)
  (alias abc-interned-class-name abc-u30)
  (alias abc-interned-metadata abc-u30))

;;;; implement similar to character IDs?
;;(define-swf-type abc-interned-string ()
;;  ;; string
;;)
;;(define-swf-type abc-interned-namespace ()
;;  ;; list (kind name) ? might want a distinguishable type instead of a list
;;  ;; to make interned-option-detail easier?
;;)
;;(define-swf-type abc-interned-namespace-set ()
;;  ;; list of ns? / list of interned-namespace?
;;)
;;(define-swf-type abc-interned-value+kind-constant ()
;;  ;; u30 val, u8 kind
;;  ;; autoselect kind based on value?
;;  ;; int/uint/double/string/t/nil/:null/:undefined?
;;  ;; namespace/(:protected namespace)/etc?
;;  ;; page 26
;;)

(define-swf-type abc-interned-value+kind-constant ()
  :auto
  ((value (swf-type 'abc-u30))
   (kind (ui8))))

(define-swf-type abc-interned-value+optional-kind-constant ()
  :auto
  ((value (swf-type 'abc-u30))
   (kind (ui8) :optional (not (zerop value)))))



(define-swf-type abc-namespace ()
  :auto
  ;; fixme: map kind to keywords?
  ((kind (ui8))
   (name (swf-type 'abc-interned-string))))

(define-swf-type abc-ns-set ()
  :this-var o
  :auto
  ((ns-count (swf-type 'abc-u30) :derived (length (ns o)))
   (ns (counted-list (swf-type 'abc-interned-namespace) ns-count))))


(define-swf-type abc-multiname ()
  :this-var o
  :auto
  ((kind (ui8) :derived (subclass-id o 'abc-multiname)))
  :subclass (subclass-from-id 'abc-multiname kind))

(define-swf-type abc-multiname-qname (abc-multiname)
  :id #x07
  :auto
  ((ns (swf-type 'abc-interned-namespace))
   (name (swf-type 'abc-interned-string))))

(define-swf-type abc-multiname-qname-a (abc-multiname)
  :id #x0d
  :auto
  ((ns (swf-type 'abc-interned-namespace))
   (name (swf-type 'abc-interned-string))))

(define-swf-type abc-multiname-rt-qname (abc-multiname)
  :id #x0f
  :auto
  ((name (swf-type 'abc-interned-string))))

(define-swf-type abc-multiname-rt-qname-a (abc-multiname)
  :id #x10
  :auto
  ((name (swf-type 'abc-interned-string))))

(define-swf-type abc-multiname-rt-qname-l (abc-multiname)
  :id #x11
  :auto
  ()) ;; no contents

(define-swf-type abc-multiname-rt-qname-la (abc-multiname)
  :id #x12
  :auto
  ())

(define-swf-type abc-multiname-multiname (abc-multiname)
  :id #x09
  :auto
  ((name (swf-type 'abc-interned-string))
   (ns-set (swf-type 'abc-interned-namespace-set))))

(define-swf-type abc-multiname-multiname-a (abc-multiname)
  :id #x0e
  :auto
  ((name (swf-type 'abc-interned-string))
   (ns-set (swf-type 'abc-interned-namespace-set))))

(define-swf-type abc-multiname-multiname-l (abc-multiname)
  :id #x1b
  :auto
  ((ns-set (swf-type 'abc-interned-namespace-set))))

(define-swf-type abc-multiname-multiname-la (abc-multiname)
  :id #x1c
  :auto
  ((ns-set (swf-type 'abc-interned-namespace-set))))

;; see http://blog.richardszalay.com/2009/02/generics-vector-in-avm2.html
(define-swf-type abc-multiname-generic (abc-multiname)
  :id #x1d
  :this-var o
  :auto
  ((type-name (swf-type 'abc-interned-multiname))
   (param-count (swf-type 'abc-u30) :derived (length (params o)))
   (params (counted-list (swf-type 'abc-interned-multiname) param-count))))

(define-swf-type abc-constant-pool ()
  :this-var o
  :auto
  ((integer-count (swf-type 'abc-u30+1) :derived (length (integers o))
                  ;:extra (format t "integers = ~s~%" integer-count)
                  )
   (integers (counted-list (swf-type 'abc-s32) integer-count))

   (unsigned-integer-count (swf-type 'abc-u30+1)
                           :derived (length (unsigned-integers o))
                           ;:extra (format t "uintegers = ~s~%" unsigned-integer-count)
                           )

   (unsigned-integers (counted-list (swf-type 'abc-u32) unsigned-integer-count))

   (double-count (swf-type 'abc-u30+1) :derived (length (doubles o))
                 ;:extra (format t "doubles = ~s~%" double-count)
                 )
   (doubles (counted-list (float64) double-count))

   (string-count (swf-type 'abc-u30+1) :derived (length (strings o)))
   (strings (counted-list (swf-type 'abc-string) string-count))

   (namespace-count (swf-type 'abc-u30+1) :derived (length (namespaces o)))
   (namespaces (counted-list (swf-type 'abc-namespace) namespace-count))
   (ns-set-count (swf-type 'abc-u30+1) :derived (length (ns-sets o)))
   (ns-sets (counted-list (swf-type 'abc-ns-set) ns-set-count))
   (multiname-count (swf-type 'abc-u30+1) :derived (length (multinames o)))
   (multinames (counted-list (swf-type 'abc-multiname) multiname-count))))

(define-swf-type abc-method-info-option-info ()
  :this-var o
  :auto
  ((option-count (swf-type 'abc-u30) :derived (length (options o)))
   (options (counted-list (swf-type 'abc-interned-value+kind-constant) option-count)))
)
#+nil
(define-swf-type abc-method-info-param-info ()

)

(define-swf-type abc-method-info ()
  :this-var o
  :auto
  ((param-count (swf-type 'abc-u30) :derived (length (param-types o)))
   (return-type (swf-type 'abc-interned-multiname))
   (param-types (counted-list (swf-type 'abc-interned-multiname) param-count))
   (name (swf-type 'abc-interned-string))
   ;; fixme: are thes in the right order?
   (has-param-names (bit-flag) :derived (not (null (param-names o)))) ;; #x80
   (set-dxns (bit-flag) :initform nil) ;; #x40
   (reserved-flags (ub 2) :initform 0) ;; #x10, #x20
   (has-options (bit-flag) :derived (not (null (options o)))) ;; #x08
   (need-rest (bit-flag) :initform nil) ;; #x04
   (need-activation (bit-flag) :initform nil) ;; #x02
   (need-arguments (bit-flag) :initform nil) ;; low bit #x01
   ;;(options (swf-type 'abc-method-info-option-info) :optional has-options)
   (option-count (swf-type 'abc-u30) :derived (length (options o))
                 :optional has-options)
   (options (counted-list (swf-type 'abc-interned-value+kind-constant) option-count)
            :optional has-options)
   (param-names (counted-list (swf-type 'abc-interned-string) param-count)
                :optional has-param-names)))

(define-swf-type abc-metadata-item-info ()
  :auto
  ((key (swf-type 'abc-interned-string))
   (value (swf-type 'abc-interned-string))))

(define-swf-type abc-metadata-info ()
  :this-var o
  :auto
  ((name (swf-type 'abc-interned-string))
   (item-count (swf-type 'abc-u30) :derived (length (items o)))
   (items (counted-list (swf-type 'abc-metadata-item-info) item-count))))

(define-swf-type abc-trait-info ()
  :this-var o
  :auto
  ((name (swf-type 'abc-interned-multiname))
   (reserved=0 (bit-flag) :initform nil)
   (has-metadata-p (bit-flag) :derived (not (null (metadata o))))
   (override-p (bit-flag) :initform nil)
   (final-p (bit-flag) :initform nil)
   ;; not sure if it would be uglier to put the data in a field with a
   ;; separate type, or to subclass and put metadata field in every subclass
   ;; by hand...
   ;; having a data field is API ugliness, while metadata is
   ;; implementation ugliness though, so going with subclasses for
   ;; now.
   (kind (ub 4) :derived (subclass-id o 'abc-trait-info)))
  ;; (metadata-count (swf-type 'abc-u30) :derived (length (metadata o)))
  ;; (metadata (counted-list (swf-type 'abc-interned-metadata) metadata-count))
  :subclass (subclass-from-id 'abc-trait-info kind))

(define-swf-type abc-trait-info-slot (abc-trait-info)
  :id 0
  :this-var o
  :auto
  ((slot-id (swf-type 'abc-u30) :initform 0)
   (type-name (swf-type 'abc-interned-multiname))
   (value (swf-type 'abc-interned-value+optional-kind-constant))
   (metadata-count (swf-type 'abc-u30) :derived (length (metadata o))
                   :optional (super has-metadata-p))
   (metadata (counted-list (swf-type 'abc-interned-metadata) metadata-count)
             :initform nil)))

(define-swf-type abc-trait-info-constant (abc-trait-info)
  ;;fixme: same as abc-trait-info-slot except for ID, can they be combined?
  :id 6
  :this-var o
  :auto
  ((slot-id (swf-type 'abc-u30) :initform 0)
   (type-name (swf-type 'abc-interned-multiname))
   (value (swf-type 'abc-interned-value+optional-kind-constant))
   (metadata-count (swf-type 'abc-u30) :derived (length (metadata o))
                   :optional (super has-metadata-p))
   (metadata (counted-list (swf-type 'abc-interned-metadata) metadata-count)
             :initform nil)))

;; fixme: can method/getter/setter be combined?
(define-swf-type abc-trait-info-method (abc-trait-info)
  :id 1
  :this-var o
  :auto
  ((slot-id (swf-type 'abc-u30) :initform 0)
   (method-id (swf-type 'abc-u30))
   (metadata-count (swf-type 'abc-u30) :derived (length (metadata o))
                   :optional (super has-metadata-p))
   (metadata (counted-list (swf-type 'abc-interned-metadata) metadata-count)
             :initform nil)))

(define-swf-type abc-trait-info-getter (abc-trait-info)
  :id 2
  :this-var o
  :auto
  ((slot-id (swf-type 'abc-u30) :initform 0)
   (method-id (swf-type 'abc-u30))
   (metadata-count (swf-type 'abc-u30) :derived (length (metadata o))
                   :optional (super has-metadata-p))
   (metadata (counted-list (swf-type 'abc-interned-metadata) metadata-count)
             :initform nil)))

(define-swf-type abc-trait-info-setter (abc-trait-info)
  :id 3
  :this-var o
  :auto
  ((slot-id (swf-type 'abc-u30) :initform 0)
   (method-id (swf-type 'abc-u30))
   (metadata-count (swf-type 'abc-u30) :derived (length (metadata o))
                   :optional (super has-metadata-p))
   (metadata (counted-list (swf-type 'abc-interned-metadata) metadata-count)
             :initform nil)))

(define-swf-type abc-trait-info-class (abc-trait-info)
  :id 4
  :this-var o
  :auto
  ((slot-id (swf-type 'abc-u30) :initform 0)
   (class-name (swf-type 'abc-interned-class-name))
   (metadata-count (swf-type 'abc-u30) :derived (length (metadata o))
                   :optional (super has-metadata-p))
   (metadata (counted-list (swf-type 'abc-interned-metadata) metadata-count)
             :initform nil)))

(define-swf-type abc-trait-info-function (abc-trait-info)
  :id 5
  :this-var o
  :auto
  ((slot-id (swf-type 'abc-u30) :initform 0)
   (function-name (swf-type 'abc-interned-function-name))
   (metadata-count (swf-type 'abc-u30) :derived (length (metadata o))
                   :optional (super has-metadata-p))
   (metadata (counted-list (swf-type 'abc-interned-metadata) metadata-count)
             :initform nil)))

(define-swf-type abc-instance-info ()
  :this-var o
  :auto
  ((name (swf-type 'abc-interned-multiname))
   (super-name (swf-type 'abc-interned-multiname))
   (reserved=0 (ub 4) :initform 0) ;; #x80 #x40 #x20 #x10
   (class-protected-ns-p (bit-flag)
                         :derived (not (null (protected-ns o)))) ;; #x08
   (class-interface-p (bit-flag) :initform nil) ;; #x04
   (class-final-p (bit-flag) :initform nil) ;; #x02
   (class-sealed-p (bit-flag) :initform nil) ;; #x01
   (protected-ns (swf-type 'abc-interned-namespace)
                 :optional class-protected-ns-p)
   (interface-count (swf-type 'abc-u30) :derived (length (interfaces o)))
   (interfaces (counted-list (swf-type 'abc-interned-multiname)
                             interface-count))
   (instance-init (swf-type 'abc-interned-method-name)) ;; ?
   (trait-count (swf-type 'abc-u30) :derived (length (traits o)))
   (traits (counted-list (swf-type 'abc-trait-info) trait-count))))

;; possibly should add a setter at some point too...
(defmethod %raw-flags ((o abc-instance-info))
  (logior
   (if (class-protected-ns-p o) #x08 0)
   (if (class-interface-p o) #x04 0)
   (if (class-final-p o) #x02 0)
   (if (class-sealed-p o) #x01 0)))

(define-swf-type abc-class-info ()
  :this-var o
  :auto
  ((class-init (swf-type 'abc-interned-method-name))
   (trait-count (swf-type 'abc-u30) :derived (length (traits o)))
   (traits (counted-list (swf-type 'abc-trait-info) trait-count))))

(define-swf-type abc-script-info ()
  :this-var o
  :auto
  ((script-init (swf-type 'abc-interned-method-name))
   (trait-count (swf-type 'abc-u30) :derived (length (traits o)))
   (traits (counted-list (swf-type 'abc-trait-info) trait-count))))

(define-swf-type abc-exception-info ()
  :auto
  ((from (swf-type 'abc-u30))
   (to (swf-type 'abc-u30))
   (target (swf-type 'abc-u30))
   (exception-type (swf-type 'abc-interned-string))
   (var-name (swf-type 'abc-interned-string))))

(define-swf-type abc-method-body-info ()
  :this-var o
  :auto
  ((method-name (swf-type 'abc-interned-method-name))
   (max-stack (swf-type 'abc-u30))
   (local-count (swf-type 'abc-u30))
   (init-scope-depth (swf-type 'abc-u30))
   (max-scope-depth (swf-type 'abc-u30))
   (code-length (swf-type 'abc-u30) :derived (length (code o)))
   (code (counted-list (ui8) code-length))
   (exception-count (swf-type 'abc-u30) :derived (length (exceptions o)))
   (exceptions (counted-list (swf-type 'abc-exception-info) exception-count))
   (trait-count (swf-type 'abc-u30) :derived (length (traits o)))
   (traits (counted-list (swf-type 'abc-trait-info) trait-count))))

;; stored inline in do-abc-tag now...
#+nil
(define-swf-type abc-data ()
  :this-var o
  :auto
  ((minor-version (ui16) :initform 16)
   (major-version (ui16) :initform 46)
   (constant-pool (swf-type 'abc-constant-pool))

   (method-count (swf-type 'abc-u30) :derived (length (method-info o)))
   (method-info (counted-list (swf-type 'abc-method-info) method-count))

   (metadata-count (swf-type 'abc-u30) :derived (length (metadata-info o)))
   (metadata-info (counted-list (swf-type 'abc-metadata-info) metadata-count))

   (class-count (swf-type 'abc-u30) :derived (length (class-info o)))
   (instance-info (counted-list (swf-type 'abc-instance-info) class-count))

   (class-info (counted-list (swf-type 'abc-class-info) class-count))
   (script-count (swf-type 'abc-u30) :derived (length (script-info o)))
   (script-info (counted-list (swf-type 'abc-script-info) script-count))
   (method-body-count (swf-type 'abc-u30)
                      :derived (length (method-body-info o)))
   (method-body-info (counted-list (swf-type 'abc-method-body-info)
                                   method-body-count))))

(define-swf-type do-abc-tag (swf-tag) ;;+r?w
  :id 82
  :this-var o
  :auto
  ((flags (ui32)) ;; 1 = lazy initialize
   (name (string-sz-utf8))
   ;;(data (swf-type 'abc-data))
   (minor-version (ui16) :initform 16)
   (major-version (ui16) :initform 46)
   (constant-pool (swf-type 'abc-constant-pool))

   (method-count (swf-type 'abc-u30) :derived (length (method-info o)))
   (method-info (counted-list (swf-type 'abc-method-info) method-count))

   (metadata-count (swf-type 'abc-u30) :derived (length (metadata-info o)))
   (metadata-info (counted-list (swf-type 'abc-metadata-info) metadata-count))

   (class-count (swf-type 'abc-u30) :derived (length (class-info o)))
   (instance-info (counted-list (swf-type 'abc-instance-info) class-count))

   (class-info (counted-list (swf-type 'abc-class-info) class-count))
   (script-count (swf-type 'abc-u30) :derived (length (script-info o)))
   (script-info (counted-list (swf-type 'abc-script-info) script-count))
   (method-body-count (swf-type 'abc-u30)
                      :derived (length (method-body-info o)))
   (method-body-info (counted-list (swf-type 'abc-method-body-info)
                                   method-body-count))
)
  :print-unreadably ("flags:~x name:~s " (flags o) (name o)))



;; fixme: - numbers in integer pool