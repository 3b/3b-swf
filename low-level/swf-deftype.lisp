(in-package :3b-swf)

;;;; stuff for defining lower level compound types (possibly tags also
;;;; at some point?)



;;;todo list
;;
;; figure out how to use (foo) directly instead if (swf-type 'foo)?

;; get rid of the (super foo) thing if possible?
;;   (needed since we don't have an object to access in reader)

;; handle loading a subclass directly
;;  ex. focal-gradient

;; make order of args match in subclass-if and subclass-from-id?

;; rename super, and use it in cases where we need to access
;;   later slots in a class too?
;; possibly get rid of the binding completely, and just put the vars in
;;   a plist and use getf or accessor to access them from other slots?

;; slot option to disable default print for that slot (or maybe
;;   specify printing per slot instead of a separate print option for
;;   the whole class?)

;; shortcut for derived flags, to avoid (not (null ..))
;;   (since we don't want the full object in the printouts)

;; list-until-type to avoid all the (lambda (x) (typep x 'type-foo))

;; pass derived args to subclass during creation as well as stored args,
;;   so we can use them from (super ...) for optional etc.

;; make ver# suffixes consistent (foo-bar-2 vs. foo-bar2)

;; add a list-until-next-byte-0 and simplify the structs that coud use that
;;  (currently either depending on splitting out end record and ending
;;    with typep, or list-until .. next-octet-zero-p)
;;  first case probably can get simpler type hierarchy if it never has
;;     to distinguish  end record
;;  second case avoids the lambda, and the extra 0 byte slot in the
;;    enlcosing record

;; flag to not print reserved fields?

;; patch chipz to allow specifying max bytes to read when decompressing from stream
;; add pull mode to chipz
;;   (with-input-from-compressed-stream (s 'chipz:zlib <input-stream>) ...) ?

;; possibly should use unbound slots instead of nil for optional fields?

;; pass current bit pos to swf-part-size instead of using global?



;;; generic functions for reading/writing/calculating size of objects
;;;
;;; we want to be able to compose poerations from the base class down,
;;;  so we can calculate size based on rest of class, or bind specials
;;   around the contents of subclasses, etc. so define a custom method
;;   combination
(define-method-combination swf-part
    (&optional (order :most-specific-last))
  ;; like the the default, but allow order arg for all parts, and
  ;; default to most-specific-last...
  ((around (:around) :order order)
   (before (:before) :order order)
   (primary (swf-part) :order order :required t)
   (after (:after) :order order))
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods)))
    (let ((form (if (or before after (rest primary))
                    `(multiple-value-prog1
                         (progn ,@(call-methods before)
                                (call-method ,(first primary)
                                             ,(rest primary)))
                       ,@(call-methods (reverse after)))
                    `(call-method ,(first primary)))))
      (if around
          `(call-method ,(first around)
                        (,@(rest around)
                           (make-method ,form)))
          form))))

(defgeneric read-swf-part (type stream &rest initargs)
  (:documentation "read an object of class TYPE or a subclass from stream STREAM"))


;;(unintern 'read-swf-part)
;;(unintern 'swf-part-size)

;; fixme: rename this to make it obvious it returns bits
;; also rename it to clarify that it is internal, since it needs
;; *swf-sizer-bitpos* bound around it to work properly
;;  (binding it internally doesn't work very well, since it depends on the
;;   state of the bitstream in the caller, since (align 8) takes up more space
;;   if the bitstream isn't aligned)
(defgeneric %swf-part-size (type part &key body-only &allow-other-keys)
  (:method-combination swf-part :most-specific-last)
  (:documentation "calculate size of a swf-part in bits"))

(defmethod %swf-part-size swf-part (type part &rest keys &key)
  (when (next-method-p) (apply #'call-next-method type part keys)))

(defun swf-part-size (type part &key align body-only)
  (with-swf-sizers (vvvv)
      (let ((*swf-sizer-bitpos* 0)
            (vvvv nil))
        (%swf-part-size type part :body-only body-only)
        (when align (align align))
        *swf-sizer-bitpos*)))

(defgeneric write-swf-part (type part stream)
  (:method-combination swf-part :most-specific-last)
  (:documentation "write a swf-part to a stream"))

;;;
(defmacro define-swf-type (class-name supers &body keys &key
                           auto reader writer sizer
                           (print-unreadably :auto) slots
                           this-var value-var
                           subclass
                           swf-min-version ;; not used yet
                           align-after
                           id)
  (declare (ignore keys swf-min-version))
  ;; :auto looks like (foo (do stuff) :local . :derived .)
  ;;   where foo is the name of the var/accessor,
  ;;     (do stuff) uses the dsl to read/write/size the var
  ;;     or (swf-type <type>) to read/write/size using specified type
  ;;   keyword args:
  ;;     :local  =  nil/t
  ;;        value is not read/written/sized, just bound for use by
  ;;        later values (or nested objects if special)
  ;;      :derived  = nil/form
  ;;        value isn't stored in a slot, but an accessor method is generated
  ;;        containing the specified form, and used to calculate the value
  ;;        for size/write pass
  ;;      :optional  =  form
  ;;        if set, only use value if (form) evaluates to true
  ;;      :extra = nil/form
  ;;        if set, is form to execute befor going on to next form
  ;;        (for stuff like debug prints, etc.. not sure if it will
  ;;         have any direct use or not)
  ;;        -- probably should split between passes (:extra-read, :extra-write
  ;;           if we keep this long term)
  ;;
  ;; :reader, :writer, :sizer are same, but only apply to a specific pass
  ;;    (overriding :auto if both appear)
  ;;   --- might be simpler to require the :auto part, and only
  ;;       specify the individual slots to override in :reader, etc?
  ;;
  ;; x:slots
  ;; x  names of slots/derived accessors if no :auto field
  ;; x    name or (name :derived <form>)
  ;;
  ;; :printer, if defined is a format string and arglist for a print-object
  ;;   method
  ;;   - todo probably would be nice to wrap the FORMAT in a with-accessors
  ;;     either with all slots of the object, or an explicitly provided list?
  ;;     not sure what syntax would be good for providing a list
  ;;     ("foo ~s ~s" bar baz) - is easiest, but no room for bindings
  ;;     ("foo ~s ~s" (bar baz) :accessors (bar baz)) - maybe?
  ;;     ("foo ~s ~s" (bar baz) (bar baz)) - or as an optional arg
  ;;     - actually probably easier to just use directly, since
  ;;        naming the accessors twice is probably longer than calling them
  ;;        if the object is given a short name
  ;;     ("foo ~s ~s" (bar o) (bar o))
  ;;     ++ so either bind all automatically, or none
  ;;
  ;; :thisvar, if set is a var to bind to the object during execution of
  ;;    the defined methods (except reader, since object isn't created yet)
  ;;  :value-var
  ;;    specifies a var to which current value is bound during execution
  ;;    of sizer/writer bodies
  ;;
  ;; :subclass
  ;;   form to evaluate to determine if a subclass should be constructed
  ;;     instead of the current class, should return name of class
  ;;     to continue reading, or nil to construct current class
  ;;
  ;;  :id  id / (parent id)
  ;;     defines 2 methods,
  ;;       subclass-id ((object ,class-name) (p (eql ',parent)))  and
  ;;       subclass-from-id ((p (eql ',parent)) (id (eql ',id))
  ;;     to be used for mapping classes to/from enumerated values in file
  ;;
  ;;  :align-after, if set, aligns source to specified # of bits after
  ;;     read/write/size
  ;;
  ;; (might drop the &key for auto at some point, if it ends up applying to
  ;;  most types?)
  ;;
  ;; todo: allow (values foo bar) for var name?
  ;;   not sure it is needed except for tags, so probably not worth it for now
  ;;   and trying to generate nice code for the general case would be ugly,
  ;;   though might not be too bad to add a :values key that does m-v-b
  ;;    but doesn't generate slots and require assigning the values to
  ;;    slots by hand...
  (let ((real-slots (reverse (copy-list (find-if-not 'consp slots))))
        (all-slots (reverse (mapcar (lambda (A) (if (consp a) (car a) a)) slots)))
        (this-var (or this-var (gensym "THIS-")))
        (type-var (gensym "TYPE-"))
        (value-arg (or value-var (gensym))))
    (multiple-value-bind (read-forms size-forms write-forms
                                     derived-forms
                                     slot-options)
        (loop for (name form . options) in auto
              for local = (getf options :local)
              for derived = (getf options :derived)
              for extra = (getf options :extra)
              for read-override = (cadr (assoc name reader))
              for size-override = (cadr (assoc name sizer))
              for write-override = (cadr (assoc name writer))
              unless (or local derived)
              do (pushnew name real-slots)
              unless local
              do (pushnew name all-slots)
              collect `(,name ,(or read-override form) ,@options) into read
              collect `(,name ,(or size-override form) ,@options) into size
              collect `(,name ,(or write-override form) ,@options) into write
              collect `(,name ,@options) into slot-options
              when derived
              collect (list name derived) into derived-forms
              finally (return (values read size write derived-forms
                                      slot-options)))
      (setf real-slots (nreverse real-slots))
      (setf all-slots (nreverse all-slots))
      (alexandria:with-gensyms (source type sub-type initargs rest-var)
        `(progn
           ;; define class
           (defclass ,class-name ,supers
             ,(loop for slot-name in real-slots
                    for options = (cdr (assoc slot-name slot-options))
                    collect `(,slot-name :initarg ,slot-name
                                         :accessor ,slot-name
                                         ,@(if (get-properties options '(:initform))
                                               `(:initform
                                                 ,(getf options :initform))
                                               (if (getf options :optional)
                                                   `(:initform nil))))))
           ;; define accessors for derived slots
           ,@ (loop for (slot-name form) in derived-forms
                    collect `(defmethod ,slot-name ((,this-var ,class-name))
                               ,form))
              ;; define reader
           (defmethod read-swf-part ((,type (eql ',class-name)) ,source &rest ,initargs)
             (with-swf-readers (,source)
               (macrolet ((super (slot)
                            `(getf ,',initargs ',slot)))
                 (prog1
                     , (labels ((make-bindings (forms instance-form)
                                  (if forms
                                      (destructuring-bind
                                            (name form &key extra optional
                                                  &allow-other-keys)
                                          (car forms)
                                        `(let ((,name ,@(if optional
                                                            `((when ,optional
                                                                ,form))
                                                            `(,form))))
                                           (declare (ignorable ,name))
                                           ,@(when extra (list extra))
                                           ,(make-bindings (cdr forms)
                                                           instance-form)))
                                   instance-form)))
                         (make-bindings
                          read-forms
                          (let ((m-i `(apply
                                       'make-instance
                                       ',class-name
                                       :allow-other-keys t
                                       ,@(loop for i in real-slots
                                               collect `',i
                                               collect i)
                                       ,initargs)))
                            (if subclass
                                `(let ((,sub-type ,subclass))
                                   (cond
                                     ((or (not ,sub-type)
                                          (eq ,sub-type ',class-name))
                                      ,m-i)
                                     (t
                                      (apply 'read-swf-part
                                             ,sub-type
                                             ,source
                                             ,@(loop for i in all-slots
                                                     collect `',i
                                                     collect i)
                                             ,initargs))))
                                m-i))))
                   ,@ (when align-after `((align ,align-after)))))))
           ;; define sizer
           (defmethod %swf-part-size swf-part (,type-var (,this-var ,class-name) &rest ,rest-var &key)
             (declare (ignore ,type-var))
             (with-swf-sizers (,value-arg)
               (macrolet ((super (slot)
                            `(,slot ,',this-var)))
                 (prog1
                     ,@ (labels
                            ((make-bindings (forms body-forms)
                               (if forms
                                   ;; fixme: probably shouldn't bind all
                                   ;; these slots not sure if limiting to
                                   ;; derived + local is enough or not...
                                   (destructuring-bind (name form &key local extra derived optional &allow-other-keys)
                                       (car forms)
                                     (if local
                                         `((let ((,name ,form))
                                             (declare (ignorable ,name))
                                             ,@(when extra (list extra))
                                             ,@(make-bindings (cdr forms)
                                                              body-forms)))
                                         `((let ((,name
                                                  ,(if derived
                                                       `(,name ,this-var)
                                                       `(if (slot-boundp
                                                             ,this-var ',name)
                                                            (,name ,this-var)
                                                            nil))))
                                             (declare (ignorable ,name))
                                             ,@(when extra (list extra))
                                             (symbol-macrolet ((,value-arg
                                                                ,name))
                                               ,(if optional
                                                    `(when ,optional ,form)
                                                    form))
                                             ,@(make-bindings (cdr forms)
                                                              body-forms)))))
                                   body-forms)))
                          (make-bindings
                           size-forms
                           `((when (next-method-p)
                               (apply #'call-next-method ,type-var ,this-var ,rest-var))))))
                 ,@ (when align-after `((align ,align-after))))))
           ;; define writer
           (defmethod write-swf-part swf-part (,type-var (,this-var ,class-name) ,source)
             (declare (ignore ,type-var))
             (with-swf-writers (,source ,value-arg)
               (macrolet ((super (slot)
                            `(,slot ,',this-var)))
                 (prog1
                     ,@ (labels ((make-bindings (forms body-forms)
                                   (if forms
                                       ;; fixme: probably shouldn't bind all
                                       ;; these slots not sure if limiting to
                                       ;; derived + local is enough or not...
                                       ;; fixme: factor out common stuff between write/size


                                       ;; need to write slots right after
                                       ;; they are bound so later slots
                                       ;; can modify specials that earlier
                                       ;; specials might want to see the
                                       ;; old value of
                                       ;; (specifically style-change-shape-record)
                                       ;; fixme: find a better way to do this?

                                       (destructuring-bind (name form &key local extra derived optional &allow-other-keys)
                                           (car forms)
                                         (if local
                                             `((let ((,name ,form))
                                                 ,@(when extra (list extra))
                                                 ,@(make-bindings (cdr forms) body-forms)))
                                             `((let ((,name
                                                      ,(if derived
                                                           `(,name ,this-var)
                                                           `(if (slot-boundp
                                                                 ,this-var ',name)
                                                                (,name ,this-var)
                                                                nil))))
                                                 ,@(when extra (list extra))
                                                 (symbol-macrolet ((,value-arg ,name))
                                                   ,(if optional
                                                        `(when ,optional ,form)
                                                        form))
                                                 ,@(make-bindings (cdr forms) body-forms)))))
                                       body-forms)))

                          (make-bindings
                           write-forms
                           `((when (next-method-p)
                               (call-next-method ,type-var ,this-var ,source))))))
                 ,@ (when align-after `((align ,align-after))))))
           ;; define ID mapping methods if needed
           ,@ (when id
                (let ((p (if (consp id) (car id) (car supers)))
                      (id (if (consp id) (second id) id)))
                  (assert p (p) "missing class name for subclass :id clause")
                  ;; might be worth keeping NIL as valid id, for things
                  ;; that are selected by a bit-flag... just using
                  ;; (ub 1) and 0/1 for now though
                  (assert id (id) "missing id for subclass :id clause")
                  `((defmethod subclass-id ((object ,class-name)
                                            (p (eql ',p)))
                      ,id)
                    (defmethod subclass-from-id ((p (eql ',p))
                                                 (id (eql ',id)))
                      ',class-name))))
              ;; define printer
           ,@ (when print-unreadably
                `((defmethod print-object ((,this-var ,class-name) ,source)
                    (print-unreadable-object (,this-var ,source
                                              :type ',class-name
                                              :identity nil)
                      ,@(if (eq print-unreadably :auto)
                            `((format ,source "~{~s:~s~#[~:; ~:_~]~}"
                                      (list ,@(loop for i in all-slots
                                                    collect `',i
                                                    if (find i real-slots)
                                                    collect
                                                    `(if (slot-boundp ,this-var
                                                                      ',i)
                                                         (,i ,this-var)
                                                         :unbound)
                                                    else
                                                    collect `(,i ,this-var)))))
                            `((format ,source ,(car print-unreadably)
                                      ,@(cdr print-unreadably)))))))))))))
