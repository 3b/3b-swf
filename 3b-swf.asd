
(asdf:defsystem :3b-swf
  :description "Common Lisp .swf file manipulation library"
  :depends-on ("ieee-floats"
               "flexi-streams"
               "alexandria"
               "chipz"
               "salza2"
               ;; fixme: split some of these off into separate packages
               "cl-jpeg"
               "vecto"
               "cxml"
               "zpng")
  :components ((:module "low-level"
                :components ((:file "package")
                             (:file "swf-tag-ids")
                             (:file "swftype-size")
                             (:file "swftype-read")
                             (:file "swftype-write")
                             (:file "swf-primitive-types")
                             (:file "swf-deftype")
                             (:file "swf-action-records")
                             (:file "generics")
                             (:file "swf-types")
                             (:file "swf-clip")
                             (:file "swf-shape")
                             (:file "swf-filter")
                             (:file "swf-morph")
                             (:file "swf-bitmap")
                             (:file "swf-sound")
                             (:file "swf-font")
                             (:file "swf-tags")
                             (:file "swf-abc")
                             (:file "swf-file"))
                :serial t)
               (:file "package")
               (:file "api")
               (:file "shape"))
  :serial t)
