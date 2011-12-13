
(asdf:defsystem :3b-swf-swc
  :description "Partial hacks for extracting data from flash .swc files."
  :depends-on ("3b-swf"
               "zip"
               "cxml")
  :components ((:file "swc"))
  :serial t)
