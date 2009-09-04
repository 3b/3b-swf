
(asdf:defsystem :3b-swf-swc
           :depends-on ("3b-swf"
                        "zip"
                        "cxml")
           :components ((:file "swc"))
           :serial t)
