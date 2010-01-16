(in-package :3b-swf)

;; todo: finish this...

;; needs cxml, zip

(setf cxml:*catalog* (cxml:make-catalog))

;; fixme: is this complete/correct? alternately, can we skip it? (only
;;        used for whitespace-normalization in the xml parser

(defparameter *swc-dtd* "
<!ELEMENT swc (versions*, features*, components*, libraries*, files*)>
<!ELEMENT versions (swc*, flex*)>
<!ELEMENT features (feature-script-deps*, feature-components*, feature-files*)>
<!ELEMENT components (component*)>
<!ELEMENT libraries (library*)>
<!ELEMENT files (file*)>
<!ELEMENT library (script*, keep-as3-metadata*,digests*)>
<!ELEMENT script (def*,dep*)>
<!ELEMENT keep-as3-metadata (metadata*)>
<!ELEMENT digests (digest*)>")

(defun catalog-deps (octets)
  (let* ((xml (cxml:parse-octets octets
                                 #++(stp:make-builder)
                                 (cxml:make-whitespace-normalizer
                                  (cxml-xmls:make-xmls-builder)
                                  (flex:with-input-from-sequence
                                      (s (babel:string-to-octets *swc-dtd*))
                                    (cxml:parse-dtd-stream s)))))
         (deps (loop
                  for ((tag . nil) nil . body)
                  in (cddr (assoc "library"
                                  (cddr (assoc "libraries" (cddr xml)
                                               :test 'equal :key 'car))
                                  :test 'equal :key 'car))
                  when (string= tag "script")
                  collect (loop for ((tag2 . nil) . body2) in body
                             collect (list* tag2 body2))))
         (hash (make-hash-table :test 'equal)))
    (loop for dep in deps
       for def = (second (assoc "id" (second (assoc "def" dep  :test 'string=))
                                :test 'string=))
       do (setf (gethash def hash)
                (loop for i in dep
                   when (string= (first i) "dep")
                   collect (list :id (second (assoc "id" (second i)
                                                    :test 'string=))
                                 :type (second (assoc "type" (second i)
                                                      :test 'string=))))))
    hash
    ))

(defun import-from-swc (imports catalog tags &key verbose)
  (let ((deps (copy-list imports)))
    (labels ((dep (import)
               (let ((dep (gethash import catalog)))
                 (loop for i in dep
                    for d = (getf i :id)
                    unless (member d deps :test 'string=)
                    do (pushnew d deps :test 'string=)
                      (dep d)))))
      (loop for id in imports
         do (dep id)))
    (setf deps (mapcar (lambda (x)
                         (substitute #\/ #\: (substitute #\/ #\. x)))
                       deps))
    (loop for tag in tags
       when (and (typep tag '%swf:do-abc-tag)
                 (member (%swf:name tag) deps :test 'string=))
       collect tag
       else if (and verbose (typep tag '%swf:do-abc-tag))
       do (format t "skipping abc tag ~s~%" (%swf:name tag) ))))

(defun read-swc (path)
  (zip:with-zipfile (z path)
    (let* ((%swf::*blob-tags* (list))
           (%swf::*trace-tags* (list))
           (lib (flex:with-input-from-sequence (s (zip:zipfile-entry-contents (zip:get-zipfile-entry "library.swf" z)))
                  (%swf:read-swf s)))
           (deps (catalog-deps (zip:zipfile-entry-contents
                                (zip:get-zipfile-entry "catalog.xml" z)))))
      (values lib deps))))



(defun extract-from-swc (path scripts &key verbose)
  (multiple-value-bind (lib deps) (read-swc path)
    (import-from-swc scripts deps (getf (cdr lib) :tags) :verbose verbose)))
