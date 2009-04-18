(in-package :3b-swf)


;;; outer interface to .swf writer

(defun make-rect (x1 y1 x2 y2)
  (make-instance 'rect 'xmin x1 'ymin y1 'xmax x2 'ymax y2))

(defun make-file-attributes (&key (direct-blit t) (use-gpu t) (as3 t)
                             has-metadata (use-network t))
  (make-instance 'file-attributes-tag
                 'use-direct-blit direct-blit 'use-gpu use-gpu
                 'has-metadata has-metadata 'actionscript-3 as3
                 'use-network use-network))

(defun make-script-limits (timeout max-recursion)
  (make-instance 'script-limits-tag
                 'max-recursion-depth max-recursion
                 'script-timeout-seconds timeout))



(defun rgb (&rest args)
  (cond
    ((integerp (car args))
     (make-instance 'rgb
                    'r (ldb (byte 8 16) (car args))
                    'g (ldb (byte 8 08) (car args))
                    'b (ldb (byte 8 00) (car args))))
    ((keywordp (car args))
     (destructuring-bind (&key r g b) args
       (make-instance 'rgb 'r r 'g g 'b b)))
    (t (error "unknown args in rgb ~s" args))))
(defun rgba (&rest args)
  (cond
    ((integerp (car args))
     (make-instance 'rgba
                    'a (ldb (byte 8 24) (car args))
                    'r (ldb (byte 8 16) (car args))
                    'g (ldb (byte 8 08) (car args))
                    'b (ldb (byte 8 00) (car args))))
    ((keywordp (car args))
     (destructuring-bind (&key r g b a) args
       (make-instance 'rgba 'r r 'g g 'b b 'a a)))
    (t (error "unknown args in rgb ~s" args))))




(Defun place-object-2 (id depth x y &key (sx 1.0) (sy 1.0))
  (make-instance
   'place-object-2-tag
   'depth depth
   'character-id id
   'matrix (make-instance
            'matrix
            'translate (make-instance
                        'matrix-part-translate
                        'value1 x
                        'value2 y)
            'scale (make-instance
                    'matrix-part-fixed
                    'value1 sx
                    'value2 sy)) ))







#+nil
(flex:with-input-from-sequence
    (is
     (flex:with-output-to-sequence (s)
       (let ((*partial-octet-write* nil))
         (with-swf-writers (s vv)
           #+nil(write-swf-part (make-file-attributes :direct-blit nil :use-gpu nil
                                                 :has-metadata t) s)
           #+nil(write-swf-part
                 (make-script-limits 60 1000) s)
           (write-swf-part
            #+nil(make-instance 'frame-label-tag
                                'name "foo")
            (make-instance 'symbol-class-tag
                   'symbol-class-symbols '((0 "foo")))
            #+nil(make-instance 'set-background-color-tag
                                'background-color (rgb #x869ca7)) s)
           #+nil(let ((vv #xffffff))
             (ub 9)
             (ui8)
             (ub 17))
           (align 8))))

)  (read-swf-part 'swf-tag is))


;;(trace write-swf-part)
;;(trace swf-part-size)
;;(trace %swf-part-size)
;;(trace read-swf-part)
;;
;;(untrace write-swf-part)
;;(untrace swf-part-size)
;;(untrace %swf-part-size)
;;(untrace read-swf-part)

(defun write-swf (stream body-tags
                  &key (flash-version 9)
                  (x-twips 400.0) (y-twips 300.0)
                  (frame-rate 30.0)
                  (frame-count 1)
                  (attributes-tag nil)
                  (script-limits-stack 1000)
                  (script-limits-timeout 60)
                  compress)
  (let ((*partial-octet-write* nil))
    (flet
        ((body (s)
           (with-swf-writers (s vvv)
             (write-swf-part (make-rect 0 0 x-twips y-twips) s)
             (write-fixed8 frame-rate stream)
             (write-ui16 frame-count stream )
             ;; flags: reserved=000, HasMetadata=1,AS3=1,res=00, UseNetwork=1
             (write-swf-part (or attributes-tag
                                 (make-file-attributes
                                  :has-metadata (find +metadata-tag+
                                                      body-tags :key 'tag))) s)

            (write-swf-part (make-script-limits script-limits-timeout
                                                script-limits-stack) s)

            (loop for tag in body-tags
                  do (write-swf-part tag s))
            (write-swf-part (make-instance 'swf-end-tag) s)

            )))
      (with-swf-writers (stream vv)
        ;; magic #, version
        (if compress (write-ui8 #x43 stream) (write-ui8 #x46 stream)) ;F/C
        (write-ui8 #x57 stream)         ;W
        (write-ui8 #x53 stream)         ;S
        (write-ui8 flash-version stream)
        #+nil(let ((size (+ 2 (reduce '+ body-tags :key 'swf-part-size)) ))
          (format t "size = ~s~%" size)
          (write-ui32 size stream))
        (let ((size
               (loop for tag in body-tags
                     for i from 0
                     for size = (swf-part-size tag)
                     do (format t "~s size=~s~%" i size)
                     sum size)))
          (format t "size = ~s~%" size)
          (write-ui32 size stream))
        (if compress
            (write-sequence (salza2:compress-data
                             (flex:with-output-to-sequence (s)
                               (body s)) 'salza2:zlib-compressor)
                            stream)
            (body stream))))))


#+nil
(defun write-simple-swf (stream frame-label symbol-classes &key pngs)
  (write-swf
   stream
   (list*
    (make-instance 'set-background-color-tag
                   'background-color (rgb #x869ca7))

    (make-instance 'frame-label-tag
                   'name frame-label)

    ;;optionally embed some .png files, probably limited to 32 bit RGBA for now
    #+nil(loop for (pathname nil) in pngs
          for id from  (length symbol-classes)
          do (write-png-tag id pathname stream))

    ;; AS3 tag
    (make-instance 'do-abc-tag
                   'flags 1 ;; lazy initialize
                   'name "frame"
                   'data (flex:with-output-to-sequence (s)
                           (write-abc-file avm2-asm::*assembler-context* s)))

    (make-instance 'symbol-class-tag
                   'symbol-class-symbols
                   (append symbol-classes
                           (loop for (nil classname) in pngs
                                 for id from (length symbol-classes)
                                 collect (list id classname))))

    (make-instance 'show-frame-tag)
    (make-instance 'swf-end-tag)

    )))

(defun write-test-swf (stream frame-label symbol-classes &key pngs)
  (declare (ignorable symbol-classes pngs))
  (write-swf
   stream
   (append
    (list
     (make-instance 'set-background-color-tag
                    'background-color (rgb #xffffff))

     (make-instance 'frame-label-tag
                    'name frame-label)

     (make-instance
      'define-shape-tag
      'shape-id 1
      'bounds (make-rect -20 -20 20 20)
      'shapes (make-instance
               'shape-with-style
               'line-styles (make-instance
                             'line-style-array
                             'line-styles
                             (list (make-instance 'line-style
                                                  'width 1
                                                  'color (rgb 0))))
               'fill-styles (make-instance
                             'fill-style-array
                             'fill-styles nil)

               'shape-records
               (list
                (make-instance 'style-change-shape-record
                               'line-style 1
                               'move-to (make-instance 'state-move-to
                                                       'delta-x 20
                                                       'delta-y 20))
                (make-instance 'straight-edge-shape-record
                               'delta-y 10)
                (make-instance 'straight-edge-shape-record
                               'delta-x -10)
                (make-instance 'straight-edge-shape-record
                               'delta-y -10)
                (make-instance 'straight-edge-shape-record
                               'delta-x 10)
                (make-instance 'shape-end-record))))

     #+nil(make-instance 'place-object-2-tag
                    'depth 2 'character-id 1
                    'matrix (make-instance 'matrix
                                           'translate (make-instance
                                                       'matrix-part-translate
                                                       'value1 125
                                                       'value2 125)))
     #+nil(make-instance 'swf-show-frame-tag))

    (let ((tags)
          (d 3))
           (flet ((box-in-cell (x y)
                    (let ((real-x (* x 12))
                          (real-y (* y 12)))
                      (push
                       (make-instance
                        'place-object-2-tag
                        'depth (incf d)
                        'character-id 1
                        'matrix (make-instance
                                 'matrix
                                 'translate (make-instance
                                             'matrix-part-translate
                                             'value1 (+ 23 real-x)
                                             'value2 real-y)) )
                       tags)
                      (push (make-instance 'swf-show-frame-tag) tags))))
             ;; GIMME AN "L"
             (dolist (i '(1 2 3 4 5))
               (box-in-cell 1 i))
             (dolist (i '(2 3))
               (box-in-cell i 5))
             ;; GIMME AN "I"
             (dolist (i '(1 2 3 4 5))
               (box-in-cell 5 i))
             ;; GIMME AN "S"
             (dolist (i '(1 2 3 4 5))
               (box-in-cell 8 i))
             (box-in-cell 7 5)
             (box-in-cell 9 1)
             ;; GIMME A "P"
             (dolist (i '(1 2 3 4 5))
               (box-in-cell 11 i))
             (box-in-cell 12 1)
             (box-in-cell 13 1)
             (box-in-cell 13 2)
             (box-in-cell 13 3)
             (box-in-cell 12 3))
           (nreverse tags))

    (list
     #+nil
     (make-instance 'symbol-class-tag
                    'symbol-class-symbols
                    (append symbol-classes
                            (loop for (nil classname) in pngs
                                  for id from (length symbol-classes)
                                  collect (list id classname))))

     (make-instance 'swf-show-frame-tag)))
   :x-twips 250.0
   :y-twips 250.0
   :frame-rate 4))


#+nil
(with-open-file (s "/tmp/new-write.swf" :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
  (write-test-swf s "foo" '((0 "foo")))
  )


(defparameter *bleh* nil)
#+nil
(setf
 *bleh* (let ((*partial-octet-read* nil))
          (with-open-file (s "/tmp/new-write.swf"
                             :element-type '(unsigned-byte 8))
            (read-swf s))))

#+nil
(defmethod write-swf-part :around (o s)
  (let ((start (file-position s)))
    (prog1
        (call-next-method o s)
      (format t "^^^^^ wrote ~s bytes~%" (- (file-position s) start) ))
    ))
#+nil
(defmethod %swf-part-size :around (o &rest r)
  (let ((start *swf-sizer-bitpos*))
    (prog1
        (apply #'call-next-method o r)
      (format t "#### ~s bits in ~s (started @~s=rem~s)~%" (- *swf-sizer-bitpos* start)
              (type-of o)
              start
              (rem start 8)))
    ))


;;
;;(swf-part-size
;; (make-instance 'place-object-2-tag
;;                'depth 2 'character-id 1
;;                'matrix (make-instance 'matrix
;;                                       'translate (make-instance
;;                                                   'matrix-part-translate
;;                                                   'value1 1
;;                                                   'value2 2))))
;;
;;(swf-part-size  (make-instance 'matrix
;;                                       'translate (make-instance
;;                                                   'matrix-part-translate
;;                                                   'value1 1
;;                                                   'value2 2)))
;;(swf-part-size  (make-instance 'matrix-part-translate
;;                               'value1 1
;;                               'value2 2))
;;
;;
#+nil
(flex:with-input-from-sequence
    (is
     (flex:with-output-to-sequence (s)
       (let ((*partial-octet-write* nil))
         (with-swf-writers (s vv)
           (write-swf-part
           (make-instance
            'define-shape-tag
            'shape-id 1
            'bounds (make-rect -10 -10 10 10)
            'shapes (make-instance
                     'shape-with-style
                     'line-styles (make-instance
                                   'line-style-array
                                   'line-styles
                                   (list (make-instance 'line-style
                                                        'width 20
                                                        'color (rgb 0))))
                     'fill-styles (make-instance
                                   'fill-style-array
                                   'fill-styles nil)

                     'shape-records
                     (list
                      (make-instance 'style-change-shape-record
                                     'line-style 1
                                     'move-to (make-instance 'state-move-to
                                                             'delta-x 20
                                                             'delta-y 20))
                      (make-instance 'straight-edge-shape-record
                                     'delta-y 10)
                      (make-instance 'straight-edge-shape-record
                                     'delta-x -10)
                      (make-instance 'straight-edge-shape-record
                                     'delta-y -10)
                      (make-instance 'straight-edge-shape-record
                                     'delta-x 10)
                      #+nil(make-instance 'shape-end-record))))
           s))))

)  (read-swf-part 'swf-tag is))
;;
;;
;;(typep   (make-instance 'straight-edge-shape-record
;;                                    'delta-y 10) 'edge-shape-record)
;;
;;(type-flag (make-instance 'straight-edge-shape-record
;;                                    'delta-y 10))
;;(type-flag  (make-instance 'style-change-shape-record
;;                                    'line-style 1
;;                                    'move-to (make-instance 'state-move-to
;;                                                            'delta-x 20
;;                                                            'delta-y 20)))
;;
;;


#+nil
(with-open-file (s "/tmp/new-write.swf" :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
  (write-swf
   s
   (append
    (list
     (make-instance 'set-background-color-tag
                    'background-color (rgb #x505080))

     (make-instance 'frame-label-tag
                    'name "foo")

     (make-instance
      'define-shape-tag
      'shape-id 1
      'bounds (make-rect -200 -200 200 200)
      'shapes (make-instance
               'shape-with-style
               'line-styles (make-instance
                             'line-style-array
                             'line-styles
                             (list (make-instance 'line-style
                                                  'width 1
                                                  'color (rgb 0))))
               'fill-styles (make-instance
                             'fill-style-array
                             'fill-styles nil)

               'shape-records
               (list
                (make-instance 'style-change-shape-record
                               'line-style 1
                               'move-to (make-instance 'state-move-to
                                                       'delta-x 20
                                                       'delta-y 20))
                (make-instance 'straight-edge-shape-record
                               'delta-y 10)
                (make-instance 'straight-edge-shape-record
                               'delta-x -10)
                (make-instance 'straight-edge-shape-record
                               'delta-y -10)
                (make-instance 'straight-edge-shape-record
                               'delta-x 10)
                (make-instance 'shape-end-record))))

     #+nil
     (place-object-2 2 1 125 125)

     (make-instance 'swf-show-frame-tag))

    (let ((tags)
          (d 300))
      (flet ((box-in-cell (x y)
               (let ((real-x (* x 12))
                     (real-y (* y 12)))
                 (push
                  (place-object-2 1 (incf d) (+ 23 real-x) real-y)

                  tags)
                 (push (make-instance 'swf-show-frame-tag) tags))))
        ;; GIMME AN "L"
        (dolist (i '(1 2 3 4 5))
          (box-in-cell 1 i))
        (dolist (i '(2 3))
          (box-in-cell i 5))
        ;; GIMME AN "I"
        (dolist (i '(1 2 3 4 5))
          (box-in-cell 5 i))
        ;; GIMME AN "S"
        (dolist (i '(1 2 3 4 5))
          (box-in-cell 8 i))
        (box-in-cell 7 5)
        (box-in-cell 9 1)
        ;; GIMME A "P"
        (dolist (i '(1 2 3 4 5))
          (box-in-cell 11 i))
        (box-in-cell 12 1)
        (box-in-cell 13 1)
        (box-in-cell 13 2)
        (box-in-cell 13 3)
        (box-in-cell 12 3))
      (nreverse tags))

    #+nil
    (loop repeat 20
          for i = 1 then (* i 1.25)
          collect (let ((vecto::*curve-tolerance* i))
                    (vecto::curve-test2 (+ i 234)))
          collect (place-object-2 (+ i 234) (- 234 i) 40 40 :sx 2.0 :sy 2.0)
          )

    (list (vecto::test 123)
          #+nil(make-instance
            'define-shape-4-tag
            'shape-id 123
            'shape-bounds (make-rect -20 -20 20 20)
            'edge-bounds (make-rect -20 -20 20 20)
            'shapes (make-instance
                     'shape-with-style
                     'line-styles
                    #+nil (make-instance
                                   'line-style-array
                                   'line-styles nil)
                     (make-instance
                                   'line-style-array
                                   'line-styles
                                   (list (make-instance
                                          'line-style-2
                                          'width 8
                                          'color (rgba :r 22 :g 222 :b 0 :a 255)
                                          '3b-swf::join-style 0
                                          '3b-swf::start-cap-style 0
                                          '3b-swf::end-cap-style 0
                                          '3b-swf::no-close t)
                                         (make-instance
                                          'line-style-2
                                          'width 5
                                          'color (rgba :r 55 :g 222 :b 0 :a 255)
                                          '3b-swf::join-style 0
                                          '3b-swf::start-cap-style 0
                                          '3b-swf::end-cap-style 0
                                          '3b-swf::no-close t)))
                     'fill-styles (make-instance
                                   'fill-style-array
                                   'fill-styles nil)

                     'shape-records
                     (list

                      (make-instance 'style-change-shape-record
                                     'line-style 1
                                     'move-to (make-instance 'state-move-to
                                                             'delta-x 0
                                                             'delta-y 0))
                      (make-instance 'straight-edge-shape-record
                                     'delta-y 10)
                      (make-instance 'straight-edge-shape-record
                                     'delta-x -10)
                      (make-instance 'straight-edge-shape-record
                                     'delta-y -10)
                      (make-instance 'straight-edge-shape-record
                                     'delta-x 10)

                      (make-instance 'style-change-shape-record
                                     'line-style 1
                                     'line-styles
                                     (make-instance
                                      'line-style-array
                                      'line-styles
                                      (list (make-instance
                                             'line-style-2
                                             'width 3
                                             'color (rgba :r 222 :g 222 :b 0 :a 255)
                                             '3b-swf::join-style 0
                                             '3b-swf::start-cap-style 0
                                             '3b-swf::end-cap-style 0
                                             '3b-swf::no-close t)
                                            (make-instance
                                             'line-style-2
                                             'width 20
                                             'color (rgba :r 111 :g 222 :b 0 :a 255)
                                             '3b-swf::join-style 0
                                             '3b-swf::start-cap-style 0
                                             '3b-swf::end-cap-style 0
                                             '3b-swf::no-close t)))

                                     'fill-styles (make-instance
                                                   'fill-style-array
                                                   'fill-styles nil
)
                                     'move-to (make-instance 'state-move-to
                                                             'delta-x 5
                                                             'delta-y 5))

                      (make-instance 'straight-edge-shape-record
                                     'delta-y 10)
                      (make-instance 'straight-edge-shape-record
                                     'delta-x -10)
                      (make-instance 'straight-edge-shape-record
                                     'delta-y -10)
                      (make-instance 'straight-edge-shape-record
                                     'delta-x 10)

                      #+nil(make-instance 'shape-end-record))))
          (place-object-2 123 123 25 25 :sx 2.0 :sy 2.0)
          (make-instance 'swf-show-frame-tag))


    (list
     #+nil
     (make-instance 'symbol-class-tag
                    'symbol-class-symbols
                    (append symbol-classes
                            (loop for (nil classname) in pngs
                                  for id from (length symbol-classes)
                                  collect (list id classname))))

     (make-instance 'swf-show-frame-tag)))
   :x-twips 250.0
   :y-twips 250.0
   :frame-rate 10))

#+nil
(setf
 *bleh* (let ((*partial-octet-read* nil))
          (with-open-file (s "/tmp/new-write.swf"
                             :element-type '(unsigned-byte 8))
            (read-swf s))))


#+nil
(flex:with-input-from-sequence
    (is
     (flex:with-output-to-sequence (s)
       (let ((*partial-octet-write* nil))
         (write-swf-part (vecto::test 123) s)))

)  (read-swf-part 'swf-tag is))

#+nil
(flex:with-input-from-sequence
    (is
     (flex:with-output-to-sequence (s)
       (let ((*partial-octet-write* nil))
         (with-swf-writers (s vv)
           (write-swf-part
            (make-instance
            'define-shape-4-tag
            'shape-id 1
            'shape-bounds (make-rect -10 -10 10 10)
            'edge-bounds (make-rect -10 -10 10 10)
            'shapes (make-instance
                     'shape-with-style
                     'line-styles (make-instance
                                   'line-style-array
                                   'line-styles
                                   (list (make-instance
                                          'line-style-2
                                          'width 20
                                          'color (rgba 0)
                                          '3b-swf::join-style 0
                                          '3b-swf::start-cap-style 0
                                          '3b-swf::end-cap-style 0
                                          '3b-swf::no-close t)))
                     'fill-styles (make-instance
                                   'fill-style-array
                                   'fill-styles nil)

                     'shape-records
                     (list
                      (make-instance 'style-change-shape-record
                                     'line-style 1
                                     'move-to (make-instance 'state-move-to
                                                             'delta-x 20
                                                             'delta-y 20))
                      (make-instance 'straight-edge-shape-record
                                     'delta-y 10)
                      (make-instance 'straight-edge-shape-record
                                     'delta-x -10)
                      (make-instance 'straight-edge-shape-record
                                     'delta-y -10)
                      (make-instance 'straight-edge-shape-record
                                     'delta-x 10)
                      #+nil(make-instance 'shape-end-record))))
           s))))

)  (read-swf-part 'swf-tag is))


#+nil

(with-open-file (s "/tmp/new-write.swf" :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
  (write-swf
   s
   (append
    (list
     (make-instance 'set-background-color-tag
                    'background-color (rgb #x505080))

     (make-instance 'frame-label-tag
                    'name "foo"))

    (list
     (hef-svg::render-document)
     (place-object-2 122 122 15 15 :sx 0.2 :sy 0.2))
    #+nil(list
     ;;(vecto::center-test "foo " 123)
     ;;(vecto::twittertext "foo " 20 "c:/windows/fonts/arial.ttf" 123)
     ;;(vecto::circle-test  123)
     ;;(vecto::text-test :string "foo " :size 150 :font "c:/windows/fonts/arial.ttf" :file 123)
     ;;(vecto::sign-test "foo " "c:/windows/fonts/arial.ttf" 123)

     (vecto::radiant-lambda  123)
     (place-object-2 123 123 15 15 :sx 1.0 :sy 1.0)
     (vecto::feedlike-icon 124)
     (place-object-2 124 124 120 15 :sx 1.0 :sy 1.0)
     (vecto::line-styletest 125)
     (place-object-2 125 125 15 120 :sx 1.0 :sy 1.0))

    (list (make-instance 'swf-show-frame-tag)))
   :x-twips 250.0
   :y-twips 250.0
   :frame-rate 10
   :flash-version 10)
)