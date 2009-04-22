(in-package :3b-swf)


;;; outer interface to .swf writer



#+nil
(Defun place-object-2 (id depth x y &key (sx 1.0) (sy 1.0))
  (make-instance
   'place-object-2-tag
   'depth depth
   'character-id id
   'matrix (matrix :tx x :ty y :sx sx :sy sy)))







#+nil
(flex:with-input-from-sequence
    (is
     (flex:with-output-to-sequence (s)
       (let ((*partial-octet-write* nil))
         (with-swf-writers (s vv)
           #+nil(write-swf-part nil (file-attributes :direct-blit nil :use-gpu nil
                                                 :has-metadata t) s)
           #+nil(write-swf-part nil
                 (script-limits 60 1000) s)
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
  (%3b-swf::with-character-id-maps
   (let ((%3b-swf::*partial-octet-write* nil))
     (flet
         ((body (s)
            (%3b-swf::with-swf-writers (s vvv)
              (%3b-swf::write-swf-part '%3b-swf::rect
                                       (rect 0 0 x-twips y-twips) s)
              (%3b-swf::write-fixed8 frame-rate stream)
              (%3b-swf::write-ui16 frame-count stream )
              ;; flags: reserved=000, HasMetadata=1,AS3=1,res=00, UseNetwork=1
              (%3b-swf::write-swf-part
               nil
               (or attributes-tag
                   (file-attributes
                    :has-metadata (find %3b-swf::+metadata-tag+
                                        body-tags :key '%3b-swf::tag))) s)

              (%3b-swf::write-swf-part
               nil (script-limits script-limits-timeout
                                  script-limits-stack) s)

              (loop for tag in body-tags
                    do (%3b-swf::write-swf-part nil tag s))
              (%3b-swf::write-swf-part '%3b-swf::swf-end-tag (end-tag) s)

              )))
       (%3b-swf::with-swf-writers (stream vv)
         ;; magic #, version
         (if compress
             (%3b-swf::write-ui8 #x43 stream)
             (%3b-swf::write-ui8 #x46 stream)) ;F/C
         (%3b-swf::write-ui8 #x57 stream)      ;W
         (%3b-swf::write-ui8 #x53 stream)      ;S
         (%3b-swf::write-ui8 flash-version stream)
         #+nil(let ((size (+ 2 (reduce '+ body-tags :key 'swf-part-size)) ))
                (format t "size = ~s~%" size)
                (write-ui32 size stream))
         (let ((size
                (loop for tag in body-tags
                      for i from 0
                      for size = (%3b-swf::swf-part-size nil tag)
                      do (format t "~s size=~s~%" i size)
                      sum size)))
           (format t "size = ~s~%" size)
           (%3b-swf::write-ui32 size stream))
         (if compress
             (write-sequence (salza2:compress-data
                              (flex:with-output-to-sequence (s)
                                (body s)) 'salza2:zlib-compressor)
                             stream)
             (body stream)))))))


#+nil
(defun write-simple-swf (stream frame-label symbol-classes &key pngs)
  (write-swf
   stream
   (list*
    (background-color #x869ca7)

    (frame-label frame-label)

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

    (show-frame)
    (end-tag)

    )))

(defun write-test-swf (stream frame-label symbol-classes &key pngs)
  (declare (ignorable symbol-classes pngs))
  (write-swf
   stream
   (append
    (list
     (background-color #xffffff)

     (frame-label frame-label)

     (make-instance
      'define-shape-tag
      'character-id 1
      'bounds (rect -20 -20 20 20)
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

     #+nil(place-object 1 2 :matrix (matrix :tx 125 :ty 125))

     #+nil(show-frame))

    (let ((tags)
          (d 3))
           (flet ((box-in-cell (x y)
                    (let ((real-x (* x 12))
                          (real-y (* y 12)))
                      (push
                       (place-object 1 (incf d)
                                     :matrix (matrix :tx (+ 23 real-x)
                                                     :ty real-y))
                       tags)
                      (push (show-frame) tags))))
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

     (show-frame)))
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
(defmethod write-swf-part :around (type o s)
  (let ((start (file-position s)))
    (prog1
        (call-next-method type o s)
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
            'define-shape-tag
           (make-instance
            'define-shape-tag
            'character-id 1
            'bounds (rect -10 -10 10 10)
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
     (background-color #x505080)

     (frame-label "foo")

     (make-instance
      'define-shape-tag
      'character-id 1
      'bounds (rect -200 -200 200 200)
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
     (place-object-at 1 2 125 125)

     (show-frame))

    (let ((tags)
          (d 300))
      (flet ((box-in-cell (x y)
               (let ((real-x (* x 12))
                     (real-y (* y 12)))
                 (push (place-object-at 1 (incf d) (+ 23 real-x) real-y)
                       tags)
                 (push (show-frame) tags))))
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
          collect (place-object-at (+ i 234) (- 234 i) 40 40 :sx 2.0 :sy 2.0))

    (list (vecto::test 123)
          #+nil(make-instance
            'define-shape-4-tag
            'character-id 123
            'bounds (rect -20 -20 20 20)
            'edge-bounds (rect -20 -20 20 20)
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
          (place-object-at 123 123 25 25 :sx 2.0 :sy 2.0)
          (show-frame))


    (list
     #+nil
     (make-instance 'symbol-class-tag
                    'symbol-class-symbols
                    (append symbol-classes
                            (loop for (nil classname) in pngs
                                  for id from (length symbol-classes)
                                  collect (list id classname))))

     (show-frame)))
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
         (write-swf-part nil (vecto::test 123) s)))

)  (read-swf-part 'swf-tag is))

#+nil
(flex:with-input-from-sequence
    (is
     (flex:with-output-to-sequence (s)
       (let ((*partial-octet-write* nil))
         (with-swf-writers (s vv)
           (write-swf-part
            'define-shape-4-tag
            (make-instance
            'define-shape-4-tag
            'character-id 1
            'bounds (rect -10 -10 10 10)
            'edge-bounds (rect -10 -10 10 10)
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
     (background-color #xf05080)

     (frame-label "foo"))

    (let ((foo (hef-svg::render-document)))
           foo)
    (list
     (place-object-at 122 122 35 35 :sx 0.2 :sy 0.2))
    #+nil(loop for i below 20
          collect (place-object-at :foo (+ i 123)
                                   (- 100 (random 200))
                                   (- 100 (random 200))
                                   :sx (random 1.0) :sy (random 1.0)))
    (vecto::with-swf-canvas (:width 256 :height 256)
      (vecto::set-rgb-stroke 1 0 0)
      (vecto::set-rgb-fill 0 0 1)
      (vecto:set-line-width 10)
      (vecto::rounded-rectangle 20 20 200 200 20 20)
      (vecto::fill-and-stroke)
      (vecto::add-shape :bar1)
      (vecto::swf-sprite :baz))
    (vecto::with-swf-canvas (:width 256 :height 256)
      (vecto::set-rgba-stroke 0 0 0 1)
      (vecto::set-rgba-fill 0 0 0 1)
      (vecto:set-line-width 0)
      (vecto::rounded-rectangle 60 60 140 140 20 20)
      (vecto::rounded-rectangle 25 147 80 80 20 20)
      ;;(vecto::fill-and-stroke)
      (vecto::fill-path)
      ;;(vecto::clip-path)
      (vecto::add-shape :c1)
      (vecto::swf-sprite :cbehg))
    (list
     (place-object-at :baz 12 00 00)
     (place-object-at :bar1 13 -5 -10 :sx 0.5 :sy 0.5)
     ;;(place-object :c1 15 :clip-layers 17)
     (make-instance '%3b-swf::place-object-3-tag
                    '%3b-swf::character-id :bar1
                    '%3b-swf::depth 15
                    '%3b-swf::clip-depth 20
                    '%3b-swf::bitmap-cache 1
)
     #+nil(make-instance '%3b-swf::place-object-3-tag
                    '%3b-swf::character-id :bar1
                    '%3b-swf::depth 17
                    '%3b-swf::bitmap-cache 2
                    '%3b-swf::blend-mode 0
                    )
)
    (vecto::star-clipping :star)
    (list (place-object-at :star 16 40 40 :sx 1 :sy 1))
    #+nil(list
          ;;(vecto::center-test "foo " 123)
          ;;(vecto::twittertext "foo " 20 "c:/windows/fonts/arial.ttf" 123)
          ;;(vecto::circle-test  123)
          ;;(vecto::text-test :string "foo " :size 150 :font "c:/windows/fonts/arial.ttf" :file 123)
          ;;(vecto::sign-test "foo " "c:/windows/fonts/arial.ttf" 123)

          (vecto::radiant-lambda  123)
          (place-object-at 123 123 15 15 :sx 1.0 :sy 1.0)
          (vecto::feedlike-icon 124)
          (place-object-at 124 124 120 15 :sx 1.0 :sy 1.0)
          (vecto::line-styletest 125)
          (place-object-at 12c5 125 15 120 :sx 1.0 :sy 1.0))

    (list (show-frame)))
   :x-twips 250.0
   :y-twips 250.0
   :frame-rate 10
   :flash-version 10)
)
(untrace %3b-swf::%swf-part-size)