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
                  (frame-count  (loop for i in body-tags
                                      when (typep i '%3b-swf::swf-show-frame-tag)
                                      count 1))
                  (attributes-tag nil)
                  (script-limits-stack 1000)
                  (script-limits-timeout 60)
                  compress)
  (format t "frame count = ~s~%" frame-count)
  (%3b-swf::with-character-id-maps
   (let ((%3b-swf::*partial-octet-write* nil)
         (bounds (rect 0 0 x-twips y-twips))
         (header-tags
          (list
           ;; flags: reserved=000, HasMetadata=1,AS3=1,res=00, UseNetwork=1
           #+nil (or attributes-tag
                     (file-attributes
                      :has-metadata (find %3b-swf::+metadata-tag+
                                          body-tags :key '%3b-swf::tag)))
           #+nil(script-limits script-limits-timeout
                               script-limits-stack)))
         (end-tag (end-tag)))
     (flet
         ((body (s)
            (%3b-swf::with-swf-writers (s vvv)
              (%3b-swf::write-swf-part '%3b-swf::rect bounds s)
              (%3b-swf::write-fixed8 frame-rate stream)
              (%3b-swf::write-ui16 frame-count stream )
              (loop for tag in header-tags
                    do (%3b-swf::write-swf-part nil tag s))
              (loop for tag in body-tags
                    do (%3b-swf::write-swf-part nil tag s))
              (%3b-swf::write-swf-part '%3b-swf::swf-end-tag end-tag s)

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
         ;; 12 bytes for header
         (let* ((%3b-swf::*swf-sizer-bitpos* (* 12 8)))
           (%3b-swf::%swf-part-size '%3b-swf::rect bounds)
           (loop for tag in header-tags
                 for i from 0
                 do (%3b-swf::%swf-part-size nil tag :body-only nil))
           (loop for tag in body-tags
                 for i from 0
                 do (%3b-swf::%swf-part-size nil tag :body-only nil))

           (%3b-swf::%swf-part-size nil end-tag)
           (format t "size = ~s~%" %3b-swf::*swf-sizer-bitpos*)
           (%3b-swf::write-ui32 (/ %3b-swf::*swf-sizer-bitpos* 8) stream))
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
     (background-color #xffffff) ;;#x027f

     ;;(frame-label "foo")

     (make-instance
      '%3b-swf::define-shape-tag
      '%3b-swf::character-id 1
      '%3b-swf::bounds (rect (/ -190 20) 0.5 (/ 30 20) (/ 230 20))
      '%3b-swf::shapes (make-instance
               '%3b-swf::shape-with-style
               '%3b-swf::line-styles (make-instance
                                      '%3b-swf::line-style-array
                                      '%3b-swf::line-styles
                                      (list (make-instance '%3b-swf::line-style
                                                           '%3b-swf::width 1
                                                           '%3b-swf::color (rgb 0))))
               '%3b-swf::fill-styles (make-instance
                                      '%3b-swf::fill-style-array
                                      '%3b-swf::fill-styles nil)

               '%3b-swf::shape-records
               (list
                (make-instance '%3b-swf::style-change-shape-record
                               '%3b-swf::line-style 1
                               '%3b-swf::move-to (make-instance
                                                  '%3b-swf::state-move-to
                                                  '%3b-swf::delta-x 1
                                                  '%3b-swf::delta-y 1))
                (make-instance '%3b-swf::straight-edge-shape-record
                               '%3b-swf::delta-y 10)
                (make-instance '%3b-swf::straight-edge-shape-record
                               '%3b-swf::delta-x -10)
                (make-instance '%3b-swf::straight-edge-shape-record
                               '%3b-swf::delta-y -10)
                (make-instance '%3b-swf::straight-edge-shape-record
                               '%3b-swf::delta-x 10)
                (make-instance '%3b-swf::shape-end-record))))

     #+nil
     (place-object-at 1 2 125 125)

     #+nil(show-frame))

    (let ((tags)
          (d 300))
      (flet ((box-in-cell (x y)
               (let ((real-x (* x 12))
                     (real-y (* y 12)))
                 (push (place-object-at 1 (incf d)
                                        (+ 0 real-x) real-y)
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

    #+nil(list (vecto::test 123)
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

     #+nil(show-frame)))
   :x-twips 550
   :y-twips 550
   :frame-rate 10
   :flash-version 10))

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
    #+nil(loop for i below 20
               collect (place-object-at :foo (+ i 123)
                                        (- 100 (random 200))
                                        (- 100 (random 200))
                                        :sx (random 1.0) :sy (random 1.0)))
    (vecto::with-swf-canvas (:width 256 :height 256)
      (vecto::set-rgb-stroke 1 0 0)
      (vecto::set-rgb-fill 0 0 1)
      (vecto:set-line-width 5)
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
    (vecto::star-clipping :star)


    (list
     #+nil(place-object-at 122 122 35 35 :sx 0.2 :sy 0.2)
     (place-object-at 122 122 35 35 :sx 1.2 :sy 1.2))
    (list
     (place-object-at :baz 12 00 00)
     (place-object-at :bar1 13 -5 -10 :sx 0.5 :sy 0.5)
     ;;(place-object :c1 15 :clip-layers 17)
     (make-instance '%3b-swf::place-object-3-tag
                    '%3b-swf::character-id :bar1
                    '%3b-swf::depth 15
                    '%3b-swf::clip-depth 20
                    '%3b-swf::bitmap-cache 1)
     #+nil(make-instance '%3b-swf::place-object-3-tag
                         '%3b-swf::character-id :bar1
                         '%3b-swf::depth 17
                         '%3b-swf::bitmap-cache 2
                         '%3b-swf::blend-mode 0))
    (list (place-object-at :star 16 40 40 :sx 1 :sy 1))

    #+nil(list (place-object-at :baz 300 0 0 :sx 1 :sy 1)
          (place-object-at :star 301 30 30 :sx 1 :sy 1))

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

    (list (show-frame))

    #+nil(loop for i from 1 below 120
               for r = (float (/ (* pi i) 30.0) 1.0)
               collect (place-object-at 122 122
                                        (+ (* 20 (sin r)) 35)
                                        (+ (* 20 (cos r)) (- 35 20))
                                        :sx (+ 0.2 (* 0.01 (sin (* 2 r))))
                                        :sy (+ (- 0.2 0.01) (* 0.01 (cos (* 2 r))))
                                        :move-p t)
               collect (show-frame))

    )
   :x-twips 250.0
   :y-twips 250.0
   :frame-rate 30
   :flash-version 10)
)
(untrace %3b-swf::%swf-part-size)

       gradientTransform="matrix(-2.8581916e-2,,-0.7689111,-1.7457566e-2,19.648021,-81.321825)"


(with-open-file (s "/tmp/svg-test.swf" :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
  ;;(setf svg-swf::*document* (cxml:parse-file "/tmp/q.svg" (cxml-dom:make-dom-builder)))
  ;;(setf svg-swf::*document* (cxml:parse-file "/home/bbotta/src/backup-repos/svg_/hrum_bottle.svg" (cxml-dom:make-dom-builder)))
  ;;(setf svg-swf::*document* (cxml:parse-file "/home/bbotta/src/backup-repos/svg_/mario.svg" (cxml-dom:make-dom-builder)))
  (setf svg-swf::*document* (cxml:parse-file "/home/bbotta/src/backup-repos/svg_/test2.svg" (cxml-dom:make-dom-builder)))
  ;(setf svg-swf::*document* (cxml:parse-file "/tmp/q3.svg" (cxml-dom:make-dom-builder)))
  ;;(setf svg-swf::*document* (cxml:parse-file "/tmp/q4.svg" (cxml-dom:make-dom-builder)))
  (multiple-value-bind (doc w h)
      (svg-swf::render-document :qqq)
   (write-swf
    s
    (append
     (list
      (background-color #xf05080)
      ;;(background-color #xffffff)

      (frame-label "foo"))

     doc

     (list
      (place-object-at :qqq 123 0 0 :sx 1.0 :sy 1.0))
     (list (show-frame))
     )
    :x-twips w
    :y-twips h
    :frame-rate 30
    :flash-version 10))
)



#+_
(let ((%swf::*blob-tags* (list))
      (%swf::*trace-tags* (list)))
 (defparameter *foo*
   (with-open-file (s "/tmp/kongregate-shootorial.swf"
                      :element-type '(unsigned-byte 8))
     (%swf:read-swf s))))

#+_
(defparameter *swa*
  (with-open-file (s "/tmp/SmallWorld-All.swf"
                     :element-type '(unsigned-byte 8))
    (%swf:read-swf s)))
#+_
(defparameter *foo2*
  (with-open-file (s "/tmp/test.swf"
                     :element-type '(unsigned-byte 8))
    (%swf:read-swf s)))

#+_
(let ((%swf::*trace-tags* (list )))
  (defparameter *ks*
    (with-open-file (s "/tmp/shoot.swf"
                       :element-type '(unsigned-byte 8))
      (%swf:read-swf s))))
#+_
(let ((%swf::*trace-tags* (list )))
  (defparameter *ks2*
    (with-open-file (s "/tmp/stest.swf"
                       :element-type '(unsigned-byte 8))
      (%swf:read-swf s))))
#+_
(untrace %swf:read-swf-part)

#+_
(time
 (with-open-file (s "/tmp/stest.swf" :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
   (write-swf
    s
    (getf (cdr *foo*) :tags)
    :x-twips 600
    :y-twips 300
    :frame-rate 30
    :flash-version 9)
   ))

#+nil
(format t "~{~s~%~}~%" (list-exported-tags (getf (cdr *swa*) :tags)))

#+_
(format t "~{~s~%~}~%" (loop for i in (getf (cdr *foo*) :tags)
                          when (%swf:character-id i)
                          collect it))

#+_
(let* ((tags (getf (cdr *foo*) :tags))
       (tag (find-tag-by-id (find-exported-tag "Explosion" tags)
                            tags)))
  (format t "~%~s~%~%deps=~s~%" (%swf:control-tags tag)
          (tag-dependencies tag tags)))

#+_
(let* ((all-tags (getf (cdr *foo*) :tags))
       (tags (extract-tag "Explosion" all-tags :rename :foo))
       (*print-pretty* nil))
  (format t "~{~s~%~}~%" tags))

#+_
(with-open-file (s "/tmp/test.swf" :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
  (write-swf
   s
   (append
    (list
     (background-color #x301018)
     ;;(background-color #xffffff)

     (frame-label "foo"))

    (list
     (shape* 1 '((:line-style 10 0 0 (:solid  1 0 0 1))
                 (:move-to -80 -80)
                 (:line-to 80 -80)
                 (:line-to 80 80)
                 (:line-to -80 80)
                 (:line-to -80 -80)))
     (place-object 1 1 :matrix (matrix :tx 125 :ty 125)))

    (let ((all-tags (getf (cdr *foo*) :tags)))
      (extract-tag "Explosion" all-tags :rename :qqq))

    (let ((all-tags (getf (cdr *swa*) :tags)))
      (append (extract-tag "Windmill" all-tags :rename :windmill)
              (extract-tag "WatchTower" all-tags :rename :watchtower)))

    (list
     (place-object-at :watchtower 3 40 100 :sx 1.0 :sy 1.0)
     (place-object-at :windmill 4 120 100 :sx 1.0 :sy 1.0)
     (place-object-at :qqq 5 125 80 :sx 1.0 :sy 1.0)
)

    (list (show-frame))



    (loop repeat 13
       for dy from 0 by 1.22
       for y = 80 then (+ y dy)
       collect (place-object-at :qqq 5 125 y :move-p t)
       collect (show-frame))
    )
   :x-twips 256
   :y-twips 256
   :frame-rate 24
   :flash-version 10)
  )
