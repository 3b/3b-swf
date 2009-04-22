
(in-package :3b-swf)




(with-open-file (s "/tmp/new-write.swf" :direction :output
                 :element-type '(unsigned-byte 8) :if-exists :supersede)
  (write-swf
   s
   (append
    (list
     (background-color #xffff0fff)
     (frame-label "foo")


     (make-instance
      '%3b-swf::define-shape-3-tag
      '%3b-swf::character-id :clip
      '%3b-swf::bounds (rect -1 -1 (/ 6520 20) (/ 3000 20))
      '%3b-swf::shapes
      (make-instance
       '%3b-swf::shape-with-style
       '%3b-swf::line-styles (make-instance
                              '%3b-swf::line-style-array
                              '%3b-swf::line-styles
                              (list (make-instance '%3b-swf::line-style
                                                   '%3b-swf::width 1
                                                   '%3b-swf::color (rgba #xff000000))))
       '%3b-swf::fill-styles (make-instance
                              '%3b-swf::fill-style-array
                              '%3b-swf::fill-styles
                              (list (make-instance '%3b-swf::fill-style-solid
                                                   '%3b-swf::color (rgba #x00ff9526))))
       '%3b-swf::shape-records
       (list
        (make-instance '%3b-swf::style-change-shape-record
                       '%3b-swf::line-style 1
                       ;;'%3b-swf::fill-style-0 1
                       '%3b-swf::fill-style-1 1)
        (make-instance '%3b-swf::style-change-shape-record
                       '%3b-swf::move-to (make-instance
                                          '%3b-swf::state-move-to
                                          '%3b-swf::delta-x (/ 2040 20)
                                          '%3b-swf::delta-y 0))
        ;;<curve cdx='1270' cdy='360' dx='1230' dy='-360' />
        ;;<line dx='1960' dy='2960' />
        ;;<line dx='-6500' dy='20' />
        ;;<line dx='2040' dy='-2980' />
        (make-instance '%3b-swf::curved-edge-shape-record
                       '%3b-swf::control-delta-x (/ 1270 20)
                       '%3b-swf::control-delta-y (/ 360 20)
                       '%3b-swf::anchor-delta-x (/ 1230 20)
                       '%3b-swf::anchor-delta-y (/ -360 20))
        (make-instance '%3b-swf::straight-edge-shape-record
                       '%3b-swf::delta-x (/ 1960 20)
                       '%3b-swf::delta-y (/ 2960 20))
        (make-instance '%3b-swf::straight-edge-shape-record
                       '%3b-swf::delta-x (/ -6500 20)
                       '%3b-swf::delta-y (/ 20 20))
        (make-instance '%3b-swf::straight-edge-shape-record
                       '%3b-swf::delta-x (/ 2040 20)
                       '%3b-swf::delta-y (/ -2980 20))
        (make-instance '%3b-swf::shape-end-record))))




     (make-instance
      '%3b-swf::define-shape-3-tag
      '%3b-swf::character-id :2
      '%3b-swf::bounds (rect -1 -1 (/ 6520 20) (/ 3000 20))
      '%3b-swf::shapes
      (make-instance
       '%3b-swf::shape-with-style
       '%3b-swf::line-styles (make-instance
                              '%3b-swf::line-style-array
                              '%3b-swf::line-styles
                              (list (make-instance '%3b-swf::line-style
                                                   '%3b-swf::width 1
                                                   '%3b-swf::color (rgba #xff000000))))
       '%3b-swf::fill-styles (make-instance
                              '%3b-swf::fill-style-array
                              '%3b-swf::fill-styles
                              (list (make-instance '%3b-swf::fill-style-solid
                                                   '%3b-swf::color (rgba #xff0f9526))))
       '%3b-swf::shape-records
       (list
        (make-instance '%3b-swf::style-change-shape-record
                       '%3b-swf::line-style 1
                       '%3b-swf::fill-style-1 1)
        (make-instance '%3b-swf::style-change-shape-record
                       '%3b-swf::move-to (make-instance
                                          '%3b-swf::state-move-to
                                          '%3b-swf::delta-x (/ 2040 20)
                                          '%3b-swf::delta-y 0))
        (make-instance '%3b-swf::curved-edge-shape-record
                       '%3b-swf::control-delta-x (/ 1270 20)
                       '%3b-swf::control-delta-y (/ 360 20)
                       '%3b-swf::anchor-delta-x (/ 1230 20)
                       '%3b-swf::anchor-delta-y (/ -360 20))
        (make-instance '%3b-swf::straight-edge-shape-record
                       '%3b-swf::delta-x (/ 1960 20)
                       '%3b-swf::delta-y (/ 2960 20))
        (make-instance '%3b-swf::straight-edge-shape-record
                       '%3b-swf::delta-x (/ -6500 20)
                       '%3b-swf::delta-y (/ 20 20))
        (make-instance '%3b-swf::straight-edge-shape-record
                       '%3b-swf::delta-x (/ 2040 20)
                       '%3b-swf::delta-y (/ -2980 20))
        (make-instance '%3b-swf::shape-end-record))))

     (make-instance '%3b-swf::place-object-2-tag
                    '%3b-swf::character-id :clip
                    '%3b-swf::depth 6
                    '%3b-swf::clip-depth 20
                    '%3b-swf::matrix (3b-swf::translate (/ -360 20) (/ 3220 20))
)
     #+nil(make-instance '%3b-swf::place-object-2-tag
                    '%3b-swf::character-id :clip
                    '%3b-swf::depth 6
                    '%3b-swf::move-flag t
                    '%3b-swf::matrix (3b-swf::translate (/ -360 20) (/ 3220 20)))


     (make-instance '%3b-swf::place-object-2-tag
                    '%3b-swf::character-id :2
                    '%3b-swf::depth 8
                    '%3b-swf::matrix (3b-swf::translate 70 180))



     (show-frame)
    ))
   :x-twips 350.0
   :y-twips 350.0
   :frame-rate 20
   :flash-version 10)
)


;;----------------------------------------------------------



(with-open-file (s "/tmp/new-write.swf" :direction :output
                 :element-type '(unsigned-byte 8) :if-exists :supersede)
  (write-swf
   s
   (append
    (list
     (background-color #xffff0fff)
     (frame-label "foo")


     #+nil(make-instance
      '%3b-swf::define-shape-3-tag
      '%3b-swf::character-id :clip
      '%3b-swf::bounds (rect -1 -1 326 150)
      '%3b-swf::shapes
      (make-instance
       '%3b-swf::shape-with-style
       '%3b-swf::line-styles (make-instance
                              '%3b-swf::line-style-array
                              '%3b-swf::line-styles
                              (list (make-instance '%3b-swf::line-style
                                                   '%3b-swf::width 1
                                                   '%3b-swf::color (rgba #xff000000))))
       '%3b-swf::fill-styles (make-instance
                              '%3b-swf::fill-style-array
                              '%3b-swf::fill-styles
                              (list (make-instance '%3b-swf::fill-style-solid
                                                   '%3b-swf::color (rgba #x00ff9526))))
       '%3b-swf::shape-records
       (list
        #+nil(make-instance '%3b-swf::style-change-shape-record
                       '%3b-swf::line-style 1
                       '%3b-swf::fill-style-0 0
                       '%3b-swf::fill-style-1 1
                       '%3b-swf::line-styles (make-instance
                                              '%3b-swf::line-style-array
                                              '%3b-swf::line-styles
                                              (list (make-instance '%3b-swf::line-style
                                                                   '%3b-swf::width 1
                                                                   '%3b-swf::color (rgba #xff000000))))
                       '%3b-swf::fill-styles (make-instance
                                              '%3b-swf::fill-style-array
                                              '%3b-swf::fill-styles
                                              (list (make-instance '%3b-swf::fill-style-solid
                                                                   '%3b-swf::color (rgba #x00ff9526)))))
        (make-instance '%3b-swf::style-change-shape-record
                       '%3b-swf::line-style 1
                       '%3b-swf::fill-style-0 1)
        (make-instance '%3b-swf::style-change-shape-record
                       '%3b-swf::move-to (make-instance
                                          '%3b-swf::state-move-to
                                          '%3b-swf::delta-x 102
                                          '%3b-swf::delta-y 0))
        (make-instance '%3b-swf::curved-edge-shape-record
                       '%3b-swf::control-delta-x 63.5
                       '%3b-swf::control-delta-y 18
                       '%3b-swf::anchor-delta-x 61.5
                       '%3b-swf::anchor-delta-y -18)
        (make-instance '%3b-swf::straight-edge-shape-record
                       '%3b-swf::delta-x 98
                       '%3b-swf::delta-y 148)
        (make-instance '%3b-swf::straight-edge-shape-record
                       '%3b-swf::delta-x -325
                       '%3b-swf::delta-y 1)
        (make-instance '%3b-swf::straight-edge-shape-record
                       '%3b-swf::delta-x 102
                       '%3b-swf::delta-y -149)
        (make-instance '%3b-swf::shape-end-record))))

     (shape :clip
            (list
             `(:color (#x00ff9526) :id :fill1 ))
            (list
             `(:id :line1 :width 1 :color (#xff000000) :join :round :cap :round
                     :open :closed-polyline))

             `((:line-style :line1)
               (:fill-style :fill1)
               (:move-to (102 . 0))
               (:quadratic-to (165.5 . 18) (227 . 0))
               (:line-to (325 . 148))
               (:line-to (0 . 149))
               (:line-to (102 . 0)))
             -1 -1 326 150))



     (vecto::with-swf-canvas (:width 330 :height 160)
       (vecto::set-rgba-fill 0 1 0 1)
       (vecto::set-rgba-stroke 1 0 0 1)
       (vecto::set-line-width 20)
       (vecto::rounded-rectangle 20 00 250 150 20 20)
       (vecto::fill-and-stroke)
       (vecto::add-shape :2)
       (vecto::swf-sprite :2s))
     (list

     (place-object :clip 6 :clip-layers 14 :matrix (translate -20 160))
     (place-object-at :2s 8 00 155)

     (show-frame)
    ))
   :x-twips 350.0
   :y-twips 350.0
   :frame-rate 20
   :flash-version 10)
)