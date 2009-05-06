(in-package :%3b-swf)

;;; some hacks to dump a shape tag to svg

;; (require 'cl-svg)



(defun shape-to-svg (shape-with-style svg-scene &key (dx 0) (dy 0))
  (let ((fill-styles (fill-styles shape-with-style))
        (line-styles (line-styles shape-with-style))
        (shape-records (shape-records shape-with-style))
        (fill-style-0 0)
        (fill-style-1 0)
        (line-style 0)
        (path (svg:make-path))
        (x (- dx))
        (y (- dy)))
    (flet ((dx (d) (incf x d))
           (dy (d) (incf y d))
           (sx (d) (setf x (- d dx)))
           (sy (d) (setf y (- d dy)))
           (draw-path ()
             (let* ((fill "#ff00ff")
                    (line "none")
                    (line-width "4"))
               (when (not (zerop fill-style-1))
                 (let ((fs1 (elt (fill-styles fill-styles) (1- fill-style-1))))
                   (format t "dropping fill-style-1=~s, rgb=~s type=~s~%"
                           fill-style-1 (if (typep fs1 'fill-style-solid)
                                            (format nil "#~2,'0x~2,'0x~2,'0x"
                                                    (r (color fs1))
                                                    (g (color fs1))
                                                    (b (color fs1)))
                                            :no-color)
                           (type-of fs1))))
               (if (zerop fill-style-1)
                   (setf fill "none")
                   (let ((fs0 (elt (fill-styles fill-styles) (1- fill-style-1))))
                     (if (typep fs0 'fill-style-solid)
                         (setf fill (format nil "#~2,'0x~2,'0x~2,'0x"
                                            (r (color fs0))
                                            (g (color fs0))
                                            (b (color fs0))))
                         (format t "fill type = ~s~%" (type-of fs0)))))
               (when (not (zerop line-style))
                 (let ((ls (elt (line-styles line-styles) (1- line-style))))
                   (setf line (format nil "#~2,'0x~2,'0x~2,'0x"
                                      (r (color ls)) (g (color ls))
                                      (b (color ls)))
                         line-width (width ls))))
               (svg:draw svg-scene (:path :d path)
                         :fill fill :stroke line :stroke-width line-width))))
      (loop for rec in shape-records
            do (etypecase rec
                (straight-edge-shape-record
                 (svg:with-path path
                   (svg:line-to (dx (or (delta-x rec) 0))
                                (dy (or (delta-y rec) 0)))))
                (curved-edge-shape-record
                 (svg:with-path path
                   (svg:quadratic-curve-to (dx (control-delta-x rec))
                                           (dy (control-delta-y rec))
                                           (dx (anchor-delta-x rec))
                                           (dy (anchor-delta-y rec)))))
                (style-change-shape-record
                 ;; if style changes, finish current path
                 (when (or (fill-style-1 rec) (fill-style-0 rec)
                           (line-style rec) (fill-styles rec) (line-styles rec))
                   (draw-path)
                   (setf path (svg:make-path))
                   (unless (move-to rec)
                     (svg:with-path path
                       (svg:move-to x y))))
                 (when (move-to rec)
                   (svg:with-path path
                     (svg:move-to (sx (delta-x (move-to rec)))
                                  (sy (delta-y (move-to rec))))))
                 (when (fill-style-0 rec)
                   (setf fill-style-0 (fill-style-0 rec)))
                 (when (fill-style-1 rec)
                   (setf fill-style-1 (fill-style-1 rec)))
                 (when (line-style rec)
                   (setf line-style (line-style rec)))
                 (when (fill-styles rec)
                   (setf fill-styles (fill-styles rec)))
                 (when (line-styles rec)
                   (setf line-styles (line-styles rec)))))
           finally (draw-path)))))

#+nil
(let* ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 700 :width 700
                                 :viewbox "0 0 700 700")))
  (title scene "Path test")
  (draw scene (:path :d (path
                          (move-to 100 400)
                          (line-to-r 50 -25)
                          (arc-to-r 25 25 -30 0 1 50 -25)
                          (line-to-r 50 -25)
                          (arc-to-r 25 50 -30 0 1 50 -25)
                          (line-to-r 50 -25)
                          (arc-to-r 25 75 -30 0 1 50 -25)
                          (line-to-r 50 -25)
                          (arc-to-r 25 100 -30 0 1 50 -25)
                          (line-to-r 50 -25)
                          (vertical-to-r 50)))
               :fill "none" :stroke "blue" :stroke-width 5)
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))