
(in-package #:vecto)



(defun test (character-id)
  (with-swf-canvas (:width 100 :height 100)
    (set-line-width 5.0)
    ;; red stroke
    (set-rgb-stroke 1 0 0)
    (move-to 10 10)
    (line-to 90 90)
    (swf-stroke)
    ;; green stroke
    (set-rgb-stroke 0 1 0)
    (move-to 10 90)
    (line-to 90 10)
    (swf-stroke)
    ;; blue+alpha transform stroke
    (set-rgba-stroke 0 0 1 0.5)
    (flet ((elbow (radians)
	     (with-graphics-state
	       (translate 50 50)
	       (rotate radians)
	       (scale 0.25 0.25)
	       (move-to 0 0)
	       (curve-to 0 100
			 0 100
			 100 100)
	       (set-line-width 5.0)
               ;;(break *graphics-state*)
	       (swf-stroke))))
      (let* ((rotations 25)
	     (step (/ (* pi 2) rotations)))
	(dotimes (i rotations)
	  (elbow (* i step)))))
    (add-shape character-id)))
#+nil
(test 2)


(defun test-rotate (output-file)
  (with-swf-canvas (:width 100 :height 100)
    (translate 50 50)
    (move-to 0 0)
    (line-to 0 10)
    (rotate (- (/ pi 4)))
    (set-line-width 15)
    (stroke)
    (save-png output-file)))

(defun test-skew (output-file) ;; ??
  (with-swf-canvas (:width 100 :height 100)
    (move-to 0 0)
    (line-to 0 75)
    (skew (- (/ pi 4)) (- (/ pi 4)))
    (set-line-width 15)
    (stroke)
    (save-png output-file)))

(defun hole-test (file)
  (with-swf-canvas (:width 100 :height 100)
    (translate 10 10)
    (scale 50 50)
    (set-line-width 0.1)
    (move-to 0 0)
    (line-to 0 1)
    (line-to 1 1)
    (line-to 1 0)
    (line-to 0 0)
    (move-to 0.1 0.8)
    (line-to 0.1 0.1)
    (line-to 0.8 0.1)
    (line-to 0.8 0.8)
    (line-to 0.1 0.8)
    (fill-path)
    (save-png file)))

(defun rectangle-test (id)
  (with-swf-canvas (:width 100 :height 100)
    (rectangle 10 10 50 50)
    (stroke)
    (rectangle 15 15 55 55)
    (stroke)
    (add-shape id)))
#+nil
(rectangle-test 4)


(defun rectangle-fill-test (file)
  (with-swf-canvas (:width 50 :height 50)
    (set-rgba-fill 1 0 0 0.5)2
    (rectangle 0 0 50 50)
    (fill-path)
    (save-png file)))

(defun circle-test1 (id)
  (with-swf-canvas (:width 100 :height 100)
    (rectangle 30 30 40 40)
    (stroke)
    (centered-circle-path 50 50 20)
    (stroke)
    (centered-circle-path 50 50 20)
    (stroke)
    (centered-circle-path 50 50 20)
    (stroke)
    (add-shape id)))
(defun circle-test (string file)
  (with-swf-canvas (:width 250 :height 180)
    (set-rgb-fill 1 1 1)
    (set-line-width 1)
    (translate 10 10)
    (centered-circle-path 0 0 5)
    (fill-and-stroke)
    (translate 15 15)
    (centered-circle-path 0 0 8)
    (fill-and-stroke)
    (translate 20 24)
    (centered-circle-path 0 0 11)
    (fill-and-stroke)
    (centered-ellipse-path 75 60 100 40)
    (fill-and-stroke)
    (let ((font (get-font "c:/windows/fonts/vera.ttf")))
      (set-font font 50)
      (translate 10 45)
      (let ((bbox (string-bounding-box string (size (font *graphics-state*))
                                       (loader (font *graphics-state*)))))
        (set-line-width 1)
        (set-rgba-fill 1 0 0 0.5)
        (rectangle (xmin bbox) (ymin bbox)
                   (- (xmax bbox) (xmin bbox))
                   (- (ymax bbox) (ymin bbox)))
        (fill-path))
      (set-rgb-fill 0 1 0)
      (draw-string 0 0 string))
    (save-png file)))

(defun center-test (string file)
  (with-swf-canvas (:width 200 :height 100)
    (let ((font (get-font #p"c:/windows/fonts/times.ttf")))
      (set-font font 36)
      (draw-centered-string 100 25 string)
      (set-rgba-fill 1 0 0 0.5)
      (set-rgb-stroke 0 0 0)
      (centered-circle-path 100 25 5)
      (stroke)
      (save-png file))))

(defun twittertext (string size font file)
  (zpb-ttf:with-font-loader (loader font)
    (let ((bbox (string-bounding-box string size loader)))
      (with-swf-canvas (:width (- (ceiling (xmax bbox)) (floor (xmin bbox)))
                           :height (- (ceiling (ymax bbox)) (floor (ymin bbox))))
        (set-font loader size)
        (set-rgba-fill 1 0 0 0.1)
        (clear-canvas)
        (set-rgb-fill 0 0 0)
        (translate (- (xmin bbox)) (- (ymin bbox)))
        (draw-string 0 0 string)
        (save-png file)))))

(defun arc-to (center-x center-y radius start extent)
  ;; An arc of extent zero will generate an error at bezarc (divide by zero).
  ;; This case may be given by two aligned points in a polyline.
  ;; Better do nothing.
  (unless (zerop extent)
    (if (<= (abs extent) (/ pi 2.0))
        (multiple-value-bind (x1 y1 x2 y2 x3 y3)
            (bezarc center-x center-y radius start extent)
          (curve-to x1 y1 x2 y2 x3 y3))
        (let ((half-extent (/ extent 2.0)))
          (arc-to center-x center-y radius start half-extent)
          (arc-to center-x center-y radius (+ start half-extent) half-extent)))))

(defun bezarc (center-x center-y radius start extent)
  ;; start and extent should be in radians.
  ;; Returns first-control-point-x first-control-point-y
  ;;         second-control-point-x second-control-point-y
  ;;         end-point-x end-point-y
  (let* ((end (+ start extent))
         (s-start (sin start)) (c-start (cos start))
         (s-end (sin end)) (c-end (cos end))
         (ang/2 (/ extent 2.0))
         (kappa (* (/ 4.0 3.0)
                   (/ (- 1 (cos ang/2))
                      (sin ang/2))))
	 (x1 (- c-start (* kappa s-start)))
	 (y1 (+ s-start (* kappa c-start)))
	 (x2 (+ c-end   (* kappa s-end)))
	 (y2 (- s-end   (* kappa c-end))))
    (values (+ (* x1 radius) center-x)(+ (* y1 radius) center-y)
	    (+ (* x2 radius) center-x)(+ (* y2 radius) center-y)
	    (+ (* c-end radius) center-x)(+ (* s-end radius) center-y))))

(defun degrees (degrees)
  (* (/ pi 180) degrees))

(defun arc-test (file)
  (with-swf-canvas (:width 100 :height 100)
    (rotate-degrees 15)
    (translate 0 10)
    (set-line-width 10)
    (move-to 75 0)
    (arc-to 0 0 75 0 (degrees 15))
    (stroke)
    (save-png file)))


(defun rect-test (file)
  (with-swf-canvas (:width 5 :height 5)
    (set-rgba-fill 1 0 0 0.5)
    (rectangle 0 0 5 5)
    (fill-path)
    (save-png file)))

(defun text-test (&key string size font file)
  (with-swf-canvas (:width 200 :height 200)
    (let ((loader (get-font font)))
      (set-rgb-fill 0.8 0.8 0.9)
      (clear-canvas)
      (set-font loader size)
      (set-rgb-fill 0.0 0.0 0.3)
      (scale 0.5 0.5)
      (rotate (* 15 (/ pi 180)))
      (draw-string 10 10 string)
      (save-png file))))


(defun sign-test (string font file &key
                  (font-size 72)
                  (outer-border 2)
                  (stripe-width 5)
                  (inner-border 2)
                  (corner-radius 10))
  (zpb-ttf:with-font-loader (loader font)
    (let* ((bbox (string-bounding-box string font-size loader))
           (text-height (ceiling (- (ymax bbox) (ymin bbox))))
           (text-width (ceiling (- (xmax bbox) (xmin bbox))))
           (stripe/2 (/ stripe-width 2.0))
           (b1 (+ outer-border stripe/2))
           (b2 (+ inner-border stripe/2))
           (x0 0)
           (x1 (+ x0 b1))
           (x2 (+ x1 b2))
           (y0 0)
           (y1 (+ y0 b1))
           (y2 (+ y1 b2))
           (width (truncate (+ text-width (* 2 (+ b1 b2)))))
           (width1 (- width (* b1 2)))
           (height (truncate (+ text-height (* 2 (+ b1 b2)))))
           (height1 (- height (* b1 2))))
      (with-swf-canvas (:width width :height height)
        (set-rgb-fill 0.0 0.43 0.33)
        (set-rgb-stroke 0.95 0.95 0.95)
        ;; Stripe shadow + stripe
        (set-line-width stripe-width)
        (with-graphics-state
          (translate 2 -2)
          (set-rgba-stroke 0.0 0.0 0.0 0.3)
          (rounded-rectangle x1 y1
                             width1 height1
                             corner-radius corner-radius)
          (fill-and-stroke))
        (with-graphics-state
          (rounded-rectangle x1 y1
                             width1 height1
                             corner-radius corner-radius)
          (set-dash-pattern #(10 20) 0)
          (stroke))
        ;; Text shadow & text
        (set-font loader font-size)
        (translate (- (xmin bbox)) (- (ymin bbox)))
        (with-graphics-state
          (translate 1 -1)
          (set-rgba-fill 0.0 0.0 0.0 1.0)
          (draw-string x2 y2 string))
        (set-rgb-fill 0.95 0.95 0.95)
        (draw-string x2 y2 string)
        (save-png file)))))


           

      





(defun fill-test (file)
  (with-swf-canvas (:width 100 :height 100)
    (set-rgb-stroke 1 0 0)
    (set-rgb-fill 0 1 0)
    (move-to 0 0)
    (line-to 50 50)
    (line-to 100 10)
    (fill-and-stroke)
    (save-png file)))

(defun circle-test (file)
  (with-swf-canvas (:width 100 :height 100)
    ;;(scale 5 10)
    (scale 1 2)
    (set-line-width 3)
    (centered-circle-path 50 50 45)
    (set-rgb-fill 1 1 0)
    (fill-and-stroke)
    (save-png file)))



(defun test-gradient (file fun)
  (with-swf-canvas (:width 500 :height 500)
    (with-graphics-state
      (set-gradient 100 100 1 0 0 1
                    200 235 0 1 0 1
                    :domain-function fun)
      (rectangle 0 0 500 500)
      (fill-path))
    (with-graphics-state
      (set-rgba-stroke 1 1 1 0.5)
      (set-dash-pattern #(10 10) 0)
      (move-to 100 100)
      (line-to 200 235)
      (stroke))
    (set-rgb-stroke 1 1 1)
    (centered-circle-path 100 100 10)
    (stroke)
    (set-rgb-stroke 0 0 0)
    (centered-circle-path 200 235 10)
    (stroke)
    (set-rgb-fill 1 1 1)
    (let* ((font (get-font #p"c:/windows/fonts/cour.ttf"))
           (name (string-downcase fun))
           (bbox (geometry:bbox-box (string-bounding-box name 24 font))))
      (translate 200 300)
      (set-font font 24)
      (setf bbox (geometry:expand bbox 10))
      (rectangle (geometry:xmin bbox) (geometry:ymin bbox)
                 (geometry:width bbox) (geometry:height bbox))
      (fill-and-stroke)
      (set-rgb-fill 0 0 0)
      (draw-string 0 0 (string-downcase fun))
      (fill-path)
      (save-png file))))



(defun dash-test (character-id)
  (with-swf-canvas (:width 200 :height 200)
    (rectangle 10 10 125 125)
    (set-rgba-fill 0.3 0.5 0.9 0.5)
    (set-line-width 4)
    (set-dash-pattern #(10 10) 5)
    (fill-and-stroke)
    (add-shape character-id)))
#+nil
(dash-test 3)

(defun curve-test (id)
  (with-swf-canvas (:width 100 :height 100)
    ;;(rotate-degrees 15)
    (set-rgb-stroke 1 0 0)
    (translate 0 00)
    (set-line-width 3)
    (flet ((a ()
             (move-to 0 0)
             (curve-to 0 100
                       100 0
                       100 100)
             (swf-stroke)))
      (a)
      (set-rgb-stroke 0 1 0)
      (set-line-width 2)
      (set-dash-pattern #(2 2) 1)
      (a)
      (set-dash-pattern nil nil)
      (set-rgb-stroke 0 0 1)
      (set-line-width 1)
      (a))
    (add-shape id)))

(defun curve-test2 (id)
  (with-swf-canvas (:width 100 :height 100)
    ;;(rotate-degrees 15)
    (set-rgb-stroke 1 0 0)
    (translate 0 00)
    (set-line-width 1)
    (flet ((a ()
             (move-to 0 0)
             (curve-to 0 100
                       100 0
                       100 100)
             (swf-stroke)))
      (a))
    (add-shape id)))


(defun center-test (string id)
  (with-swf-canvas (:width 200 :height 100)
    (let ((font (get-font #p"c:/windows/fonts/times.ttf")))
      (set-font font 36)
      (draw-centered-string 100 25 string)
      (set-rgba-fill 1 0 0 0.5)
      (set-rgb-stroke 0 0 0)
      (centered-circle-path 100 25 5)
      (swf-stroke)
      (add-shape id))))

(center-test "foo" 123)



(defun radiant-lambda (file)
  (with-swf-canvas (:width 90 :height 90)
    (let ((font (get-font "c:/windows/fonts/times.ttf"))
          (step (/ pi 7)))
      (set-font font 40)
      (translate 45 45)
      (draw-centered-string 0 -10 #(#x3BB))
      (set-rgb-stroke 1 0 0)
      (centered-circle-path 0 0 35)
      (stroke)
      (set-rgba-stroke 0 0 1.0 0.5)
      (set-line-width 4)
      (dotimes (i 14)
        (with-graphics-state
          (rotate (* i step))
          (move-to 30 0)
          (line-to 40 0)
          (stroke)))
      (save-png file))))


(defun feedlike-icon (file)
  (with-swf-canvas (:width 100 :height 100)
    (set-rgb-fill 1.0 0.65 0.3)
    (rounded-rectangle 0 0 100 100 10 10)
    (fill-path)
    (set-rgb-fill 1.0 1.0 1.0)
    (centered-circle-path 20 20 10)
    (fill-path)
    (flet ((quarter-circle (x y radius)
             (move-to (+ x radius) y)
             (arc x y radius 0 (/ pi 2))))
      (set-rgb-stroke 1.0 1.0 1.0)
      (set-line-width 15)
      (quarter-circle 20 20 30)
      (stroke)
      (quarter-circle 20 20 60)
      (stroke))
    (rounded-rectangle 5 5 90 90 7 7)
    (set-gradient-fill 50 90
                       1.0 1.0 1.0 0.7
                       50 20
                       1.0 1.0 1.0 0.0)

    #+nil(set-gradient-fill 100 100
                       1.0 0.0 1.0 1.0
                       0 0
                       0.0 0.0 1.0 1.0)

    (set-line-width 2)
    (set-rgba-stroke 1.0 1.0 1.0 0.1)
    (fill-and-stroke)
    (save-png file)))

(defun star-clipping (file)
  (with-swf-canvas (:width 200 :height 200)
    (let ((size 100)
          (angle 0)
          (step (* 2 (/ (* pi 2) 5))))
      (translate size size)
      (move-to 0 size)
      (dotimes (i 5)
        (setf angle (+ angle step))
        (line-to (* (sin angle) size)
                 (* (cos angle) size)))
      (even-odd-clip-path)
      (end-path-no-op)
      (flet ((circle (distance)
               (set-rgba-fill distance 0 0
                              (- 1.0 distance))
               (centered-circle-path 0 0 (* size distance))
               (fill-path)))
        (loop for i downfrom 1.0 by 0.05
              repeat 20 do
              (circle i)))
      (save-png file))))

(defun line-styletest (file)
  (with-swf-canvas (:width 200 :height 100)
    (set-line-width 10)
    (set-line-cap :butt)
    (move-to 10 10)
    (line-to 10 90)
    (stroke)
    (set-line-cap :square)
    (move-to 30 10)
    (line-to 30 90)
    (stroke)
    (set-line-cap :round)
    (move-to 50 10)
    (line-to 50 90)
    (stroke)
    (set-line-cap :butt)
    (set-line-join :miter)
    (move-to 70 10)
    (line-to 85 90)
    (line-to 100 10)
    (stroke)
    (set-line-join :bevel)
    (move-to 115 10)
    (line-to 130 90)
    (line-to 145 10)
    (stroke)
    (set-line-join :round)
    (move-to 160 10)
    (line-to 175 90)
    (line-to 190 10)
    (stroke)
    (save-png file)
))
