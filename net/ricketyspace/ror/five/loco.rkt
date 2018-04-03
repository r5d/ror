#lang racket
(require 2htdp/universe 2htdp/image)

(define WIDTH 300)
(define HEIGHT 300)
(define CAROILLE (bitmap/file "resources/caroille.png"))
(define CAROILLE-WIDTH (image-width CAROILLE))

;;;
;;; If the car is placed at X = 1/2 its width, its back will be
;;; touching the left edge of the World.
;;;
;;; If the car is place at X = - 1/2 its width, its front will be touching
;;; the left edge of the World.
;;;
(define CAROILLE-WIDTH-HALF (/ CAROILLE-WIDTH 2.0))

;;; Structure to represent the X position of two cars in animation.
(struct cars (one two))

(define (caroille-past-right-edge? pos)
  (> pos (- WIDTH CAROILLE-WIDTH-HALF)))

(define (caroille-fully-past-right-edge? pos)
  (>= pos (+ WIDTH CAROILLE-WIDTH-HALF)))

(define (caroille-fully-past-left-edge? pos)
  (>= pos CAROILLE-WIDTH-HALF))

(define (caroille-fully-inside? pos)
  (and (caroille-fully-past-left-edge? pos)
       (not (caroille-past-right-edge? pos))))

(define (move caroilles)
  (let ((caroille-one (cars-one caroilles))
        (caroille-two (cars-two caroilles)))
    (cond
      ;; Case set I - one of the cars is fully inside.
      ((caroille-fully-inside? caroille-one)
       (cars (+ 1 caroille-one) caroille-two))
      ((caroille-fully-inside? caroille-two)
       (cars caroille-one (+ 1 caroille-two)))
      ;; Case set II - one of the cars disappeared into the right edge.
      ((caroille-fully-past-right-edge? caroille-one)
       (cars (- CAROILLE-WIDTH-HALF) (+ 1 caroille-two)))
      ((caroille-fully-past-right-edge? caroille-two)
       (cars (+ 1 caroille-one) (- CAROILLE-WIDTH-HALF)))
      ;; Case else - Both cars are partially out.
      (else (cars (+ 1 caroille-one) (+ 1 caroille-two))))))

(define (draw-cars caroilles)
  (place-image CAROILLE (cars-one caroilles) (/ HEIGHT 2)
               (place-image CAROILLE (cars-two caroilles) (/ HEIGHT 2)
                            (empty-scene WIDTH HEIGHT))))

(define (start)
  (big-bang (cars CAROILLE-WIDTH-HALF (- CAROILLE-WIDTH-HALF))
            (on-tick move)
            (to-draw draw-cars)))
