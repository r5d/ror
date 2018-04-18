#lang racket
(require 2htdp/universe 2htdp/image)

;;; world structure
(struct ufo (x y fart))

;;; constants
(define WORLD-WIDTH 300)
(define WORLD-HEIGHT 325)
(define MOVE-LEN 3)
(define UFO (bitmap/file "resources/zarking-ufo.png"))
(define UFO-FART (bitmap/file "resources/ufo-fart.png"))
(define UFO-WIDTH (image-width UFO))
(define UFO-HEIGHT (image-height UFO))
(define UFO-FART-HEIGHT (image-height UFO-FART))

;;; ufo movement functions
(define (ufo-move-up current-state)
  (let ((x (ufo-x current-state))
        (y-up (- (ufo-y current-state) MOVE-LEN))
        (fart #t))
    (cond [(>= y-up (+ (/ UFO-HEIGHT 2) (/ UFO-FART-HEIGHT 2)))
           (ufo x y-up fart)]
          [else current-state])))

(define (ufo-move-down current-state)
  (let ((x (ufo-x current-state))
        (y-down (+ (ufo-y current-state) MOVE-LEN)))
    (cond [(<= y-down (- (+ WORLD-HEIGHT (/ UFO-FART-HEIGHT 2))
                         (/ UFO-HEIGHT 2)))
           (ufo x y-down #t)]
          [else current-state])))


(define (ufo-move-left current-state)
  (let ((x-left (- (ufo-x current-state) MOVE-LEN))
        (y (ufo-y current-state))
        (fart #t))
    (cond [(>= x-left (/ UFO-WIDTH 2))
           (ufo x-left y fart)]
          [else current-state])))

(define (ufo-move-right current-state)
  (let ((x-right (+ (ufo-x current-state) MOVE-LEN))
        (y (ufo-y current-state))
        (fart #t))
    (cond [(<= x-right (- WORLD-WIDTH (/ UFO-WIDTH 2)))
           (ufo x-right y fart)]
          [else current-state])))


;;; big bang functions
(define (draw-a-ufo current-state)
  (place-image (overlay/align/offset
                "middle" "bottom" UFO 0 35
                (if (ufo-fart current-state)
                    UFO-FART
                    (circle 0 "outline" "white")))
               (ufo-x current-state)
               (ufo-y current-state)
               (empty-scene WORLD-WIDTH WORLD-HEIGHT)))

(define (add-3-to-posy current-state)
  (ufo (ufo-x current-state)
           (+ (ufo-y current-state) 3)))

(define (posy-is-300 current-state)
  (>= (ufo-y current-state) 300))

(define (move-ufo current-state key)
  (cond [(key=? key "up") (ufo-move-up current-state)]
        [(key=? key "down") (ufo-move-down current-state)]
        [(key=? key "left") (ufo-move-left current-state)]
        [(key=? key "right") (ufo-move-right current-state)]
        [else current-state]))

(define (ufo-stopped current-state key)
  (let ((fart #f))
    (ufo (ufo-x current-state) (ufo-y current-state) fart)))

;;; the big bang
(big-bang (ufo (/ WORLD-WIDTH 2) (/ WORLD-HEIGHT 2) #f)
          (to-draw draw-a-ufo)
          (on-key move-ufo)
          (on-release ufo-stopped))

