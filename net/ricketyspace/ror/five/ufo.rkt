#lang racket
(require 2htdp/universe 2htdp/image)

;;; world structure
(struct ufo-pos (x y))

;;; constants
(define WORLD-WIDTH 300)
(define WORLD-HEIGHT 325)
(define MOVE-LEN 3)
(define UFO (bitmap/file "resources/zarking-ufo.png"))
(define UFO-WIDTH (image-width UFO))
(define UFO-HEIGHT (image-height UFO))

;;; ufo movement functions
(define (ufo-move-up current-state)
  (let ((x (ufo-pos-x current-state))
        (y-up (- (ufo-pos-y current-state) MOVE-LEN)))
    (cond [(>= y-up (/ UFO-HEIGHT 2)) (ufo-pos x y-up)]
          [else current-state])))

(define (ufo-move-down current-state)
  (let ((x (ufo-pos-x current-state))
        (y-down (+ (ufo-pos-y current-state) MOVE-LEN)))
    (cond [(<= y-down (- WORLD-HEIGHT (/ UFO-HEIGHT 2)))
           (ufo-pos x y-down)]
          [else current-state])))


(define (ufo-move-left current-state)
  (let ((x-left (- (ufo-pos-x current-state) MOVE-LEN))
        (y (ufo-pos-y current-state)))
    (cond [(>= x-left (/ UFO-WIDTH 2))
           (ufo-pos x-left y)]
          [else current-state])))

(define (ufo-move-right current-state)
  (let ((x-right (+ (ufo-pos-x current-state) MOVE-LEN))
        (y (ufo-pos-y current-state)))
    (cond [(<= x-right (- WORLD-WIDTH (/ UFO-WIDTH 2)))
           (ufo-pos x-right y)]
          [else current-state])))


;;; big bang functions
(define (draw-a-ufo-onto-an-empty-scene current-state)
  (place-image UFO
               (ufo-pos-x current-state)
               (ufo-pos-y current-state)
               (empty-scene WORLD-WIDTH WORLD-HEIGHT)))

(define (add-3-to-posy current-state)
  (ufo-pos (ufo-pos-x current-state)
           (+ (ufo-pos-y current-state) 3)))

(define (posy-is-300 current-state)
  (>= (ufo-pos-y current-state) 300))

(define (move-ufo current-state key)
  (cond [(key=? key "up") (ufo-move-up current-state)]
        [(key=? key "down") (ufo-move-down current-state)]
        [(key=? key "left") (ufo-move-left current-state)]
        [(key=? key "right") (ufo-move-right current-state)]
        [else current-state]))

;;; the big bang
(big-bang (ufo-pos (/ WORLD-WIDTH 2) (/ WORLD-HEIGHT 2))
          (to-draw draw-a-ufo-onto-an-empty-scene)
          (on-key move-ufo))

