#lang racket
(require 2htdp/universe 2htdp/image)

(struct interval (small big guesses))

;;; constants
(define TEXT-SIZE 12)
(define HELP-TEXT
  (text "↑ for larger numbers, ↓ for smaller ones"
        TEXT-SIZE
        "blue"))
(define HELP-TEXT2
  (text "Press = when your number is guessed; q to quit."
        TEXT-SIZE
        "blue"))
(define COLOR "red")
(define WIDTH (+ (image-width HELP-TEXT2) 10))
(define HEIGHT 150)
(define SIZE 72)
(define TEXT-X 3)
(define TEXT-UPPER-Y 10)
(define TEXT-LOWER-Y 135)
(define GUESSES-SIZE 12)
(define GUESSES-COLOR "green")
(define MT-SC
  (place-image/align
   HELP-TEXT TEXT-X TEXT-UPPER-Y "left" "top"
   (place-image/align
    HELP-TEXT2 TEXT-X TEXT-LOWER-Y "left" "bottom"
    (empty-scene WIDTH HEIGHT))))

;; main
(define (start lower upper)
  (big-bang (interval lower upper 0)
            (on-key deal-with-guess)
            (to-draw render)
            (stop-when single? render-last-scene)))

;; key events
(define (deal-with-guess w key)
  (cond [(key=? key "up") (bigger w)]
        [(key=? key "down") (smaller w)]
        [(key=? key "q") (stop-with w)]
        [(key=? key "=") (stop-with w)]
        [else w]))

(define (smaller w)
  (interval (interval-small w)
            (max (interval-small w) (sub1 (guess w)))
            (+ 1 (interval-guesses w))))

(define (bigger w)
  (interval (min (interval-big w) (add1 (guess w)))
            (interval-big w)
            (+ 1 (interval-guesses w))))

(define (guess w)
  (quotient (+ (interval-small w) (interval-big w)) 2))

(define (render w)
  (overlay (overlay/offset
            (text (number->string (guess w)) SIZE COLOR) 0 40
            (text (number->string (interval-guesses w))
                  GUESSES-SIZE GUESSES-COLOR))
           MT-SC))

(define (render-last-scene w)
  (overlay (overlay/offset
            (text "End" SIZE COLOR) 0 40
            (text (number->string (interval-guesses w))
                  GUESSES-SIZE GUESSES-COLOR))
           MT-SC))

(define (single? w)
  (= (interval-small w) (interval-big w)))
