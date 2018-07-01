#lang racket
(require 2htdp/universe 2htdp/image)

;; data
(struct pit (snake-1 snake-2 goos obstacles dinged))
(struct snake (dir segs))
(struct goo (loc expire type))
(struct obstacle (loc expire))
(struct posn (x y))

;; constants
(define TICK-RATE 1/10)

(define SIZE 30)

(define SEG-SIZE 15)

(define EXPIRATION-TIME 150)
(define OBSTACLE-EXPIRATION-TIME 250)

(define WIDTH-PX (* SEG-SIZE 30))
(define HEIGHT-PX (* SEG-SIZE 30))

(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define GOO-IMG (bitmap "resources/goo.gif"))
(define GOO-RED-IMG (bitmap "resources/goo-red.gif"))
(define OBSTACLE-IMG (bitmap "resources/obstacle.gif"))
(define SEG-IMG (bitmap "resources/body.gif"))
(define HEAD-IMG (bitmap "resources/head.gif"))

(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))

(define ENDGAME-TEXT-SIZE 15)

;; main
(define (start-snakes)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (snake "d" (list (posn 1 10)))
                 (list (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo))
                 (list (fresh-obstacle)
                       (fresh-obstacle))
                 0)
            (on-tick next-pit TICK-RATE)
            (on-pad direct-snakes)
            (to-draw render-pit)
            (stop-when dead? render-end)))

(define (next-pit w)
  (define snake-1 (pit-snake-1 w))
  (define snake-2 (pit-snake-2 w))
  (define goos (pit-goos w))
  (define obstacles (pit-obstacles w))
  (define dinged (pit-dinged w))
  (define goo-to-eat-sn1 (can-eat snake-1 goos))
  (define goo-to-eat-sn2 (can-eat snake-2 goos))
  (cond [(and goo-to-eat-sn1 goo-to-eat-sn2) ; sn1 and sn2 dinged.
         (pit (grow-size snake-1 (goo-type goo-to-eat-sn1))
              (grow-size snake-2 (goo-type goo-to-eat-sn2))
              (age-goo (eat (eat goos goo-to-eat-sn1) goo-to-eat-sn2))
              (age-obstacle obstacles) (+ 2 dinged))]
        [(and goo-to-eat-sn1 (not goo-to-eat-sn2)) ; sn1 dinged.
         (pit (grow-size snake-1 (goo-type goo-to-eat-sn1))
              (slither  snake-2)
              (age-goo (eat goos goo-to-eat-sn1))
              (age-obstacle obstacles) (+ 1 dinged))]
        [(and (not goo-to-eat-sn1) goo-to-eat-sn2) ; sn2 dinged.
         (pit (slither  snake-1)
              (grow-size snake-2 (goo-type goo-to-eat-sn2))
              (age-goo (eat goos goo-to-eat-sn2))
              (age-obstacle obstacles) (+ 1 dinged))]
        [else ; none dinged.
         (pit (slither snake-1)
              (slither snake-2)
              (age-goo goos)
              (age-obstacle obstacles) dinged)]))


(define (direct-snakes w ke)
  (cond [(arrow-key? ke) (direct-snake-1 w ke)]
        [(wasd-key? ke) (direct-snake-2 w ke)]
        [else w]))

(define (direct-snake-1 w ke)
  (world-change-dir 1 w ke))

(define (direct-snake-2 w ke)
  (world-change-dir 2 w ke))

(define (render-pit w)
  (snake+scene (pit-snake-1 w)
               (snake+scene (pit-snake-2 w)
                            (goo-list+scene (pit-goos w)
                                            (obstacle-list+scene
                                             (pit-obstacles w) MT-SCENE)))))

(define (dead? w)
  (define snake-1 (pit-snake-1 w))
  (define snake-2 (pit-snake-2 w))
  (define (colliding? sn sn-other)
    (or (self-colliding? sn)
        (wall-colliding? sn)
        (obstacle-colliding? sn (pit-obstacles w))
        (snake-colliding? sn sn-other)))
  (or (colliding? snake-1 snake-2) (colliding? snake-2 snake-1)))

(define (render-end w)
  (overlay (above (text "Game Over" ENDGAME-TEXT-SIZE "black")
                  (text (string-append "Dinged "
                                       (number->string (pit-dinged w))
                                       " goos.")
                        ENDGAME-TEXT-SIZE "black"))
           (render-pit w)))


;; clock
(define (can-eat snake goos)
  (cond [(empty? goos) #f]
        [else (if (close? (snake-head snake) (first goos))
                  (first goos)
                  (can-eat snake (rest goos)))]))

(define (eat goos goo-to-eat)
  (append (list (fresh-goo)) (remove goo-to-eat goos)))

(define (close? s g)
  (posn=? s (goo-loc g)))

(define (grow-size sn size)
  (cond [(= size 0) sn]
        [else (grow-size (grow sn) (- size 1))]))

(define (grow sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (snake-segs sn))))

(define (slither sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (all-but-last (snake-segs sn)))))

(define (next-head sn)
  (define head (snake-head sn))
  (define dir (snake-dir sn))
  (cond [(or (string=? dir "up") (string=? dir "w")) (posn-move head 0 -1)]
        [(or (string=? dir "down") (string=? dir "s")) (posn-move head 0 1)]
        [(or (string=? dir "left") (string=? dir "a")) (posn-move head -1 0)]
        [(or (string=? dir "right") (string=? dir "d")) (posn-move head 1 0)]))

(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

(define (all-but-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs) (all-but-last (rest segs)))]))

(define (age-goo goos)
  (rot (renew goos)))

(define (renew goos)
  (cond [(empty? goos) empty]
        [(rotten? (first goos))
         (append (fresh-goos) (renew (rest goos)))]
        [else
         (append (list (first goos)) (renew (rest goos)))]))

(define (rot goos)
  (cond [(empty? goos) empty]
        [else (cons (decay (first goos)) (rot (rest goos)))]))

(define (rotten? g)
  (zero? (goo-expire g)))

(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expire g)) (goo-type g)))

(define (fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       EXPIRATION-TIME
       (random 1 3)))

(define (fresh-goos)
  (define (gen-goos n)
    (cond [(= n 0) empty]
          [else (cons (fresh-goo) (gen-goos (- n 1)))]))
  (let ((n (random 3)))
    (gen-goos n)))

(define (age-obstacle obstacles)
  (rot-obstacles (renew-obstacles obstacles)))

(define (renew-obstacles obstacles)
  (cond [(empty? obstacles)  empty]
        [(obstacle-expired? (first obstacles))
         (cons (fresh-obstacle) (renew-obstacles (rest obstacles)))]
        [else
         (cons (first obstacles) (renew-obstacles (rest obstacles)))]))

(define (rot-obstacles obstacles)
  (cond [(empty? obstacles) empty]
        [else (cons (decay-obstacle (first obstacles))
                    (rot-obstacles (rest obstacles)))]))

(define (obstacle-expired? obs)
  (zero? (obstacle-expire obs)))

(define (decay-obstacle obs)
  (obstacle (obstacle-loc obs) (sub1 (obstacle-expire obs))))

(define (fresh-obstacle)
  (obstacle (posn (add1 (random (sub1 SIZE)))
                  (add1 (random (sub1 SIZE))))
            OBSTACLE-EXPIRATION-TIME))

;; keys
(define (dir? x)
  (or (arrow-key? x)
      (wasd-key? x)))

(define (arrow-key? x)
  (or (key=? x "up")
      (key=? x "down")
      (key=? x "left")
      (key=? x "right")))

(define (wasd-key? x)
  (or (key=? x "w")
      (key=? x "s")
      (key=? x "a")
      (key=? x "d")))

(define (world-change-dir sn-number w d)
  (define snake-1 (pit-snake-1 w))
  (define snake-2 (pit-snake-2 w))
  (cond [(and (= sn-number 1) ;snake-1
              (opposite-dir? (snake-dir snake-1) d)
              (cons? (rest (snake-segs snake-1))))
         (stop-with w)]
        [(and (= sn-number 2) ;snake-2
              (opposite-dir? (snake-dir snake-2) d)
              (cons? (rest (snake-segs snake-2))))
         (stop-with w)]
        [(= sn-number 1) ;snake-1 change dir.
         (pit (snake-change-dir snake-1 d)
              (pit-snake-2 w)
              (pit-goos w)
              (pit-obstacles w)
              (pit-dinged w))]
        [else ;snake-2 change dir.
         (pit (pit-snake-1 w)
              (snake-change-dir snake-2 d)
              (pit-goos w)
              (pit-obstacles w)
              (pit-dinged w))]))

(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "up") (string=? d2 "down")]
        [(string=? d1 "down") (string=? d2 "up")]
        [(string=? d1 "left") (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]
        [(string=? d1 "w") (string=? d2 "s")]
        [(string=? d1 "s") (string=? d2 "w")]
        [(string=? d1 "a") (string=? d2 "d")]
        [(string=? d1 "d") (string=? d2 "a")]))


;; render
(define (snake+scene snake scene)
  (define snake-body-scene
    (img-list+scene (snake-body snake) SEG-IMG scene))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake)
             (cond [(or (string=? "up" dir) (string=? "w" dir))
                    HEAD-UP-IMG]
                   [(or (string=? "down" dir) (string=? "s" dir))
                    HEAD-DOWN-IMG]
                   [(or (string=? "left" dir) (string=? "a" dir))
                    HEAD-LEFT-IMG]
                   [(or (string=? "right" dir) (string=? "d" dir))
                    HEAD-RIGHT-IMG])
             snake-body-scene))

(define (goo-list+scene goos scene)
  (define (get-posns-from-goo goos type)
    (cond [(empty? goos) empty]
          [(= (goo-type (first goos)) type)
           (cons (goo-loc (first goos))
                 (get-posns-from-goo (rest goos) type))]
          [else (get-posns-from-goo (rest goos) type)]))
  (img-list+scene (get-posns-from-goo goos 1) GOO-IMG
                  (img-list+scene (get-posns-from-goo goos 2)
                                  GOO-RED-IMG scene)))

(define (obstacle-list+scene obstacles scene)
  (define (get-posns-from-obstacle obstacles)
    (cond [(empty? obstacles) empty]
          [else (cons (obstacle-loc (first obstacles))
                      (get-posns-from-obstacle (rest obstacles)))]))
  (img-list+scene (get-posns-from-obstacle obstacles)
                  OBSTACLE-IMG scene))

(define (img-list+scene posns img scene)
  (cond [(empty? posns) scene]
        [else (img+scene
               (first posns)
               img
               (img-list+scene (rest posns) img scene))]))

(define (img+scene posn img scene)
  (place-image img
               (* (posn-x posn) SEG-SIZE)
               (* (posn-y posn) SEG-SIZE)
               scene))


;; end game
(define (self-colliding? snake)
  (cons? (member (snake-head snake) (snake-body snake))))

(define (wall-colliding? snake)
  (define x (posn-x (snake-head snake)))
  (define y (posn-y (snake-head snake)))
  (or (= 0 x) (= x SIZE)
      (= 0 y) (= y SIZE)))

(define (obstacle-colliding? snake obstacles)
  (cond [(empty? obstacles) #f]
        [(posn=? (snake-head snake)
                 (obstacle-loc (first obstacles))) #t]
        [else (obstacle-colliding? snake (rest obstacles))]))

(define (snake-colliding? snake snake-other)
  (cons? (member (snake-head snake) (snake-segs snake-other))))

;; aux
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(define (snake-head sn)
  (first (snake-segs sn)))

(define (snake-body sn)
  (rest (snake-segs sn)))

(define (snake-tail sn)
  (last (snake-segs sn)))

(define (snake-change-dir sn d)
  (snake d (snake-segs sn)))
