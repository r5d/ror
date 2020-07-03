#lang racket

(require 2htdp/image 2htdp/universe "shared.rkt")

(provide launch-guess-client)

(struct client-state (type clue guess action done))

(define ClientState0  (client-state -1 "" #f "" #f))

(define SCENE-WIDTH 300)
(define SCENE-HEIGHT 200)

(define (launch-guess-client n host)
  (big-bang ClientState0
            (on-draw draw-guess)
            (on-key handle-keys)
            (name n)
            (register host)
            (on-receive handle-msg)))

(define (handle-keys w key)
  (cond [(= (client-state-type w) PLAYER) (handle-keys-player w key)]
        [(= (client-state-type w) GUESSER) (handle-keys-guesser w key)]
        [else w]))

(define (handle-keys-player w key)
  (define (action)
    (client-state-action w))
  (define (guess)
    (client-state-guess w))
  (define (set-clue clue)
    (client-state PLAYER clue (guess) (action) #f))
  (cond [(and (string=? (action) "c") (key=? key "c"))
         (make-package w (server-msg PLAYER "c" ""))]
        [(and (string=? (action) "a") (key=? key "up"))
         (make-package (set-clue "up") (server-msg PLAYER "a" "up"))]
        [(and (string=? (action) "a") (key=? key "down"))
         (make-package (set-clue "down") (server-msg PLAYER "a" "down"))]
        [(and (string=? (action) "a") (key=? key "="))
         (make-package (set-clue "=") (server-msg PLAYER "a" "="))]
        [else w]))

(define (handle-keys-guesser w key)
  (define (action)
    (client-state-action w))
  (cond [(and (string=? (action) "c") (key=? key "c")
              (make-package w (server-msg GUESSER "c" "")))]
        [(and (string=? (action) "g") (key=? key "g")
              (make-package w (server-msg GUESSER "g" "")))]
        [else w]))

(define (handle-msg c c-msg)
  (cond [(not (client-msg? c-msg)) c]
        [(= (client-msg-type c-msg) PLAYER)
         (handle-msg-player c c-msg)]
        [(= (client-msg-type c-msg) GUESSER)
         (handle-msg-guesser c c-msg)]
        [else c]))

(define (handle-msg-player c c-msg)
  (define (is-done)
    (client-msg-done c-msg))
  (define (action)
    (client-msg-action c-msg))
  (define (set-done)
    (let ([guess (client-msg-guess c-msg)])
      (client-state PLAYER "" guess "" #t)))
  (define (set-check)
    (let ([clue (client-state-clue c)])
      (client-state PLAYER clue #f "c" #f)))
  (define (set-act)
    (let ([guess (client-msg-guess c-msg)])
      (client-state PLAYER "" guess "a" #f)))
  (cond [(is-done) (set-done)]
        [(string=? (action) "c") (set-check)]
        [(string=? (action) "a") (set-act)]
        [else c]))

(define (handle-msg-guesser c c-msg)
  (define (is-done)
    (client-msg-done c-msg))
  (define (action)
    (client-msg-action c-msg))
  (define (set-done)
    (let ([guess (client-msg-guess c-msg)])
      (client-state GUESSER "" guess "" #t)))
  (define (set-check)
    (let ([clue (client-msg-clue c-msg)]
          [guess (client-msg-guess c-msg)])
      (client-state GUESSER clue guess "c" #f)))
  (define (set-guess)
    (let ([clue (client-msg-clue c-msg)]
          [guess (client-msg-guess c-msg)])
      (client-state GUESSER clue guess "g" #f)))
  (cond [(is-done) (set-done)]
        [(string=? (action) "c") (set-check)]
        [(string=? (action) "g") (set-guess)]
        [else c]))

(define (draw-guess c)
  (define (render type result desc help)
    (place-image/align
     type 5 5  "left" "top"
     (overlay (above result desc help)
              (empty-scene SCENE-WIDTH SCENE-HEIGHT))))
  (let ([type (draw-type c)]
        [result (draw-result c)]
        [desc (draw-desc c)]
        [help (draw-help c)])
    (render type result desc help)))

(define (draw-type c)
  (text (cond [(= (client-state-type c) PLAYER) "Player"]
              [(= (client-state-type c) GUESSER) "Guesser"]
              [else "..."])
        14 "black"))

(define (draw-result c)
  (text  (cond [(= (client-state-type c) PLAYER)
                (draw-result-player c)]
               [else (draw-result-guesser c)])
         14 "black"))

(define (draw-result-player c)
  (define (done)
    (client-state-done c))
  (define (action)
    (client-state-action c))
  (define (guess)
    (number->string (client-state-guess c)))
  (cond [(and (not (done)) (string=? (action) "")) "..."]
        [(done) (string-append (guess) " it is!")]
        [(string=? (action) "a") (string-append "Guess: " (guess))]
        [else ""]))

(define (draw-result-guesser c)
  (define (done)
    (client-state-done c))
  (define (action)
    (client-state-action c))
  (define (guess)
    (let ([g (client-state-guess c)])
      (cond [(number? g) (number->string g)]
            [else ""])))
  (define (clue)
    (cond [(string=? (client-state-clue c) "up") ">"]
          [else "<"]))
  (cond [(and (not (done)) (string=? (action) "") "...")]
        [(done) (string-append (guess) " it is!")]
        [(and (string=? (action) "g") (> (string-length (guess)) 0))
         (string-append "Number " (clue) " " (guess))]
        [(string=? (action) "c") (string-append "Guess: " (guess))]
        [else ""]))

(define (draw-desc c)
  (text (cond [(= (client-state-type c) PLAYER)  ""]
              [else (draw-desc-guesser c)])
        10 "black"))

(define (draw-desc-guesser c)
  (define (action)
    (client-state-action c))
  (cond [(string=? (action) "c") "Waiting for player to act on guess"]
        [else ""]))

(define (draw-help c)
  (define (type)
    (client-state-type c))
  (text (cond [(= (type) PLAYER) (draw-help-player c)]
              [else (draw-help-guesser c)])
        10 "black"))

(define (draw-help-player c)
  (define (action)
    (client-state-action c))
  (cond [(string=? (action) "c") "Press 'c' to check"]
        [(string=? (action) "a") "Press ↑, ↓, or = "]
        [else ""]))

(define (draw-help-guesser c)
  (define (action)
    (client-state-action c))
  (define (done)
    (client-state-done c))
  (cond [(string=? (action) "g") "Press 'g' to guess"]
        [(string=? (action) "c") "Press 'c' to check"]
        [(done) "Good Job!"]
        [else ""]))
