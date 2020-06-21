#lang racket

(provide launch-guess-server)

(require 2htdp/image 2htdp/universe "shared.rkt")

(struct interval (small big) #:transparent)

;; paction -> 'c' or 'a'
;; gaction -> 'c' or 'g'
(struct server-state  (interval clue guess paction gaction clients done))

(define u0 (server-state (interval LOWER UPPER) "" #f "c" "" 1 #f))

(define (launch-guess-server)
  (universe #f
            (state #t)
            (on-new connect)
            (on-msg handle-msg)))

(define (connect u client)
  (cond [(false? u)
         (make-bundle
          u0
          (list (make-mail client (client-msg PLAYER "" #f "c" #f)))
          '())]
        [(= (server-state-clients u) 1)
         (make-bundle
          (server-state
           (server-state-interval u) (server-state-clue u)
           (server-state-guess u) (server-state-paction u)
           "g" 2 #f)
          (list (make-mail client (client-msg GUESSER "" #f "g" #f)))
          '())]
        [else (make-bundle u empty (list client))]))

(define (handle-msg u client s-msg)
  (cond [(not (server-msg? s-msg)) (make-bundle u empty (list client))]
        [(= (server-msg-type s-msg) PLAYER)
         (handle-msg-player u client s-msg)]
        [(= (server-msg-type s-msg) GUESSER)
         (handle-msg-guesser u client s-msg)]
        [else (make-bundle u empty (list client))]))

(define (handle-msg-player u client s-msg)
  (define (set-paction paction)
    (let ([interval (server-state-interval u)]
          [clue (server-state-clue u)]
          [guess (server-state-guess u)]
          [gaction (server-state-gaction u)]
          [clients (server-state-clients u)])
      (server-state interval clue guess paction gaction clients #f)))
  (define (set-clue clue)
    (let ([interval (server-state-interval u)]
          [guess (server-state-guess u)]
          [gaction (server-state-gaction u)]
          [clients (server-state-clients u)]
          [done (server-state-done u)])
      (server-state interval clue guess "c" gaction clients done)))
  (define (set-done)
    (let ([interval (server-state-interval u)]
          [guess (server-state-guess u)]
          [gaction (server-state-gaction u)]
          [clients (server-state-clients u)])
      (server-state interval "" guess "" gaction clients #t)))
  (define (mail clue guess action done)
    (list (make-mail client (client-msg PLAYER clue guess action done))))
  (let* ([clue (server-state-clue u)]
         [guess (server-state-guess u)]
         [action (server-msg-action s-msg)]
         [done (server-state-done u)]
         [action-ok (string=? (server-state-paction u) action)]
         [has-guess (number? guess)]
         [data (server-msg-data s-msg)])
    (cond [(not action-ok)
           (make-bundle u empty (list client))]
          [(and (string=? action "c") (not has-guess))
           (make-bundle u (mail clue guess action done) empty)]
          [(and (string=? action "c") has-guess)
           (make-bundle (set-paction "a") (mail clue guess "a" done) empty)]
          [(and (string=? action "a") (member data '("up" "down")))
           (make-bundle (set-clue data) (mail data #f "c" done) empty)]
          [(and (string=? action "a") (string=? data "="))
           (make-bundle (set-done) (mail "" guess "" #t) empty)]
          [else (make-bundle u empty (list client))])))

(define (handle-msg-guesser u client s-msg)
  (define (set-guess interval clue guess)
    (let ([paction (server-state-paction u)]
          [clients (server-state-clients u)]
          [done (server-state-done u)])
      (server-state interval clue guess paction "c" clients done)))
  (define (set-gaction-guess)
    (let ([interval (server-state-interval u)]
          [clue (server-state-clue u)]
          [guess (server-state-guess u)]
          [paction (server-state-paction u)]
          [clients (server-state-clients u)]
          [done (server-state-done u)])
      (server-state interval clue guess paction "g" clients done)))
  (define (has-clue)
    (> (string-length (server-state-clue u)) 0))
  (define (is-done)
    (server-state-done u))
  (define (mail clue guess action done)
    (list (make-mail client
                     (client-msg GUESSER clue guess action done))))
  (let* ([action (server-msg-action s-msg)]
         [interval (server-state-interval u)]
         [clue (server-state-clue u)]
         [current-guess (server-state-guess u)]
         [done (server-state-done u)]
         [action-ok (string=? (server-state-gaction u) action)])
    (cond [(not action-ok) (make-bundle u empty (list client))]
          [(is-done) (make-bundle u (mail "" current-guess "" #t) empty)]
          [(and (string=? action "g") (not (has-clue)))
           (let ([guess (guess interval)])
             (make-bundle (set-guess interval "" guess)
                          (mail "" guess "c" done) empty))]
          [(and (string=? action "g") (has-clue))
           (let* ([n-interval (next-interval interval clue)]
                  [guess (guess n-interval)])
             (make-bundle (set-guess n-interval "" guess)
                          (mail "" guess "c" done) empty))]
          [(and (string=? action "c") (has-clue))
           (make-bundle (set-gaction-guess)
                        (mail clue current-guess "g" done) empty)]
          [else (make-bundle u (mail clue current-guess action done)
                             empty)])))

(define (next-interval interval clue)
  (cond [(not (string? clue))   interval]
        [(string=? "up" clue)   (bigger interval)]
        [(string=? "down" clue) (smaller interval)]
        [else interval]))

(define (single? w)
  (= (interval-small w) (interval-big w)))

(define (guess w)
  (quotient (+ (interval-small w) (interval-big w)) 2))

(define (smaller w)
  (interval (interval-small w) (max (interval-small w) (sub1 (guess w)))))

(define (bigger w)
  (interval (min (interval-big w) (add1 (guess w))) (interval-big w)))
