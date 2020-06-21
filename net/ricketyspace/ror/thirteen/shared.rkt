#lang racket

(provide
 UPPER
 LOWER
 PLAYER
 GUESSER
 client-msg
 client-msg?
 client-msg-type
 client-msg-clue
 client-msg-guess
 client-msg-action
 client-msg-done
 server-msg
 server-msg?
 server-msg-type
 server-msg-action
 server-msg-data)

(define UPPER 100)
(define LOWER 0)

(define PLAYER 0)
(define GUESSER 1)

(struct client-msg (type clue guess action done) #:prefab)
(struct server-msg (type action data) #:prefab)
