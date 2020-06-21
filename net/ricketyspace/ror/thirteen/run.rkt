#lang racket

(require 2htdp/universe "client.rkt" "server.rkt")

(define (run)
  (launch-many-worlds (launch-guess-client "Adam" LOCALHOST)
                      (launch-guess-server)
                      (launch-guess-client "Eve" LOCALHOST)))

(define (bad)
  (launch-many-worlds (launch-guess-client "Adam" LOCALHOST)
                      (launch-guess-server)
                      (launch-guess-client "Beatrice" LOCALHOST)))
