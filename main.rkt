#lang racket/base

(require racket/match)
(struct msg (id content))

(define get-time current-inexact-milliseconds)
(define (message n)
  (msg n
       (make-string 1024
                    (integer->char (modulo n 256)))))

(define worst 0.0)

(define (push-msg st msg)
  (define before (get-time))
  (match-define (list hash count min-key) st)
  (define inserted (hash-set hash (msg-id msg) (msg-content msg)))
  (define new-count (add1 count))
  (define new-st
    (cond [(200000 . < . new-count)
           (list (hash-remove inserted min-key)
                 (sub1 new-count)
                 (add1 min-key))]
          [else
           (list inserted new-count min-key)]))
  (define after (get-time))
  (define duration (after . - . before))
  (set! worst (max worst duration))
  new-st)

(define _
 (for/fold
     ([st (list (make-immutable-hash) 0 0)])
     ([i (in-range 1000000)])
   (push-msg st (message i))))
(displayln worst)
