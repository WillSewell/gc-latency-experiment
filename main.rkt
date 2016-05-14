#lang racket/base

(require racket/match)
(struct msg (id content))
(struct min-hash (table min-key))

(define (mk-min-hash)
  (min-hash (make-immutable-hash) #f))

(define (min-hash-set mh k v)
  (define mk  (min-hash-min-key mh))
  (define tab (min-hash-table mh))
  (define new-mk
    (if mk (min k mk) k))
  (min-hash (hash-set tab k v)
            new-mk))

(define (min-hash-remove-min mh)
  (define mk  (min-hash-min-key mh))
  (define tab (min-hash-table mh))
  (min-hash (hash-remove tab mk) (add1 mk)))

(define get-time current-inexact-milliseconds)
(define (message n)
  (msg n
       (make-string 1024
                    (integer->char (modulo n 256)))))

(define worst 0.0)

(define (push-msg st msg)
  (match-define (list count map) st)
  (define before (get-time))
  (define inserted (min-hash-set map (msg-id msg) (msg-content msg)))
  (define new-count (add1 count))
  (define res
    (cond [(200000 . < . new-count)
           (list count
                 (min-hash-remove-min inserted))]
          [else (list new-count inserted)]))
  (define after (get-time))
  (define duration (after . - . before))
  (set! worst (max worst duration))
  res)

(define _
 (for/fold
     ([st (list 0 (mk-min-hash))])
     ([i (in-range 1000000)])
   (push-msg st (message i))))
(displayln worst)
