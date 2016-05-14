#lang racket/base

(require racket/match)

(define window-size 200000)
(define msg-count 1000000)

(define get-time current-inexact-milliseconds)
(define (message n)
  (make-string 1024
               (integer->char (modulo n 256))))

(define worst 0.0)

(define (push-msg chan id-high)
  (define before (get-time))
  (define id-low (id-high . - . window-size))
  (define inserted (hash-set chan id-high (message id-high)))
  (define result
    (if (id-low . < . 0) inserted
        (hash-remove inserted id-low)))
  (define after (get-time))
  (define duration (after . - . before))
  (set! worst (max worst duration))
  result)

(define _
 (for/fold
     ([chan (make-immutable-hash)])
     ([i (in-range msg-count)])
   (push-msg chan i)))
(displayln worst)
