#lang racket/base

(require racket/match)

(define window-size 200000)
(define msg-count  1000000)

(define (message n) (make-bytes 1024 (modulo n 256)))

(define (push-msg chan id-high)
  (define id-low (id-high . - . window-size))
  (define inserted (hash-set chan id-high (message id-high)))
  (if (id-low . < . 0) inserted
      (hash-remove inserted id-low)))

(define _
  (for/fold
     ([chan (make-immutable-hash)])
     ([i (in-range msg-count)])
     (push-msg chan i)))
