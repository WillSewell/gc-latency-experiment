#lang racket/base

(require racket/match)

(define window-size 200000)
(define msg-count  2000000)

; the incremental GC seems to have a hard time adapting its dynamic
; parameters to the end of the ramp-up period filling the chan. We can
; help by explicitly calling for GC collections around the end of this
; ramp-up period, but this seems to be rather sensitive to the
; specific parameters used (the limits of the ramp-up zone, and the
; frequency of collections).
;
; On Racket trunk, enabling this flag reduces the worst pause from
; 120ms to 40ms.
(define gc-during-rampup #f)

(define (maybe-gc i)
  (when (and gc-during-rampup
             (i . > . (window-size . / . 2))
             (i . < . (window-size . * . 2))
             (zero? (modulo i 50)))
        (collect-garbage 'incremental)
        (collect-garbage 'minor)))

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
     (maybe-gc i)
     (push-msg chan i)))
