#lang racket
(provide check-pict=?
         check-pict=?/msg)
(require
  pict
  rackunit
  (for-syntax syntax/parse
              racket/sequence))


(define (->bitmap p)
  (define b (pict->bitmap p))
  (define w (send b get-width))
  (define h (send b get-height))

   (define its (make-bytes
                (*
                 w h
                 4)
                255))
   (send b get-argb-pixels
         0 0
         (send b get-width)
         (send b get-height)
         its)
   (define mask (send b get-loaded-mask))
   (when mask
     (send b get-argb-pixels 0 0 w h its #t))
  its)


(define-check (check-pict=?/msg actual expected msg)
  (unless (equal? (->bitmap actual) (->bitmap expected))
    (fail-check msg)))
    
(define-syntax check-pict=?
  (syntax-parser
    [(_ actual expected)
     (syntax/loc this-syntax
       (check-pict=?/msg actual expected ""))]
    [(_ actual expected msg)
     (syntax/loc this-syntax
       (check-pict=?/msg actual expected msg))]))