#lang racket
(require pict rackunit
         (for-syntax syntax/parse racket/sequence
                     racket/syntax)
         syntax/location
         racket/struct
         math/flonum
         "utils.rkt")

;; This number is mostly arbitrary
(define MAX_FLOATING_POINT_ERROR
  0
  #;(- (flnext 10.0) 10.0))

(define old-scale
  (case-lambda
    [(p x-factor y-factor)
     (define drawer (make-pict-drawer p))
     (define new
       (dc
        (λ (dc x y)
          (define t (send dc get-transformation))
          (send dc scale x-factor y-factor)
          (drawer dc
                  (/ x x-factor)
                  (/ y y-factor))
          (send dc set-transformation t))
        (* (pict-width p) x-factor)
        (* (pict-height p) y-factor)
        (* (pict-ascent p) y-factor)
        (* (pict-descent p) y-factor)))
     (make-pict (pict-draw new)
                (pict-width new)
                (pict-height new)
                (pict-ascent new)
                (pict-descent new)
                (list (make-child p 0 0 x-factor y-factor 0 0))
                #f
                (pict-last p))]
    [(p factor) (scale p factor factor)]))

(define (old-shear p shear-x shear-y)
  (define drawer (make-pict-drawer p))
  (define x-shift (* shear-x (pict-height p)))
  (define y-shift (* shear-y (pict-width p)))
  (define new
    (dc
     (λ (dc dx dy)
       (define t (send dc get-transformation))
       (send dc transform (vector 1 shear-y shear-x 1 (- dx (min 0 x-shift)) (- dy (min 0 y-shift))))
       (drawer dc 0 0)
       (send dc set-transformation t))
     (+ (pict-width p) (abs x-shift))
     (+ (pict-height p) (abs y-shift))
     (pict-ascent p)
     (pict-descent p)))
  (make-pict (pict-draw new)
             (pict-width new)
             (pict-height new)
             (pict-ascent new)
             (pict-descent new)
             (list (make-child p 0 0 1 1 shear-y shear-x))
             #f
             (pict-last p)))

(define (old-rotate p theta)
  (let ([w (pict-width p)]
        [h (pict-height p)]
        [drawer (make-pict-drawer p)])
    (let ([dl (min 0 (* w (cos theta)) (* h (sin theta)) (+ (* w (cos theta)) (* h (sin theta))))]
          [dr (max 0 (* w (cos theta)) (* h (sin theta)) (+ (* w (cos theta)) (* h (sin theta))))]
          [dt (min 0 (* w -1 (sin theta)) (* h (cos theta)) (+ (* w -1 (sin theta)) (* h (cos theta))))]
          [db (max 0 (* w -1 (sin theta)) (* h (cos theta)) (+ (* w -1 (sin theta)) (* h (cos theta))))]
          [da (- (* (pict-ascent p) (cos theta)) (* (sin theta) w 1/2))]
          [dd (- (* (- (pict-height p) (pict-descent p)) (cos theta)) (* (sin theta) w 1/2))])
      (let ([new (dc
                  (lambda (dc x y)
                    (let ([t (send dc get-transformation)])
                      (send dc translate (- x dl) (- y dt))
                      (send dc rotate theta)
                      (drawer dc 0 0)
                      (send dc set-transformation t)))
                  (- dr dl) (- db dt) 
                  (min (- da dt) (- (- db dt) (- db dd)))
                  (min (- db da) (- db dd)))])
        (make-pict (pict-draw new)
                   (pict-width new)
                   (pict-height new)
                   (pict-ascent new)
                   (pict-descent new)
                   (list (make-child p 
                                     (- (* h (sin theta)) dl) 
                                     (max 0 (- db (* h (cos theta))))
                                     (cos theta) (cos theta) 
                                     (sin theta) (- (sin theta))))
                   #f
                   (pict-last p))))))


;                                                                    
;                                                                    
;                                                                    
;               ;                                ;                   
;      ;;;;;    ;                                ;                   
;     ;;   ;;   ;                                ;                   
;    ;;         ;                                ;                   
;    ;          ;  ;;;       ;;;;       ;;;;     ;    ;;;    ;;;;;   
;   ;;          ;;;  ;;    ;;   ;;     ;;  ;;    ;   ;;     ;;   ;;  
;   ;;          ;;   ;;    ;     ;    ;;         ;  ;;      ;        
;   ;;          ;    ;;    ;     ;    ;          ;  ;       ;;       
;   ;;          ;    ;;   ;;;;;;;;    ;          ; ;;        ;;;;    
;   ;;          ;    ;;   ;;          ;          ; ;;          ;;;   
;    ;          ;    ;;    ;          ;          ;  ;;           ;;  
;    ;;         ;    ;;    ;;         ;;         ;   ;;          ;;  
;     ;;   ;;   ;    ;;    ;;;  ;;     ;;  ;;    ;    ;;    ;;  ;;;  
;      ;;;;;    ;    ;;      ;;;;       ;;;;     ;     ;;  ; ;;;;    
;                                                                    
;                                                                    
;                                                                    
;                                                                    
;                                                                    

(define-syntax check
  (syntax-parser
    [(_ check:id
        (~optional (~and #:omit-ascent/descent x))
        (pict ...) (~and as (_ ...)) ...)
     #`(test-case (symbol->string 'check) 
         #,@(for*/list ([p (in-syntax #'(pict ...))]
                        [a (in-syntax #'(as ...))])
              (define/with-syntax pict p)
              (define/with-syntax (arg ...) a)
              #`(with-check-info
                 (['args '(pict arg ...)])
                 #,(quasisyntax/loc p
                     (check
                      #,(and (attribute x) #t)
                      pict arg ...)))))]))

(define-check (do-check a/d? p1 p2)
  (check-pict=? p1 p2 "different bitmaps")
  (unless a/d?
    (check-within (pict-ascent p1)
                  (pict-ascent p2)
                  MAX_FLOATING_POINT_ERROR
                  "pict-ascent")
    (check-within (pict-descent p1)
                  (pict-descent p2)
                  MAX_FLOATING_POINT_ERROR
                  "pict-ascent"))
  (check-children p1 p2))
(define-check (check-children p1 p2)
  (define c1 (pict-children p1))
  (define c2 (pict-children p2))
  (check-equal? (length c1) (length c2)
                "different number of children")
  (for ([c1 (in-list c1)]
        [c2 (in-list c2)])
    (field-fp-check child-dx c1 c2)
    (field-fp-check child-dy c1 c2)
    (field-fp-check child-sx c1 c2)
    (field-fp-check child-sy c1 c2)
    (field-fp-check child-sxy c1 c2)
    (field-fp-check child-syx c1 c2)))
(define-syntax field-fp-check
  (syntax-parser
    [(_ fld:id a b)
     (syntax/loc this-syntax
       (check-within
        (fld a) (fld b)
        MAX_FLOATING_POINT_ERROR
        (symbol->string 'fld)))]))
    
(define-check (check-rotate a/d? p θ)
  (do-check a/d? (rotate p θ) (old-rotate p θ)))
(define-check (check-scale a/d? p x y)
  (do-check a/d? (scale p x y) (old-scale p x y)))
(define-check (check-shear a/d? p x y)
  (do-check a/d? (shear p x y) (old-shear p x y)))


;                                                                                                                                      
;                                                                                                                                      
;                                                                                                                                      
;                                                          ;;;;;                                                                       
;      ;;;                                                    ;;                 ;;;;;;;;;                                             
;      ;;;                   ;                                ;;                     ;                               ;                 
;      ; ;                   ;                                ;;                     ;                               ;                 
;     ;; ;        ;;;;    ;;;;;;;     ;    ;;    ;;;;;        ;;                     ;         ;;;;      ;;;;;    ;;;;;;;      ;;;;;   
;     ;  ;;      ;;  ;;      ;        ;    ;;    ;   ;;       ;;                     ;       ;;   ;;    ;;   ;;      ;        ;;   ;;  
;     ;   ;     ;;           ;        ;    ;;         ;;      ;;                     ;       ;     ;    ;            ;        ;        
;    ;;   ;     ;            ;        ;    ;;         ;;      ;;                     ;       ;     ;    ;;           ;        ;;       
;    ;    ;;    ;            ;        ;    ;;     ;;;;;;      ;;                     ;      ;;;;;;;;     ;;;;        ;         ;;;;    
;    ;;;;;;;    ;            ;        ;    ;;    ;;   ;;      ;;                     ;      ;;             ;;;       ;           ;;;   
;   ;;     ;    ;            ;        ;    ;;   ;;    ;;      ;;                     ;       ;               ;;      ;             ;;  
;   ;;     ;;   ;;           ;        ;    ;;   ;;    ;;      ;;                     ;       ;;              ;;      ;             ;;  
;   ;      ;;    ;;  ;;      ;;  ;    ;;  ;;;    ;;  ;;;      ;;  ;                  ;       ;;;  ;;    ;;  ;;;      ;;  ;    ;;  ;;;  
;  ;;       ;     ;;;;        ;;;;     ;;;  ;    ;;;;  ;       ;;;;                  ;         ;;;;    ; ;;;;         ;;;;   ; ;;;;    
;                                                                                                                                      
;                                                                                                                                      
;                                                                                                                                      
;                                                                                                                                      
;                                                                                                                                      


(check check-rotate
       ((rectangle 10 10)
        (arrowhead 10 0))
       (0)
       (pi)
       ((- pi))
       ((/ pi 2))
       ((- (/ pi 2)))
       ((/ pi 3))
       ((- (/ pi 3))))

(check check-scale
       ((rectangle 10 10)
        (arrowhead 10 0))
       (1 1)
       (5 5)
       (3.5 7)
       (10 8)
       (4.9 99)
       (1/2 1/2)
       (1 1/2)
       (1/2 1/2)
       (.2 5)
       (5 .2))

(check check-shear
       #:omit-ascent/descent
       ((rectangle 10 10)
        (arrowhead 10 0))
       (1 1)
       (5 5)
       (3.5 7)
       (10 8)
       (4.9 99)
       (1/2 1/2)
       (1 1/2)
       (1/2 1/2)
       (.2 5)
       (5 .2)

       (1 -1)
       (5 -5)
       (3.5 -7)
       (10 -8)
       (4.9 -99)
       (1/2 -1/2)
       (1 -1/2)
       (1/2 -1/2)
       (.2 -5)
       (5 -.2)

       (-1 1)
       (-5 5)
       (-3.5 7)
       (-10 8)
       (-4.9 99)
       (-1/2 1/2)
       (-1 1/2)
       (-1/2 1/2)
       (-.2 5)
       (-5 .2)

       (-1 -1)
       (-5 -5)
       (-3.5 -7)
       (-10 -8)
       (-4.9 -99)
       (-1/2 -1/2)
       (-1 -1/2)
       (-1/2 -1/2)
       (-.2 -5)
       (-5 -.2))