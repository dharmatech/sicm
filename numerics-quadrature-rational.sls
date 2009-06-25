
(library

 (sicm numerics-quadrature-rational)

 (export integrate-open
  integrate-closed-closed-1
  integrate-closed-open-1
  integrate-open-closed-1
  integrate-open-open
  )

 (import (rnrs)
         (only (rnrs r5rs) quotient)
         (only (srfi :27) random-real)
         (srfi :41)
         (sicm kernel-strutl)
         (sicm kernel-numeric)
         )

 (define (rational-interpolation dt c dx-list dx-new eps)

   (if (null? dt)
       
       '()
       
       (let* ( ( dt1 (car dt)                         )
               ( w   (- c dt1)                        )
               ( b1  (* (/ (car dx-list) dx-new) dt1) )
               ( den (- b1 c)                         ) )
         
         (if (= den 0.0)
             
             (cons dt1
                   (rational-interpolation (cdr dt)
                                           c
                                           (cdr dx-list)
                                           dx-new
                                           eps))
             
             (let* ( ( b     (/ w den) )
                     ( new-d (* c b)   ) )
               
               (cons new-d
                     (rational-interpolation (cdr dt)
                                             (* b1 b)
                                             (cdr dx-list)
                                             dx-new
                                             eps)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define (sum-list-flo l)
   (if (null? (cdr l)) 
       (car l) 
       (+ (car l) (sum-list-flo (cdr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define (sigma-flo f low high)
   (let lp ((i low) (sum 0.0) (c 0.0))
     (if (> i high)
         sum
         (let* ((y (- (f i) c)) (t (+ sum y)))
           (lp (+ i 1) t (- (- t sum) y))))))

 (define (rat-square x)
   (let ((fx (inexact x)))
     (* fx fx)))

 (define *new-bs-steps*
   (merge-streams (stream-of-iterates (lambda (x) (* 2 x)) 4)
                  (stream-of-iterates (lambda (x) (* 2 x)) 6)))

 (define (make-bs-intervals a b)
   (stream-map (lambda (x) (rat-square (/ (- b a) x)))
               *new-bs-steps*))


 (define (build-tableau-streams dt dx-list x-stream y-stream eps estimate)
   (if (null? x-stream) 
       '()
       (let ((dx-new (stream-car x-stream))
             (c (stream-car y-stream)))
         (let ((new-dt
                (cons c (rational-interpolation dt c dx-list dx-new eps))))
           (let ((new-estimate (sum-list-flo new-dt)))
             (if (and (close-enuf? new-estimate estimate eps)
                      (> (length new-dt) 2))
                 new-estimate
                 (build-tableau-streams new-dt (cons dx-new dx-list) 
                                        (stream-cdr x-stream)
                                        (stream-cdr y-stream)
                                        eps
                                        new-estimate)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define (uniform-random) (random-real))

;;; To make quadrature deterministic, but sensitive to special choices
;;;  make the choice: (define *quadrature-neighborhood-width* #f)

 (define *quadrature-neighborhood-width* 0.05)

 (define (from-neighborhood a b)
   (if *quadrature-neighborhood-width*
       (+ a
          (* (+ 0.5	 
                (* (- (* 2.0 (uniform-random)) 1.0)
                   *quadrature-neighborhood-width*))
             (- b a)))
       (* 0.5 (+ a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define (extrapolate-streams-to-zero x-stream y-stream eps)
   (build-tableau-streams '() '() x-stream y-stream eps (stream-car y-stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define *roundoff-cutoff* 1e-14)

 (define integrate-roundoff-wallp? #f)

 (define *INTEGRATE-N* 10)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define (rat-trapezoid f a b)
   (lambda (n)
     (let ((h (/ (- b a) (inexact n))))
       (let ((fx (lambda (i)
                   (inexact (f (+ a (* (inexact i) h)))))))
         (* h (+ (/ (+ (inexact (f a))
                       (inexact (f b)))
                    2.0)
                 (sigma-flo fx 1 (- n 1))))))))

 (define (trapezoid-using-previous-sum f a b Sn/2 n)
   (let ((h (/ (- b a) (inexact n))))
     (let ((fx
            (lambda (i)
              (inexact
               (f (+ a (* (inexact (- (+ i i) 1)) h)))))))
       (+ (/ Sn/2 2.0)
          (* h (sigma-flo fx 1 (quotient n 2)))))))

 (define (trapezoid-stream f a b n0)
   (let ((steps (stream-of-iterates (lambda (x) (* 2 x)) n0))
         (first-S ((rat-trapezoid f a b) n0)))
     (let loop ((steps (stream-cdr steps)) (Sn/2 first-S))
       (let ((S (trapezoid-using-previous-sum f a b Sn/2 (stream-car steps))))
         (stream-cons S (loop (stream-cdr steps) S))))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define (second-euler-maclaurin f a b)
   (lambda (n)
     (let ((h (/ (- b a) (inexact n))))
       (let ((h/2 (/ h 2.0)))
         (let ((fx
                (lambda (i)
                  (inexact
                   (f (+ a (+ h/2 (* (inexact i) h))))))))
           (* h (sigma-flo fx 0 (- n 1))))))))

 (define (integrate-closed-finite f a b n eps)
   (if (<= (abs (- b a))
           (* *roundoff-cutoff*
              (+ (abs b) (abs a))))
       (begin (if integrate-roundoff-wallp?
                  (display `(roundoff-cutoff ,a ,b)))
              (* (/ (+ (f a) (f b)) 2.0) (- b a)))
       (extrapolate-streams-to-zero
        (shorten-stream n (make-bs-intervals a b))
        (merge-streams (trapezoid-stream f a b 2)
                       (trapezoid-stream f a b 3))
        eps)))

 (define (integrate-open-finite f a b n eps)
   (if (<= (abs (- b a))
           (* *roundoff-cutoff*
              (+ (abs b) (abs a))))
       (begin (if integrate-roundoff-wallp?
                  (begin (display `(roundoff-cutoff ,a ,b))
                         (newline)))
              (* (f (/ (+ a b) 2.0)) (- b a)))
       (extrapolate-streams-to-zero
        (shorten-stream n (make-bs-intervals a b))
        (stream-map (second-euler-maclaurin f a b)
                    *new-bs-steps*)
        eps)))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 (define (integrate-closed-closed f a b eps)
   (let ((m (from-neighborhood a b)))
     (+ (integrate-closed-closed-1 f a m eps)
        (integrate-closed-closed-1 f m b eps))))

 (define (integrate-closed-closed-1 f a b eps)
   (let ((ans (integrate-closed-finite f a b *INTEGRATE-N* eps)))
     (if (null? ans)
         (integrate-closed-closed f a b eps)
         ans)))

 (define (integrate-open-closed f a b eps)
   (let ((m (from-neighborhood a b)))
     (+ (integrate-open-closed-1 f a m eps)
        (integrate-closed-closed-1 f m b eps))))

 (define (integrate-open-closed-1 f a b eps)
   (let ((ans (integrate-open-finite f a b *INTEGRATE-N* eps)))
     (if (null? ans)
         (integrate-open-closed f a b eps)
         ans)))

 (define (integrate-closed-open f a b eps)
   (let ((m (from-neighborhood a b)))
     (+ (integrate-closed-closed-1 f a m eps)
        (integrate-closed-open-1 f m b eps))))

 (define (integrate-closed-open-1 f a b eps)
   (let ((ans
          (integrate-open-finite f a b *INTEGRATE-N* eps)))
     (if (null? ans)
         (integrate-closed-open f a b eps)
         ans)))

 (define (integrate-open-open f a b eps)
   (let* ((m (from-neighborhood a b)))
     (+ (integrate-open-closed-1 f a m eps)
        (integrate-closed-open-1 f m b eps))))

 (define (integrate-open-open-1 f a b eps)
   (let ((ans (integrate-open-finite f a b *INTEGRATE-N* eps)))
     (if (null? ans)
         (integrate-open-open f a b eps)
         ans)))

 (define integrate-open integrate-open-open-1)

 )