
(library

 (sicm numerics-quadrature-quadrature)

 (export evaluate-definite-integral
         )

 (import (rnrs)
         (sicm numerics-quadrature-rational)
         )

 (define :-infinity ':-infinity)
 (define :+infinity ':+infinity)
 (define *infinities* (list :-infinity :+infinity))

 (define evaluate-definite-integral
   
   (lambda (method integrand lower-limit upper-limit allowable-error)

     (if (not (and integrand lower-limit upper-limit))
         (error "Missing parameter for definite integral"
                `(integrand ,integrand
                            lower-limit ,lower-limit
                            upper-limit ,upper-limit)))

     (let ((lower-limit (if (memq lower-limit *infinities*)
                            lower-limit
                            (inexact lower-limit)))
           (upper-limit (if (memq upper-limit *infinities*)
                            upper-limit
                            (inexact upper-limit)))
           (allowable-error (inexact allowable-error)))
       
       (if (or (memq lower-limit *infinities*)
               (memq upper-limit *infinities*))

           ;; (evaluate-improper-integral method
           ;;                             integrand
           ;;                             upper-limit
           ;;                             lower-limit
           ;;                             allowable-error)

           #f
           
           (case method

             ((open)
              (integrate-open integrand
                              lower-limit upper-limit
                              allowable-error))

             ((closed-closed)
              (integrate-closed-closed-1 integrand
                                         lower-limit upper-limit
                                         allowable-error))

             ((closed-open)
              (integrate-closed-open-1 integrand
                                       lower-limit upper-limit
                                       allowable-error))

             ((open-closed)
              (integrate-open-closed-1 integrand
                                       lower-limit upper-limit
                                       allowable-error))

             ((open-open)
              (integrate-open-open integrand
                                   lower-limit upper-limit
                                   allowable-error))

             ;; ((romberg)
             ;;  (romberg-quadrature integrand
             ;;                      lower-limit upper-limit
             ;;                      allowable-error))

             ;; ((bulirsch-stoer)
             ;;  (bulirsch-stoer-quadrature integrand
             ;;                             lower-limit upper-limit
             ;;                             allowable-error))
             
             (else
              (error "Unknown method -- DEFINITE-INTEGRAL" method)))))))

 )