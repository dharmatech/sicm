
(library

 (sicm kernel-numeric)

 (export close-enuf?
  )

 (import (rnrs)

         )

 (define *machine-epsilon*
   (let loop ((e 1.0))
     (if (= 1.0 (+ e 1.0))
         (* 2 e)
         (loop (/ e 2)))))

 (define (close-enuf? h1 h2 tolerance)
   (<= (magnitude (- h1 h2))
       (* .5
          (max tolerance *machine-epsilon*)
          (+ (magnitude h1)
             (magnitude h2)
             2.0))))

 )