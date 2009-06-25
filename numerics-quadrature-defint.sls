
(library

 (sicm numerics-quadrature-defint)

 (export definite-integral)

 (import (rnrs)
         (sicm numerics-quadrature-quadrature)
         )

 (define (definite-integral-with-tolerance f x1 x2 tolerance)
   (evaluate-definite-integral 'open f x1 x2 tolerance))

 (define (definite-integral-numerical f t1 t2 tolerance)
   
   (if (and (number? t1)
            (number? t2)
            (= t1 t2))
       
       0

       (definite-integral-with-tolerance f t1 t2 tolerance)))

 (define definite-integral definite-integral-numerical)

 )