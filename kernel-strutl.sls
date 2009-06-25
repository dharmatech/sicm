
(library

 (sicm kernel-strutl)

 (export merge-streams
         stream-of-iterates
         shorten-stream
         )

 (import (rnrs)
         (srfi :41))

 (define (stream-of-iterates next value)
   (stream-cons value
                (stream-of-iterates next (next value))))

 (define (shorten-stream n s)
   (if (or (= n 0)
           (stream-null? s))
       stream-null
       (stream-cons (stream-car s)
                    (shorten-stream (- n 1)
                                    (stream-cdr s)))))

 (define (merge-streams s1 s2)
   (stream-cons (stream-car s1)
                (stream-cons (stream-car s2)
                             (merge-streams (stream-cdr s1)
                                            (stream-cdr s2)))))

 )