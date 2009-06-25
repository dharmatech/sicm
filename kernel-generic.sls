
(library

 (sicm kernel-generic)

 (export)

 (import (rnrs))

 (define (magnitude obj)

   (cond ((number? obj) (abs obj))))
 
 )