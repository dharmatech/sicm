
(library

 (sicm symbolic)

 (export derivative
         to-procedure
         make-constant
         make-var
         make-add
         make-mul
         make-pow
         make-identity
         make-tuple
         make-compose
         make-sin
         make-cos

         make-tuple-sum

         make-tuple*tuple-elementwise

         tuple-items
         
         )

 (import (rnrs))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define-record-type constant (fields value))

 (define-record-type var (fields name))

 (define-record-type add (fields f g))

 (define-record-type mul (fields f g))

 (define-record-type pow (fields f g))

 (define-record-type identity)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define-record-type tuple (fields items))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define-record-type compose (fields f g))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define-record-type (<sin> make-sin sin?))

 (define-record-type (<cos> make-cos cos?))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define (vector-sum v)
   (let ((n (vector-length v)))
     (let loop ((i 0) (sum 0))
       (if (>= i n)
           sum
           (loop (+ i 1)
                 (+ sum (vector-ref v i)))))))

 (define-record-type tuple-sum (fields a))

 (define-record-type tuple*tuple-elementwise (fields a b))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define (derivative expr)

   (cond ( (constant? expr) (make-constant 0) )

         ( (var?      expr) (make-constant 1) )

         ( (identity? expr) (make-constant 1) )

         ( (add? expr)

           (make-add (derivative (add-f expr))
                     (derivative (add-g expr))) )

         ( (mul? expr)

           (let ( ( f (mul-f expr) )
                  ( g (mul-g expr) ) )

             (make-add (make-mul (derivative f) g)
                       (make-mul f (derivative g)))) )

         ( (pow? expr)

           (let ((f (pow-f expr))
                 (g (pow-g expr)))

             (make-mul g (make-pow f (make-add g (make-constant -1))))) )

         ( (compose? expr)

           (let ((f (compose-f expr))
                 (g (compose-g expr)))

             (make-mul (make-compose (derivative f) g)
                       (derivative g))) )

         ( (sin? expr) (make-cos) )

         ( (cos? expr) (make-mul (make-sin)
                                 (make-constant -1)) )

         ( (tuple? expr)

           (make-tuple (vector-map derivative (tuple-items expr))) )

         ( else #f )))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define (to-procedure expr)

   (cond ( (constant? expr)

           (let ((value (constant-value expr)))

             (lambda (x) value)) )

         ( (add? expr)

           (let ((f (to-procedure (add-f expr)))
                 (g (to-procedure (add-g expr))))

             (lambda (x)

               (+ (f x)
                  (g x)))) )

         ( (mul? expr)

           (let ((f (to-procedure (mul-f expr)))
                 (g (to-procedure (mul-g expr))))

             (lambda (x)

               (* (f x)
                  (g x)))) )

         ( (pow? expr)

           (let ((f (to-procedure (pow-f expr)))
                 (g (to-procedure (pow-g expr))))

             (lambda (x)

               (expt (f x)
                     (g x)))) )

         ( (identity? expr) (lambda (x) x) )

         ( (tuple? expr)

           (let ((items (vector-map to-procedure (tuple-items expr))))

             (lambda (x)

               (vector-map (lambda (f) (f x)) items))) )

         ( (compose? expr)

           (let ((f (to-procedure (compose-f expr)))
                 (g (to-procedure (compose-g expr))))

             (lambda (x)

               (f (g x)))) )

         ( (sin? expr) sin )

         ( (cos? expr) cos )

         ( (tuple-sum? expr)

           (let ((a (to-procedure (tuple-sum-a expr))))

             (lambda (x)

               (vector-sum (a x)))) )

         ( (tuple*tuple-elementwise? expr)

           (let ((a (to-procedure (tuple*tuple-elementwise-a expr)))
                 (b (to-procedure (tuple*tuple-elementwise-b expr))))

             (lambda (x)

               (vector-map * (a x) (b x)))) )

         (else #f) ))

 )