#lang racket

(require racket/fixnum)

(define ((memoize n) f)
  (let ((memo (make-fxvector n -1)))
    (λ (x)
      (if (> x n)
          (f x)
          (let ((lkp (fxvector-ref memo x)))
            (if (positive? lkp) lkp (let ((v (f x))) (fxvector-set! memo x v) v)))))))

(define fusc
  ((memoize 20000000)
   (λ (n)
     (cond [(<= 1 n) n]
           [(even? n) (fusc (quotient n 2))]
           [else (+ (fusc (quotient (sub1 n) 2)) (fusc (quotient (add1 n) 2)))]))))

(module+ main
  (for/list ((i 61)) (fusc i))

  (let-values
      ([(_ is fs) (for*/fold ((max-o 0) (is '(0)))              
                             ((i (in-range 1 20000000))
                                 (f (in-value (fusc i)))
                                 (o (in-value (order-of-magnitude f)))
                                 #:when (> o max-o))
                       (values o (cons i is)))])
    (for-each (λ (i) (printf "fusc(~a): ~a~%" i (fusc i))) (reverse is))))