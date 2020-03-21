#lang racket/base

(require racket/set)

(define a098550
  (let ((cache (make-hash '((1 . 1) (2 . 2) (3 . 3)))) (used (mutable-set 1 2 3)))
    (λ (n)
       (hash-ref! cache n
		  (λ ()
		     (define a (for/first
				 ((i (in-naturals 4))
				  #:unless (set-member? used i)
				  #:when (= 1 (gcd i (a098550 (- n 1))))
				  #:unless (= 1 (gcd i (a098550 (- n 2)))))
				 i))
		     (set-add! used a)
		     a)))))

(define (Yellowstone_sequence)
  (for/list ((i (in-range 1 (add1 30)))) (a098550 i)))

(module+ main
  (Yellowstone_sequence))

(module+ test
  (require rackunit)
  (check-not-exn Yellowstone_sequence))
