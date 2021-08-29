#lang racket

(provide prime?)

(define (miller-rabin-expmod base e m)
  (define (squaremod-with-check x)
    (define (check-nontrivial-sqrt1 x square)
      (if (and (= square 1) (not (= x 1)) (not (= x (- m 1)))) 0 square))
    (check-nontrivial-sqrt1 x (remainder (expt x 2) m)))
  (cond ((= e 0) 1)
        ((even? e) (squaremod-with-check (miller-rabin-expmod base (/ e 2) m)))
        (else (remainder (* base (miller-rabin-expmod base (- e 1) m)) m))))
 
(define (miller-rabin-test n)
  (define (try-it a)
    (define (check-it x)
      (and (not (= x 0)) (= x 1)))
    (check-it (miller-rabin-expmod a (- n 1) n)))
  (try-it (+ 1 (random (remainder (- n 1) 4294967087)))))
 
(define (fast-prime? n times)
  (for/and ((i (in-range times))) (miller-rabin-test n)))
 
(define (prime? n (times 100))
  (fast-prime? n times))
 
(module+ test
 (require rackunit)
 (check-true
  (prime? 4547337172376300111955330758342147474062293202868155909489)))
