#lang racket

(require math/number-theory
         ; this is the code from http://rosettacode.org/wiki/Miller%E2%80%93Rabin_primality_test#Racket
         ; only change is a (provide prime?) at the top
         (prefix-in mr: "miller-rabin.rkt"))

;; I see no reason why circular primes should be a base10-only thing
(define *current-base* (make-parameter 10))

; like order of magnitude, but based on *current-base*
(define (oom n (b (*current-base*)))
  (inexact->exact (floor (log n b))))

(define (circulate n (fail-digit? (λ (d) #f)) (b (*current-base*)))
  (define M (expt b (oom n b)))
  (define (loop n′ (digits n′) (acc (list n′)))
    (if (zero? digits) acc
        (let* ((d (remainder digits b))
               (n′ (+ (quotient n′ b) (* M d))))
          (and (not (< n′ n))
               (not (fail-digit? d))
               (if (= n′ n) acc (loop n′ (quotient digits b) (cons n′ acc)))))))
  (loop n))

(define (R n (b (*current-base*)))
  (let loop ((r 0) (n n)) (if (zero? n) r (loop (add1 (* r b)) (sub1 n)))))

(define (circular-prime? n (prime? prime?) (b (*current-base*)))
  (or (and (> b 2) (= n 2))
      (let ((circle (circulate n even? b)))
        (and circle (for/and ((c circle)) (prime? c))))))

(define (Circular-primes)
  (printf "First 19 circular primes: ~a~%"
          (for/list ((i 19) (n (sequence-filter circular-prime? (in-naturals 2)))) n))
  ;; just about fast enough not to use miller-rabin
  (printf "Next four: ~a~%" (for/list ((i 4)
                                       (n (sequence-filter
                                           (λ (n) (circular-prime? (R n)))
                                           (in-naturals 7)))) `(R ,n)))
  (for-each (λ (n) (printf "R(~a) is ~a a prime~%" n
                           (if (mr:prime? (R n)) "probably" "not")))
    '(5003 9887 15073 25031 35317 49081)))

(module+ main (Circular-primes))