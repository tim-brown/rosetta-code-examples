#lang racket/base

(define (seq n) (build-string n (lambda _ (string-ref "TGAC" (random 4)))))

(define (subsequence-indices full part)
  (let ((part-length (string-length part))
        (full-length (string-length full))
   (for*/list ((i (- full-length part-length))
               #:when (for/and ((n part) (h (in-string full i))) (eq? n h)))
    (cons i (+ i part-length -1)))))

(define (Bioinformatics/Subsequence (full (seq 400)) (sub (seq 4)))
  (printf "Finding indices of ~a in~%~a (length ~a)~%~a~%"
   sub full (string-length full) (subsequence-indices full sub)))

(module+ main (for ((i 4)) (Bioinformatics/Subsequence)))
