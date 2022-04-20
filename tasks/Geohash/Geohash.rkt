#lang racket
;; {trans|Wren}

(define full-lat-rng (cons -90 90))
(define full-lon-rng (cons -180 180))

(define number->gbase-32 (for/hash ((c (in-string "0123456789bcdefghjkmnpqrstuvwxyz"))
                                    (i (in-naturals)))
                           (values i c)))

(define range-mid (match-lambda [(cons from to) (/ (+ from to) 2)]))

(define (encode-geo-hash location precision)
  (define inner-loop
    (match-lambda**
     [(_ _ _ _ _ _ (and (app length (== precision))
                        (app reverse (app list->string geo-hash)))) geo-hash]
     [(val (and (cons rng-from rng-to) (app range-mid mid)) alt-val alt-rng hash-value bits rev-chars)
      (let* ((past-mid? (> val mid))
             (hash-value′ (+ (* hash-value 2) (if past-mid? 1 0)))
             (rng′ (if past-mid? (cons mid rng-to) (cons rng-from mid))))
        (define loop-args (if (< bits 4)
                              (list hash-value′ (add1 bits) rev-chars)
                              (list 0 0 (cons (hash-ref number->gbase-32 hash-value′) rev-chars))))
        (apply inner-loop alt-val alt-rng val rng′ loop-args))]))  
  (match-define (cons lat lon) location)
  (inner-loop lon full-lon-rng lat full-lat-rng 0 0 null))

(define gbase-32->number (for/hash (([k v] (in-hash number->gbase-32))) (values v k)))

(define (gbase-32-string->bits s)
  (for*/fold ((rv 0)) ((c (in-string s)))
    (+ (* rv 32) (hash-ref gbase-32->number c))))

(define (bisect-range-on-bit all-bits test-bit rng)
  (define mid (range-mid rng))
  (if (bitwise-bit-set? all-bits test-bit) (cons mid (cdr rng)) (cons (car rng) mid)))

(define (bisect-range-on-even-bits all-bits rng precision-remaining)
  (if (zero? precision-remaining)
      rng
      (bisect-range-on-even-bits
       (arithmetic-shift all-bits -2)
       (bisect-range-on-bit all-bits 0 rng)
       (sub1 precision-remaining))))

(define (decode-geo-hash geo-code)
  (define all-bits (gbase-32-string->bits geo-code))
  (define-values (bit-count) (* 5 (string-length geo-code)))
  (eprintf "all-bits: ~a prec:~a~%" (number->string all-bits 2) precision)
  (let inner-loop ((lon-bit-idx (* precision 2))
                   (lon-rng full-lon-rng)
                   (lat-rng full-lat-rng))
    (if (zero? lon-bit-idx)
        (values lat-rng lon-rng)
        (inner-loop (- lon-bit-idx 2)
                    (bisect-range-on-bit all-bits lon-bit-idx lon-rng)
                    (bisect-range-on-bit all-bits (sub1 lon-bit-idx) lat-rng)))))

(module+ main
  (define test-data
    '((51.433718   -0.214126  2 "Ireland, most of England and Wales, small part of Scotland")
      (51.433718   -0.214126  9 "the umpire's chair on Center Court at Wimbledon")
      (51.433718   -0.214126 17 "likely an individual molecule of the chair")
      (57.649110   10.407440 11 "Wikipedia test value - Råbjerg Mile in Denmark")
      (59.358639   24.744778  7 "Lake Raku in Estonia")
      (29.2021188 81.5324561  7 "Village Raku in Nepal")))
  
  (for ((t test-data))
    (match-define (list lat lon p desc) t)
    (let*-values (((geo-hash) (encode-geo-hash (cons lat lon) p))
                  ((geo-dehash-lat geo-dehash-lon) (decode-geo-hash geo-hash p)))
      (printf "~s: lat:~a lon:~a prec:~a «~a»~%" geo-hash lat lon p desc)
      (printf "dehashes exactly to:   lat range:~a lon range:~a~%" geo-dehash-lat geo-dehash-lon)
      (printf "dehashes inexactly to: lat range:~a lon range:~a~%"
              (cons (exact->inexact (car geo-dehash-lat)) (exact->inexact (cdr geo-dehash-lat)))
              (cons (exact->inexact (car geo-dehash-lon)) (exact->inexact (cdr geo-dehash-lon))))
      (newline))))