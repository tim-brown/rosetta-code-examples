#lang racket/base

(module+ test (require rackunit))

(define (yellowstone-sequence . args)
  (void))

(module+ main
  (yellowstone-sequence))

(module+ test
  (yellowstone-sequence))
