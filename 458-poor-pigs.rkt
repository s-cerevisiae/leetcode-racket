#lang racket

(define (poor-pigs buckets minutesToDie minutesToTest)
  (exact-ceiling (log buckets (+ 1 (/ minutesToTest minutesToDie)))))
