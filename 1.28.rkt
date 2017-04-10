#lang racket
(require (planet williams/science/random-source))

(define (miller-rabin n)
  (define (iter a)
     (= (expmod a (- n 1) n) 1))
  (iter (+ 2 (random-integer (- n 2)))))

(define (square-find a n)
  (if (and (not (or (= a 1) (= a (- n 1))))
           (= (remainder (square a) n) 1))
      0
      (remainder (square a) n)))

(define (square x) (* x x))

(define (even? n) (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (square-find (expmod base (/ exp 2) m) m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(miller-rabin 6)
(miller-rabin 13)
(miller-rabin 561)