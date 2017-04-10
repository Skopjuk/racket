#lang racket
(define (carmicheal_find? n)
   (define (iter a)
     (cond ((= a 1) true)
           ((not (fermat-test n a)) false)
           (else (iter (- a 1)))))
   (iter (- n 1)))

(define (square x) (* x x))

(define (even? n) (= (remainder n 2) 0))

(define (fermat-test n a) (= (expmod a n n) a))

(define (expmod base exp m)
   (cond ((= exp 0) 1)
         ((even? exp)
          (remainder (square (expmod base (/ exp 2) m))
                      m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(carmicheal_find? 4)
(carmicheal_find? 17)
(carmicheal_find? 561)