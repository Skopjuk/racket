#lang racket
;(define (search-for-primes frst scnd)
;   (if (even? frst)
;       (search-for-primes (+ frst 1) scnd)
;       (cond ((< frst scnd) (timed-prime-test frst)
;                            (search-for-primes (+ frst 2) scnd)))))

;(define (even? n)
;   (= (remainder n 2) 0))

(define runtime current-inexact-milliseconds)

(define (timed-prime-test n)
   (start-prime-test n (runtime)))

(define (fermat-test n) 
   (define (try-it a) 
     (= (expmod a n n) a)) 
   (try-it (+ 1 (random (- n 1))))) 

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


;(define (start-prime-test n start-time)
;  (cond ((prime? n) ((report-prime n (- (runtime) start-time))))))

(define (start-prime-test n start-time) 
   (if (prime? n) 
       (report-prime n (- (runtime) start-time))
       (void)))

(define (prime? n)
   (fast-prime? n 100))

(define (report-prime n elapsed-time) 
  (newline) 
  (display n) 
  (display " *** ")
  (display elapsed-time)) 

(define (square x) (* x x))

(define (find-divisor n test-divisor)
   (cond ((> (square test-divisor) n) n)
         ((divides? test-divisor n) test-divisor)
         (else (find-divisor n (next test-divisor)))))

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (divides? a b)
   (= (remainder b a) 0))

(define (expmod base exp m) 
   (cond ((= exp 0) 1) 
         ((even? exp) 
          (remainder (square (expmod base (/ exp 2) m)) 
                     m)) 
         (else 
          (remainder (* base (expmod base (- exp 1) m)) 
                     m))))

(timed-prime-test 1013)