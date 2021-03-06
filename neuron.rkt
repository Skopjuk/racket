#lang racket
(require racket/trace)
(require math/matrix)

(define (nonlin x)
  (/ 1 (+ 1 (exp (- x)))))

(define (nonlin-deriv x)
  (* x (- 1 x)))

(define X (matrix [[0 0 1]
                   [0 1 1]   
                   [1 0 1]
                   [1 1 1]]))

(define y (col-matrix [0 0 1 1]))

(random-seed 1)

(define syn0 (build-matrix 3 1 (lambda (i j) (- (* 2 (random)) 1))))

(define (predict l0)
  (matrix-map nonlin (matrix* l0 syn0)))

(define (learn X y)
  (for ([iter 10000])
    (define l0 X)
    (define l1 (predict l0))
    (define l1_error (matrix- y l1))
    (define l1_delta (matrix-map * l1_error (matrix-map nonlin-deriv l1)))
    (set! syn0 (matrix+ syn0 (matrix* (matrix-transpose l0) l1_delta)))
    l1))

(learn X y)
(predict X)
