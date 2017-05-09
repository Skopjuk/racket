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

(define syn0 (build-matrix 3 4 (lambda (i j) (- (* 2 (random)) 1))))
(define syn1 (build-matrix 4 1 (lambda (i j) (- (* 2 (random)) 1))))

(define (predict l0)
  (matrix-map nonlin (matrix* l0 syn0)))
(define (predict1 l1)
  (matrix-map nonlin (matrix* l1 syn1)))

(define (learn X y)
  (for ([iter 60000])
    (define l0 X)
    (define l1 (predict l0))
    (define l2 (predict1 l1))

    (define l2_error (matrix- y l2))
    (if (= (modulo iter 10000) 0)
      (println (string-append "Error " (~r (/ (apply + (matrix->list (matrix-map abs  l2_error))) 4))))
      void)
    
    (define l2_delta (matrix-map * l2_error (matrix-map nonlin-deriv l2)))
    (define l1_error (matrix* l2_delta (matrix-transpose syn1)))

    
    (define l1_delta (matrix-map * l1_error (matrix-map nonlin-deriv l1)))

    
    (set! syn1 (matrix+ syn1 (matrix* (matrix-transpose l1) l2_delta)))
    (set! syn0 (matrix+ syn0 (matrix* (matrix-transpose l0) l1_delta)))
    l2))

(learn X y)
