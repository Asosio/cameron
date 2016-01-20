#lang racket/base

(require racket/generic
         cameron/defs)

(provide
 gen:nth-get nth-get? nth-get/c
 nth)

(define-generics nth-get
  (nth nth-get n)
  #:defaults
  ([list?
    (def nth list-ref)]
   [vector?
    (def nth vector-ref)]))

(module+ test
  (require rackunit
           (only-in racket/local local))

  (test-case
      "nth - list tests"
    (local [(def lst (list 'a 'b 'c 'd 'e))]
           (check-equal? (nth lst 0) 'a)
           (check-equal? (nth lst 4) 'e)))

  (test-case
      "nth - vector tests"
    (local [(def vec (vector 'a 'b 'c 'd 'e))]
           (check-equal? (nth vec 0) 'a)
           (check-equal? (nth vec 4) 'e)
           (vector-set! vec 4 'f)
           (check-equal? (nth vec 4) 'f))))
