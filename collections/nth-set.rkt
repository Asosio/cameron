#lang racket/base

(require racket/generic
         cameron/defs
         (only-in racket/function negate conjoin)
         (only-in racket/list list-set))

(provide
 gen:nth-set nth-set? nth-set/c
 nth-set)

(def mutable? (negate immutable?))

(defn -vector-set! [v & args]
  (apply vector-set! v args)
  v)

(define-generics nth-set
  (nth-set nth-set n val)
  #:defaults
  ([(conjoin list? mutable?)
    (def nth-set list-set)]
   [(conjoin vector? mutable?)
    (def nth-set -vector-set!)]))

(module+ test
  (require rackunit
           (only-in racket/local local))

  (test-case
      "nth-set - list tests"
    (local [(def lst (list 'a 'b 'c 'd 'e))]
           (check-equal? (list-ref (nth-set lst 0 'f) 0) 'f)
           (check-equal? (list-ref lst 0) 'a)
           (check-equal? (list-ref (nth-set lst 4 'q) 4) 'q)
           (check-equal? (list-ref lst 4) 'e)))

  (test-case
      "nth-set - vector tests"
    (local [(def vec (vector 'a 'b 'c 'd 'e))]
           (check-equal? (vector-ref (nth-set vec 0 'f) 0) 'f)
           ;; expect assignment above to persist
           (check-equal? (vector-ref vec 0) 'f)
           (check-equal? (vector-ref (nth-set vec 4 'q) 4) 'q)
           ;; expect assignment above to persist
           (check-equal? (vector-ref vec 4) 'q))))
