#lang racket/base

(require racket/generic
         (only-in racket/function negate)
         (only-in racket/set set? set-mutable?)
         (only-in racket/dict dict? dict-mutable?)
         cameron/defs)

(provide
 gen:frozen-type frozen-type? frozen-type/c
 frozen?)

(define-generics frozen-type
  (frozen? frozen-type)
  #:fallbacks
  [(defn frozen? [x] #f)]
  #:defaults
  ([list?
    (defn frozen? [x] #t)]
   [dict?
    (defn frozen? [x]
      ((negate dict-mutable?) x))]
   [vector?
    (defn frozen? [x] #f)]
   [set?
    (defn frozen? [x] #t)]
   [set-mutable?
    (defn frozen? [x] #f)]))

(module+ test
  (require rackunit
           (only-in racket/set mutable-set set))

  (test-case
      "list"
    (check-equal? (frozen? (list)) #t))

  (test-case
      "dict (list)"
    (check-equal? (frozen? (list '((a . 1) (b . 2)))) #t))

  (test-case
      "immutable dict (hash)"
    (check-equal? (frozen? (make-immutable-hash '())) #t))

  (test-case
      "mutable dict (hash)"
    (check-equal? (frozen? (make-hash '())) #f))

  (test-case
      "vector"
    (check-equal? (frozen? (vector)) #f))

  (test-case
      "set"
    (check-equal? (frozen? (set)) #t))

  (test-case
      "mutable set"
    (check-equal? (frozen? (mutable-set)) #f)))
