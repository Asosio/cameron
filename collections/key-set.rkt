#lang racket/base

(require racket/generic
         (only-in racket/function negate conjoin)
         (only-in racket/dict dict? dict-set dict-set!)
         (prefix-in dict- cameron/collections/dict)
         cameron/defs)

(provide
 gen:key-set key-set? key-set/c
 key-set)

(def mutable? (negate immutable?))

(defn -dict-set! [d & args]
  (apply dict-set! d args)
  d)

(define-generics key-set
  (key-set key-set k v)
  #:defaults
  ([(conjoin dict? immutable?)
    (def key-set dict-set)]
   ;; regular list of pairs
   [(conjoin dict? mutable? list?) 
    (def key-set dict-set)]
   [(conjoin dict? mutable?)
    (def key-set -dict-set!)]))

;; TODO - missing multiple assignment tests (if wanted)
(module+ test
  (require rackunit
           (only-in racket/local local)
           (only-in racket/dict dict-ref)
           racket/hash)

  (test-case
      "key-set - mutable dict tests"
    (local [(def d (make-hash '((a . 1) (c . 3))))]
           (check-equal? (dict-ref (key-set d 'b 2) 'b) 2)
           ;; Ensure the value is persisted in the dictionary
           (check-equal? (dict-ref d 'b #f) 2)))

  (test-case
      "key-set - list-as-dict tests"
    (local [(def d (list '((a . 1) (c . 3))))]
           ;; Even though lists are technically mutable a list of pairs
           ;; is implemented to act as an immutable dict.
           (check-equal? (dict-ref (key-set d 'b 2) 'b) 2)
           (check-equal? (dict-ref d 'b #f) #f)
           (check-equal? (dict-ref d 'b #t) #t)))

  (test-case
      "key-set - immutable dict tests"
    (local [(def d (make-immutable-hash '((a . 1) (c . 3))))]
           ;; ensure dict value is set and orig values retained
           (check-equal? (dict-ref (key-set d 'b 2) 'a) 1)
           (check-equal? (dict-ref (key-set d 'b 2) 'b) 2)
           (check-equal? (dict-ref (key-set d 'b 2) 'c) 3)

           ;; old dict should not be altered
           (check-equal? (dict-ref d 'b #f) #f)
           (check-equal? (dict-ref d 'b #t) #t))))
