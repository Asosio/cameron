#lang racket/base

(require racket/generic
         (only-in racket/dict dict? dict-ref)
         cameron/defs)

(provide
 gen:key-get key-get? key-get/c
 key)

(define-generics key-get
  (key key-get k . args)
  #:defaults
  ([dict?
    (def key dict-ref)]))

(module+ test
  (require rackunit
           (only-in racket/local local)
           racket/hash
           racket/dict)                                

  (test-case
      "key - dict tests"
    (local [(def d (make-hash '((a . 1) (c . 3))))]
           (check-equal? (key d 'a) 1)
           (check-equal? (key d 'c) 3)
           
           ;; errors are raised if non-existing keys are required
           ;; (and no fallback val is provided)
           (check-exn exn:fail?
                      (fn [] (key d 'b)))
           ;; default values
           (check-equal? (key d 'b #f) #f)
           (check-equal? (key d 'b "nothing") "nothing")
           
           (hash-set! d 'b "something")
           (check-equal? (key d 'b) "something"))))
