#lang racket/base

(require racket/generic
         cameron/defs
         (only-in racket/dict dict?)
         (only-in racket/function negate conjoin)
         (only-in cameron/collections/dict dict-dissoc dict-dissoc!))

(provide
 gen:key-rm key-rm? key-rm/c
 key-rm)

(def mutable? (negate immutable?))

(define-generics key-rm
  (key-rm key-rm k . ks)
  #:defaults
  ([(conjoin dict? immutable?)
    (def key-rm dict-dissoc)]
   [(conjoin dict? mutable? list?)
    (def key-rm dict-dissoc)]
   [(conjoin dict? mutable?)
    (def key-rm dict-dissoc!)]))

(module+ test
  (require rackunit
           (only-in racket/local local)
           (only-in racket/dict dict-ref)
           racket/hash)

  (test-case
      "key-rm - immutable dict  - rm single key tests"
    (local [(def d (make-immutable-hash '((a . 1) (c . 3) (d . 4) (e . 5))))]
           ;; ensure a's presence
           (check-equal? (dict-ref d 'a) 1)
           ;; returned dict has no 'a entry
           (check-equal? (dict-ref (key-rm d 'a) 'a #f) #f)
           ;; change should NOT persist
           (check-equal? (dict-ref d 'a #f) 1)

           ;;rm non-existing value
           (check-equal? (key-rm d 'f) d)))

  (test-case
      "key-rm - immutable dict  - rm multiple keys tests"
    (local [(def d (make-immutable-hash '((a . 1) (c . 3) (d . 4) (e . 5))))]
           ;; ensure a & d's presence
           (check-equal? (dict-ref d 'a) 1)
           (check-equal? (dict-ref d 'd) 4)

           (let ([ret-dict (key-rm d 'a 'd)])
             (check-equal? ret-dict (make-immutable-hash '((c . 3) (e . 5)))))

           ;; ensure changes didn't modify in-place
           (check-equal? d (make-immutable-hash '((a . 1) (c . 3) (d . 4) (e . 5))))))
           
;;  (test-case
;;      "key-rm - dict list  - rm single key tests")

;;  (test-case
;;      "key-rm - dict list  - rm multiple keys tests")

  (test-case
      "key-rm - mutable dict - rm single key tests"
    (local [(def d (make-hash '((a . 1) (c . 3) (d . 4) (e . 5))))]
           ;; ensure a's presence
           (check-equal? (dict-ref d 'a) 1)
           ;; returned dict has no 'a entry
           (check-equal? (dict-ref (key-rm d 'a) 'a #f) #f)
           ;; change should persist
           (check-equal? (dict-ref d 'a #f) #f)

           ;;rm non-existing value
           (check-equal? (key-rm d 'f) d)))

  (test-case
      "key-rm - mutable dict - rm multiple keys tests"
    (local [(def d (make-hash '((a . 1) (c . 3) (d . 4) (e . 5))))]
           ;; ensure keys to be rm'ed are present
           (check-equal? (dict-ref d 'a) 1)
           (check-equal? (dict-ref d 'd) 4)
           (check-equal? (dict-ref d 'e) 5)

           ;; check that returned dictionary lacks the rm'ed keys
           (let ([ret-dict (key-rm d 'a 'd 'e)])
             (check-equal? (dict-ref ret-dict 'a #f) #f)
             (check-equal? (dict-ref ret-dict 'd #f) #f)
             (check-equal? (dict-ref ret-dict 'e #f) #f))

           ;; ensure changes were persisted
           (check-equal? (dict-ref d 'a #f) #f)
           (check-equal? (dict-ref d 'd #f) #f)
           (check-equal? (dict-ref d 'e #f) #f)

           ;; rm non-existing values
           (check-equal? (key-rm d 'f 'g 'h) d))))
