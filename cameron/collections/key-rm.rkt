#lang racket/base

(require racket/generic
         (only-in racket/require multi-in)
         (prefix-in b: (only-in racket/set set mutable-set))
         (prefix-in c/ (multi-in cameron/collections [dict set]))
         cameron/defs)

(provide
 gen:key-rm key-rm? key-rm/c
 key-rm)

(define-generics key-rm
  (key-rm key-rm k . ks)
  #:defaults
  ([c/dict-immutable?
    (def key-rm c/dict-dissoc)]
   [c/dict-mutable?
    (def key-rm c/dict-dissoc!)]
   [c/set-immutable?
    (def key-rm c/set-dissoc)]
   [c/set-mutable?
    (def key-rm c/set-dissoc!)]))

(module+ test
  (require rackunit
           (only-in racket/local local)
           (only-in racket/dict dict-ref)
           racket/hash)

  ;; key-rm - immutable dicts
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (test-case
      "immutable dicts - remove an entry"
    (local [(def ih (make-immutable-hash '((a . 1) (c . 3) (d . 4))))]
           ;; remove an entry
           (let ([ret-dict (key-rm ih 'd)])
             (check-equal? ret-dict (make-immutable-hash '((a . 1) (c . 3)))))
           ;; ensure modification is not in-place
           (check-equal? ih (make-immutable-hash '((a . 1) (c . 3) (d . 4))))))

  (test-case
      "immutable dicts - remove several entries"
    (local [(def ih (make-immutable-hash '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6))))]
           ;; remove several entries
           (let ([ret-dict (key-rm ih 'b 'e 'f)])
             (check-equal? ret-dict (make-immutable-hash '((a . 1) (c . 3) (d . 4))))
             ;; ensure modification is in-place
             (check-equal? ih (make-immutable-hash '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6)))))))

  ;; key-rm - mutable dicts
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (test-case
      "mutable dicts - remove an entry"
    (local [(def mh (make-hash '((a . 1) (c . 3 ) (d . 4))))]
           ;; remove an entry
           (let ([ret-dict (key-rm mh 'd)])
             (check-equal? ret-dict (make-hash '((a . 1) (c . 3))))
             ;; ensure modification is in-place
             (check-equal? mh ret-dict))))

  (test-case
      "mutable dicts - remove several entries"
    (local [(def mh (make-hash '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6))))]
           ;; remove several entries
           (let ([ret-dict (key-rm mh 'b 'e 'f)])
             (check-equal? ret-dict (make-hash '((a . 1) (c . 3) (d . 4))))
             ;; ensure modification is in-place
             (check-equal? mh ret-dict))))

  ;; key-rm - immutable sets
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (test-case
      "immutable sets - remove an entry"
    (local [(def is (b:set 1 3 5))]
           ;; remove an entry
           (let ([ret-set (key-rm is 3)])
             (check-equal? ret-set (b:set 1 5)))
           ;; ensure modification is not in-place
           (check-equal? is (b:set 1 3 5))))

  (test-case
      "immutable sets - remove several entries"
    (local [(def is (b:set 1 3 5))]
           ;; remove an entry
           (let ([ret-set (key-rm is 1 5)])
             (check-equal? ret-set (b:set 3)))
           ;; ensure modification is not in-place
           (check-equal? is (b:set 1 3 5))))

  ;; key-rm - mutable sets
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (test-case
      "mutable sets - remove an entry"
    (local [(def is (b:mutable-set 4 1 3 5 10 15))]
           ;; remove an entry
           (let ([ret-set (key-rm is 5)])
             (check-equal? ret-set (b:mutable-set 4 1 3 10 15)))
           ;; ensure modification is in-place
           (check-equal? is (b:mutable-set 1 3 4 10 15))))

  (test-case
      "mutable sets - remove several entries"
    (local [(def is (b:mutable-set 4 1 3 5 10 15))]
           ;; remove an entry
           (let ([ret-set (key-rm is 4 10 15)])
             (check-equal? ret-set (b:mutable-set 1 3 5)))
           ;; ensure modification is in-place
           (check-equal? is (b:mutable-set 1 3 5)))))
