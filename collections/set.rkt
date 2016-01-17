#lang racket/base

(require
 racket/contract
 (only-in racket/function negate conjoin)
 (only-in racket/list first rest)
 (prefix-in b: (only-in racket/set set? set-mutable? set-add set-add! set-remove set-remove!))
 cameron/defs)

;; TODO - can remove the explicit (s k & ks) setup (case-fn matching solves the case of zero args)
(provide
 set-immutable? set-immutable/c
 set-mutable? set-mutable/c
 (contract-out
  [set-assoc   (->* (b:set? any/c) () #:rest (listof any/c) b:set?)]
  [set-assoc!  (->* (b:set-mutable? any/c) () #:rest (listof any/c) b:set-mutable?)]
  [set-dissoc  (->* (b:set? any/c) () #:rest (listof any/c) b:set?)]
  [set-dissoc! (->* (b:set-mutable? any/c) () #:rest (listof any/c) b:set-mutable?)]))

(def set-immutable?
  (conjoin b:set? (negate b:set-mutable?)))

(def set-immutable/c
  (flat-named-contract 'immutable-set set-immutable?))

(def set-mutable? b:set-mutable?)

(def set-mutable/c
  (flat-named-contract 'mutable-set set-mutable?))

(def set-assoc
  (case-fn
   [(s k) (b:set-add s k)]
   [(s k & ks)
    (let loop ([ks ks] [s (b:set-add s k)])
      (if (equal? '() ks)
          s
          (loop (rest ks) (b:set-add s (first ks)))))]))

(def set-assoc!
  (case-fn
   [(s k) (begin (b:set-add! s k) s)]
   [(s k & ks)
    (b:set-add! s k)
    (let loop ([ks ks])
      (if (equal? '() ks)
          s
          (begin (b:set-add! s (first ks))
                 (loop (rest ks)))))]))

(def set-dissoc
  (case-fn
   [(s k) (b:set-remove s k)]
   [(s k & ks)
    (let loop ([ks ks] [s (b:set-remove s k)])
      (if (equal? '() ks)
          s
          (loop (rest ks) (b:set-remove s (first ks)))))]))

(def set-dissoc!
  (case-fn
   [(s k) (begin (b:set-remove! s k) s)]
   [(s k & ks)
    (b:set-remove! s k)
    (let loop ([ks ks])
      (if (equal? '() ks)
          s
          (begin (b:set-remove! s (first ks))
                 (loop (rest ks)))))]))

(module+ test
  (require rackunit
           (only-in racket/local local)
           racket/set)

  (test-case
      "set-assoc - immutable sets - add an entry"
    (local [(def is (set 1 3 5))]
           ;; add an entry
           (let ([ret-set (set-assoc is 4)])
             (check-equal? ret-set (set 4 1 3 5)))
           ;; ensure modification is not in-place
           (check-equal? is (set 1 3 5))))

  (test-case
      "set-assoc - immutable sets - add several entries"
    (local [(def is (set 1 3 5))]
           ;; add an entry
           (let ([ret-set (set-assoc is 4 10 15)])
             (check-equal? ret-set (set 4 1 3 5 10 15)))
           ;; ensure modification is not in-place
           (check-equal? is (set 1 3 5))))

  (test-case
      "set-assoc! - mutable sets - add an entry"
    (local [(def is (mutable-set 1 3 5))]
           ;; add an entry
           (let ([ret-set (set-assoc! is 4)])
             (check-equal? ret-set (mutable-set 4 1 3 5 )))
           ;; ensure modification is in-place
           (check-equal? is (mutable-set 1 3 5 4))))

  (test-case
      "set-assoc! - mutable sets - add several entries"
    (local [(def is (mutable-set 1 3 5))]
           ;; add an entry
           (let ([ret-set (set-assoc! is 4 10 15)])
             (check-equal? ret-set (mutable-set 4 1 3 5 10 15)))
           ;; ensure modification is in-place
           (check-equal? is (mutable-set 1 3 5 4 10 15))))

  (test-case
      "set-dissoc - immutable sets - remove an entry"
    (local [(def is (set 1 3 5))]
           ;; remove an entry
           (let ([ret-set (set-dissoc is 3)])
             (check-equal? ret-set (set 1 5)))
           ;; ensure modification is not in-place
           (check-equal? is (set 1 3 5))))

  (test-case
      "set-dissoc - immutable sets - remove several entries"
    (local [(def is (set 1 3 5))]
           ;; remove an entry
           (let ([ret-set (set-dissoc is 1 5)])
             (check-equal? ret-set (set 3)))
           ;; ensure modification is not in-place
           (check-equal? is (set 1 3 5))))

  (test-case
      "set-dissoc! - mutable sets - remove an entry"
    (local [(def is (mutable-set 4 1 3 5 10 15))]
           ;; remove an entry
           (let ([ret-set (set-dissoc! is 5)])
             (check-equal? ret-set (mutable-set 4 1 3 10 15)))
           ;; ensure modification is in-place
           (check-equal? is (mutable-set 1 3 4 10 15))))

  (test-case
      "set-dissoc! - mutable sets - remove several entries"
    (local [(def is (mutable-set 4 1 3 5 10 15))]
           ;; remove an entry
           (let ([ret-set (set-dissoc! is 4 10 15)])
             (check-equal? ret-set (mutable-set 1 3 5)))
           ;; ensure modification is in-place
           (check-equal? is (mutable-set 1 3 5)))))
