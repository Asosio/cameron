#lang racket/base

(require racket/generic
         (only-in racket/dict dict-mutable?)
         (only-in racket/set set mutable-set set-mutable?)
         racket/require
         (prefix-in c/ (multi-in cameron/collections [dict set]))
         cameron/defs)

(provide
 gen:key-set key-set? key-set/c
 key-set)

(define-generics key-set
  (key-set key-set k . rst)
  #:defaults
  ([c/dict-immutable?
    (def key-set c/dict-assoc)]
   [c/dict-mutable?
    (def key-set c/dict-assoc!)]
   [c/set-immutable?
    (def key-set c/set-assoc)]
   [c/set-mutable?
    (def key-set c/set-assoc!)]))

;; TODO - missing multiple assignment tests (if wanted)
(module+ test
  (require rackunit
           (only-in racket/local local)
           (only-in racket/dict dict-ref)
           racket/hash)

  ;; key-set - immutable dicts
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (test-case
      "list-as-dict tests"
    (local [(def d (list '((a . 1) (c . 3))))]
           ;; Even though lists are technically mutable a list of pairs
           ;; is implemented to act as an immutable dict.
           (check-equal? (dict-ref (key-set d 'b 2) 'b) 2)
           (check-equal? (dict-ref d 'b #f) #f)
           (check-equal? (dict-ref d 'b #t) #t)))

  (test-case
      "immutable dicts - add an entry"
    (local [(def ih (make-immutable-hash '((a . 1) (d . 4))))]
           ;; add an entry
           (let ([ret-dict (key-set ih 'e 10)])
             (check-equal? ret-dict (make-immutable-hash '((a . 1) (d . 4) (e . 10)))))
           ;; ensure modification is not in-place
           (check-equal? ih (make-immutable-hash '((a . 1) (d . 4))))))

  (test-case
      "immutable dicts - overwrite an entry"
    (local [(def ih (make-immutable-hash '((a . 1) (d . 4))))]
           ;; add an entry
           (let ([ret-dict (key-set ih 'a 10)])
             (check-equal? ret-dict (make-immutable-hash '((a . 10) (d . 4)))))
           ;; ensure modification is not in-place
           (check-equal? ih (make-immutable-hash '((a . 1) (d . 4))))))

  (test-case
      "immutable dicts - add several entries"
    (local [(def ih (make-immutable-hash '((a . 1) (d . 4))))]
           ;; add several entries
           (let ([ret-dict (key-set ih 'd 7 'e 10 'f 25)])
             (check-equal? ret-dict (make-immutable-hash '((a . 1) (d . 7) (e . 10) (f . 25))))
             ;; ensure modification is in-place
             (check-equal? ih (make-immutable-hash '((a . 1) (d . 4)))))))

  ;; key-set - mutable dicts
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (test-case
      "mutable dicts - add an entry"
    (local [(def mh (make-hash '((a . 1) (d . 4))))]
           ;; add an entry
           (let ([ret-dict (key-set mh 'e 10)])
             (check-equal? ret-dict (make-hash '((a . 1) (d . 4) (e . 10))))
             ;; ensure modification is in-place
             (check-equal? mh ret-dict))))

  (test-case
      "mutable dicts - overwrite an entry"
    (local [(def mh (make-hash '((a . 1) (d . 4))))]
           ;; overwrite an entry
           (let ([ret-dict (key-set mh 'a 10)])
             (check-equal? ret-dict (make-hash '((a . 10) (d . 4))))
             ;; ensure modification is in-place
             (check-equal? mh ret-dict))))

  (test-case
      "mutable dicts - add several entries"
    (local [(def mh (make-hash '((a . 1) (d . 4))))]
           ;; add several entries
           (let ([ret-dict (key-set mh 'd 7 'e 10 'f 25)])
             (check-equal? ret-dict (make-hash '((a . 1) (d . 7) (e . 10) (f . 25))))
             ;; ensure modification is in-place
             (check-equal? mh ret-dict))))

  ;; key-set - immutable sets
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (test-case
      "immutable sets - add an entry"
    (local [(def is (set 1 3 5))]
           ;; add an entry
           (let ([ret-set (key-set is 4)])
             (check-equal? ret-set (set 4 1 3 5)))
           ;; ensure modification is not in-place
           (check-equal? is (set 1 3 5))))

  (test-case
      "immutable sets - add several entries"
    (local [(def is (set 1 3 5))]
           ;; add an entry
           (let ([ret-set (key-set is 4 10 15)])
             (check-equal? ret-set (set 4 1 3 5 10 15)))
           ;; ensure modification is not in-place
           (check-equal? is (set 1 3 5))))

  ;; key-set - mutable sets
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (test-case
      "mutable sets - add an entry"
    (local [(def is (mutable-set 1 3 5))]
           ;; add an entry
           (let ([ret-set (key-set is 4)])
             (check-equal? ret-set (mutable-set 4 1 3 5 )))
           ;; ensure modification is in-place
           (check-equal? is (mutable-set 1 3 5 4))))

  (test-case
      "mutable sets - add several entries"
    (local [(def is (mutable-set 1 3 5))]
           ;; add an entry
           (let ([ret-set (key-set is 4 10 15)])
             (check-equal? ret-set (mutable-set 4 1 3 5 10 15)))
           ;; ensure modification is in-place
           (check-equal? is (mutable-set 1 3 5 4 10 15)))))
