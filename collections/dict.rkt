#lang racket/base

(require
 (only-in racket/list first rest)
 racket/contract
 cameron/defs
 racket/dict)

(provide
 assoc assoc!
 dissoc dissoc!)

(def assoc
  (case-fn
   [(d k v) (dict-set d k v)]
   [(d & kv-args)
    (when (positive? (modulo (length kv-args) 2))
      (raise-arguments-error
       'assoc
       "uneven number of expressions. kv-args must come in <key> <val> pairs."
       "kv-args" kv-args))
    (let loop ([kw kv-args] [d d])
      (if (equal? '() kw)
          d
          (loop (cddr kw) (dict-set d (car kw) (cadr kw)))))]))

(def assoc!
  (case-fn
   [(d k v) (begin (dict-set! d k v) d)]
   [(d & kv-args)
    (when (positive? (modulo (length kv-args) 2))
      (raise-arguments-error
       'assoc!
       "uneven number of expressions. kv-args must come in <key> <val> pairs."
       "kv-args" kv-args))
    (begin (apply dict-set*! d kv-args) d)]))

(def dissoc
  (case-fn
   [(d k) (dict-remove d k)]
   [(d & ks)
    (let loop ([ks ks] [d d])
      (if (equal? '() ks)
          d
          (loop (rest ks) (dict-remove d (first ks)))))]))

(def dissoc!
  (case-fn
   [(d k) (begin (dict-remove! d k) d)]
   [(d & ks)
    (begin (for ([k ks]) (dict-remove! d k)) d)]))

;; TODO - test assoc using wrong num of args - err out (check contract)

(module+ test
  (require rackunit
           (only-in racket/local local)
           racket/hash)

  (test-case
      "assoc - immutable dicts - add an entry"
    (local [(def ih (make-immutable-hash '((a . 1) (d . 4))))]
           ;; add an entry
           (let ([ret-dict (assoc ih 'e 10)])
             (check-equal? ret-dict (make-immutable-hash '((a . 1) (d . 4) (e . 10)))))
           ;; ensure modification is not in-place
           (check-equal? ih (make-immutable-hash '((a . 1) (d . 4))))))

  (test-case
      "assoc - immutable dicts - overwrite an entry"
    (local [(def ih (make-immutable-hash '((a . 1) (d . 4))))]
           ;; add an entry
           (let ([ret-dict (assoc ih 'a 10)])
             (check-equal? ret-dict (make-immutable-hash '((a . 10) (d . 4)))))
           ;; ensure modification is not in-place
           (check-equal? ih (make-immutable-hash '((a . 1) (d . 4))))))

  (test-case
      "assoc - immutable dicts - add several entries"
    (local [(def ih (make-immutable-hash '((a . 1) (d . 4))))]
           ;; add several entries
           (let ([ret-dict (assoc ih 'd 7 'e 10 'f 25)])
             (check-equal? ret-dict (make-immutable-hash '((a . 1) (d . 7) (e . 10) (f . 25))))
             ;; ensure modification is in-place
             (check-equal? ih (make-immutable-hash '((a . 1) (d . 4)))))))

  (test-case
      "dissoc - immutable dicts - remove an entry"
    (local [(def ih (make-immutable-hash '((a . 1) (c . 3) (d . 4))))]
           ;; remove an entry
           (let ([ret-dict (dissoc ih 'd)])
             (check-equal? ret-dict (make-immutable-hash '((a . 1) (c . 3)))))
           ;; ensure modification is not in-place
           (check-equal? ih (make-immutable-hash '((a . 1) (c . 3) (d . 4))))))

  (test-case
      "dissoc - immutable dicts - remove several entries"
    (local [(def ih (make-immutable-hash '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6))))]
           ;; remove several entries
           (let ([ret-dict (dissoc ih 'b 'e 'f)])
             (check-equal? ret-dict (make-immutable-hash '((a . 1) (c . 3) (d . 4))))
             ;; ensure modification is in-place
             (check-equal? ih (make-immutable-hash '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6)))))))
  
  (test-case
      "assoc - mutable dicts - add an entry"
    (local [(def mh (make-hash '((a . 1) (d . 4))))]
           ;; add an entry
           (let ([ret-dict (assoc! mh 'e 10)])
             (check-equal? ret-dict (make-hash '((a . 1) (d . 4) (e . 10))))
             ;; ensure modification is in-place
             (check-equal? mh ret-dict))))

  (test-case
      "assoc - mutable dicts - overwrite an entry"
    (local [(def mh (make-hash '((a . 1) (d . 4))))]
           ;; overwrite an entry
           (let ([ret-dict (assoc! mh 'a 10)])
             (check-equal? ret-dict (make-hash '((a . 10) (d . 4))))
             ;; ensure modification is in-place
             (check-equal? mh ret-dict))))

  (test-case
      "assoc - mutable dicts - add several entries"
    (local [(def mh (make-hash '((a . 1) (d . 4))))]
           ;; add several entries
           (let ([ret-dict (assoc! mh 'd 7 'e 10 'f 25)])
             (check-equal? ret-dict (make-hash '((a . 1) (d . 7) (e . 10) (f . 25))))
             ;; ensure modification is in-place
             (check-equal? mh ret-dict))))

  (test-case
      "dissoc - mutable dicts - remove an entry"
    (local [(def mh (make-hash '((a . 1) (c . 3 ) (d . 4))))]
           ;; remove an entry
           (let ([ret-dict (dissoc! mh 'd)])
             (check-equal? ret-dict (make-hash '((a . 1) (c . 3))))
             ;; ensure modification is in-place
             (check-equal? mh ret-dict))))

  (test-case
      "dissoc - mutable dicts - remove several entries"
    (local [(def mh (make-hash '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6))))]
           ;; remove several entries
           (let ([ret-dict (dissoc! mh 'b 'e 'f)])
             (check-equal? ret-dict (make-hash '((a . 1) (c . 3) (d . 4))))
             ;; ensure modification is in-place
             (check-equal? mh ret-dict)))))
