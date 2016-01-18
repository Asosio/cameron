#lang racket/base

(require
 (only-in racket/function negate conjoin)
 (only-in racket/list first rest)
 (prefix-in b: (only-in racket/dict dict-mutable?))
 (for-syntax racket/base
             (only-in cameron/defs def defn)
             (only-in racket cond empty?))
 racket/contract
 cameron/defs
 racket/dict)

(provide
 dict-immutable? dict-immutable/c
 dict-mutable? dict-mutable/c
 dict dict!
 (contract-out
  [dict-assoc   (->* (dict? any/c any/c) () #:rest (listof any/c) dict-immutable?)]
  [dict-assoc!  (->* (dict-mutable? any/c any/c) () #:rest (listof any/c) dict-mutable?)]
  [dict-dissoc  (->* (dict? any/c) () #:rest (listof any/c) dict?)]
  [dict-dissoc! (->* (dict-mutable? any/c) () #:rest (listof any/c) dict-mutable?)]))

;; contracts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def dict-immutable?
  (conjoin dict? (negate b:dict-mutable?)))

(def dict-immutable/c
  (flat-named-contract 'immutable-dict dict-immutable?))

;; if we don't do this, we'll get a contract violation
(def dict-mutable?
  (conjoin dict? b:dict-mutable?))

(def dict-mutable/c
  (flat-named-contract 'mutable-dict b:dict-mutable?))

;; constructor functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-syntax
  ;; convert a list of arguments to a list of cons'ed pairs.
  (defn pair-list [lst]
    (defn iter [acc lst]
      (cond
        [(empty? lst) acc]
        [else (iter (append acc (list (cons (car lst) (cadr lst))))
                    (cddr lst))]))
    (iter '() lst)))

;; Make mutable hash
;; (dict! a 1 b 2) === (make-hash '((a . 1) (b . 2))
(define-syntax (dict! stx)
  (def xs (cdr (syntax->list stx)))
  (if (even? (length xs))
      (let ((res (pair-list xs)))
        #`(make-hash '#,res))
      (raise-syntax-error #f
                          "expects an even number of forms"
                          stx
                          #'xs)))

;; Make immutable hash
;; (dict a 1 b 2) === (make-immutable-hash '((a . 1) (b . 2)))
(define-syntax (dict stx)
  (def xs (cdr (syntax->list stx)))
  (if (even? (length xs))
      (let ((res (pair-list xs)))
        #`(make-immutable-hash '#,res))
      (raise-syntax-error #f
                          "expects an even number of forms"
                          stx #'xs)))

;; assoc & dissoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def dict-assoc
  (case-fn
   [(d k v) (dict-set d k v)]
   [(d & kv-args)
    (when (positive? (modulo (length kv-args) 2))
      (raise-arguments-error
       'dict-assoc
       "uneven number of expressions. kv-args must come in <key> <val> pairs."
       "kv-args" kv-args))
    (let loop ([kw kv-args] [d d])
      (if (equal? '() kw)
          d
          (loop (cddr kw) (dict-set d (car kw) (cadr kw)))))]))

(def dict-assoc!
  (case-fn
   [(d k v) (begin (dict-set! d k v) d)]
   [(d & kv-args)
    (when (positive? (modulo (length kv-args) 2))
      (raise-arguments-error
       'dict-assoc!
       "uneven number of expressions. kv-args must come in <key> <val> pairs."
       "kv-args" kv-args))
    (begin (apply dict-set*! d kv-args) d)]))

(def dict-dissoc
  (case-fn
   [(d k) (dict-remove d k)]
   [(d & ks)
    (let loop ([ks ks] [d d])
      (if (equal? '() ks)
          d
          (loop (rest ks) (dict-remove d (first ks)))))]))

(def dict-dissoc!
  (case-fn
   [(d k) (begin (dict-remove! d k) d)]
   [(d & ks)
    (begin (for ([k ks]) (dict-remove! d k)) d)]))

;; TODO - test dict-assoc using wrong num of args - err out (check contract)

(module+ test
  (require rackunit
           (only-in racket/local local)
           racket/hash)

  (test-case
      "dict-assoc - immutable dicts - add an entry"
    (local [(def ih (make-immutable-hash '((a . 1) (d . 4))))]
           ;; add an entry
           (let ([ret-dict (dict-assoc ih 'e 10)])
             (check-equal? ret-dict (make-immutable-hash '((a . 1) (d . 4) (e . 10)))))
           ;; ensure modification is not in-place
           (check-equal? ih (make-immutable-hash '((a . 1) (d . 4))))))

  (test-case
      "dict-assoc - immutable dicts - overwrite an entry"
    (local [(def ih (make-immutable-hash '((a . 1) (d . 4))))]
           ;; add an entry
           (let ([ret-dict (dict-assoc ih 'a 10)])
             (check-equal? ret-dict (make-immutable-hash '((a . 10) (d . 4)))))
           ;; ensure modification is not in-place
           (check-equal? ih (make-immutable-hash '((a . 1) (d . 4))))))

  (test-case
      "dict-assoc - immutable dicts - add several entries"
    (local [(def ih (make-immutable-hash '((a . 1) (d . 4))))]
           ;; add several entries
           (let ([ret-dict (dict-assoc ih 'd 7 'e 10 'f 25)])
             (check-equal? ret-dict (make-immutable-hash '((a . 1) (d . 7) (e . 10) (f . 25))))
             ;; ensure modification is in-place
             (check-equal? ih (make-immutable-hash '((a . 1) (d . 4)))))))

  (test-case
      "dict-dissoc - immutable dicts - remove an entry"
    (local [(def ih (make-immutable-hash '((a . 1) (c . 3) (d . 4))))]
           ;; remove an entry
           (let ([ret-dict (dict-dissoc ih 'd)])
             (check-equal? ret-dict (make-immutable-hash '((a . 1) (c . 3)))))
           ;; ensure modification is not in-place
           (check-equal? ih (make-immutable-hash '((a . 1) (c . 3) (d . 4))))))

  (test-case
      "dict-dissoc - immutable dicts - remove several entries"
    (local [(def ih (make-immutable-hash '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6))))]
           ;; remove several entries
           (let ([ret-dict (dict-dissoc ih 'b 'e 'f)])
             (check-equal? ret-dict (make-immutable-hash '((a . 1) (c . 3) (d . 4))))
             ;; ensure modification is in-place
             (check-equal? ih (make-immutable-hash '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6)))))))
  
  (test-case
      "dict-assoc - mutable dicts - add an entry"
    (local [(def mh (make-hash '((a . 1) (d . 4))))]
           ;; add an entry
           (let ([ret-dict (dict-assoc! mh 'e 10)])
             (check-equal? ret-dict (make-hash '((a . 1) (d . 4) (e . 10))))
             ;; ensure modification is in-place
             (check-equal? mh ret-dict))))

  (test-case
      "dict-assoc - mutable dicts - overwrite an entry"
    (local [(def mh (make-hash '((a . 1) (d . 4))))]
           ;; overwrite an entry
           (let ([ret-dict (dict-assoc! mh 'a 10)])
             (check-equal? ret-dict (make-hash '((a . 10) (d . 4))))
             ;; ensure modification is in-place
             (check-equal? mh ret-dict))))

  (test-case
      "dict-assoc - mutable dicts - add several entries"
    (local [(def mh (make-hash '((a . 1) (d . 4))))]
           ;; add several entries
           (let ([ret-dict (dict-assoc! mh 'd 7 'e 10 'f 25)])
             (check-equal? ret-dict (make-hash '((a . 1) (d . 7) (e . 10) (f . 25))))
             ;; ensure modification is in-place
             (check-equal? mh ret-dict))))

  (test-case
      "dict-dissoc - mutable dicts - remove an entry"
    (local [(def mh (make-hash '((a . 1) (c . 3 ) (d . 4))))]
           ;; remove an entry
           (let ([ret-dict (dict-dissoc! mh 'd)])
             (check-equal? ret-dict (make-hash '((a . 1) (c . 3))))
             ;; ensure modification is in-place
             (check-equal? mh ret-dict))))

  (test-case
      "dict-dissoc - mutable dicts - remove several entries"
    (local [(def mh (make-hash '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6))))]
           ;; remove several entries
           (let ([ret-dict (dict-dissoc! mh 'b 'e 'f)])
             (check-equal? ret-dict (make-hash '((a . 1) (c . 3) (d . 4))))
             ;; ensure modification is in-place
             (check-equal? mh ret-dict)))))
