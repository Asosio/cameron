#lang racket/base

(require
 (only-in racket/function negate conjoin)
 (only-in racket/list first rest)
 (prefix-in b: (only-in racket/dict dict-mutable?))
 (for-syntax racket/base
             syntax/parse
             (only-in cameron/defs def defn)
             (only-in racket cond empty?))
 racket/contract
 (only-in cameron/collections/list list->listof-pairs)
 cameron/defs
 racket/dict)

(provide
 dict-immutable? dict-immutable/c
 dict-mutable? dict-mutable/c
 (contract-out
  [dict         (->* () () #:rest (listof any/c) dict-immutable?)]
  [dict!        (->* () () #:rest (listof any/c) dict-mutable?)]
  [list->dict   (->* ((listof any/c)) () dict-immutable?)]
  [list->dict!  (->* ((listof any/c)) () dict-mutable?)]
  [listof-pairs->dict (->* ((listof pair?)) () dict-immutable?)]
  [listof-pairs->dict! (->* ((listof pair?)) () dict-mutable?)]
  [dict-assoc   (->* (dict? any/c any/c) () #:rest (listof any/c) dict-immutable?)]
  [dict-assoc!  (->* (dict-mutable? any/c any/c) () #:rest (listof any/c) dict-mutable?)]
  [dict-dissoc  (->* (dict? any/c) () #:rest (listof any/c) dict?)]
  [dict-dissoc! (->* (dict-mutable? any/c) () #:rest (listof any/c) dict-mutable?)]
  [dict-merge   (->* (dict-immutable/c) () #:rest (listof dict-immutable/c) dict-immutable/c)]
  [dict-merge!  (->* (dict-mutable/c) () #:rest (listof dict-mutable/c) dict-mutable/c)]))

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
(defn dict [& args]
  (make-immutable-hash (list->listof-pairs args)))

(defn dict! [& args]
  (make-hash (list->listof-pairs args)))

;; list -> dict
(defn list->dict [lst]
  (make-immutable-hash (list->listof-pairs lst)))

(defn list->dict! [lst]
  (make-hash (list->listof-pairs lst)))

;; list of pairs -> dict
(defn listof-pairs->dict [lst]
  (make-immutable-hash lst))

(defn listof-pairs->dict! [lst]
  (make-hash lst))

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

;; assoc & dissoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -dict-merge
  [assoc-fn]
  (def merge-fn
    (case-fn [(d) d]
             [(d1 d2) (let loop ([res d1] [next (dict-iterate-first d2)])
                        (if next (loop (assoc-fn res (dict-iterate-key d2 next)
                                                 (dict-iterate-value d2 next))
                                       (dict-iterate-next d2 next))
                            res))]
             [(d1 & ds) (foldl (fn [e acc] (merge-fn acc e)) d1 ds)]))
  merge-fn)

(defn dict-merge
  [& args]
  (apply (-dict-merge dict-assoc) args))

;;TODO - one bug / inconsistency
;; dict-merge can accept dict!'s as non-first arguments & still work
;; -- but dict-merge! cannot accept dict's as non-first arguments
(module+ test
  (require rackunit
           (only-in racket/local local)
           racket/hash)
  (test-case
      "dict-merge - immutable dict - one entry"
    (local [(def d1 (dict 'a 1 'b 2))
            (def d2 (dict 'c 3))]
           (let ([ret-dict (dict-merge d1 d2)])
             (check-equal? ret-dict (dict 'a 1 'b 2 'c 3)))))
  (test-case
      "dict-merge - immutable dict - several entries"
    (local [(def d1 (dict 'a 1 'b 2))
            (def d2 (dict 'c 3 'd 4 'e 5))]
           (let ([ret-dict (dict-merge d1 d2)])
             (check-equal? ret-dict (dict 'a 1 'b 2 'c 3 'd 4 'e 5)))))

  (test-case
      "dict-merge - immutable dict - multiple dicts"
    (local [(def d1 (dict 'a 1 'b 2))
            (def d2 (dict 'c 3 'd 4))
            (def d3 (dict 'e 5 'f 6 'g 7))
            (def d4 (dict 'h 8 'i 9 'j 10))]
           (let ([ret-dict (dict-merge d1 d2 d3 d4)])
             (check-equal? ret-dict (dict 'a 1 'b 2 'c 3 'd 4 'e 5 'f 6 'g 7 'h 8 'i 9 'j 10)))))

  (test-case
      "dict-merge - immutable dict - merge preference"
    (local [(def d1 (dict 'a 1 'b 2 'c 3 'd 4 'e 5))
            (def d2 (dict 'b "d2" 'c "d2" 'd "d2"))
            (def d3 (dict 'c "d3" 'd "d3"))]
           (let ([ret-dict (dict-merge d1 d2 d3)])
             (check-equal? ret-dict (dict 'a 1 'b "d2" 'c "d3" 'd "d3" 'e 5))))))

(defn dict-merge!
  [& args]
  (apply (-dict-merge dict-assoc!) args))

(module+ test
  (require rackunit
           (only-in racket/local local)
           racket/hash)
  (test-case
      "dict-merge! - mutable dict - one entry"
    (local [(def d1 (dict! 'a 1 'b 2))
            (def d2 (dict! 'c 3))]
           (let ([ret-dict (dict-merge! d1 d2)])
             (check-equal? ret-dict (dict! 'a 1 'b 2 'c 3)))))
  (test-case
      "dict-merge! - mutable dict - several entries"
    (local [(def d1 (dict! 'a 1 'b 2))
            (def d2 (dict! 'c 3 'd 4 'e 5))]
           (let ([ret-dict (dict-merge! d1 d2)])
             (check-equal? ret-dict (dict! 'a 1 'b 2 'c 3 'd 4 'e 5)))))

  (test-case
      "dict-merge! - mutable dict - multiple dicts"
    (local [(def d1 (dict! 'a 1 'b 2))
            (def d2 (dict! 'c 3 'd 4))
            (def d3 (dict! 'e 5 'f 6 'g 7))
            (def d4 (dict! 'h 8 'i 9 'j 10))]
           (let ([ret-dict (dict-merge! d1 d2 d3 d4)])
             (check-equal? ret-dict (dict! 'a 1 'b 2 'c 3 'd 4 'e 5 'f 6 'g 7 'h 8 'i 9 'j 10)))))

  (test-case
      "dict-merge! - mutable dict - merge preference"
    (local [(def d1 (dict! 'a 1 'b 2 'c 3 'd 4 'e 5))
            (def d2 (dict! 'b "d2" 'c "d2" 'd "d2"))
            (def d3 (dict! 'c "d3" 'd "d3"))]
           (let ([ret-dict (dict-merge! d1 d2 d3)])
             (check-equal? ret-dict (dict! 'a 1 'b "d2" 'c "d3" 'd "d3" 'e 5))))))
