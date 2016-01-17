#lang racket/base

(require
 (only-in racket/list first rest)
 (only-in racket/set set-add set-add! set-remove set-remove!)
 cameron/defs)

;; TODO - can remove the explicit (s k & ks) setup (case-fn matching solves the case of zero args)

(def assoc
  (case-fn
   [(s k) (set-add s k)]
   [(s k & ks)
    (let loop ([ks ks] [s (set-add s k)])
      (if (equal? '() ks)
          s
          (loop (rest ks) (set-add s (first ks)))))]))

(def assoc!
  (case-fn
   [(s k) (begin (set-add! s k) s)]
   [(s k & ks)
    (set-add! s k)
    (let loop ([ks ks])
      (if (equal? '() ks)
          s
          (begin (set-add! s (first ks))
                 (loop (rest ks)))))]))

(def dissoc
  (case-fn
   [(s k) (set-remove s k)]
   [(s k & ks)
    (let loop ([ks ks] [s (set-remove s k)])
      (if (equal? '() ks)
          s
          (loop (rest ks) (set-remove s (first ks)))))]))

(def dissoc!
  (case-fn
   [(s k) (begin (set-remove! s k) s)]
   [(s k & ks)
    (set-remove! s k)
    (let loop ([ks ks])
      (if (equal? '() ks)
          s
          (begin (set-remove! s (first ks))
                 (loop (rest ks)))))]))

(module+ test
  (require rackunit
           (only-in racket/local local)
           racket/set)

  (test-case
      "assoc - immutable sets - add an entry"
    (local [(def is (set 1 3 5))]
           ;; add an entry
           (let ([ret-set (assoc is 4)])
             (check-equal? ret-set (set 4 1 3 5)))
           ;; ensure modification is not in-place
           (check-equal? is (set 1 3 5))))

  (test-case
      "assoc - immutable sets - add several entries"
    (local [(def is (set 1 3 5))]
           ;; add an entry
           (let ([ret-set (assoc is 4 10 15)])
             (check-equal? ret-set (set 4 1 3 5 10 15)))
           ;; ensure modification is not in-place
           (check-equal? is (set 1 3 5))))

  (test-case
      "assoc! - mutable sets - add an entry"
    (local [(def is (mutable-set 1 3 5))]
           ;; add an entry
           (let ([ret-set (assoc! is 4)])
             (check-equal? ret-set (mutable-set 4 1 3 5 )))
           ;; ensure modification is in-place
           (check-equal? is (mutable-set 1 3 5 4))))

  (test-case
      "assoc! - mutable sets - add several entries"
    (local [(def is (mutable-set 1 3 5))]
           ;; add an entry
           (let ([ret-set (assoc! is 4 10 15)])
             (check-equal? ret-set (mutable-set 4 1 3 5 10 15)))
           ;; ensure modification is in-place
           (check-equal? is (mutable-set 1 3 5 4 10 15))))

  (test-case
      "dissoc - immutable sets - remove an entry"
    (local [(def is (set 1 3 5))]
           ;; remove an entry
           (let ([ret-set (dissoc is 3)])
             (check-equal? ret-set (set 1 5)))
           ;; ensure modification is not in-place
           (check-equal? is (set 1 3 5))))

  (test-case
      "dissoc - immutable sets - remove several entries"
    (local [(def is (set 1 3 5))]
           ;; remove an entry
           (let ([ret-set (dissoc is 1 5)])
             (check-equal? ret-set (set 3)))
           ;; ensure modification is not in-place
           (check-equal? is (set 1 3 5))))

  (test-case
      "dissoc! - mutable sets - remove an entry"
    (local [(def is (mutable-set 4 1 3 5 10 15))]
           ;; remove an entry
           (let ([ret-set (dissoc! is 5)])
             (check-equal? ret-set (mutable-set 4 1 3 10 15)))
           ;; ensure modification is in-place
           (check-equal? is (mutable-set 1 3 4 10 15))))

  (test-case
      "dissoc! - mutable sets - remove several entries"
    (local [(def is (mutable-set 4 1 3 5 10 15))]
           ;; remove an entry
           (let ([ret-set (dissoc! is 4 10 15)])
             (check-equal? ret-set (mutable-set 1 3 5)))
           ;; ensure modification is in-place
           (check-equal? is (mutable-set 1 3 5)))))
