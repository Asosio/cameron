#lang racket/base

(require racket/generic
         (only-in racket/set set-count)
         (only-in racket vector-length length dict-count dict? set? set-mutable? set mutable-set)
         cameron/defs)

(provide
 gen:countable countable? countable/c
 empty? len)

;; fallbacks
;; -----------------------------------------------------------------------------
(defn -empty?
  [coll]
  (zero? (len coll)))

;; generic interfaces
;; -----------------------------------------------------------------------------
(define-generics countable
  (empty? countable)
  (len countable)
  #:fallbacks
  [(def empty? -empty?)]
  #:defaults
  ([list?
    (def len length)]
   [dict?
    (def len dict-count)]
   [vector?
    (def len vector-length)]
   [set?
    (def len set-count)]
   [set-mutable?
    (def len set-count)]))

(module+ test
  (require rackunit)

  ;; lists
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (test-case
      "list - len"
    (check-equal? (len '('a 'b 'c)) 3)
    (check-equal? (len '()) 0))

  (test-case
      "list - empty?"
    (check-equal? (empty? '('a 'b 'c)) #f)
    (check-equal? (empty? '()) #t))

  ;; dicts
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (test-case
      "dict - len"
    (check-equal? (len (make-immutable-hash '((a . 1) (b . 2) (c . 45)))) 3)
    (check-equal? (len (make-immutable-hash '((dog . 'canine)))) 1)
    (check-equal? (len (make-immutable-hash '())) 0))

  (test-case
      "dict - empty"
    (check-equal? (empty? (make-immutable-hash '((a . 1) (b . 2) (c . 45)))) #f)
    (check-equal? (empty? (make-immutable-hash '((dog . 'canine)))) #f)
    (check-equal? (empty? (make-immutable-hash '())) #t))

  (test-case
      "mutable dict - len"
    (check-equal? (len (make-hash '((a . 1) (b . 2) (c . 45)))) 3)
    (check-equal? (len (make-hash '((dog . 'canine)))) 1)
    (check-equal? (len (make-hash '())) 0))

  (test-case
      "mutable dict - empty"
    (check-equal? (empty? (make-hash '((a . 1) (b . 2) (c . 45)))) #f)
    (check-equal? (empty? (make-hash '((dog . 'canine)))) #f)
    (check-equal? (empty? (make-hash '())) #t))

  ;; vectors
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (test-case
      "vector - len"
    (check-equal? (len (vector 1 2 3)) 3)
    (check-equal? (len (vector)) 0))

  (test-case
      "vector - empty?"
    (check-equal? (empty? (vector 1 2 3)) #f)
    (check-equal? (empty? (vector)) #t))

  ;; sets
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (test-case
      "set - len"
    (check-equal? (len (set 1 2 3)) 3)
    (check-equal? (len (set 1)) 1)
    (check-equal? (len (set)) 0))

  (test-case
      "set - empty?"
    (check-equal? (empty? (set 1 2 3)) #f)
    (check-equal? (empty? (set 1)) #f)
    (check-equal? (empty? (set)) #t))

  (test-case
      "mutable set - len"
    (check-equal? (len (mutable-set 1 2 3)) 3)
    (check-equal? (len (mutable-set 1)) 1)
    (check-equal? (len (mutable-set)) 0))

  (test-case
      "mutable set - empty?"
    (check-equal? (empty? (mutable-set 1 2 3)) #f)
    (check-equal? (empty? (mutable-set 1)) #f)
    (check-equal? (empty? (mutable-set)) #t)))
              
