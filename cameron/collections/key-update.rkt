#lang racket/base
(require racket/generic
         threading ;; (only-in threading ~>>)
         (only-in racket/dict dict?)
         cameron/defs
         cameron/collections/key-get
         cameron/collections/key-set)

(defn -key-update
  [d k fnc]
  (~>> (key d k #f)
       (fnc)
       (key-set d k)))

(provide
 gen:key-update key-update? key-update/c
 key-update)

(define-generics key-update
  (key-update key-update k fn)
  #:defaults
  ([dict?
    (def key-update -key-update)]))

;; immutable dicts
(module+ test
  (require rackunit
           (only-in racket/local local))

  (test-case "immutable dict - replace existing value"
    (local [(def point #hash((x . 2) (y . 3)))]
           (let ([point2 (key-update point 'x (fn [v] 4))])
             (check-equal? (key point2 'x) 4 "expected x value to be updated to 4")
             (check-equal? (key point2 'y) 3 "expected y to retain its value of 3"))
           (check-equal? (key point 'x) 2 "expected original dict to be unchanged")))

  (test-case "immutable-dict -- 'replace' non-existing value"
    (local [(def point #hash((x . 3) (y . 6)))]
           (let ([point2 (key-update point 'z (fn [v] 9))])
             (check-equal? (key point2 'z) 9 "expected 'z to be set to 9"))
           (check-equal? (key point 'z "unset") "unset" "expected original dict to be unchanged")))

  (test-case "immutable-dict -- examine fn input values"
    (local [(def person #hash((name . "peter") (surname . "pan")))]
           (let ([person2 (key-update person 'name (fn [v] v))])
             (check-equal? (key person2 'name) "peter" "expected argument to update function to be 'peter'"))
           (let ([person3 (key-update person 'age (fn [v] v))])
             (check-equal? (key person3 'age) #f "expected default value for previously unset entries to be #f")))))

;; mutable dicts
(module+ test
  (require rackunit
           (only-in racket/local local))

  (test-case "mutable dict - replace existing value"
    (local [(def point (make-hash '((x . 2) (y . 3))))]
           (let ([point2 (key-update point 'x (fn [v] 4))])
             (check-equal? (key point2 'x) 4 "expected x value to be updated to 4")
             (check-equal? (key point2 'y) 3 "expected y to retain its value of 3"))
           (check-equal? (key point 'x) 4 "expected original dict to be changed")))

  (test-case "mutable-dict -- 'replace' non-existing value"
    (local [(def point (make-hash '((x . 3) (y . 6))))]
           (let ([point2 (key-update point 'z (fn [v] 9))])
             (check-equal? (key point2 'z) 9 "expected 'z to be set to 9"))
           (check-equal? (key point 'z "unset") 9 "expected original dict to be changed")))

  (test-case "mutable-dict -- examine fn input values"
    (local [(def person (make-hash '((name . "peter") (surname . "pan"))))]
           (let ([person2 (key-update person 'name (fn [v] v))])
             (check-equal? (key person2 'name) "peter" "expected argument to update function to be 'peter'"))
           (let ([person3 (key-update person 'age (fn [v] v))])
             (check-equal? (key person3 'age) #f "expected default value for previously unset entries to be #f")))))
