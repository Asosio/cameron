#lang racket/base

(require
 racket/contract
 cameron/defs)

(provide
 (contract-out
  [list->listof-pairs (->* (list?) () list?)]
  [list-all-elems? (->* (list? procedure?) () boolean?)]
  [list-any-elems? (->* (list? procedure?) () boolean?)]))

;; test predicate against each element of a list
(defn list-all-elems?
  [lst pred]
  (for/and ([elem lst])
    (pred elem)))

(module+ test
  (require rackunit
           (only-in racket/function conjoin negate))
  (test-case
      "check w. empty lists (should trivially satisfy everything)"
    (check-equal? (list-all-elems? '() (conjoin string? procedure? number?)) #t))

  (test-case
      "check w. single-element lists"
    (check-equal? (list-all-elems? '(1) number?) #t)
    (check-equal? (list-all-elems? (list (fn [] 1)) procedure?) #t)
    (check-equal? (list-all-elems? '(#"byte string") string?) #f)
    (check-equal? (list-all-elems? '("regular string") bytes?) #f))

  (test-case
      "check w. multiple arguments (pos)"
    (check-equal? (list-all-elems? '(1 2 3) number?) #t)
    (check-equal? (list-all-elems? '(1 2 #"asd") (negate string?)) #t)
    (check-equal? (list-all-elems? '(1 2 "3") number?) #f)
    (check-equal? (list-all-elems? (list "weeh" (fn [] 1)) (negate string?)) #f)))

(defn list-any-elems?
  [lst pred]
  (or (= (length lst) 0) ;;apparently for/or yields false for empty lists
      (for/or ([elem lst])
        (pred elem))))

(module+ test
  (require rackunit
           (only-in racket/function conjoin negate))
  (test-case
      "check w. empty lists (should trivially satisfy everything)"
    (check-equal? (list-any-elems? '() (conjoin string? procedure? number?)) #t))

  (test-case
      "check w. single-elem lists"
    (check-equal? (list-any-elems? '("str") string?) #t)
    (check-equal? (list-any-elems? '("asd") number?) #f))

  (test-case
      "check w. multiple elem lists"
    (check-equal? (list-any-elems? '(2 3 "s") string?) #t)
    (check-equal? (list-any-elems? (list 2 (fn [] 1) "3" #"asda") procedure?) #t)))

(defn list->listof-pairs
  [lst]
  ;; reverse the list because building up a long list by cons'ing
  ;; is much faster than continuously appending to a list.
  (with-handlers ([exn:fail:contract?
                   (fn [exn]
                     (raise-arguments-error
                      'list->listof-pairs
                      "Received an uneven number of list elements"))])
    (let loop ([acc '()] [rst (reverse lst)])
      (if (eq? '() rst)
          acc
          (loop (cons (cons (cadr rst) (car rst)) acc) (cddr rst))))))

(module+ test
  (require rackunit
           (only-in racket/local local))

  (test-case
      "list->listof-pairs -- empty list"
    (check-equal? (list->listof-pairs '()) '()
                  "Expected an empty list with no pairs"))

  (test-case
      "list->listof-pairs -- even number of elements"
    (check-equal? (list->listof-pairs '(a 1)) '((a . 1))
                  "Expected a list with a single pair")
    (check-equal? (list->listof-pairs '(a 1 b 2)) '((a . 1) (b . 2))
                  "Expected a list with two pairs")
    (check-equal? (list->listof-pairs '(a 1 b 2 c 3)) '((a . 1) (b . 2) (c . 3))
                  "Expected a list of three pairs"))

  (test-case
      "list->listof-pairs -- uneven number of elements"
    (check-exn exn:fail:contract?
               (fn [] (list->listof-pairs '(a 1 b 2 c))))))
