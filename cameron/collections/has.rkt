#lang racket/base

(require racket/generic
         cameron/defs
         (only-in racket/dict dict? dict-has-key?)
         (only-in racket/local local)
         (only-in racket/set set? set-member?)
         (only-in racket/string string-contains?))

(provide
 gen:has-able has-able? has-able/c
 has?)

;; Unlike API fn we don't want the tail of the list
(defn -list-has? [l v]
  (if (member v l) #t #f))

(defn -vector-has? [vec val]
  (for/or ([i vec])
    (equal? val i)))

(define-generics has-able
  (has? has-able v)
  #:defaults
  ([set?
    (def has? set-member?)]
   [list?
    (def has? -list-has?)]
   [vector?
    (def has? -vector-has?)]
   [dict?
    (def has? dict-has-key?)]
   [string?
    (def has? string-contains?)]))

(module+ test
  (require rackunit
           racket/set
           racket/dict racket/hash)

  (test-case
      "has? set tests"
    (local [(def s (set 'a 'b 'c 'g))]
           (check-equal? (has? s 'a) #t)
           (check-equal? (has? s 'e) #f)
           (check-equal? (has? s 'g) #t)))

  (test-case
      "has? list tests"
    (local [(def lst (list 10 7 8 3 5))]
           (check-equal? (has? lst 10) #t)
           (check-equal? (has? lst 15) #f)))
  
  (test-case
      "has? vector tests"
    (local [(def v (make-vector 10 0))]
           (vector-set! v 2 3)
           (vector-set! v 4 1)
           (check-equal? (has? v 10) #f)
           (check-equal? (has? v 3) #t)
           (check-equal? (has? v 1) #t)))

  (test-case
      "has? dict tests"
    (local [(def d (make-hash '((a . 1) (c . 3))))]
           (check-equal? (has? d 'a) #t)
           (check-equal? (has? d 'b) #f)
           (hash-set! d 'b 2)
           (check-equal? (has? d 'b) #t)))

  (test-case
      "has? string tests"
    (local [(def str1 "The quick brown fox jumps over the lazy dog")]
           (check-equal? (has? str1 "lazy") #t)
           (check-equal? (has? str1 "lazy dog") #t)
           (check-equal? (has? str1 "quick fox") #f)
           (check-equal? (has? str1 "") #t))))
