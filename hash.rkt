#lang racket
(require
 threading
 (for-syntax (only-in racket/list empty?))
 (only-in racket [hash-ref get]))

(provide get
         make make!
         update update!
         assoc assoc!
         dissoc dissoc!)

(begin-for-syntax
  (define (pair-list lst)
    (define (iter acc lst)
      (cond
        [(empty? lst) acc]
        [else (iter (append acc (list (cons (car lst) (cadr lst))))
                    (cddr lst))]))
    (iter '() lst)))

;; Make mutable hash
;; (mh a 1 b 2) === (make-hash '((a . 1) (b . 2))
(define-syntax (make! stx)
  (define xs (cdr (syntax->list stx)))
  (if (even? (length xs))
      (let ((res (pair-list xs)))
        #`(make-hash (quote #,res)))
      (raise-syntax-error #f
                          "expects an even number of forms"
                          stx
                          #'xs)))

;; Make immutable hash
;; (ih a 1 b 2) === (make-immutabl-hash '((a . 1) (b . 2)))
(define-syntax (make stx)
  (define xs (cdr (syntax->list stx)))
  (if (even? (length xs))
      (let ((res (pair-list xs)))
        #`(make-immutable-hash (quote #,res)))
      (raise-syntax-error #f
                          "expects an even number of forms"
                          stx #'xs)))

;; DONE
;;;;;;;
;; (get h k &def) -- alias hash-ref
;; (update h k f &f-args)
;; (update! h k f &f-args)

;; TODO
;;;;;;;

;; fns to cast between mutable & immutable
;; fns to check hash-type (mutable <> immutable)
;; (get-in h [ks] &def)  -- recursive variant of hash-ref

;; (update-in h [k &ks] f & f-args)

;; (select-keys h [key-seq])
;; merge
;; merge-in

;; 'Updates' a value in a hash, where k is a key and f is a
;; function that will take the old value and any supplied args
;; (f-args) and return the new value. update returns the
;; resulting hash.
;;
;; Note
;; * a new hash is returned - to update in-place, use update!
;; * if k doesn't already exist, the value passed to 'f' will be #f
(define (update h k f . f-args)
  (hash-set h k (apply f (get h k #f) f-args)))

;; Updates a value in a hash, where k is a key and f is a
;; function that will take the old value and any supplied args
;; (f-args) and return the new value. update! returns the
;; resulting hash.
;;
;; Note:
;; * updates are in-place (see 'update' for alternatives)
;; * if k doesn't already exist, the value passed to 'f' will be #f
(define (update! h k f . f-args)
  (hash-set! h k (apply f (get h k #f) f-args))
  h)

(define (assoc h . kv-args)
  (apply hash-set* h kv-args)
  h)

(define (assoc! h . kv-args)
  (apply hash-set*! h kv-args)
  h)

(define dissoc
  (case-lambda
    [(h k) (begin (hash-remove h k) h)]
    [(h . ks)
     (begin (for ([k ks]) (hash-remove h k)) h)]))

(define dissoc!
  (case-lambda
    [(h k) (begin (hash-remove! h k) h)]
    [(h . ks)
     (begin (for ([k ks]) (hash-remove! h k)) h)]))

(require racket/hash)
;;(hash-union)


(module+ test
  (require rackunit)
  (test-case
      ""
    (check-equal?
     (~> #'(make! 'a 1 'b 2) (expand-once) (syntax->datum))
     '(make-hash '(('a . 1) ('b . 2)))))

  (test-case
      ""
    (check-equal?
     (~> #'(make! a 1 b 2) (expand-once) (syntax->datum))
     '(make-hash '((a . 1) (b . 2)))))

  (test-case
      ""
    (check-equal?
     (~> #'(make a 1 b 2) (expand-once) (syntax->datum))
     '(make-immutable-hash '((a . 1) (b . 2)))))

  ;; get
  ;;;;;;
  (test-case
      "get key (existing)"
    (define hm (make! a1 "a1" 'a2 "a2" "a3" "a3"))

    (check-equal? (get hm 'a1 #f) "a1")
    (check-equal? (get hm ''a2 #f) "a2")
    (check-equal? (get hm "a3" #f) "a3"))

  (test-case
      "get key (non-existing)"
    (define hm (make a1 "a1"))
    (check-equal? (get hm 'a3 #f) #f)
    (check-equal? (get hm 'a3 "none") "none"))

  ;; update / update!
  (test-case
      "update - single-arg update-fn"
    (check-equal?
     (let ((hm (make a "a" b "b")))
       (update hm 'a (lambda [ov] "new")))
     (make a "new" b "b")))

  (test-case
      "update - 2-arg update-fn"
    (check-equal?
     (let ((hm (make a "a" b "b")))
       (update hm 'a (lambda [ov a1] (string-append ov a1)) "-new"))
     (make a "a-new" b "b")))

  (test-case
      "update - 3-arg update-fn"
    (check-equal?
     (let ((hm (make a "a" b "b")))
       (update hm 'a (lambda [ov a1 a2] (string-append a1 ov a2)) "<" ">"))
     (make a "<a>" b "b")))

  (test-case
      "update - ensure original isn't modified"
    (check-equal?
     (let ((hm (make a "a" b "b")))
       (update hm 'a (lambda [ov] "x"))
       hm)
     (make a "a" b "b")))

  (test-case
      "update - update a string key"
    (check-equal?
     (let ((hm (make "akey" "a" "bkey" "b")))
       (update hm "bkey" (lambda [ov] "x")))
     (make "akey" "a" "bkey" "x")))

  ;; assume update! deals with this just like update (hence fewer tests)
  (test-case
      "update! - 2-arg update-fn"
    (check-equal?
     (let ((hm (make! a "a" b "b")))
       (update! hm 'a (lambda [ov a1] (string-append ov a1)) "-new"))
     (make! a "a-new" b "b")))

  (test-case
      "update! - ensure original is modified"
    (check-equal?
     (let ((hm (make! a "a" b "b")))
       (update! hm 'b (lambda [ov] (string-append ov "-new")))
       hm)
     (make! a "a" b "b-new")))
  
  )

