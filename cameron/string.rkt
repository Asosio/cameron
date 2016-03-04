#lang racket/base

(require cameron/defs)
(provide str
         string-empty? string-reverse string-slice
         string-indexof-l string-indexof-r)

(def str string-append)

;; return #t iff string 'str' is emptry
(defn string-empty? [str] (equal? str ""))

;; returns the opposite of the input string
;; (e.g. "otto"=>"otto", "palindrome"=>"emordnilap")
(defn string-reverse
  [str]
  (list->string (reverse (string->list str))))

(def string-slice substring)

;; Return index (in string) to the first character of the leftmost (first)
;; occurrence of a character sequence, 'sub' found in string 'str'.
;; If no match is found, #f is returned.
(defn string-indexof-l
  [str sub #:offset [offset 0]]
  (def str-len (- (string-length str) offset))
  (def sub-len (string-length sub))
  (and (>= str-len sub-len)
       (let loop ([start offset])
         (and (<= sub-len (- str-len start))
              (or (let loop2 ([str-off 0])
                    (or (if (= str-off sub-len) start #f)
                        (if (and (char=? (string-ref str (+ start str-off))
                                         (string-ref sub str-off))
                                 (loop2 (add1 str-off)))
                            start #f)))
                  (loop (add1 start)))))))

;; Return index (in string) to the first character of the rightmost (last)
;; occurrence of a character sequence, 'sub', found in string 'str'.
;; If no match is found, #f is returned.
(defn string-indexof-r
  [str sub #:offset [offset 0]]
  (def str-len (string-length str))
  (def sub-len (string-length sub))
  (and (>= (- str-len offset) sub-len)
       (let loop ([end (- str-len offset)])
         (and (<= sub-len end)
              (or (let loop2 ([start 0])
                    (or (if (= start sub-len) (- end start) #f)
                        (if (char=? (string-ref str (- end 1 start))
                                    (string-ref sub (- sub-len 1 start)))
                            (loop2 (add1 start)) #f)))
                  (loop (sub1 end)))))))

(module+ test
  (require rackunit
           (only-in racket/local local))

  (test-case
      "string-indexof-l - single-char matches"
    (local [(def msg "Expected match on single-character substring failed")]
           (check-equal? (string-indexof-l "catfish" "c") 0 msg)
           (check-equal? (string-indexof-l "catfish" "a") 1 msg)
           (check-equal? (string-indexof-l "catfish" "h") 6 msg)))

  (test-case
      "string-indexof-l - multi-char matches"
    (local [(def msg "Expected match on multi-character substring failed")]
           (check-equal? (string-indexof-l "catfish" "ca") 0 msg)
           (check-equal? (string-indexof-l "catfish" "sh") 5 msg)
           (check-equal? (string-indexof-l "catfish" "tfi") 2 msg)))

  (test-case
      "string-indexof-l - match first instance"
    (local [(def msg "failed to correctly identify the first instance to match")]
           (check-equal? (string-indexof-l "there is covariance and contravariance" "variance") 11 msg)
           (check-equal? (string-indexof-l "papaya" "pa" #:offset 2) 2)
           (check-equal? (string-indexof-l "papaya is papaya" "pa" #:offset 3) 10)
           (check-equal?
            (string-indexof-l "This is a poodle of bantha poodoo" "poo" #:offset 11) 27)))

  (test-case
      "string-indexof-l - match multiple instances (offset test)"
    (local [(def msg "function is expected to respect offset arg")]
           (check-equal?
            (string-indexof-l "This is a poodle of bantha poodoo" "poo" #:offset 0) 10 msg)
           (check-equal?
            (string-indexof-l "This is a poodle of bantha poodoo" "poo" #:offset 10) 10 msg)
           (check-equal?
            (string-indexof-l "This is a poodle of bantha poodoo" "poo" #:offset 11) 27 msg)))

  (test-case
      "string-indexof-l - trivially match the empty substring"
    (local [(def msg "should always match the empty substring immediately")]
           (check-equal? (string-indexof-l "catfish" "") 0 msg)))
  
  (test-case
      "string-indexof-l - #f if no matches"
    (local [(def msg "should not have yielded a match")]
           (check-false (string-indexof-l "catfish" "caty") msg)))

  (test-case
      "string-indexof-r - single-char matches"
    (local [(def msg "expected match in single-character substring failed")]
           (check-equal? (string-indexof-r "catfish" "h") 6 msg)
           (check-equal? (string-indexof-r "catfish" "s") 5 msg)
           (check-equal? (string-indexof-r "catfish" "c") 0 msg)))

  (test-case
      "string-indexof-r - multi-char matches"
    (local [(def msg "Expected match on multi-character substring failed")]
      (check-equal? (string-indexof-r "catfish" "sh") 5 msg)
      (check-equal? (string-indexof-r "catfish" "ca") 0 msg)
      (check-equal? (string-indexof-r "catfish" "tfi") 2 msg)))

  (test-case
      "string-indexof-r - match last instance"
    (local [(def msg "failed to correctly identify the last instance to match")]
           (check-equal? (string-indexof-r "there is covariance and contravariance" "variance") 30 msg)
           (check-equal? (string-indexof-r "papaya" "pa") 2 msg)
           (check-equal? (string-indexof-r "papaya" "pa" #:offset 2) 2 msg)
           (check-equal? (string-indexof-r "papaya" "pa" #:offset 3) 0 msg)
           (check-equal? (string-indexof-r "papaya is papaya" "pa" #:offset 5) 2 msg)
           ))

  (test-case
      "string-indexof-r - match multiple instances (offset test)"
    (local [(def msg "function is expected to respect offset arg")]
           (check-equal?
            (string-indexof-r "This is a poodle of bantha poodoo" "poo" #:offset 0) 27)
           (check-equal?
            (string-indexof-r "This is a poodle of bantha poodoo" "poo" #:offset 2) 27)
           (check-equal?
            (string-indexof-r "This is a poodle of bantha poodoo" "poo" #:offset 4) 10)))

  (test-case
      "string-indexof-r - trivially match the empty substring"
    (local [(def msg "should always match the empty substring immediately")]
           (check-equal? (string-indexof-r "catfish" "") 7 msg)))
  
  (test-case
      "string-indexof-r - #f if no matches"
    (local [(def msg "should not have yielded a match")]
           (check-false (string-indexof-r "catfish" "caty") msg))))
